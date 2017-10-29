using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Analogy", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class CreateAnalogyCodelet : Codelet {

		/// <summary>
		/// The group elements to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement ge1;
		private GroupElement ge2;


		public CreateAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Create Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public CreateAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, 
			GroupElement ge1, GroupElement ge2)
            : base("Create Analogy", urgency, parent, coderack, workspace, slipnet) {
			this.ge1 = ge1;
			this.ge2 = ge2;
		}

		public override void Run() {
			// If groups are not specified already,
			// look for two groups that are linked.
			if (ge1 == null || ge2 == null || !workspace.GroupElements.Contains(ge1) || !workspace.GroupElements.Contains(ge2)) {
				// Pick a link.
				Relationship r = workspace.PickRandomRelationshipByRecencyAndStrength();
				if (r == null)
					return;
				ge1 = r.LHS;
				ge2 = r.RHS;

				// We prefer an analogy between parent groups containing the ends of this link.
				//Alternately, just try to find analogies when starting-relatinoships are found/

				// If both have parents, spawn a codelet to look at that!
				// If they don't have parents, try to make groups including them.

				if (ge1.hasParent && ge2.hasParent) {
					if (ge1.parentGroup != ge2.parentGroup) {
						CreateAnalogyCodelet cac = new CreateAnalogyCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, ge1.parentGroup, ge2.parentGroup);
						coderack.AddCodelet(cac);
					}
				} else {
					MetaGrouperCodelet mgc1 = new MetaGrouperCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, ge1);
					MetaGrouperCodelet mgc2 = new MetaGrouperCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, ge2);
					coderack.AddCodelet(mgc1);
					coderack.AddCodelet(mgc2);
				}

				// Now try to work with this relationship as well, just in case it's at a good level and has good support.
			}

			if (ge1 == null || ge2 == null || ge1 == ge2 || !workspace.GroupElements.Contains(ge1) || !workspace.GroupElements.Contains(ge2))
				return;

			if (ge1.Location > ge2.Location) {
				GroupElement tmp = ge1;
				ge1 = ge2;
				ge2 = tmp;
			}

			// Make analogies between single measures?
			if (Constants.MAKE_SINGLE_MEASURE_ANALOGIES) {
				if (!(ge1 is Group && ge2 is Group))
					return;
			}

			// Check for redundant (identical) analogies!
			foreach (Analogy a2 in workspace.analogies)
				if (a2.LHS == ge1 && a2.RHS == ge2)
					return;

			// Make sure the 2 sides of the analogy span distinct time intervals (no mapping of m. 1-3 onto m.1-5, for instance)
			if (ge1.MaxLocation >= ge2.MinLocation)
				return;

			
			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, ge1.MinLocation, ge1.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, ge2.MinLocation, ge2.MaxLocation);

			// So now we have 2 group elements, and we want to consider an analogy between them.
			// Look for a relationship between these elements (if bottom-level) or their subelements (if they have children)
			Analogy a = new Analogy(ge1, ge2, workspace);
			foreach (Relationship r in workspace.relationships) {
				// Check if this relatinoship is relevant.
				if (ge1 is Group) {
					Group g1 = (Group)ge1;
					if (!g1.GroupElements.Contains(r.LHS))
						continue;
				} else {
					if (r.LHS != ge1)
						continue;
				}
				if (ge2 is Group) {
					Group g2 = (Group)ge2;
					if (!g2.GroupElements.Contains(r.RHS))
						continue;
				} else {
					if (r.RHS != ge2)
						continue;
				}

				// Inside an analogy, we can only have one relationship (of normal similarity type) for each measure. For multiple ones, have them compete.
				// Compete against each conflicting relationship. Only add if it beats them all.
				bool won = true;
				List<Relationship> conflicting = a.FindConflictingRelationships(r);
				foreach (Relationship r2 in conflicting) {
					if (FightItOut(r, r2) == r2) {
						won = false;
						break;
					}
				}
				if (!won)
					continue;

				foreach (Relationship r2 in conflicting)
					a.relationships.Remove(r2);

				a.TryToAddRelationship(r);
			}

			// Make sure we were able to add something.
			if (a.relationships.Count == 0)
				return;


			// Create analogy if it's strong enough. Then other codelets will strengthen it and use it if necessary.. this
			// codelet just starts it up.
			double rnd = Utilities.rand.NextDouble() * 100;
			double score = a.Strength;

			if (rnd < score) {
				if (workspace.AddAnalogy(a)) {
					// Spawn more codelets to improve this analogy.
					AddRelationshipsToAnalogyCodelet arac = new AddRelationshipsToAnalogyCodelet(100, this, coderack, workspace, slipnet, a);
					
					// Consider a metagroup if the two items in the analogy are neighbors.
					if (ge1.MaxLocation + 1 == ge2.MinLocation) {
						MetaGrouperCodelet mgc = new MetaGrouperCodelet(100, this, coderack, workspace, slipnet, ge1, ge2, a);
						coderack.AddCodelet(mgc);
					}

					// Improve strength of subgroups.
					SpawnAnalogyReasonCodeletsRecur(ge1, a);
					SpawnAnalogyReasonCodeletsRecur(ge2, a);

					// Add contour analysis for LHS-RHS.
					LookForContourRelationshipCodelet lrc = new LookForContourRelationshipCodelet(100, this, coderack, workspace, slipnet, ge1, ge2);
					coderack.AddCodelet(lrc);
					AddRelationshipsToAnalogyCodelet arc = new AddRelationshipsToAnalogyCodelet(50, this, coderack, workspace, slipnet, a);
					coderack.AddCodelet(arc);

				}


			}

		}

		private void SpawnAnalogyReasonCodeletsRecur(GroupElement ge, Analogy a) {
			if (!(ge is Group))
				return;
			GroupReasonAnalogyComponentCodelet gcc = new GroupReasonAnalogyComponentCodelet(100, this, coderack, workspace, slipnet, (Group)ge, a);
			coderack.AddCodelet(gcc);

			foreach (GroupElement ge2 in ((Group)ge).GroupElements)
				SpawnAnalogyReasonCodeletsRecur(ge2, a);
		}

		private Relationship FightItOut(List<Relationship> fighting) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < fighting.Count; i++) {
				Relationship r = fighting[i];

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(r, r.Strength);
				pairs.Add(pair);
			}

			return (Relationship)Utilities.PickItemWeighted(pairs);
		}

		private Relationship FightItOut(Relationship r1, Relationship r2 ) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			pairs.Add(new Utilities.ObjectValuePair(r1, r1.Strength));
			pairs.Add(new Utilities.ObjectValuePair(r2, r2.Strength));

			return (Relationship)Utilities.PickItemWeighted(pairs);
		}
	}
}
