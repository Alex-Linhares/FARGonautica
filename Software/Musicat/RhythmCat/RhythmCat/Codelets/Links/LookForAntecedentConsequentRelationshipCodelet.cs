using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Linker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class LookForAntecedentConsequentRelationshipCodelet : Codelet {

		private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The group elements to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement ge1;
		private GroupElement ge2;


		public LookForAntecedentConsequentRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Look for Antecedent-Consequent Relationship", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which measure to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public LookForAntecedentConsequentRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			GroupElement ge1, GroupElement ge2)
			: base("Look for Antecedent-Consequent Relationship", urgency, parent, coderack, workspace, slipnet) {
			this.ge1 = ge1;
			this.ge2 = ge2;
		}

		public override void Run() {
			if (ge1 == null) {
				ge1 = workspace.PickRandomGroupByStrength();
			}

			if (ge1 == null || !(ge1 is Group))
				return;

			Group g1 = (Group)ge1;
			if (ge2 == null) {
				ge2 = workspace.PickRandomGroupAdjacentTo(g1);
			}

			if (ge2 == null || ge2 == ge1 || !(ge2 is Group))
				return;

			Group g2 = (Group)ge2;
			
			// Reorder in time if out-of-order. m1 comes first.
			if (g1.Location > g2.Location) {
				Group tmp = g1;
				g1 = g2;
				g2 = tmp;
			}

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, g1.MinLocation, g1.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, g2.MinLocation, g2.MaxLocation);

			// Make sure groups are non-overlapping!!!
			if (g1.Overlaps(g2))
				return;


			////////////////
			// Check for ending dominant in 1st, tonic in 2nd.
			GroupReason gr1 = null;
			foreach (GroupReason gr in g1.Reasons) {
				if (gr is GroupReasonEndDominant) {
					gr1 = gr;
					break;
				}
			}

			if (gr1 == null)
				return;

			GroupReason gr2 = null;
			foreach (GroupReason gr in g2.Reasons) {
				if (gr is GroupReasonEndTonic) {
					gr2 = gr;
					break;
				}
			}

			if (gr2 == null)
				return;

			// Got a dom->tonic.
			double strength = (gr1.ReasonStrength + gr2.ReasonStrength) / 2;
			
			double r = Utilities.rand.NextDouble() * 100;
			if (r < strength) {
				workspace.AddRelationship( new RelationshipAntecedentConsequentTonality(g1, g2, (float)strength));

				// TODO: specicy analogy or relationship:
				AddRelationshipsToAnalogyCodelet arac = new AddRelationshipsToAnalogyCodelet(100, this, coderack, workspace, slipnet);
				coderack.AddCodelet(arac);
			}
		}
	}
}
