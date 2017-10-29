using System;
using System.Collections.Generic;
using System.Text;
using RhythmCat.Codelets;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, true)]
	public class AddRelationshipsToAnalogyCodelet : Codelet {


		/// <summary>
		/// The links to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;

		public AddRelationshipsToAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Add Relationships To Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine. 
		/// Otherwise, it picks one randoml.y
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public AddRelationshipsToAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Analogy analogy)
			: base("Add Relationships To Analogy", urgency, parent, coderack, workspace, slipnet) {
			this.analogy = analogy;
		}

		public override void Run() {
			if (analogy == null) {
				analogy = workspace.PickRandomAnalogyByRecencyAndIncompletenessAndSize();
			}

			if (analogy == null)
				return;

			if (!workspace.analogies.Contains(analogy))
				return;

			// Find unmapped elements on left and right.
			List<GroupElement> unmappedLeft;
			List<GroupElement> unmappedRight;
			analogy.GetUnmappedElements(out unmappedLeft, out unmappedRight);
			
			// If there were unmapped elements on both sides, look for relationships.
			if (unmappedLeft.Count == 0 || unmappedRight.Count == 0)
				// At least one side is all mapped! Return.
				return;

			// Look for relationships and try to add to analogy. 
			foreach (Relationship r in workspace.relationships) {
				if (unmappedLeft.Contains(r.LHS) && unmappedRight.Contains(r.RHS)) {
					if (analogy.TryToAddRelationship(r)) {
						// Remove from unmapped if adding was successful.
						unmappedLeft.Remove(r.LHS);
						unmappedRight.Remove(r.RHS);
					}
					// Add to attention history.
					workspace.RecordCodeletAttentionHistory(this, r.LHS.MinLocation, r.LHS.MaxLocation);
					workspace.RecordCodeletAttentionHistory(this, r.RHS.MinLocation, r.RHS.MaxLocation);

				}
			}

			// Try again after scouting more, if there are still unmapped pairs.
			if (unmappedLeft.Count == 0 || unmappedRight.Count == 0)
				// At least one side is all mapped! Returned.
				return;

			// Send out scouts for any left over unmapped segments.
			foreach (GroupElement geLeft in unmappedLeft) {
				foreach (GroupElement geRight in unmappedRight) {
					LookForRelationshipCodelet lrc = new LookForRelationshipCodelet(90, this, coderack, workspace, slipnet, geLeft, geRight);
					coderack.AddCodelet(lrc);
                    
                    if (geLeft is Group && geRight is Group) {
                        LookForMetricPositionRelationshipCodelet lmrc = new LookForMetricPositionRelationshipCodelet(90, this, coderack, workspace, slipnet, (Group)geLeft, (Group)geRight);
					    coderack.AddCodelet(lmrc);
                    }
				}
			}

			// Try to add relationships again later (respawn a codelet just like this one).
			AddRelationshipsToAnalogyCodelet arac = new AddRelationshipsToAnalogyCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, analogy);
			coderack.AddCodelet(arac);

		}

		
	}
}
