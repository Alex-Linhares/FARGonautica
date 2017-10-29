using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Linker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class LookForRelationshipCodelet : Codelet {

		private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The group elements to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement ge1;
		private GroupElement ge2;


		public LookForRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Look for Relationship", urgency, parent, coderack, workspace, slipnet) {

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
		public LookForRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			GroupElement ge1, GroupElement ge2)
			: base("Look for Relationship", urgency, parent, coderack, workspace, slipnet) {
			this.ge1 = ge1;
			this.ge2 = ge2;
		}

		public override void Run() {
			if (ge1 == null) {
				ge1 = workspace.PickRandomGroupElementByRecency();
			}

			if (ge1 == null)
				return;

			if (ge2 == null) {
				ge2 = workspace.PickRandomGroupElementByRecency();
			}

			if (ge2 == null || ge2 == ge1)
				return;

			// Reorder in time if out-of-order. m1 comes first.
			if (ge1.Location > ge2.Location) {
				GroupElement tmp = ge1;
				ge1 = ge2;
				ge2 = tmp;
			}

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, ge1.MinLocation, ge1.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, ge2.MinLocation, ge2.MaxLocation);



			float similarity = workspace.ComputeSimilarity(ge1, ge2, this, coderack);

			double r = Utilities.rand.NextDouble() * 100;

			if (r < similarity) {
				if (similarity > 99.999)
					workspace.AddRelationship(new RelationshipIdentical(ge1, ge2, similarity));
				else
					workspace.AddRelationship(new RelationshipSimilar(ge1, ge2, similarity));

				// Post labeling codelets.
				FormLabelAssignerCodelet flc = new FormLabelAssignerCodelet((int)Urgency, this, coderack, workspace, slipnet, ge1);
				coderack.AddCodelet(flc);
				flc = new FormLabelAssignerCodelet((int)Urgency, this, coderack, workspace, slipnet, ge2);
				coderack.AddCodelet(flc);
			}

		}
	}
}
