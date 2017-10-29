using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Linker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class LookForContourRelationshipCodelet : Codelet {

		private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The group elements to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement ge1;
		private GroupElement ge2;


		public LookForContourRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Look for Contour Relationship", urgency, parent, coderack, workspace, slipnet) {

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
		public LookForContourRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			GroupElement ge1, GroupElement ge2)
			: base("Look for Contour Relationship", urgency, parent, coderack, workspace, slipnet) {
			this.ge1 = ge1;
			this.ge2 = ge2;
		}

		public override void Run() {
			if (ge1 == null) {
				ge1 = workspace.PickRandomGroupByStrength();
			}

			if (ge1 == null)
				return;

			if (ge2 == null) {
				ge2 = workspace.PickRandomGroupByRecencyAndStrength();
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

			// Compute and compare melody contours.
			//TODO! currently we require groups!
			//if (!(ge1 is Group && ge2 is Group))
			//	return;

			// Make sure contours are non-overlapping!!!
			//Group g1 = (Group)ge1;
			//Group g2 = (Group)ge2;
            
//			if (g1.Overlaps(g2))
//				return;

            if (ge1.Overlaps(ge2))
            	return;

			//MelodyContour mc1 = new MelodyContour(g1);
			//MelodyContour mc2 = new MelodyContour(g2);

            MelodyContour mc1 = new MelodyContour(ge1);
            MelodyContour mc2 = new MelodyContour(ge2);

			////////////////
			float similarity = mc1.ComputeSimilarity(mc2);

//			if (similarity < 75)
	//			return;
				
			double r = Utilities.rand.NextDouble() * 100;

			if (r < similarity) {
				workspace.AddRelationship(new RelationshipMelodyContour(ge1, ge2, similarity));
			}
		}
	}
}
