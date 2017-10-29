using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Linker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class MeasureLinkerCodelet : Codelet {

		private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The measure to examine. If none given, we select randomly.
		/// </summary>
		private Measure measure1;
		private Measure measure2;


		public MeasureLinkerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Measure Linker", urgency, parent, coderack, workspace, slipnet) {

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
		public MeasureLinkerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Measure measure)
			: base("Measure Linker", urgency, parent, coderack, workspace, slipnet) {
			this.measure1 = measure;
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
		public MeasureLinkerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Measure measure1, Measure measure2)
			: base("Measure Linker", urgency, parent, coderack, workspace, slipnet) {
			this.measure1 = measure1;
			this.measure2 = measure2;
		}

		public override void Run() {
			if (measure1 == null) {
				measure1 = workspace.PickRandomMeasureByRecency();
			}

			if (measure1 == null)
				return;

			if (measure2 == null) {
				measure2 = workspace.PickRandomMeasureByRecency();
			}

			if (measure2 == null || measure2 == measure1)
				return;

			// Reorder in time if out-of-order. m1 comes first.
			if (measure1.Location > measure2.Location) {
				Measure tmp = measure1;
				measure1 = measure2;
				measure2 = tmp;
			}

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, measure1.Location);
			workspace.RecordCodeletAttentionHistory(this, measure2.Location);


			float similarity = measure1.ComputeRhythmicSimilarity(measure2);

			double r = Utilities.rand.NextDouble() * 100;

			if (r < similarity) {
				workspace.AddMeasureLink(measure1, measure2, similarity);
				
				// Try to label the measures.
				FormLabelAssignerCodelet c = new FormLabelAssignerCodelet(90, this, coderack, workspace, slipnet, measure1);
				coderack.AddCodelet(c);

				c = new FormLabelAssignerCodelet(60, this, coderack, workspace, slipnet, measure2);
				coderack.AddCodelet(c);
			}
			

		}
	}
}
