using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, false)]
	public class ExamineLinkedPairsCodelet : Codelet {

		private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The pair to examine. If none given, we select randomly.
		/// </summary>
		private MeasureLink link;


		public ExamineLinkedPairsCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Examine Linked Pairs", urgency, parent, coderack, workspace, slipnet) {

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
		public ExamineLinkedPairsCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, MeasureLink link)
			: base("Examine Linked Pairs", urgency, parent, coderack, workspace, slipnet) {
			this.link = link;
		}

		public override void Run() {
			if (link == null) {
				//link = workspace.PickRandomMeasureLinkByRecencyAndStrength();
			}

			if (link == null)
				return;

			if (!workspace.measureLinks.Contains(link))
				return;

			Measure m1 = link.m1;
			Measure m2 = link.m2;

			// Add to attention history.
			//workspace.RecordCodeletAttentionHistory(this, m1.Location);
			//workspace.RecordCodeletAttentionHistory(this, m2.Location);

			/*
						// Post codelets to check out these measures.
						for (int i = 0; i < 2; i++) {
							FeatureScoutCodelet c = new FeatureScoutCodelet(50, this, coderack, workspace, slipnet, m1);
							coderack.AddCodelet(c);
							c = new FeatureScoutCodelet(50, this, coderack, workspace, slipnet, m2);
							coderack.AddCodelet(c);
						}
						*/
		}
	}
}
