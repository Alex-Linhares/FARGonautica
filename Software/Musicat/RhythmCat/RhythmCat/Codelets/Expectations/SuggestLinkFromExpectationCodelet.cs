    using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	/// <summary>
	/// Makes a link based on an expected link.
	/// </summary>
	[Codelet("Create", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class SuggestLinkFromExpectationCodelet : Codelet {


		/// <summary>
		/// The link to examine. If none given, we select randomly.
		/// </summary>
		private ExpectedMeasureLink elink;


		public SuggestLinkFromExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Suggest Link From Expectation", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public SuggestLinkFromExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, ExpectedMeasureLink elink)
			: base("Suggest Link From Expectation", urgency, parent, coderack, workspace, slipnet) {

			this.elink = elink;
		}


		public override void Run() {
			if (elink == null) {
				// Pick a link ending now.
				elink = workspace.expectations.PickRandomLinkWithEndTime(workspace.measures.Count - 1);
			}

			if (elink == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, elink.m1.Location, elink.m2.Location);

			// Get the real measures for this expectation.
			Measure m1 = workspace.measures[elink.m1.Location];
			Measure m2 = workspace.measures[elink.m2.Location];

			// Make new codelet to link!
			MeasureLinkerCodelet mlc = new MeasureLinkerCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, elink.m1, elink.m2);
			coderack.AddCodelet(mlc);
		}
	}
}
