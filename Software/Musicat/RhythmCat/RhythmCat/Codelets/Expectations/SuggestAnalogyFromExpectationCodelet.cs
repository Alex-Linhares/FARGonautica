using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

		/// <summary>
		/// Makes a link based on an expected link.
		/// </summary>
		[Codelet("Create", CodeletAttribute.CodeletWorkType.Create, 20, true)]
		public class SuggestAnalogyFromExpectationCodelet : Codelet {


			/// <summary>
			/// The link to examine. If none given, we select randomly.
			/// </summary>
			private ExpectedAnalogy eanalogy;


			public SuggestAnalogyFromExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
				: base("Suggest Analogy From Expectation", urgency, parent, coderack, workspace, slipnet) {

			}

			/// <summary>
			/// Use this constructer to tell the codelet which analogy to examine. 
			/// Otherwise, it picks randomly.
			/// </summary>
			/// <param name="urgency"></param>
			/// <param name="parent"></param>
			/// <param name="coderack"></param>
			/// <param name="workspace"></param>
			/// <param name="slipnet"></param>
			/// <param name="notes"></param>
			public SuggestAnalogyFromExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, ExpectedAnalogy eanalogy)
				: base("Suggest Analogy From Expectation", urgency, parent, coderack, workspace, slipnet) {

					this.eanalogy = eanalogy;
			}


			public override void Run() {
				if (eanalogy == null) {
					// Pick an expected analogy where the RHS is not in the future.
					eanalogy = workspace.expectations.PickRandomAnalogyWithMaxEndTime(workspace.measures.Count - 1);
				}

				if (eanalogy == null)
					return;

				// Add to attention history.
				workspace.RecordCodeletAttentionHistory(this, eanalogy.LHS.MinLocation, eanalogy.LHS.MaxLocation);
				workspace.RecordCodeletAttentionHistory(this, eanalogy.RHS.MinLocation, eanalogy.RHS.MaxLocation);

				// Look for LHS and RHS.
				GroupElement LHS = workspace.FindGroupElement(eanalogy.LHS.MinLocation, eanalogy.LHS.MaxLocation);
				if (LHS == null)
					return;

				GroupElement RHS = workspace.FindGroupElement(eanalogy.RHS.MinLocation, eanalogy.RHS.MaxLocation);
				if (RHS == null)
					return;

				// Try to add an analogy.
				CreateAnalogyCodelet cac = new CreateAnalogyCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, LHS, RHS);
				coderack.AddCodelet(cac);
			}
		}
}