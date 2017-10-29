using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Expectation", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class GenerateExpectedAnalogyCopyCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The source analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;


		public GenerateExpectedAnalogyCopyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Generate Expected Analogy Copy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which link to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public GenerateExpectedAnalogyCopyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy analogy)
			: base("Generate Expected Analogy Copy", urgency, parent, coderack, workspace, slipnet) {
			this.analogy = analogy;
		}

		public override void Run() {
			if (analogy == null) {
				// Pick a source analogy.
				Group sourceGroup = workspace.expectations.SourceGroup;
				if (sourceGroup == null)
					return;
				analogy = workspace.PickRandomAnalogyInGroup(sourceGroup);
			}
			if (analogy == null)
				return;

			// We have an existing analogy. Check if its copy already exists in expectations.
			int offset = workspace.expectations.Offset;
			int idxLHS1 = analogy.LHS.MinLocation + offset;
			int idxLHS2 = analogy.LHS.MaxLocation + offset;
			int idxRHS1 = analogy.RHS.MinLocation + offset;
			int idxRHS2 = analogy.RHS.MaxLocation + offset;


			foreach (ExpectedAnalogy ea in workspace.expectations.analogies) {
				int idxLHS1e = ea.LHS.MinLocation;
				int idxLHS2e = ea.LHS.MaxLocation;
				int idxRHS1e = ea.RHS.MinLocation;
				int idxRHS2e = ea.RHS.MaxLocation;

				if (idxLHS1 == idxLHS1e && idxLHS2 == idxLHS2e && idxRHS1 == idxRHS1e && idxRHS2 == idxRHS2e)
					return;
			}

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, idxLHS1, idxLHS2);
			workspace.RecordCodeletAttentionHistory(this, idxRHS1, idxRHS2);

			// New expected analogy time!  Try to find the necessary expected group elements.
			ExpectedGroup LHSe = workspace.expectations.FindGroupByLocation(idxLHS1, idxLHS2);
			if (LHSe == null)
				return;

			ExpectedGroup RHSe = workspace.expectations.FindGroupByLocation(idxRHS1, idxRHS2);
			if (RHSe == null)
				return;

			// Create.
			workspace.expectations.MakeNewExpectedAnalogy(LHSe, RHSe, analogy.Strength, analogy.Level);
		}
	}
}
