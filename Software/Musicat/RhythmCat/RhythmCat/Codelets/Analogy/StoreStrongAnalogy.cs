using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	/// <summary>
	/// Makes a possible analogy.
	/// </summary>
	[Codelet("Create", CodeletAttribute.CodeletWorkType.Create, 50, true)]
	public class StoreStrongAnalogy : Codelet {

		/// <summary>
		/// The analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;


		public StoreStrongAnalogy(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Store Strong Analogy", urgency, parent, coderack, workspace, slipnet) {

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
		public StoreStrongAnalogy(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy analogy)
			: base("Store Strong Analogy", urgency, parent, coderack, workspace, slipnet) {

			this.analogy = analogy;
		}


		public override void Run() {
			if (analogy == null) {
				// Pick a strong analogy
				analogy = workspace.PickRandomAnalogyByRecencyAndStrengthAndSize();
			}

			if (analogy == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, analogy.LHS.MinLocation, analogy.LHS.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, analogy.RHS.MinLocation, analogy.RHS.MaxLocation);

			// If strong and big enough, add!
			if (analogy.Strength > Constants.MIN_ANALOGY_STRENGTH_LTM && analogy.LengthInMeasures >= Constants.MIN_ANALOGY_LENGTH_LTM) {
				workspace.structureCollection.AddAnalogy(analogy);
			}
		}
	}
}