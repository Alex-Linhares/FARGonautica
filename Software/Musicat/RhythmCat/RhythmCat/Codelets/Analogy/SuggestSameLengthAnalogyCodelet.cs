using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	/// <summary>
	/// Makes a possible analogy.
	/// </summary>
	[Codelet("Create", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class SuggestSameLengthAnalogyCodelet : Codelet {

		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group sourceGroup;


		public SuggestSameLengthAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Suggest Same Length Analogy", urgency, parent, coderack, workspace, slipnet) {

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
		public SuggestSameLengthAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Group sourceGroup)
			: base("Suggest Same Length Analogy", urgency, parent, coderack, workspace, slipnet) {

			this.sourceGroup = sourceGroup;
		}


		public override void Run() {
			if (sourceGroup == null) {
				// Pick an expected group to start from.
				sourceGroup = workspace.PickRandomGroupByRecencyAndStrength();
			}

			if (sourceGroup == null)
				return;

			// Try to find another group of similar size, near the first group.
			Group otherGroup = workspace.PickRandomGroupByAdjacencyAndSize(sourceGroup);

			if (otherGroup == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, sourceGroup.MinLocation, sourceGroup.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, otherGroup.MinLocation, otherGroup.MaxLocation);
			
			// If size is too different, skip.
			if (Math.Abs(otherGroup.LengthInMeasures - sourceGroup.LengthInMeasures) / (float)sourceGroup.LengthInMeasures > 0.25f)
				return;

			// Sort in order.
			Group LHS, RHS;
			if (sourceGroup.MinLocation < otherGroup.MinLocation) {
				LHS = sourceGroup;
				RHS = otherGroup;
			} else {
				LHS = otherGroup;
				RHS = sourceGroup;
			}

			// Try to add an analogy.
			CreateAnalogyCodelet cac = new CreateAnalogyCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, LHS, RHS);
			coderack.AddCodelet(cac);
		}
	}
}