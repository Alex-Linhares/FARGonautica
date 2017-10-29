using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Breaker", CodeletAttribute.CodeletWorkType.Destroy, 20, true)]
	public class DestroyOldAnalogyrCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;


		public DestroyOldAnalogyrCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Destroy Old Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which analogy to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public DestroyOldAnalogyrCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy analogy)
			: base("Destroy Old Analogy", urgency, parent, coderack, workspace, slipnet) {
				this.analogy = analogy;
		}

		public override void Run() {
			if (analogy == null) {
				analogy = workspace.PickRandomAnalogyByWeaknessAndAge();
			}

			if (analogy == null)
				return;

			if (analogy.Strength > Constants.DESTROY_ANALOGIES_BELOW_THRESHOLD || analogy.MaxLocation > workspace.CurrentTime - 4)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, analogy.LHS.MinLocation);
			workspace.RecordCodeletAttentionHistory(this, analogy.LHS.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, analogy.RHS.MinLocation);
			workspace.RecordCodeletAttentionHistory(this, analogy.RHS.MaxLocation);


			// verify analogy is unused.
			foreach (Analogy a in workspace.analogies) {
				if (analogy == a)
					continue;
				foreach (Relationship r in a.relationships) {
					if (r is RelationshipAnalogy) {
						if (((RelationshipAnalogy)r).Analogy == analogy)
							// It's used; return.
							return;
					}
				}
			}

			// Verify analogy isn't supporting a group.
			foreach (Group g in workspace.groups) {
				foreach (GroupReason gr in g.Reasons) {
					if (gr is GroupReasonAnalogySupport) {
						if (((GroupReasonAnalogySupport)gr).Analogy == analogy)
							return;
					}
				}
			}
		

			double x = Utilities.rand.NextDouble() * 100;

			if (x > analogy.Strength) {
				workspace.DropAnalogyNoChecks(analogy);
			}
		}
	}
}
