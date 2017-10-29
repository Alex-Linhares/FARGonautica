using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class MeasureSamenessGrouperCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The Link to examine. If none given, we select randomly.
		/// </summary>
		private MeasureLink link;


		public MeasureSamenessGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Measure Sameness Grouper", urgency, parent, coderack, workspace, slipnet) {

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
		public MeasureSamenessGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, MeasureLink link)
			: base("Measure Sameness Grouper", urgency, parent, coderack, workspace, slipnet) {
			this.link = link;
		}

		public override void Run() {
			if (link == null) {
				link = workspace.PickRandomMeasureLinkByRecencyAndStrength();
			}

			if (link == null)
				return;

			Measure m1 = link.m1;
			Measure m2 = link.m2;

			// Check if the measures are adjacent. Otherwise, we can't group.
			if (Math.Abs(m1.number - m2.number) != 1)
				return;

			// Check if either measure is in another group. If so, we can't group.
			//if (m1.hasParent || m2.hasParent)
			//	return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, m1.Location);
			workspace.RecordCodeletAttentionHistory(this, m2.Location);

			double r = Utilities.rand.NextDouble() * 100;

			// Try to group if the link is strong enough.
			if (r < link.strength) {
				//Group g = workspace.CreateAndAddGroup(m1, m2);

				TemporaryGroup tmpG = new TemporaryGroup(workspace, m1, m2);

				// Record the grouping reason.
				GroupReason reason;
				// Are measures identical?
				if (m1.IsExactlyEqualTo(m2))
					reason = new GroupReasonComponentsIdentical(tmpG);
				else
					reason = new GroupReasonComponentsSimilar(tmpG, link.strength);
				tmpG.AddGroupReason(reason);

				// Try to add!
				workspace.AddGroup(tmpG);
			}

		}
	}
}
