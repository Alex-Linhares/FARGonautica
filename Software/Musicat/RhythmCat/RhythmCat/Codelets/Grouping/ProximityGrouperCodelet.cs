using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class ProximityGrouperCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The group elements to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement ge1;
		private GroupElement ge2;


		public ProximityGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Proximity Grouper", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public ProximityGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement ge1)
			: base("Proximity Grouper", urgency, parent, coderack, workspace, slipnet) {

			this.ge1 = ge1;
		}


		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public ProximityGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement ge1, GroupElement ge2)
			: base("Proximity Grouper", urgency, parent, coderack, workspace, slipnet) {

			this.ge1 = ge1;
			this.ge2 = ge2;
		}



		public override void Run() {
			if (ge1 == null) {
				ge1 = workspace.PickRandomGroupElementByRecencyAndStrength();
			}

			if (ge1 == null)
				return;

			if (ge2 == null) {
				// pick one adjacent to g1....
				ge2 = workspace.PickRandomGroupElementAdjacentTo(ge1);
			}

			if (ge2 == null || ge1 == ge2)
				return;

			if (!workspace.GroupElements.Contains(ge1) || !workspace.GroupElements.Contains(ge2))
				return;

			if (ge1.MinLocation > ge2.MinLocation) {
				GroupElement tmp = ge1;
				ge1 = ge2;
				ge2 = tmp;
			}

			// Check if the group elements are adjacent. Otherwise, we can't group.
			int m1 = ge1.MaxLocation;
			int m2 = ge2.MinLocation;
			
			if (m2 - m1 != 1)
				return;

			//Make sure they are both of same level, or we can't group.
			if (ge1.Level != ge2.Level)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, ge1.MinLocation, ge1.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, ge2.MinLocation, ge2.MaxLocation);

			// Check if group already exists!
			// TODO>

			double r = Utilities.rand.NextDouble() * 100;

			// TODO! Score group strenght reasonably.
			// TODODODODODODOODODODODODODO

			// Make tmp group.
			TemporaryGroup tmpG = new TemporaryGroup(workspace, ge1, ge2);
			float score = 50;// 75 / (1 + (Math.Abs(ge2.GroupElements.Count - ge1.GroupElements.Count))); //MetaGroupStrength;

			tmpG.AddGroupReason(new GroupReasonNumberOfSubelements(tmpG, score));


			// Group if the score is strong enough.
			if (r < score) {
				// Check for conflicts, and decide whether to kill conflicting.
				workspace.AddGroup(tmpG);

				// TODO: add reasons....
			}

		}
	}
}
