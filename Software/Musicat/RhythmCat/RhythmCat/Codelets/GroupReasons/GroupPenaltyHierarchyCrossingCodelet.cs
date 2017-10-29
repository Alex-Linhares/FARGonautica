using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, true)]
	public class GroupPenaltyHierarchyCrossingCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupPenaltyHierarchyCrossingCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Penalty Hierarchy Crossing", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public GroupPenaltyHierarchyCrossingCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Penalty Hierarchy Crossing", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
		}

		public override void Run() {
			if (group == null) {
				group = workspace.PickRandomGroupByRecency();
			}

			if (group == null)
				return;

			if (!workspace.groups.Contains(group))
				return;

			if (workspace.barlines.Count < 1)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);

			// Check if this group crosses a hierarchy border that is greater than those at the group ends.

			// Find min of the hierarchy strenghts of start and end.

			int startHierarchy = 0;
			if (workspace.barlines.Count > group.MinLocation)
				startHierarchy = workspace.barlines[group.MinLocation];
			int endHierarchy = workspace.barlines[0];
			if (workspace.barlines.Count > group.MaxLocation+1)
				endHierarchy = workspace.barlines[group.MaxLocation+1];

			int minHierarchy = Math.Min(startHierarchy, endHierarchy);

			// Look for the maximal hierarhcy level inside group (excluding endpoints)
			int max = -1;
			for (int i = group.MinLocation + 1; i <= group.MaxLocation && i < workspace.barlines.Count; i++) {
				if (workspace.barlines[i] > max)
					max = workspace.barlines[i];
			}
			if (max < minHierarchy)
				return;

			// Penalize based on difference; include a small penalty for same height internally as at ends.
			// This promotes powers-of-two ideas.

			double strength = Constants.PENALTY_PER_LEVEL_HIERARCHY_CROSSING * (max - minHierarchy + 1);

			group.AddGroupPenaltyReason(new GroupPenaltyHierarchyCrossing(group, strength));
		}
	}
}
