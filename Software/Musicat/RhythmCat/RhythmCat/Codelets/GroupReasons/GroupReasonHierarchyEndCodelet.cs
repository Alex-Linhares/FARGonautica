using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 20, true)]
	public class GroupReasonHierarchyEndCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonHierarchyEndCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Hierarchy End", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonHierarchyEndCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Hierarchy End", urgency, parent, coderack, workspace, slipnet) {
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

			// Check if this group ends at a hierarchy border that is good..

			// Find min of the hierarchy strenghts of start and end.

			int endHierarchy = workspace.barlines[0];
			if (workspace.barlines.Count > group.MaxLocation + 1)
				endHierarchy = workspace.barlines[group.MaxLocation + 1];

			// Look for the maximal hierarhcy level inside group (excluding endpoints)
			int max = -1;
			for (int i = group.MinLocation + 1; i <= group.MaxLocation && i < workspace.barlines.Count; i++) {
				if (workspace.barlines[i] > max)
					max = workspace.barlines[i];
			}
			// This one has a hierarchy crossing. No bonus.
			if (max >= endHierarchy)
				return;

			//int diff = endHierarchy - max;

			// compute bonus.
			// TODO!!!! maybe need to check for relatinoship between group level and absolute
			// hiearachy end barline, not just diff from max.................
			//double strength = Math.Min(40 * diff, 100);
			

			double strength = Math.Min(40 * (endHierarchy - group.Level), 100);

			if (strength > 25)
				group.AddGroupReason(new GroupReasonHierarchyEnd(group, strength));
		}
	}
}
