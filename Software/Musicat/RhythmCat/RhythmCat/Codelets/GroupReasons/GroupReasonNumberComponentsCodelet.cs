using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, false)]
	public class GroupReasonNumberComponentsCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonNumberComponentsCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Number Components", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonNumberComponentsCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Number Components", urgency, parent, coderack, workspace, slipnet) {
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

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);

			
			// Check out the # of subcomponents of this group, and if it's a good number (2,3, or 4), add a group reason to the group.
			int num = group.GroupElements.Count;
			
			if (num < 2 || num > 4) 
				return;

			double strength = 100.0 - (num - 2) * 33.3;

			group.AddGroupReason(new GroupReasonNumberOfSubelements(group, strength));
		}
	}
}
