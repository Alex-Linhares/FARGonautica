using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Breaker", CodeletAttribute.CodeletWorkType.Create, 5, true)]
	public class GroupBreakerCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;


		public GroupBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Breaker", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Group g)
			: base("Group Breaker", urgency, parent, coderack, workspace, slipnet) {
			this.group = g;
		}

		public override void Run() {
			if (group == null) {
				group = workspace.PickRandomGroupByRecencyAndStrength();
			}

			if (group == null)
				return;

			if (!workspace.groups.Contains(group))
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);


			double r = Utilities.rand.NextDouble() * 100;
			
			double groupStrength =  group.ComputeStrength();

			// Only break weak groups for now.
			if (groupStrength > 50)
				return;

			// Group if the group is strong enough to withstand breaking.
			if (r > groupStrength) {
				// Great! We want to break it. However, if it has a parent it's immune, so try to break parent first, and re-spawn breaker later.
				if (group.hasParent) {
					GroupBreakerCodelet gbc = new GroupBreakerCodelet((int)(Math.Min(100, this.rawUrgency*2)), this, coderack, workspace, slipnet, group.parentGroup);
					coderack.AddCodelet(gbc);
					GroupBreakerCodelet gbc2 = new GroupBreakerCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet, group);
					coderack.AddCodelet(gbc2);
				} else {
					// Try to break!
					workspace.BreakGroup(group);
				}
			}

		}
	}
}
