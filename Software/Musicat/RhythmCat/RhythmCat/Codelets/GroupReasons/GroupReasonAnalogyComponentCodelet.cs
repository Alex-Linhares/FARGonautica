using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, true)]
	public class GroupReasonAnalogyComponentCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;
		private Analogy analogy;

		public GroupReasonAnalogyComponentCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Analogy Component", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonAnalogyComponentCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Analogy Component", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine, with known parent analogies. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public GroupReasonAnalogyComponentCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group, Analogy analogy)
			: base("Group Reason Analogy Component", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
			this.analogy = analogy;
		}

		public override void Run() {
			if (group == null) {
				// Pick a random analogy and post codelets to work on it.
				analogy = workspace.PickRandomAnalogyByRecency();
				if (analogy == null)
					return;

				foreach (Group g in analogy.GetAllSubGroups()) {
					GroupReasonAnalogyComponentCodelet gcc = new GroupReasonAnalogyComponentCodelet((int)this.rawUrgency*2, this, coderack, workspace, slipnet, g);
					coderack.AddCodelet(gcc);						
				}

				// If the analogy has a parent group containing the two sides (and nothing else) add it.
				Group parent = analogy.GetParent();
				if (parent != null) {
					GroupReasonAnalogyComponentCodelet gcc = new GroupReasonAnalogyComponentCodelet(100, this, coderack, workspace, slipnet, parent, analogy);
					coderack.AddCodelet(gcc);
				}

				return;
			}

			if (group == null)
				return;

			if (!workspace.groups.Contains(group))
				return;

			if (!workspace.analogies.Contains(analogy))
				return;

			if (analogy == null) {
				// Find the enclosing analogy.
				List<Analogy> enclosingAnalogies = workspace.FindAllEnclosingAnalogies(group);
				analogy = Utilities.PickItem<Analogy>(enclosingAnalogies);
			}

			if (analogy == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);

			// Add the reason, with strength == analogy strength.
			group.AddGroupReason(new GroupReasonAnalogySupport(group, analogy.Strength, analogy));
		}
	}
}
