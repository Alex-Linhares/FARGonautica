using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class ExtendGroupRightCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The source group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;


		public ExtendGroupRightCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Extend Group Right", urgency, parent, coderack, workspace, slipnet) {

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
		public ExtendGroupRightCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Group group)
			: base("Generate Expected Group Copy", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
		}

		public override void Run() {
			if (group == null) {
				// Pick a group that needs to be extended 
				List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();
				foreach (Group g in workspace.groups) {
					foreach (GroupReason r in g.Reasons) {
						if (r is GroupReasonExpected) {
							GroupReasonExpected gre = (GroupReasonExpected)r;
							if (g.Level == gre.expectedGroup.Level && g.MinLocation == gre.expectedGroup.MinLocation) {
								// Starts at right spot. If our group ends too early, consider extending.
								if (g.MaxLocation < gre.expectedGroup.MaxLocation && g.MaxLocation < workspace.measures.Count - 1) {
									pairs.Add(new Utilities.ObjectValuePair(g, gre.ReasonStrength));
									break;
								}
							}
						}
					}
				}
				group = (Group)Utilities.PickItemWeighted(pairs);
			}
			if (group != null) {
				// Add to attention history.
				workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation+1);

				workspace.AttemptExtensionRight(group);
			}
		}

	}
}
