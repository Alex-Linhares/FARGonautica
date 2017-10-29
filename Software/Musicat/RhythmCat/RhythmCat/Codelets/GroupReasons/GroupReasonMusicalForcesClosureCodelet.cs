using System;
using System.Collections.Generic;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 20, true)]
	public class GroupReasonMusicalForcesClosureCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonMusicalForcesClosureCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Musical Forces Closure", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonMusicalForcesClosureCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Musical Forces Closure", urgency, parent, coderack, workspace, slipnet) {
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
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation);
			workspace.RecordCodeletAttentionHistory(this, group.MaxLocation);


			// Check the Larson expectedness vector.
			// Score is high when expectation was relatively high.
			double score;

			Key key = workspace.Key;
			Alphabet alphabet = group.Alphabet;

			if (alphabet == null) {
				alphabet = Alphabet.GetScaleAlphabet(key);
			}

			List<float> expectedness = group.GetNoteExpectednessLarson(key, alphabet);

			int num = expectedness.Count;
			if (num < 2) {
				return;
			}

			if (expectedness[num - 1] > expectedness[num - 2]) {
				score = Math.Min(100, 100 * (expectedness[num - 1] / Constants.MAX_LARSON_EXPECTEDNESS));

				if (score > 25) {
					group.AddGroupReason(new GroupReasonEndMusicalForcesClosure(group, score));
				}
			}
		}
	}
}
