using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 20, true)]
	public class GroupReasonSlowingEndCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonSlowingEndCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Slowing End", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonSlowingEndCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Slowing End", urgency, parent, coderack, workspace, slipnet) {
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

			// Let's see if we are slowing down at the end of the group. 
			// Compare the final measure to the prev measure.
			// Find fastest rhythm in each region.

			// Final measure
			int startMeasure = group.MaxLocation;
			int startBeat = 0;
			int endMeasure = startMeasure;
			int endBeat = workspace.measures[endMeasure].MeasureDuration - 1;

			int fastestFinalMeasure = GetFastestRhythmInRegion(startMeasure, startBeat, endMeasure, endBeat);

			// Second to last measure
			float slowDownRatio = -1;
			if (group.LengthInMeasures > 1) {
				startMeasure = group.MaxLocation-1;
				startBeat = 0;
				endMeasure = startMeasure;
				endBeat = workspace.measures[endMeasure].MeasureDuration - 1;
				int fastestPrevMeasure = GetFastestRhythmInRegion(startMeasure, startBeat, endMeasure, endBeat);
				slowDownRatio = ((float)fastestFinalMeasure) / fastestPrevMeasure;
			}
			
			// Compare the final half-measure to the previous half measure.
			startMeasure = group.MaxLocation;
			startBeat = workspace.measures[endMeasure].MeasureDuration / 2;
			endMeasure = startMeasure;
			endBeat = workspace.measures[endMeasure].MeasureDuration - 1;
			int fastestLastHalfMeasure = GetFastestRhythmInRegion(startMeasure, startBeat, endMeasure, endBeat);

			startBeat = 0;
			endBeat = workspace.measures[endMeasure].MeasureDuration/2 - 1;
			int fastestPrevHalfMeasure = GetFastestRhythmInRegion(startMeasure, startBeat, endMeasure, endBeat);

			float slowDownRatioHalfMeasure = ((float)fastestLastHalfMeasure) / fastestPrevHalfMeasure;


			float score = 0;
			if (slowDownRatio > 1) {
				score += slowDownRatio * 25; ;
			}
			if (slowDownRatioHalfMeasure > 1) {
				score += slowDownRatioHalfMeasure * 10;
			}

			if (score > 100)
				score = 100;


			if (score > 25) {
				group.AddGroupReason(new GroupReasonSlowingEnd(group, score));
			}
		}

		// Returns the fastest rhythm in the region (in 16th notes).
		private int GetFastestRhythmInRegion(int startMeasure, int startBeat, int endMeasure, int endBeat) {
			int min = 32;
			for (int mIdx = startMeasure; mIdx <= endMeasure; mIdx++) {
				Measure m = workspace.measures[mIdx];
				foreach (int attackPoint in m.rhythm.AttackPoints) {
					// Make sure this attack point is in-range, If so, include it.
					if (mIdx == endMeasure && attackPoint > endBeat)
						break;
					if (mIdx == startMeasure && attackPoint < startBeat)
						continue;
					int dur = m.rhythm.GetDurationIncludingTiesAfterByAttackPoint(attackPoint);
					if (dur < min)
						min = dur;
				}
			}
			return min;
		}
	}
}
