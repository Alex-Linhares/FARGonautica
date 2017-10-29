using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 20, true)]
	public class GroupReasonRhythmGapCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonRhythmGapCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Rhythmic Gap", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonRhythmGapCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Rhythmic Gap", urgency, parent, coderack, workspace, slipnet) {
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


			// Check the start and end of the group and compute rhythmic gap scores.
			// Gap scores are based on rhythmic patterns before the first measure or after the last measure.
			float startGapScore, endGapScore;

			int m1index = group.MinLocation;
			// 1st measure automatically has a huge gap score.
			int startMaxGapScore;
			if (m1index < 1) {
				startMaxGapScore = workspace.measures[m1index].MeasureDuration; 
				startGapScore = startMaxGapScore;
			} else {
				startMaxGapScore = workspace.measures[m1index-1].MeasureDuration; 
				startGapScore = ComputeGapScoreBefore(m1index);
			}

			int m2index = group.MaxLocation;
			// last measure automatically has a huge gap score.
			int lastInputMeasureIdx = workspace.measuresInput.Count - 1;
			int lastMeasureIdx = workspace.measures.Count - 1;
			int endMaxGapScore;

			if (m2index > lastInputMeasureIdx - 1) {
				endMaxGapScore = workspace.measures[m2index].MeasureDuration;
				endGapScore = endMaxGapScore;
			} else if (m2index > lastMeasureIdx - 1) {
				endGapScore = 0;
				endMaxGapScore = 1;
			} else {
				endGapScore = ComputeGapScoreBefore(m2index + 1);
				endMaxGapScore = workspace.measures[m2index].MeasureDuration;
			}

			double strengthStart = Math.Min(100.0 * startGapScore / startMaxGapScore, 100);
			double strengthEnd = Math.Min(100.0 * endGapScore / endMaxGapScore, 100);

			if (strengthStart > 25) {
				group.AddGroupReason(new GroupReasonStartAfterGap(group, strengthStart));
			}
			if (strengthEnd > 25) {
				group.AddGroupReason(new GroupReasonEndBeforeGap(group, strengthEnd));
			}
		}

		private float ComputeGapScoreBefore(int m1index) {
			float startGapScore;
			int mprevIndex = m1index-1;
			Measure mPrev = workspace.measures[mprevIndex];
			Measure m2 = workspace.measures[m1index];

			// Temperley gap score = IOI + OOI
			// No rests allowed for now, so just compute IOI

			int gapBefore = 0;
			if (m2.rhythm.AttackPoints.Count > 0)
				gapBefore = m2.rhythm.AttackPoints[0];	// initialize to distance from barline to first note. Now go backwards searching for another attack.

			// Find previous onset .
			while (mPrev.rhythm.AttackPoints.Count == 0) {
				gapBefore += mPrev.MeasureDuration;
				if (mprevIndex > 0) {
					mprevIndex--;
					mPrev = workspace.measures[mprevIndex];
				}
			}
			if (mPrev.rhythm.AttackPoints.Count == 0)
				gapBefore += mPrev.MeasureDuration;
			else
				gapBefore += mPrev.MeasureDuration - mPrev.rhythm.AttackPoints[mPrev.rhythm.AttackPoints.Count - 1];

			startGapScore = gapBefore;

			return startGapScore;

			/*int idxFirstNote = m1.rhythm.AttackPoints.Count - 1;
			if (m2.rhythm.AttackPoints.Count == 0 || m1.rhythm.AttackPoints.Count <= idxFirstNote)
				startGapScore = 16;	// TODO: fix for prev measure duration
			else
				startGapScore = m2.rhythm.AttackPoints[0] + m1.MeasureDuration - m1.rhythm.AttackPoints[idxFirstNote];	// i.e. note2offset + 16 - note1offset
			return startGapScore;*/

		}
	}
}
