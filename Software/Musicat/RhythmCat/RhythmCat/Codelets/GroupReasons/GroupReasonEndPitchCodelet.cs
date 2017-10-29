using System;
using System.Collections.Generic;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 20, true)]
	public class GroupReasonEndPitchCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonEndPitchCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason End Pitch", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonEndPitchCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason End Pitch", urgency, parent, coderack, workspace, slipnet) {
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


			// Check the final note of the rhythm.
			// Score based on the duration and tonic/dominantness of the pitch.

			Measure m = group.Measures[group.Measures.Count-1];
			Note n = m.rhythm.notes[m.rhythm.notes.Count-1];
			if (n.midiInfo == null)
				return;

			ScaleDegree degree = n.midiInfo.GetScaleDegree(new Key());	// assume C major.
			int dur = n.duartionIncludingTiesAfter;

			double score = 0;
				
			// check for minor/chromatic.
			if (degree == null) {
				// maybe its minor (TODO: crude)
				degree = n.midiInfo.GetScaleDegree(new Key());	// assume C minor.
			}

			// maybe it's chromatic
			if (degree == null) {
				return;
			} else {
				switch (degree.Number) {
					case 1: // tonic
						score = 100;

						break;
					case 5: // dominant
						score = 100;
						break;

					case 2:	// supertonic
						score = 30;
						break;

					default:
						return;
				}
			}

			// Now average in harmonic context of pitch, if available.
            double str1 = group.AlphabetStrength;
            double str2 = m.AlphabetStrength;
            Alphabet alphabet = null;
            if (str1 >= str2)
                alphabet = group.Alphabet;
            else
                alphabet = m.Alphabet;
            if (alphabet != null) {
                // Average in 0 or 100 points depending on stability
                if (alphabet.isStable(degree)) {
                    score = (score + 100) / 2;
                } else {
                    score = (score + 0) / 2;
                }
            }

			// Duration
			if (dur < 8)
				score = score * (dur / 8.0);

			if (score > 25) {
				if (degree.Number == 1)
					group.AddGroupReason(new GroupReasonEndTonic(group, score));
				else
					group.AddGroupReason(new GroupReasonEndDominant(group, score));
			}
		}

		private float ComputeGapScoreBefore(int m1index) {
			float startGapScore;
			int mprevIndex = m1index - 1;
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
