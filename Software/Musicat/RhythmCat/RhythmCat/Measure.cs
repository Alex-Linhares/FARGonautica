using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	public class Measure : GroupElement, ICloneable {
		/// <summary>
		/// All the notes are stored in this "rhythm" thing too, for historical reasons. TODO: It should be renamed.
		/// </summary>
		public Rhythm rhythm;
		public Measure sourceMeasure;   // link to the orignal "true" measure. null for original measures.
		public int number;

		/// <summary>
		/// In 16ths.
		/// </summary>
		public int MeasureDuration {
			get {
				return rhythm.TotalDuration;
			}
		}

		public override int Location {
			get {
				return number;
			}
		}

		public override int LengthInMeasures {
			get {
				return 1;
			}
		}

		public override int MinLocation {
			get {
				return number;
			}
		}
		public override int MaxLocation {
			get {
				return number;
			}
		}

		public int NumberMod4 {
			get {
				return number % 4; 
			}
		}

		public char NumberDiv4Letter {
			get {
				return (char)((int)'a' + number / 4);
			}
		}

		public string Label {
			get {
				return (NumberMod4 + 1).ToString() + NumberDiv4Letter;
			}
		}

		public Measure(Workspace workspace) : base(workspace) {
			rhythm = new Rhythm();
		}

		/// <summary>
		/// Sets the sourceMeasure to the given measure, and sets all features to unknown.
		/// </summary>
		/// <param name="sourceMeasure"></param>
		public Measure(Measure sourceMeasure)
			: base(sourceMeasure.workspace) {
			rhythm = (Rhythm)sourceMeasure.rhythm.Clone();

			this.sourceMeasure = sourceMeasure;
			this.number = sourceMeasure.number;
		}

		public Measure(Workspace workspace, Rhythm rhythm)
			: base(workspace) {
			this.rhythm = (Rhythm)rhythm.Clone();
		}

		public override string ToString() {
			return rhythm.ToString();
		}

		public object Clone() {
			Measure m = new Measure(workspace, rhythm);
			return m;
		}

		public float ComputeRhythmicSimilarity(Measure m2) {
			int diff = ComputeSymmetricDifference(this.rhythm.AttackPoints, m2.rhythm.AttackPoints);
			int totalAttacks = this.rhythm.AttackPoints.Count + m2.rhythm.AttackPoints.Count;

			return 100 * (1.0f - ((float)diff / totalAttacks));
		}

		private static int ComputeSymmetricDifference(List<int> list, List<int> list2) {
			return CountMissingInts(list, list2) + CountMissingInts(list2, list);
		}

		private static int CountMissingInts(List<int> list, List<int> list2) {
			int count = 0;
			foreach (int i in list) {
				if (!list2.Contains(i))
					count++;
			}
			return count;
		}

		public override int Level {
			get { return 0; }
		}

		public bool IsExactlyEqualTo(Measure m2) {
			return Math.Abs(ComputeRhythmicSimilarity(m2) - 100) < 0.0001;
		}

		public override int Count {
			get {
				return 1;		// TODO: for now, measures have count=1; this means they have no children but count as 1 element in a mapping...
			}
		}

		public override List<Measure> Measures {
			get {
				List<Measure> measures = new List<Measure>();
				measures.Add(this);
				return measures;
			}
		}


		public override double ComputeHappiness(out double weight) {
			weight = 0;	// measures are ignored in happiness computation... unless they are screaming!!
			if (!this.hasParent) {
				weight = Constants.WEIGHT_SCREAMING_MEASURE;	// scream for attention!
			}

			return 0;
		}

		public override bool MatchesRangeOf(GroupElement target) {
			if (target is Group)
				return false;

			return Location == target.Location;
		}

		// Don't copy measures!!
		public override GroupElement DeepCopy() {
			return this;
		}

		public static List<Note> FlattenToNotesNoRests(List<Measure> measures) {
			List<Note> notes = new List<Note>();

			foreach (Measure m in measures) {
				foreach (Note n in m.rhythm.notes) {
					if (n.isRest)
						continue;
					notes.Add(n);
				}
			}
			return notes;
		}

		/// <summary>
		/// Creates an array mapping attackpoint index (position in measure in 16ths) to level #, 
		/// where in 4/4, downbeat = level 4, 1/2 note = level 3, 1/4 note = level 2, 1/8 note = level 1, 1/16 = level 0.
		/// If we're in 3/4, the 3rd beat is at level 2; there is no level 3 in this case.
		/// </summary>
		/// <returns></returns>
		public int[] GetAttackPointLevels() {
			bool beatThreeStrong = false;
			if (MeasureDuration >= 16)
				beatThreeStrong = true;		// hack, works ok for 3/4, 4/4, 8/4, 5/4 (if in 2+3, not 3+2), not good for 6/4 if in 2+2+2, etc.

			int[] levels = new int[MeasureDuration];

			// Make the "ruler".
			for (int i = 0; i < MeasureDuration; i++) {
				int level;
				int pow = 1;
				for (level = 0; level <= 4; level++) {
					pow *= 2;
					if (i % pow != 0)
						break;
				}
				if (level == 3 && !beatThreeStrong)
					level = 2;
				levels[i] = Math.Min(level, 4);		// cap level at 4 for downbeats.
			}

			return levels;
		}

		public void ClearNotes()
		{
			rhythm.ClearNotes();
		}
	}
}
