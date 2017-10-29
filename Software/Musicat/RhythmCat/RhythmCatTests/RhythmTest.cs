using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

using RhythmCat;
using MusicPrimitives;

namespace RhythmCatTests {
	[TestClass]
	public class RhythmTest {
		//[TestMethod]
		public void TestAdd() {
			Rhythm r = new Rhythm();
			//r.AddNote(5);
			//r.AddNote(3);
			//r.AddNote(4);

			Assert.AreEqual(r[0], 3);
			Assert.AreEqual(r[1], 4);
			Assert.AreEqual(r[2], 5);

			Assert.AreEqual(r.Count, 3);
		}

		//[TestMethod]
		public void TestDurations() {
			Rhythm r = new Rhythm();
			//r.AddNote(5);
			//r.AddNote(1);
			//r.AddNote(11);


			Assert.AreEqual(r.Durations[0], 4);
			Assert.AreEqual(r.Durations[1], 6);

			Assert.AreEqual(r.Durations.Count, 2);
		}

		[TestMethod]
		public void TestParseMeasures() {
			int upbeatOffset;
			Key key;

			Workspace w = new Workspace(-1, false, -1);
			List<Measure> measures = Rhythm.ParseRhythmMeasures(w, "Q | QQQ_ | QQQ | H_E.S", out upbeatOffset, out key);

			Assert.AreEqual(0, key.Fifths);
			Assert.AreEqual(KeyMode.Major, key.Mode);
			Assert.AreEqual(4, measures.Count);
			Assert.AreEqual(4, upbeatOffset);

			Measure m0 = measures[0];
			Measure m1 = measures[1];
			Measure m2 = measures[2];
			Measure m3 = measures[3];

			Assert.AreEqual(4, m0.MeasureDuration);
			Assert.AreEqual(12, m1.MeasureDuration);
			Assert.AreEqual(12, m2.MeasureDuration);
			Assert.AreEqual(12, m3.MeasureDuration);

			Assert.AreEqual(1, m0.rhythm.AttackPoints.Count);
			Assert.AreEqual(3, m1.rhythm.AttackPoints.Count);
			Assert.AreEqual(2, m2.rhythm.AttackPoints.Count);
			Assert.AreEqual(4, m2.rhythm.AttackPoints[0]);
			Assert.AreEqual(8, m2.rhythm.AttackPoints[1]);

			Assert.AreEqual(2, m3.rhythm.AttackPoints.Count);
			Assert.AreEqual(0, m3.rhythm.AttackPoints[0]);
			Assert.AreEqual(11, m3.rhythm.AttackPoints[1]);

			Assert.AreEqual(4, m0.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m0.rhythm.AttackPoints[0]));

			Assert.AreEqual(4, m1.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m1.rhythm.AttackPoints[0]));
			Assert.AreEqual(4, m1.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m1.rhythm.AttackPoints[1]));
			Assert.AreEqual(8, m1.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m1.rhythm.AttackPoints[2]));

			Assert.AreEqual(4, m2.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m2.rhythm.AttackPoints[0]));
			Assert.AreEqual(4, m2.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m2.rhythm.AttackPoints[1]));
			
			Assert.AreEqual(11, m3.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m3.rhythm.AttackPoints[0]));
			Assert.AreEqual(1, m3.rhythm.GetDurationIncludingTiesAfterByAttackPoint(m3.rhythm.AttackPoints[1]));

		}

		[TestMethod]
		public void TestParseMeasuresKey() {
			int upbeatOffset;
			Key key;

			Workspace w = new Workspace(-1, false, -1);

			List<Measure> measures = Rhythm.ParseRhythmMeasures(w, "C: QC QC QC QC", out upbeatOffset, out key);

			Assert.AreEqual(0, key.Fifths);
			Assert.AreEqual(KeyMode.Major, key.Mode);

			measures = Rhythm.ParseRhythmMeasures(w, "Cm: QC QC QC QC", out upbeatOffset, out key);

			Assert.AreEqual(0, key.Fifths);
			Assert.AreEqual(KeyMode.Minor, key.Mode);


			measures = Rhythm.ParseRhythmMeasures(w, "C#: QC QC QC QC", out upbeatOffset, out key);

			Assert.AreEqual(7, key.Fifths);
			Assert.AreEqual(KeyMode.Major, key.Mode);


			measures = Rhythm.ParseRhythmMeasures(w, "Bbm: QC QC QC QC", out upbeatOffset, out key);

			Assert.AreEqual(-2, key.Fifths);
			Assert.AreEqual(KeyMode.Minor, key.Mode);
		}


		[TestMethod]
		public void TestShiftRhythmMeasures() {
			int upbeatOffset;
			Key key;
			Workspace w = new Workspace(-1, false, -1);

			List<Measure> measures = Rhythm.ParseRhythmMeasures(w, "Q | QQQ_ | QQQ | H_E.S", out upbeatOffset, out key);
			Assert.AreEqual(4, upbeatOffset);
			List<Measure> expected = Rhythm.ParseRhythmMeasures(w, "QQQ | HQ | QH_ | E.S ", out upbeatOffset, out key);
			List<Measure> shifted = Rhythm.ShiftRhythmMeasures(w, measures, 4);
			Assert.AreEqual(0, upbeatOffset);
			AssertMeasureListsEqual(expected, shifted);

			measures = Rhythm.ParseRhythmMeasures(w, "H | QQQQ | HQQ_ | QQH_ | W_ | H", out upbeatOffset, out key);
			Assert.AreEqual(8, upbeatOffset);
			expected = Rhythm.ParseRhythmMeasures(w, "HQQ | QQH | QHQ | W_ | W", out upbeatOffset, out key);
			Assert.AreEqual(0, upbeatOffset);
			shifted = Rhythm.ShiftRhythmMeasures(w, measures, 8);

			AssertMeasureListsEqual(expected, shifted);
		}

		private static void AssertMeasureListsEqual(List<Measure> expected, List<Measure> shifted) {
			Assert.AreEqual(expected.Count, shifted.Count);

			for (int i = 0; i < expected.Count; i++) {
				Assert.AreEqual(expected[i].rhythm.ToString(), shifted[i].rhythm.ToString());
			}
		}

	}
}
