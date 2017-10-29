using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class MeasureLink : IComparable {
		public Measure m1;
		public Measure m2;

		/// <summary>
		/// Between 0 and 100.
		/// </summary>
		public float strength;

		public MeasureLink(Measure m1, Measure m2, float strength) {
			if (m1.number > m2.number) {
				this.m2 = m1;
				this.m1 = m2;
			} else {
				this.m1 = m1;
				this.m2 = m2;
			}
			this.strength = strength;
		}

		public Measure Other(Measure m) {
			if (m == m2)
				return m1;
			if (m == m1)
				return m2;
			throw new ArgumentException("Given measure not part of link");
		}

		public Measure MostRecentMeasure {
			get {
				if (m1.number > m2.number)
					return m1;
				else
					return m2;
			}
		}

		public override string ToString() {
			return m1.Label + "<-->" + m2.Label + " (" + strength.ToString() + ")\t" + m1.ToString() + "<-->" + m2.ToString();
		}

		public int CompareTo(object obj) {
			if (this.strength == ((MeasureLink)obj).strength)
				return 0;
			return (this.strength < ((MeasureLink)obj).strength) ? 1: -1;
		}

		public bool Involves(Measure m1, Measure m2) {
			return (this.m1 == m1 && this.m2 == m2) || (this.m1 == m2 && this.m2 == m1);
		}

		public bool Involves(Measure m) {
			return (this.m1 == m || this.m2 == m);
		}
	}
}
