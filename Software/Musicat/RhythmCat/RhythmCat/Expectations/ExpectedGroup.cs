using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	public class ExpectedGroup : Group {
		protected int minLocation;
		protected int maxLocation;
		protected int level;
		protected float expectationStrength;

		public override int Level {
			get {
				return level;
			}
		}

		public ExpectedGroup(Workspace workspace, int start, int end, int level, float expectationStrength) : base(workspace) {
			this.minLocation = start;
			this.maxLocation = end;
			this.level = level;
			this.expectationStrength = expectationStrength;
		}

		public override int MaxLocation {
			get {
				return maxLocation;
			}
		}

		public override int MinLocation {
			get {
				return minLocation;
			}
		}

		public override double ComputeStrength() {
			// Combine expectationStrength with the list of comfirming measure-similarity reasons, which strenghten it.
			int count = 0;
			double total = 0;

			foreach (GroupReason gr in reasons) {
				if (gr is GroupReasonExpectationMeasureLink) {
					count++;
					total += gr.ReasonStrength;
				}
			}
			if (count > 0) {
				double avg = total / count;

				double newStr = expectationStrength + (avg / 100) * 10.0 * count;


				newStr = Utilities.SigmoidSquash(newStr);

				// Validate range.
				if (newStr < 0)
					newStr = 0;
				else if (newStr > 100)
					throw new ArgumentOutOfRangeException("score is out of range for expectation group" + this.ToString());
				return newStr * Constants.GROUP_EXPECTATION_STRENGTH_MULTIPLIER;
			} else {
                return Utilities.SigmoidSquash(expectationStrength);
			}
		}
	}
}
