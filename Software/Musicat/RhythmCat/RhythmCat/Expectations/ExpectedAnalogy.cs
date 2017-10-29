using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class ExpectedAnalogy : Analogy {

		protected float expectationStrength;
		protected int level;

		public ExpectedAnalogy(GroupElement LHS, GroupElement RHS, Workspace workspace, float expectationStrength, int level)
			: base(LHS, RHS, workspace) {

			this.expectationStrength = expectationStrength;
			this.level = level;
		}

		public override int Level {
			get {
				return level;
			}
		}

		public override float Strength {
			get {
				return expectationStrength * Constants.ANALOGY_EXPECTATION_STRENGTH_MULTIPLIER;
			}
		}
	}
}
