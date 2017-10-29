using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class ExpectedMeasure : Measure {

		protected float expectationStrength;

		public ExpectedMeasure(Workspace workspace, float expectationStrength) : base(workspace) {
			this.expectationStrength = expectationStrength;
		}

		public ExpectedMeasure(Measure sourceMeasure, int measureNum, float expectationStrength) : base(sourceMeasure) {
			this.number = measureNum;
			this.expectationStrength = expectationStrength;
		}

	}
}
