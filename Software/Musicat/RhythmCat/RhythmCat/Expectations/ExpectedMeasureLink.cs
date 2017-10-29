using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class ExpectedMeasureLink : MeasureLink {

		protected float expectationStrength;

		public ExpectedMeasureLink(MeasureLink sourceMeasureLink, ExpectedMeasure expectedMeasure1, ExpectedMeasure expectedMeasure2, float expectationStrength)
			: base(expectedMeasure1, expectedMeasure2, expectationStrength) {

			this.expectationStrength = expectationStrength;
		}

	}
}
