using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonExpectationMeasureLink : GroupReason {
		public MeasureLink link;

		public GroupReasonExpectationMeasureLink(Group group, double reasonStrength, MeasureLink link)
			: base(group, reasonStrength) {

			this.reasonWeight = Constants.WEIGHT_REASON_ANALOGY;
			this.link = link;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonExpectationMeasureLink(g, reasonStrength, link);
		}

	}
}
