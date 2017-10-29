using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonExpected : GroupReason {
		public ExpectedGroup expectedGroup;

		public GroupReasonExpected(Group group, double reasonStrength, ExpectedGroup expectedGroup)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_EXPECTED;
			this.expectedGroup = expectedGroup;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonExpected(g, reasonStrength, expectedGroup);
		}


	}
}
