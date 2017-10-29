using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonNumberOfSubelements : GroupReason {

		public GroupReasonNumberOfSubelements(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_NUMBER_SUBELEMENTS;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonNumberOfSubelements(g, reasonStrength);
		}

	}
}
