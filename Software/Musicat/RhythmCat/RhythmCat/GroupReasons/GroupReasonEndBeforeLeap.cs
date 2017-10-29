using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonEndBeforeLeap : GroupReasonStrongEndPosition {

		public GroupReasonEndBeforeLeap(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_END_BEFORE_LEAP;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonEndBeforeLeap(g, reasonStrength);
		}
	}
}
