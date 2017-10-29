using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonEndBeforeGap : GroupReasonStrongEndPosition {

		public GroupReasonEndBeforeGap(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_END_BEFORE_GAP;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonEndBeforeGap(g, reasonStrength);
		}
	}
}
