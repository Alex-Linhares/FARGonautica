using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonStartAfterGap : GroupReasonStrongStartPosition {

		public GroupReasonStartAfterGap(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_START_AFTER_GAP;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonStartAfterGap(g, reasonStrength);
		}
	}
}
