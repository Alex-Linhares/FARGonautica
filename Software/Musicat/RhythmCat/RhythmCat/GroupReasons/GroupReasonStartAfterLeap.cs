using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonStartAfterLeap : GroupReasonStrongStartPosition {

		public GroupReasonStartAfterLeap(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_START_AFTER_LEAP;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonStartAfterLeap(g, reasonStrength);
		}

	}
}
