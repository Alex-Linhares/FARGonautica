using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonEndTonic : GroupReason {

		public GroupReasonEndTonic(Group group, double reasonStrength)
			: base(group, reasonStrength) {

			this.reasonWeight = Constants.WEIGHT_REASON_END_TONIC;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonEndTonic(g, reasonStrength);
		}
	}
}
