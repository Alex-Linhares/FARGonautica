using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonSequence : GroupReason {

		public GroupReasonSequence(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_SEQUENCE;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonSequence(g, reasonStrength);
		}


	}
}
