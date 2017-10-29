using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonEndDominant : GroupReason {

		public GroupReasonEndDominant(Group group, double reasonStrength)
			: base(group, reasonStrength) {

			this.reasonWeight = Constants.WEIGHT_REASON_END_DOMINANT;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonEndDominant(g, reasonStrength);
		}
	}
}
