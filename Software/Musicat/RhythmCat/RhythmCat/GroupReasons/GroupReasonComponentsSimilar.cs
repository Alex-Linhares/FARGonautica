using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonComponentsSimilar : GroupReason {
		public GroupReasonComponentsSimilar(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_SIMILAR_COMPONENTS;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonComponentsSimilar(g, reasonStrength);
		}

	}
}
