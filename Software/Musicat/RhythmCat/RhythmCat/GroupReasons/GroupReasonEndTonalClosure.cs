using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonEndMusicalForcesClosure : GroupReason {

		public GroupReasonEndMusicalForcesClosure(Group group, double reasonStrength)
			: base(group, reasonStrength) {

			this.reasonWeight = Constants.WEIGHT_REASON_END_MUSICAL_FORCES_CLOSURE;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonEndMusicalForcesClosure(g, reasonStrength);
		}
	}
}
