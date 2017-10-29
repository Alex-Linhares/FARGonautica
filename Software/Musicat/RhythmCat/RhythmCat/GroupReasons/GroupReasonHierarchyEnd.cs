using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonHierarchyEnd : GroupReason {

		public GroupReasonHierarchyEnd(Group group, double reasonStrength)
			: base(group, reasonStrength) {

				double levelMultiplier = Math.Min(group.Level / 2.0,
						Constants.MAX_WEIGHT_REASON_END_POSITION_LEVEL_MULTIPLIER);

				this.reasonWeight = Constants.WEIGHT_REASON_END_POSITION * levelMultiplier;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonHierarchyEnd(g, reasonStrength);
		}

	}
}
