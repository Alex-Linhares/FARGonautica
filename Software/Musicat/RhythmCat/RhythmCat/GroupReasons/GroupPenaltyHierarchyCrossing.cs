using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupPenaltyHierarchyCrossing : GroupPenaltyReason {

		public GroupPenaltyHierarchyCrossing(Group group, double penaltyStrength)
			: base(group, penaltyStrength) {

			this.reasonWeight = Constants.WEIGHT_PENALTY_REASON_HIERARCHY_CROSSING;
		}

		public override GroupPenaltyReason DeepCopy(Group g) {
			return new GroupPenaltyHierarchyCrossing(g, penaltyStrength);
		}
	}
}
