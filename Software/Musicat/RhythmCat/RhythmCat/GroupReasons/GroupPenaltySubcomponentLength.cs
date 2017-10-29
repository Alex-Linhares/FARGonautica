using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupPenaltySubcomponentLength : GroupPenaltyReason {

		public GroupPenaltySubcomponentLength(Group group, double penaltyStrength)
			: base(group, penaltyStrength) {

			this.reasonWeight = Constants.WEIGHT_PENALTY_REASON_SUBCOMPONENT_LENGTH;
		}

		public override GroupPenaltyReason DeepCopy(Group g) {
			return new GroupPenaltySubcomponentLength(g, penaltyStrength);
		}


	}
}
