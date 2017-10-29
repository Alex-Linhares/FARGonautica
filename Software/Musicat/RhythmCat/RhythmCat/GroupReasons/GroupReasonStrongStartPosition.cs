using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public abstract class GroupReasonStrongStartPosition : GroupReasonStrongBoundaryPosition {

		public GroupReasonStrongStartPosition(Group group, double reasonStrength)
			: base(group, reasonStrength) {
			
			this.reasonWeight = Constants.WEIGHT_REASON_END_POSITION;
		}

	}
}
