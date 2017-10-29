using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonComponentsIdentical : GroupReason {
		
		public GroupReasonComponentsIdentical(Group group)
			: base(group) {

			int numElements = group.GroupElements.Count;

			double strength;

			if (numElements > 2)
				strength = 100;
			else if (numElements == 2)
				strength = 90;
			else
				throw new ArgumentException("Can't have similarity of 1 element to itself as group reason");

			this.reasonStrength = strength;
			this.reasonWeight = Constants.WEIGHT_REASON_IDENTICAL_COMPONENTS;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonComponentsIdentical(g);
		}

	}
}
