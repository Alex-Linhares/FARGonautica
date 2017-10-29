using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class GroupReasonAnalogySupport : GroupReason {
		
		public Analogy Analogy {get; protected set;}

		public GroupReasonAnalogySupport(Group group, double reasonStrength, Analogy a)
			: base(group, reasonStrength) {

				this.reasonWeight = Constants.WEIGHT_REASON_ANALOGY;
				this.Analogy = a;
		}

		public override GroupReason DeepCopy(Group g) {
			return new GroupReasonAnalogySupport(g, reasonStrength, Analogy);
		}

	}
}
