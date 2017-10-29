using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class Sequence : Group {

		public List<GroupElement> SequenceElements {
			get;
			private set;
		}

		public Sequence(Workspace workspace)
			: base(workspace) {

			SequenceElements = new List<GroupElement>();
		}

		public Sequence(Workspace workspace, GroupElement ge1, GroupElement ge2, GroupElement ge3, double score)
			: base(workspace) {
		
			SequenceElements = new List<GroupElement>();
			SequenceElements.Add(ge1);
			SequenceElements.Add(ge2);
			SequenceElements.Add(ge3);

			AddGroupElement(ge1);
			AddGroupElement(ge2);
			AddGroupElement(ge3);

			this.AddGroupReason(new GroupReasonSequence(this, score));
		}
		
	}
}
