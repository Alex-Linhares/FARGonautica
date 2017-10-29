using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	public class TemporaryGroup : Group {

		public TemporaryGroup(Workspace workspace, GroupElement ge1, GroupElement ge2) : base(workspace) {
			groupElements.Add(ge1);	// add done without affecting ge1.parentGroup.
			groupElements.Add(ge2); // add done without affecting ge2.parentGroup.
		}

		public TemporaryGroup(Workspace workspace, GroupElement ge)
			: base(workspace) {
			groupElements.Add(ge); // add done without affecting ge.parentGroup.
		}

		public TemporaryGroup(Workspace workspace)
			: base(workspace) {
		}

		public Group MakeRealGroup() {
			Group g = new Group(workspace);
			foreach(GroupElement ge in groupElements) {
				g.AddGroupElement(ge);			//// NOW, add affects ge.parentGroup, for a non-temp group.
			}
			foreach (GroupReason r in reasons) {
				r.ReplaceGroup(g);
				g.AddGroupReason(r);
			}
			return g;
		}


	}
}
