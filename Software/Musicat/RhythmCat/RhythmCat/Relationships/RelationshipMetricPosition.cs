using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class RelationshipMetricPosition : Relationship {

		public RelationshipMetricPosition(GroupElement LHS, GroupElement RHS, float strength)
			: base(LHS, RHS, strength, true) {

		}

		public override Relationship DeepCopyUsingNewGroupsInAnalogy(Relationship r, Analogy a) {
			GroupElement newLHS, newRHS;

			FindNewLHSRHS(r, a, out newLHS, out newRHS);

			RelationshipMetricPosition rNew = new RelationshipMetricPosition(newLHS, newRHS, a.Strength);
			return rNew;
		}
	}
}
