using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class RelationshipParameterDecreasing : RelationshipParameter {

		public RelationshipParameterDecreasing(GroupElement LHS, GroupElement RHS, float strength)
			: base(LHS, RHS, strength) {

		}

		public override Relationship DeepCopyUsingNewGroupsInAnalogy(Relationship r, Analogy a) {
			GroupElement newLHS, newRHS;

			FindNewLHSRHS(r, a, out newLHS, out newRHS);

			RelationshipParameterDecreasing rNew = new RelationshipParameterDecreasing(newLHS, newRHS, a.Strength);
			return rNew;
		}
	}
}
