using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class RelationshipIdentical : Relationship {

		public RelationshipIdentical(GroupElement LHS, GroupElement RHS, float strength)
			: base(LHS, RHS, strength, true) {

		}


		public override Relationship DeepCopyUsingNewGroupsInAnalogy(Relationship r, Analogy a) {
			GroupElement newLHS, newRHS;

			FindNewLHSRHS(r, a, out newLHS, out newRHS);

			RelationshipIdentical rNew = new RelationshipIdentical(newLHS, newRHS, a.Strength);
			return rNew;
		}
	}
}
