using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class RelationshipMelodyContour : Relationship {

		public RelationshipMelodyContour(GroupElement LHS, GroupElement RHS, float strength)
			: base(LHS, RHS, strength, false) {

		}

		public override Relationship DeepCopyUsingNewGroupsInAnalogy(Relationship r, Analogy a) {
			GroupElement newLHS, newRHS;

			FindNewLHSRHS(r, a, out newLHS, out newRHS);

			RelationshipMelodyContour rNew = new RelationshipMelodyContour(newLHS, newRHS, a.Strength);
			return rNew;
		}
	}
}
