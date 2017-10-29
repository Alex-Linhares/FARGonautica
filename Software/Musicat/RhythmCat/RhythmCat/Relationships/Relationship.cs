using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public abstract class Relationship {
		public GroupElement LHS;
		public GroupElement RHS;

		public bool RhythmType { get; protected set; }

		/// <summary>
		/// Between 0 and 100.
		/// </summary>
		protected float strength;

		virtual public float Strength {
			get { return strength; }
			set { strength = value; }
		}

		public Relationship(GroupElement LHS, GroupElement RHS, float strength, bool rhythmType) {
			if (LHS == null || RHS == null)
				throw new ArgumentNullException();
			
			this.LHS = LHS;
			this.RHS = RHS;
			this.strength = strength;
			RhythmType = rhythmType;
		}

		public override string ToString() {
			string lhs = LHS.FormLabel;
			string rhs = RHS.FormLabel;
			string lhsRange = "m." + (LHS.MinLocation+1).ToString() + "-" + (LHS.MaxLocation+1).ToString();
			string rhsRange = "m." + (RHS.MinLocation+1).ToString() + "-" + (RHS.MaxLocation+1).ToString();

			if (lhs == null)
				lhs = lhsRange; 
			else
				lhs += " " + lhsRange;

			if (rhs == null)
				rhs = rhsRange;
			else
				rhs += " " + rhsRange;
				
			string rhythm = RhythmType ? " RHYTHM" : "";
			return lhs + "<-->" + rhs + rhythm + " (" + Strength.ToString() + ") " + this.GetType().ToString();
		}


		public bool Involves(GroupElement ge) {
			return LHS == ge || RHS == ge;
		}

		public bool IsEquivalentTypeTo(Relationship r2) {
			Type r1Type = this.GetType();
			Type r2Type = r2.GetType();
			//return r1Type.IsAssignableFrom(r2Type);
			if (r1Type == r2Type)
				return true;
			if ((this is RelationshipIdentical && r2 is RelationshipSimilar) ||
				(r2 is RelationshipIdentical && this is RelationshipSimilar))
				return true;
            if ((this is RelationshipStartIdentical && r2 is RelationshipStartSimilar) ||
                (r2 is RelationshipStartIdentical && this is RelationshipStartSimilar))
                return true;
			return false;
		}

		/// <summary>
		/// Copies the relationship, updating group pointers to use the groups in this analogy.
		/// </summary>
		/// <param name="r"></param>
		/// <param name="a"></param>
		/// <returns></returns>
		public abstract Relationship DeepCopyUsingNewGroupsInAnalogy(Relationship r, Analogy a);
	/*{
			GroupElement newLHS, newRHS;

			FindNewLHSRHS(r, a, out newLHS, out newRHS);

			Relationship rNew = new Relationship(newLHS, newRHS, a.Strength, r.RhythmType);
			return rNew;
		}
	*/
		protected static void FindNewLHSRHS(Relationship r, Analogy a, out GroupElement newLHS, out GroupElement newRHS) {
			newLHS = a.FindSubGroupEquivalentToGroup(r.LHS);
			newRHS = a.FindSubGroupEquivalentToGroup(r.RHS);
		}
	}
}
