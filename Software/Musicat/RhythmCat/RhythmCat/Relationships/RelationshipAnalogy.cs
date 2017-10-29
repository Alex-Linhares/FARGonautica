using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class RelationshipAnalogy : RelationshipSimilar {

		private Analogy analogy;

		public Analogy Analogy {
			get { return analogy; }
		}

		public RelationshipAnalogy(Analogy analogy)
			: base(analogy.LHS, analogy.RHS, 0) {

			this.analogy = analogy;
		}

		public RelationshipAnalogy(Analogy analogy, GroupElement LHS, GroupElement RHS, float strength)
			: base(analogy.LHS, analogy.RHS, strength) {

			this.analogy = analogy;
			this.RhythmType = false;
		}

		public override float Strength {
			get {
				double weight;
				return (float)analogy.ComputeHappiness(out weight);
			}
			set {
				throw new InvalidOperationException("Can't change strength of an analogy-based Relationship");
			}
		}

		/// <summary>
		/// TODO: Does this work right? Should we copy the analogy somehow too?
		/// </summary>
		/// <param name="r"></param>
		/// <param name="a"></param>
		/// <returns></returns>
		public override Relationship DeepCopyUsingNewGroupsInAnalogy(Relationship r, Analogy a) {
			GroupElement newLHS, newRHS;

			FindNewLHSRHS(r, a, out newLHS, out newRHS);

			RelationshipAnalogy rNew = new RelationshipAnalogy(a, newLHS, newRHS, a.Strength);
			return rNew;
		}

	}
}
