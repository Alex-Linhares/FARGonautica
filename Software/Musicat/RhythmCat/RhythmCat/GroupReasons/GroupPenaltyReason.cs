using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public abstract class GroupPenaltyReason {

		protected Group group;
		protected double penaltyStrength;
		protected double reasonWeight;

		/// <summary>
		/// Penalities are recorded as positive numbers. (they are subtracted when used).
		/// </summary>
		public double PenaltyStrength {
			get { return penaltyStrength; }
		}

		public Group Group {
			get { return group; }
		}

		public void ReplaceGroup(Group g) {
			group = g;
		}

		/// <summary>
		/// Penalities are recorded as positive numbers. (they are subtracted when used).
		/// </summary>
		/// <param name="group"></param>
		/// <param name="reasonStrength"></param>
		public GroupPenaltyReason(Group group, double penaltyStrength) {
			this.group = group;
			this.penaltyStrength = penaltyStrength;
		}

		public GroupPenaltyReason(Group group) {
			this.group = group;
		}

		public virtual double ComputePenaltyScore() {
			return penaltyStrength * reasonWeight;
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			sb.Append(this.GetType().Name);
			sb.Append('(');
			sb.Append(reasonWeight.ToString());
			sb.Append("): ");
			sb.Append(penaltyStrength.ToString());
			sb.Append(" --> -");
			sb.Append(ComputePenaltyScore().ToString());

			return sb.ToString();
		}

		/// <summary>
		/// Copies this reason, and sets the group link to the given group g.
		/// </summary>
		/// <param name="g"></param>
		/// <returns></returns>
		public abstract GroupPenaltyReason DeepCopy(Group g);
	}
}
