using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public abstract class GroupReason {

		protected Group group;
		protected double reasonStrength;
		protected double reasonWeight;

		public double ReasonStrength {
			get { return reasonStrength; }
		}

		public Group Group {
			get { return group; }
		}

		public void ReplaceGroup (Group g) {
			group = g;
		}

		public GroupReason(Group group, double reasonStrength) {
			this.group = group;
			this.reasonStrength = reasonStrength;
		}

		public GroupReason(Group group) {
			this.group = group;
		}

		/*
		public GroupReason(GroupReason groupReason) {
			this.group = groupReason.group;
			this.reasonStrength = groupReason.reasonStrength;
			this.reasonWeight = groupReason.reasonWeight;
		}

		public GroupReason(GroupReason groupReason, Group group) {
			this.group = group;
			this.reasonStrength = groupReason.reasonStrength;
			this.reasonWeight = groupReason.reasonWeight;
		}*/

		public virtual double ComputeReasonScore() {
			return reasonStrength * reasonWeight;
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			sb.Append(this.GetType().Name);
			sb.Append('(');
			sb.Append(reasonWeight.ToString());
			sb.Append("): ");
			sb.Append(reasonStrength.ToString());
			sb.Append(" --> ");
			sb.Append(ComputeReasonScore().ToString());

			return sb.ToString();
		}

		/// <summary>
		/// Copies this reason, and sets the group link to the given group g.
		/// </summary>
		/// <param name="g"></param>
		/// <returns></returns>
		public abstract GroupReason DeepCopy(Group g);
	}
}
