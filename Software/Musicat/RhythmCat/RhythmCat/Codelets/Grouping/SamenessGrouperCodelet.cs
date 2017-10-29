using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class SamenessGrouperCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The relatinoship to examine. If none given, we select randomly.
		/// </summary>
		private Relationship relationship;


		public SamenessGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Sameness Grouper", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which link to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public SamenessGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Relationship relationship)
			: base("Sameness Grouper", urgency, parent, coderack, workspace, slipnet) {
			this.relationship = relationship;
		}

		public override void Run() {
			if (relationship == null) {
				relationship = workspace.PickRandomRelationshipByRecencyAndStrength();
			}

			if (relationship == null)
				return;

			GroupElement ge1 = relationship.LHS;
			GroupElement ge2 = relationship.RHS;

			// TODO
			/
			......
			.

			.
			.

			// Check if the measures are adjacent. Otherwise, we can't group.
			if (Math.Abs(m1.number - m2.number) != 1)
				return;

			// Check if either measure is in another group. If so, we can't group.
			if (m1.hasParent || m2.hasParent)
				return;

			double r = Utilities.rand.NextDouble() * 100;

			// Group if the link is strong enough.
			if (r < link.strength) {
				Group g = workspace.AddGroup(m1, m2);

				// Record the grouping reason.
				GroupReason reason;
				// Are measures identical?
				if (m1.IsExactlyEqualTo(m2))
					reason = new GroupReasonComponentsIdentical(g);
				else
					reason = new GroupReasonComponentsSimilar(g, link.strength);
				g.AddGroupReason(reason);
			}

		}
	}
}
