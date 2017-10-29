using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class MetaGrouperCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The groups to examine. If none given, we select randomly.
		/// </summary>
		private Group g1;
		private Group g2;

		// A related analogy.
		private Analogy a;

		public MetaGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Meta Grouper", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public MetaGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement g1, GroupElement g2)
			: base("Meta Grouper", urgency, parent, coderack, workspace, slipnet) {
			
			if (!(g1 is Group && g2 is Group))
				return;
			this.g1 = (Group)g1;
			this.g2 = (Group)g2;
		}

		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public MetaGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement g1, GroupElement g2, Analogy a)
			: base("Meta Grouper", urgency, parent, coderack, workspace, slipnet) {

			if (!(g1 is Group && g2 is Group))
				return;
			this.g1 = (Group)g1;
			this.g2 = (Group)g2;
			this.a = a;
		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public MetaGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement g1)
			: base("Meta Grouper", urgency, parent, coderack, workspace, slipnet) {
			if (!(g1 is Group))
				return; 
			this.g1 = (Group)g1;
		}


		public override void Run() {
			if (g1 == null) {
				g1 = workspace.PickRandomGroupByRecencyAndStrength();
			}

			if (g1 == null)
				return;

			if (g2 == null) {
				// pick one adjacent to g1....
				g2 = workspace.PickRandomGroupAdjacentTo(g1);
			}

			if (g2 == null || g1 == g2)
				return;

			if (!workspace.groups.Contains(g1) || !workspace.groups.Contains(g2))
				return;

			if (g1.MinLocation > g2.MinLocation) {
				Group tmp = g1;
				g1 = g2;
				g2 = tmp;
			}

			// Check if the groups are adjacent. Otherwise, we can't group.
			int m1 = g1.MaxLocation;
			int m2 = g2.MinLocation;
			if (m2 - m1 != 1)
				return;

			// Check if either group is in another group. If so, we can't group.
			//if (g1.hasParent || g2.hasParent)
			//	return;

			// Check if group already exists!
			// TODO>


			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, g1.MinLocation, g1.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, g2.MinLocation, g2.MaxLocation);


			double r = Utilities.rand.NextDouble() * 100;

			// TODO! Score group strenght reasonably.


			// TODODODODODODOODODODODODODO

			// Make tmp group.
			TemporaryGroup tmpG = new TemporaryGroup(workspace, g1, g2); // TODO: tmp group messes up g1 and g2 parents
			float score = 75 / (1 + (Math.Abs(g2.GroupElements.Count - g1.GroupElements.Count))); //MetaGroupStrength;
			
			tmpG.AddGroupReason(new GroupReasonNumberOfSubelements(tmpG, score));
			
			if (a != null) 
				tmpG.AddGroupReason(new GroupReasonAnalogySupport(tmpG, a.Strength, a));
			
			// Group if the score is strong enough.
			if (r < score) {
				Group newGroup = workspace.AddGroup(tmpG);
				if (newGroup != null) {
					// TODO: add reasons....
				}
			}

		}
	}
}
