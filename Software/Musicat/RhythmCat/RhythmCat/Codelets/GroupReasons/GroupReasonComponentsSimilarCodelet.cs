
using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, true)]
	public class GroupReasonComponentsSimilarCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;

		public GroupReasonComponentsSimilarCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Group Reason Similar Components", urgency, parent, coderack, workspace, slipnet) {

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
		public GroupReasonComponentsSimilarCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Group group)
			: base("Group Reason Similar Components", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
		}

		public override void Run() {
			if (group == null) {
				group = workspace.PickRandomGroupByRecency();
			}

			if (group == null)
				return;

			if (!workspace.groups.Contains(group))
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);


			// Compute the similarity of each pair of subcomponents, left to right, and average.
			// Similarity determined by measure links, for measures, or analogies, for groups.
			double score = ComputeSimilarity();

			if (score < 50)
				return; // not good enough

			// Record the grouping reason.
			GroupReason reason;
			if (score > 99.99)
				reason = new GroupReasonComponentsIdentical(group);
			else
				reason = new GroupReasonComponentsSimilar(group, score);
			group.AddGroupReason(reason);
		}

		private double ComputeSimilarity() {
			// Look for a link between the subcomponents of the group.
			// Are subcomponents Groups or Measures?
			int numPairs = group.GroupElements.Count - 1;

			if (numPairs < 1)
				return 0;
 
			double total = 0;
			for (int i = 0; i < numPairs; i++) {
				total += PairSimilarityScore(group.GroupElements[i], group.GroupElements[i + 1]);
			}
			double score = total / numPairs;
			return score;
		}

		private double PairSimilarityScore(GroupElement ge1, GroupElement ge2) {
			if (ge1 is Measure && ge2 is Measure) {
				// Look for a link between measures.
				foreach (MeasureLink link in workspace.measureLinks) {
					if (link.Involves((Measure)ge1, (Measure)ge2)) {
						return link.strength;
					}
				}
				return 0;
			} else {
				// Group is involved. Look for an analogy.
				foreach (Analogy a in workspace.analogies) {
					if (a.LinksTheseTwo(ge1, ge2))
						return a.Strength;
				}
			}
			// No similarity found.
			return 0;
		}

	}
}













		/*
		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The Link to examine. If none given, we select randomly.
		/// </summary>
		private MeasureLink link;


		public MeasureSamenessGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Measure Sameness Grouper", urgency, parent, coderack, workspace, slipnet) {

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
		public MeasureSamenessGrouperCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, MeasureLink link)
			: base("Measure Sameness Grouper", urgency, parent, coderack, workspace, slipnet) {
			this.link = link;
		}

		public override void Run() {
			if (link == null) {
				link = workspace.PickRandomMeasureLinkByRecencyAndStrength();
			}

			if (link == null)
				return;

			Measure m1 = link.m1;
			Measure m2 = link.m2;

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
		*/