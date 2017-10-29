using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	/// <summary>
	/// Starts a group based on an expected group. Real groups usually start out with 1 element, and grow to the right.
	/// </summary>
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class SuggestGroupFromExpectationCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The groups to examine. If none given, we select randomly.
		/// </summary>
		private ExpectedGroup eg;


		public SuggestGroupFromExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Suggest Group From Expectation", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public SuggestGroupFromExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, ExpectedGroup eg)
			: base("Suggest Group From Expectation", urgency, parent, coderack, workspace, slipnet) {

			this.eg = eg;
		}


		public override void Run() {
			if (eg == null) {
				// Pick a 1st level group starting now.
				eg = workspace.expectations.PickRandomGroupWithConditions(workspace.measures.Count - 1, 1);
			}

			if (eg == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, eg.MinLocation, eg.MaxLocation);

			// 2 conditions: expected group starts now, or it started earlier.

			// Case 1:
			if (eg.MinLocation == workspace.measures.Count - 1) {
				// Check level. Level 1, or metagroup?
				if (eg.Level == 1) {

					// Suggest a group that matches the expected group.
					TemporaryGroup tmpG = new TemporaryGroup(workspace, workspace.measures[workspace.measures.Count - 1]); 
					tmpG.AddGroupReason(new GroupReasonExpected(tmpG, eg.ComputeStrength(), eg));
					Group newGroup = workspace.AddGroup(tmpG);
					if (newGroup == null)
						return;

					// TODO

					/*
					//Also add any metagroups above this one.
					List<ExpectedGroup> metagroups = new List<ExpectedGroup>();
					metagroups = FindExpectedGroupsAbove(eg);
					Group child = newGroup;
					foreach (ExpectedGroup meta in metagroups) {
						tmpG = new TemporaryGroup(workspace, child);
						tmpG.AddGroupReason(new GroupReasonExpected(tmpG, meta.ComputeStrength(), meta));
						newGroup = workspace.AddGroup(tmpG);
						if (newGroup == null)
							return;
						child = newGroup;
					}
*/

				} else {
					// Metagroup.
					// TODO.
				}
			} else {
				// Case 2: expected group started earlier. 

				// Check level.
				// TODO.
			}

	
			
			/*
			double r = Utilities.rand.NextDouble() * 100;

			// TODO! Score group strenght reasonably.
			// TODODODODODODOODODODODODODO

			// Make tmp group.
			float score = 75 / (1 + (Math.Abs(g2.GroupElements.Count - g1.GroupElements.Count))); //MetaGroupStrength;

			

			// Group if the score is strong enough.
			if (r < score) {
				workspace.AddGroup(tmpG);

				// TODO: add reasons....
			}
			*/
		}

		private List<ExpectedGroup> FindExpectedGroupsAbove(ExpectedGroup eg) {
			List<ExpectedGroup> above = new List<ExpectedGroup>();
			foreach (ExpectedGroup eg2 in workspace.expectations.groups) {
				if (eg2.Level > eg.Level && eg2.MinLocation == eg.MinLocation)
					above.Add(eg2);
			}
			return above;
		}
	}
}
