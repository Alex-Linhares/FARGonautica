using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Expectation", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class GenerateExpectedGroupCopyCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The source group to examine. If none given, we select randomly.
		/// </summary>
		private Group group;


		public GenerateExpectedGroupCopyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Generate Expected Group Copy", urgency, parent, coderack, workspace, slipnet) {

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
		public GenerateExpectedGroupCopyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Group group)
			: base("Generate Expected Group Copy", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
		}

		public override void Run() {
			double weight;
			if (group == null) {
				// Pick the *largest* group ending in the final measure and expect it to repeat, 
				// with probability based on group strength and hierarchy strength.
				List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();
				foreach (Group g in workspace.groups) {
					// Skip groups that don't end on the final measure.
					if (g.MaxLocation != workspace.MaxLocation)
						continue; // TODO test


					double grpWeight = ComputeWeight(g);

					Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(g, grpWeight);
					pairs.Add(pair);
				}

				//Utilities.ObjectValuePair p = Utilities.PickItemWeightedReturnPair(pairs);
				Utilities.ObjectValuePair p = Utilities.PickLargestItemReturnPair(pairs);
				group = (Group)p.obj;
				weight = p.value;
			} else {
				if (!workspace.groups.Contains(group))
					return;
				weight = ComputeWeight(group);
			}

			if (group == null) 
				return;

			// We have an existing group. Try to make an expectation.
			// Note: It will have to compete with other existing expectations  (handle hierarchy differences)
			int startExpectationLocation = workspace.MaxLocation + 1;
			int offset = startExpectationLocation - group.MinLocation;

			// TODO expectation strength is higher for stronger groups, for now.
			// Set the max strength to be the group strength.
			// Decrease strength below this amount for non-well-placed groups with respect to barlines
			float expectationStrength = (float)group.ComputeStrength();		

			// Find the length of the group, and find the typical length of a group starting/ending at the barline we're starting on in the expectation.
			int groupLength = group.LengthInMeasures;

			// Don't generate expectations if we dont' have barlines in place yet.
			if (startExpectationLocation >= workspace.barlines.Count)
				return;

			int barlineHeight = workspace.barlines[startExpectationLocation];
			int[] barlineDistances = workspace.FindPreviousBarlineDistances(startExpectationLocation);
			// Fails if barlines don't all exist yet.
			if (barlineDistances == null)
				return;
			// Find the distance closest to an existing distance.
			int min = Int32.MaxValue;
			int argmin = -1;
			for (int h = 0; h < barlineDistances.Length; h++) {
				// Ignore "-1" (unused heights)
				if (barlineDistances[h] == -1)
					continue;
				int dist = Math.Abs(barlineDistances[h] - groupLength);
				if (dist < min) {
					min = dist;
 					argmin = h;
				}
			}
			int expectedHeight = argmin;
			// Penalize if the barline is too short for this group length.
			if (barlineHeight < expectedHeight)
				expectationStrength *= 0.7f;
			// Penalize if the group length is a weird number.
			if (min > 0)
				expectationStrength *= (0.7f * ((float)min / barlineDistances[argmin]));


			ExpectedGroup expectedGroup = workspace.expectations.GenerateExpectedGroupBasedOn(group, offset, expectationStrength, true);
			if (expectedGroup != null) {
				// Also make a large-scale analogy between the group and the expected groups.
				workspace.expectations.MakeNewExpectedAnalogy(group, expectedGroup, expectationStrength, group.Level+1);
				// And add supporting analogies between the top-level components of the group and expected group.
				for (int i = 0; i < group.GroupElements.Count; i++) {
					if (group.GroupElements[i] is Group) {
						Group subgroup = (Group)group.GroupElements[i];
						int startLocation = subgroup.MinLocation;
						int endLocation = subgroup.MaxLocation;
						// Find expected subgroup for this subgroup. 
						// Note: we can't assume expected groups have elements; they just contain a start and endpoint. So we have to search.
						ExpectedGroup expectedSubgroup = null;
						foreach (ExpectedGroup eg in workspace.expectations.groups) {
							if (eg.MinLocation == startLocation + offset && eg.MaxLocation == endLocation + offset) {
								expectedSubgroup = eg;
								break;
							}
						}
						if (expectedSubgroup != null) {
							workspace.expectations.MakeNewExpectedAnalogy(subgroup,
																		  expectedSubgroup,
																		  expectationStrength, subgroup.Level+1);
						}
					}
				}


				// Now, add expected within-future analogies and links, simply copies of existing analogies and relationships, shifted by offset.
				// Search for all analogies and relationships which take place within the timespan of the original group.
				
				// Links.
				foreach (Relationship rel in workspace.relationships) {
					// Skip relationships between non-measures for now.
					if (!(rel.LHS is Measure && rel.RHS is Measure))
						continue;
					int idx1 = rel.LHS.Location;
					int idx2 = rel.RHS.Location;

					if (group.IncludesLocation(idx1) && group.IncludesLocation(idx2)) {
						workspace.expectations.MakeNewExpectedMeasureLink(idx1, idx2, offset, rel.Strength);
					}
				}

				// Analogies.
				foreach (Analogy analogy in workspace.analogies) {
					int idxLHS1 = analogy.LHS.MinLocation;
					int idxLHS2 = analogy.LHS.MaxLocation;
					int idxRHS1 = analogy.RHS.MinLocation;
					int idxRHS2 = analogy.RHS.MaxLocation;

					if (group.IncludesLocation(idxLHS1) && group.IncludesLocation(idxLHS2) &&
							group.IncludesLocation(idxRHS1) && group.IncludesLocation(idxRHS2)) {

						// Find the future LHS and RHS groups. We have to search for each.
						ExpectedGroup futureLHS = null, futureRHS = null;

						foreach (ExpectedGroup eg in workspace.expectations.groups) {
							if (eg.MinLocation == idxLHS1 + offset && eg.MaxLocation == idxLHS2 + offset) {
								futureLHS = eg;
							} else if (eg.MinLocation == idxRHS1 + offset && eg.MaxLocation == idxRHS2 + offset) {
								futureRHS = eg;
							}
							if (futureLHS != null && futureRHS != null)
								break;
						}
						if (futureLHS != null && futureRHS != null)
							workspace.expectations.MakeNewExpectedAnalogy(futureLHS, futureRHS, analogy.Strength, analogy.Level);
					}
				}

			}

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation + offset, group.MaxLocation + offset);

		}

		private double ComputeWeight(Group g) {
			return g.LengthInMeasures;
		}
	}
}
