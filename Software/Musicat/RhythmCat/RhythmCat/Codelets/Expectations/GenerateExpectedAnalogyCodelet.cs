using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Expectation", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class GenerateExpectedAnalogyCodelet : Codelet {

	
		/// <summary>
		/// The source analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;


		public GenerateExpectedAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Generate Expected Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which analogy to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public GenerateExpectedAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy analogy)
			: base("Generate Expected Analogy", urgency, parent, coderack, workspace, slipnet) {
				this.analogy = analogy;
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
			int offset = workspace.MaxLocation + 1 - group.MinLocation;
			workspace.expectations.GenerateExpectedGroupBasedOn(group, offset, true);

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation + offset, group.MaxLocation + offset);

		}

		private double ComputeWeight(Group g) {
			return g.Length;
		}
	}
}
