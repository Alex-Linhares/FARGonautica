
using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 20, true)]
	public class StrengthenExpectationCodelet : Codelet {


		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private ExpectedGroup group;

		public StrengthenExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Strengthen Expectation", urgency, parent, coderack, workspace, slipnet) {

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
		/// <param name="notesStrengthenExpectationCodeletparam>
		public StrengthenExpectationCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			ExpectedGroup group)
			: base("Strengthen Expectation", urgency, parent, coderack, workspace, slipnet) {
			this.group = group;
		}

		public override void Run() {
			if (group == null) {
				group = workspace.expectations.FindLargestExpectationGroup();
			}

			if (group == null)
				return;

			if (!workspace.expectations.groups.Contains(group))
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, group.MinLocation, group.MaxLocation);

			// Pick a measure in the group, and look for a link to the measure with inspired this expectation.
			for (int i = group.MinLocation; i <= group.MaxLocation; i++) {
				int idxFirst = i - workspace.expectations.Offset;
				if (idxFirst < 0 || idxFirst >= workspace.measures.Count || i >= workspace.measures.Count)
					continue;
				MeasureLink theLink = null;
				foreach (MeasureLink link in workspace.measureLinks) {
					if (link.m1.Location == i - workspace.expectations.Offset && link.m2.Location == i) {
						theLink = link;
						break;
					}
				}

				if (theLink == null) {
					// If no link, try to form one.
					MeasureLinkerCodelet mlc = new MeasureLinkerCodelet((int)this.rawUrgency, this, coderack, workspace, slipnet,
						workspace.measures[idxFirst], workspace.measures[i]);
					coderack.AddCodelet(mlc);
					continue;
				}

				// We have a link. 
				// Make sure it's not already listed
				bool found = false;
				foreach (GroupReason gr in group.Reasons) {
					if (gr is GroupReasonExpectationMeasureLink) {
						if (((GroupReasonExpectationMeasureLink)gr).link == theLink) {
							found = true;
							break;
						}
					}
				}
				if (found)
					continue;
				if (theLink.strength > 75) {
					GroupReasonExpectationMeasureLink grm = new GroupReasonExpectationMeasureLink(group, theLink.strength, theLink);
					group.Reasons.Add(grm);
				}
			}

		}
	}
}









