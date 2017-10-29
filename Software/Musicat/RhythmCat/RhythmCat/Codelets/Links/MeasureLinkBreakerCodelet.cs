using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Breaker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class MeasureLinkBreakerCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The link to examine. If none given, we select randomly.
		/// </summary>
		private MeasureLink link;


		public MeasureLinkBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Measure Link Breaker", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which measure to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public MeasureLinkBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, MeasureLink link)
			: base("Measure Link Breaker", urgency, parent, coderack, workspace, slipnet) {
			this.link = link;
		}

		public override void Run() {
			if (link == null) {
				link = workspace.PickRandomMeasureLinkByRecency();
			}

			if (link == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, link.m1.Location);
			workspace.RecordCodeletAttentionHistory(this, link.m2.Location);

			// Compare with other links from each measure.  Both measures need to have other alternate links, or else we leave alone.
			List<MeasureLink> links1 = workspace.GetOtherLinksForMeasure(link.m1, link);
			List<MeasureLink> links2 = workspace.GetOtherLinksForMeasure(link.m2, link);

			if (links1.Count == 0 && links2.Count == 0)
				return;

			float minStrength1 = GetMinStrength(links1);
			float minStrength2 = GetMinStrength(links2);

			float strengthToBeat = Math.Min(minStrength1, minStrength2);

			// If this link's strength is weaker than the "weakest link", try to destroy.
			if (link.strength <= strengthToBeat) {
				double r = Utilities.rand.NextDouble() * 100;

				if (r > link.strength) {
					workspace.BreakMeasureLink(link);
				}
			}
		}

		private float GetMaxStrength(List<MeasureLink> links) {
			float max = float.NegativeInfinity;

			foreach (MeasureLink link in links) {
				if (link.strength > max)
					max = link.strength;
			}

			return max;
		}

		private float GetMinStrength(List<MeasureLink> links) {
			float min = float.PositiveInfinity;

			foreach (MeasureLink link in links) {
				if (link.strength < min)
					min = link.strength;
			}

			return min;
		}
	}
}
