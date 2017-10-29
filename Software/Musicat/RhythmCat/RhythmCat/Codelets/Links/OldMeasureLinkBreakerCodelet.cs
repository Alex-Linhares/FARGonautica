using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	//[Codelet("Breaker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class OldMeasureLinkBreakerCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The link to examine. If none given, we select randomly.
		/// </summary>
		private MeasureLink link;
		private Analogy analogy;

		public OldMeasureLinkBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Old Measure Link Breaker", urgency, parent, coderack, workspace, slipnet) {

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
		public OldMeasureLinkBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, MeasureLink link)
			: base("Old Measure Link Breaker", urgency, parent, coderack, workspace, slipnet) {
			this.link = link;
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
		public OldMeasureLinkBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy analogy)
			: base("Old Measure Link Breaker", urgency, parent, coderack, workspace, slipnet) {
			this.analogy = analogy;
		}

		public override void Run() {
			double r;
			// If the analogy was specified, examine a link inside it.
			if (analogy != null) {
				// Pick links that start and end in the time span delimeted by the LHS of RHS of the analogy		
				List<MeasureLink> links = new List<MeasureLink>();

				foreach (MeasureLink link2 in workspace.measureLinks) {
					// Does it start in the LHS or RHS?
					if (analogy.IncludesLocation(link2.m1.Location) && analogy.IncludesLocation(link2.m2.Location)) {
						links.Add(link2);
					}
				}
				MeasureLink linkToDelete = Utilities.PickItem<MeasureLink>(links);

				if (linkToDelete == null)
					return;
				r = Utilities.rand.NextDouble() * 100;

				if (r > linkToDelete.strength / 2) {
					workspace.BreakMeasureLink(linkToDelete);
				}

				return;
		
			} else if (link == null) {					
				link = workspace.PickRandomMeasureLinkByOldest();
			}

			// Make sure we have a link, and make sure it's not too recent.
			if (link == null || link.MostRecentMeasure.Location + 4 > workspace.measures.Count)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, link.m1.Location);
			workspace.RecordCodeletAttentionHistory(this, link.m2.Location);


			r = Utilities.rand.NextDouble() * 100;

			if (r > link.strength) {
				workspace.BreakMeasureLink(link);
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
