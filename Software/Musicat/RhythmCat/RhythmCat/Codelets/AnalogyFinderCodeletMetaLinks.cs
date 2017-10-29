using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, false)]
	public class AnalogyFinderCodelet : Codelet {

		
		/// <summary>
		/// The links to examine. If none given, we select randomly.
		/// </summary>
		private MeasureLink link1;
		private MeasureLink link2;


		public AnalogyFinderCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Analogy Scout", urgency, parent, coderack, workspace, slipnet) {

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
		public AnalogyFinderCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, 
			MeasureLink link1, MeasureLink link2)
			: base("Analogy Scout", urgency, parent, coderack, workspace, slipnet) {
			this.link1 = link1;
			this.link2 = link2;
		}

		public override void Run() {
			if (link1 == null) {
				link1 = workspace.PickRandomMeasureLinkByRecency();
			}

			if (link1 == null)
				return;

			if (link2 == null) {
				link2 = workspace.PickRandomMeasureLinkByRecency();
			}

			if (link2 == null || link1 == link2)
				return;

			// TODO: understand the kind of link involved. For now just use strength score.

			if (Math.Abs(link1.strength - link2.strength) < 10) {
				// Try making Analogy.
				Analogy a = new Analogy();
				a.metaLinks.Add(new MetaLink(link1, link2, 100));	// TODO: metalink strength

				float strength = a.Strength;

				double r = Utilities.rand.NextDouble() * 100;

				if (r < strength) {
					workspace.AddAnalogy(a);
				}
			}

		}
	}
}
