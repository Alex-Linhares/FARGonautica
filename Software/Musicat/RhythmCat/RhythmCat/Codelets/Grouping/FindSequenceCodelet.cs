using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class FindSequenceCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The initial element to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement ge1;

		public FindSequenceCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Find Sequence", urgency, parent, coderack, workspace, slipnet) {

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
		public FindSequenceCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement ge1)
			: base("Find Sequence", urgency, parent, coderack, workspace, slipnet) {

			this.ge1 = ge1;
		}


		public override void Run() {
			if (ge1 == null) {
				ge1 = workspace.PickRandomGroupElementByRecencyAndStrength();
			}

			if (ge1 == null)
				return;

			// Pick 2 more elements to the right if we can.
			// TODO: be smarter about adjacent picking -- go for same-level/same-size groups?
			GroupElement ge2 = workspace.PickRandomGroupElementRightOf(ge1);
			if (ge2 == null)
				return;

			GroupElement ge3 = workspace.PickRandomGroupElementRightOf(ge2);
			if (ge3 == null)
				return;


			if (!workspace.GroupElements.Contains(ge1) || !workspace.GroupElements.Contains(ge2) || !workspace.GroupElements.Contains(ge3))
				return;

			//Make sure they are both of same level, or we can't group.
			// TODO: relax this appropriately for ge3
			if (ge1.Level != ge2.Level)
				return;
			if (Math.Abs(ge2.Level - ge3.Level) > 1)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, ge1.MinLocation, ge3.MaxLocation);

			// Check if sequence already exists!
			foreach (Group g in workspace.groups) {
				if (g is Sequence) {
					Sequence s = (Sequence)g;
					if (s.SequenceElements.Contains(ge1) && s.SequenceElements.Contains(ge2) && s.SequenceElements.Contains(ge3))
						return; // already exists.
				}
			}

			// Estimate the sequence's score.
			MelodyContour mc1 = new MelodyContour(ge1);
			MelodyContour mc2 = new MelodyContour(ge2);
			MelodyContour mc3 = new MelodyContour(ge3);

			// Need exact matches for first 2 elements.
			string contourStart = mc1.ToStringSimple();
			if (contourStart != mc2.ToStringSimple())
				return;

			// Need initial match for elements 2 and 3 (in case 3 is longer).
			if (!mc3.ToStringSimple().StartsWith(contourStart))
				return;

			// Strength based on # elements.
			double score = Math.Min(100, contourStart.Length * 30);

			double r = Utilities.rand.NextDouble() * 100;

			if (r < score) {
				Sequence s = new Sequence(workspace, ge1, ge2, ge3, score);
				workspace.AddSequence(s, score);	// TODO: hack: forcing score in.... but should be computed from reasons...
			}
		}
	}
}
