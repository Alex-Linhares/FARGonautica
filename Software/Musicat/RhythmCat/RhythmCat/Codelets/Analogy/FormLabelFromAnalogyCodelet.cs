using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Labeler", CodeletAttribute.CodeletWorkType.Create, 10, true)]
	public class FormLabelFromAnalogy : Codelet {

		private const float DEFAULT_ANALOGY_LABEL_STRENGTH = 80;

		/// <summary>
		/// The analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy a;


		public FormLabelFromAnalogy(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Form Label Assigner", urgency, parent, coderack, workspace, slipnet) {

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
		public FormLabelFromAnalogy(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy a)
			: base("Form Label Assigner", urgency, parent, coderack, workspace, slipnet) {
			this.a = a;
		}



		public override void Run() {
			if (a == null) {
				a = workspace.PickRandomAnalogyByRecency();
			}

			if (a == null)
				return;

			if (!workspace.analogies.Contains(a))
				return;

			// Examine each relationship
			// TODO: do we have to dig down into subanalogies??????????????????
			foreach (Relationship r in a.relationships) {
				// Add to attention history.
				workspace.RecordCodeletAttentionHistory(this, r.LHS.MinLocation, r.LHS.MaxLocation);
				workspace.RecordCodeletAttentionHistory(this, r.RHS.MinLocation, r.RHS.MaxLocation);


				GroupElement right = r.RHS;
				if (right.FormLabelStrength > 99.9999)
					continue;

				// Grab the left side, and try to make the right side labelled analogously.
				GroupElement left = r.LHS;
				if (left.FormLabelStrength > 0) {
					// If the relationship is exact, just copy the label.
					if (r.Strength > 99.999) {
						SetLabel(right, left.FormLabelStrength, left.FormLabel);
						return;
					}
					
					if (workspace.FightItOut(r.Strength * 2, right.FormLabelStrength)) {
						// Make a prime version.
						string label;
						if (left.FormLabel[left.FormLabel.Length - 1] == '\'')
							label = "(" + left.FormLabel + ")'";
						else
							label = left.FormLabel + '\'';
						SetLabel(right, 100, label);
					}
				}
			}
		}

		private void SetLabel(GroupElement ge, double strength, string label) {
			// Set the label.
			ge.FormLabel = label;
			ge.FormLabelStrength = strength;
		}

	}
}

			/*
			
			// Find previous elements linking to this object.

			if (e is Measure) {
				Measure m = (Measure)e;

				// Special case: measure 1. Just call it "a".
				if (m.number == 0) {
					//				TryToSetMeasureLabel(m, DEFAULT_LABEL_STRENGTH, workspace.GetNextFormLabel(0));
					m.FormLabel = "a";
					m.FormLabelStrength = 100;
					return;
				}


				// Find previous measures linking to this measure.
				List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();
				for (int i = 0; i < m.number; i++) {
					Measure m2 = workspace.measures[i];
					// Find links.
					foreach (MeasureLink link in workspace.measureLinks) { // TODO: speed up link lookup.
						if ((link.m1 == m && link.m2 == m2) || link.m1 == m2 && link.m2 == m) {
							// Match.
							Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(link, link.strength);
							pairs.Add(pair);
						}
					}
					// If pairs is empty, try to make a new label of default strength, and quit.
					// If a label exists, we fight.
					if (pairs.Count == 0) {
						TryToMakeNewMeasureLabel(m, DEFAULT_LABEL_STRENGTH);
						return;
					}

					// Pick a link probabilistically based on weights. 
					MeasureLink selectedLink = (MeasureLink)Utilities.PickItemWeighted(pairs);

					// Decide whether or not to try to add label based on this link, probabilitically.
					double r = Utilities.rand.NextDouble() * 100;
					if (r < selectedLink.strength) {
						// We're going to try to label. Should it be a duplicate of the former label or a "prime" version" or compeltely new? 
						// If it's a different measure at all, don't use the origianl label. If it's different, decide based on strength of link whether to make ' version.
						float similarity = m.ComputeSimilarity(m2);

						if (similarity > 99.9) {
							// Use same label.
							TryToSetMeasureLabel(m, 100, m2.FormLabel);
							return;
						}

						// Make a prime version.
						MakeLinkedMeasureLabel(m, selectedLink);
						return;
					} else {
						//Link was too weak; just assign a new label.
						TryToMakeNewMeasureLabel(m, DEFAULT_LABEL_STRENGTH);
						return;
					}
				}
			} else {
				// Group case.
			}




		}

		private void MakeLinkedMeasureLabel(Measure m, MeasureLink selectedLink) {
			// Check the original measure for a label. If it has none, assign it and this measure at the same time.
			Measure other = selectedLink.Other(m);
			if (other.FormLabel == null) {
				SetMeasureLabel(other, DEFAULT_LABEL_STRENGTH, workspace.GetNextFormLabel(0));
			}

			// Make a prime label.
			string label;
			if (other.FormLabel[other.FormLabel.Length - 1] == '\'')
				label = "(" + other.FormLabel + ")'";
			else
				label = other.FormLabel + '\'';
			TryToSetMeasureLabel(m, selectedLink.strength, label);
		}

		private void TryToMakeNewMeasureLabel(Measure m, float strength) {
			// If a label such as a' already exists, we will fight for the label.
			// However, if a single-character label already exists, there is no need to change it to a new letter; leave it alone.
			if (m.FormLabel != null && m.FormLabel.Length == 1)
				return;

			TryToSetMeasureLabel(m, strength, workspace.GetNextFormLabel(0));
		}

		private void TryToSetMeasureLabel(Measure m, float strength, string label) {
			// Try to delete an existing label, if necessary.
			if (m.FormLabel != null) {
				// If there are prime forms based on this label (or copies of it), do nothing; never change a "fixed" label.
				string primeForm1 = m.FormLabel + "'";
				string primeForm2 = "(" + m.FormLabel + ")'";

				foreach (Measure m2 in workspace.measures) {
					if (m2 == m) continue;

					if (m2.FormLabel != null) {
						if (m.FormLabel.Length == 1) {
							if (m2.FormLabel.Contains(m.FormLabel))
								return;
						}
						if (m2.FormLabel.Contains(primeForm1) || m2.FormLabel.Contains(primeForm2))
							return;
					}

				}
				// Fight! if we lose, do nothing.
				if (!workspace.FightItOut(strength, m.FormLabelStrength)) {
					return;
				}
			}
			// Set the label.
			SetMeasureLabel(m, strength, label);
		}

		private void SetMeasureLabel(Measure m, float strength, string label) {
			// Set the label.
			m.FormLabel = label;
			m.FormLabelStrength = strength;
		}


		private void MakeNewLabel(Measure m) {
			SetMeasureLabel(m, DEFAULT_LABEL_STRENGTH, workspace.GetNextFormLabel(0));
		}
	}
}
			*/