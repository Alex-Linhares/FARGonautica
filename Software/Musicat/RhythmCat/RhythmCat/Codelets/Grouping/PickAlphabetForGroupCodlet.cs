using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Alphabet", CodeletAttribute.CodeletWorkType.Create, 10, true)]
	public class PickAlphabetForGroupCodelet : Codelet {

		/// <summary>
		/// The group or measure to examine. If none given, we select randomly.
		/// </summary>
		private GroupElement e;


		public PickAlphabetForGroupCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Alphabet Picker", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which element to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public PickAlphabetForGroupCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, GroupElement e)
			: base("Alphabet Picker", urgency, parent, coderack, workspace, slipnet) {
			this.e = e;
		}



		public override void Run() {
			if (e == null) {
				// For now, just pick groups, not measures.
				//e = workspace.PickRandomGroupElementByRecency();
				e = workspace.PickRandomGroupElementByRecency();
			}

			if (e == null)
				return;

			if (!workspace.GroupElements.Contains(e))
				return;

			// Only look at 1-4 measure long groups.
			if (e.LengthInMeasures < 1 || e.LengthInMeasures > 4)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, e.MinLocation, e.MaxLocation);

			// Generate possible alphabets and scores.
			List<Tuple<Alphabet, float>> alphabets = e.GetAlphabetsWithLikelihoods();

			// TODO: transition probs.
			// Is there an alphabet in the previous group?
			float[] transitionProbs = new float[alphabets.Count];
			GroupElement ePrev = workspace.GetPreviousGroupElement(e);
			Alphabet aPrev = null;
			if (ePrev != null)
				aPrev = ePrev.Alphabet;

			if (aPrev == null) {
				// No previous alphabet.
				for (int i = 0; i < alphabets.Count; i++) 
					transitionProbs[i] = 1.0f;
			} else {
				// Previous alphabet known.
				for (int i = 0; i < alphabets.Count; i++) { 
					float prob = 1.0f;
					Alphabet a = alphabets[i].Item1;

					// Estimate transition probability. Weights dont' have to sum to 1.
					int diff = ((aPrev.RootPitchClass + 24) - a.RootPitchClass) % 12;
					if (diff == 0)
						prob = 1.5f;	// self transition is tricky.
					else if (diff == 7)
						prob = 3.0f;
					else if (aPrev.RootScaleDegree.Number == 4 && a.RootScaleDegree.Number == 5)
						prob = 3.0f;
					else if (aPrev.RootScaleDegree.Number == 5 && a.RootScaleDegree.Number == 1)
						prob = 5.0f;
					else if ((aPrev.RootScaleDegree.Number + 7 - a.RootScaleDegree.Number) % 7 == 2)
						prob = 2.0f;

					transitionProbs[i] = prob;
				}
			}

			// Is there an alphabet already?
			Alphabet existing = e.Alphabet;
			if (existing == null) {
				// Multiply by transition probabilities from previous group.
				List<Utilities.ObjectValuePair> pairs = ConvertTupleListToPairsAndMultiplyByWeights(alphabets, transitionProbs);
				
				// Pick one!
				Utilities.ObjectValuePair pair = Utilities.PickItemWeightedReturnPair(pairs);
				
				e.Alphabet = (Alphabet)pair.obj;
				e.AlphabetStrength = pair.value;
				return;
			}

			// An alphabet already exists. Need to compeete. Also, take into account previous measure alphabet and transition probability.
			Utilities.ObjectValuePair choice = Utilities.PickItemWeightedReturnPair(ConvertTupleListToPairsAndMultiplyByWeights(alphabets, transitionProbs));
			if (Utilities.FightItOut(choice.value, e.AlphabetStrength, workspace.Temperature)) {
				// The new alphabet won. Replace.
				e.Alphabet = (Alphabet)choice.obj;
				e.AlphabetStrength = choice.value;
			}

		}

		private List<Utilities.ObjectValuePair> ConvertTupleListToPairsAndMultiplyByWeights(List<Tuple<Alphabet, float>> tuples, float[] weights) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();
			for (int i = 0; i < tuples.Count; i++) {
				Tuple<Alphabet, float> x = tuples[i];
				float weight = weights[i];
				pairs.Add(new Utilities.ObjectValuePair(x.Item1, x.Item2 * weight));
			}
			return pairs;
		}

	}
}
