using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	public abstract class GroupElement {
		public bool hasChildren;
		public bool hasParent {
			get {
				return parentGroup != null;
			}
		}
		public Group parentGroup;

		#region Readonly

		private readonly List<Alphabet> possibleGroupAlphabets;	// all available alphabet choices to associate with groups

		#endregion

		public GroupElement(Workspace workspace) {
			this.workspace = workspace;
			this.hasChildren = false;
			this.parentGroup = null;
			possibleGroupAlphabets = new List<Alphabet>();
			Init();
		}

		private void Init() {
			// Set up all possible alphabets for this group:
			// Major (if key = C)
			//   C major, D dorian, E phrygian, F lydian, G mixolydian, A aolian, B locrian
			// Minor: (if key = A minor)
			//   A minor, TODO....
			// Really, I just generate the major or minor scale and set the appropriate triad members as goal tonesvia stability.
			// equivalent to keeping the C major alphabet and changing the goal alphabet, but I'm using one alphabet with stability instead.
			if (Key.Mode == KeyMode.Major) {
				for (int i = 1; i <= 7; i++) {
					possibleGroupAlphabets.Add(Alphabet.GetMajorScaleAlphabetWithTriadOnDegree(Key, i));
				}
			} else {
				// Add all 3 minor scale types.
				List<Alphabet> scales = new List<Alphabet>();
				scales.Add(Alphabet.GetScaleAlphabet(Key, false, false));
				scales.Add(Alphabet.GetScaleAlphabet(Key, true, false));
				scales.Add(Alphabet.GetScaleAlphabet(Key, false, true));
				foreach (Alphabet s in scales) {
					foreach (ScaleDegree sd in s) {
						possibleGroupAlphabets.Add(Alphabet.GetAlphabetFromScaleWithTriadOnDegree(Key, s, sd));
					}
				}
			}
		}


		public abstract GroupElement DeepCopy();

		public abstract int Location { get; }
		public abstract int MinLocation { get; }
		public abstract int MaxLocation { get; }

		public string FormLabel { get; set; }
		public double FormLabelStrength { get; set; }

		protected Workspace workspace;

		/// <summary>
		/// Returns the hierarchical level of this element. Measures are level 0. 1st-order groups and level 1, etc.
		/// </summary>
		abstract public int Level { get; }

		abstract public int Count { get; }

		public int IndexInParent {
			get {
				if (parentGroup == null)
					return -1;
				return parentGroup.GroupElements.IndexOf(this);
			}
		}

		abstract public List<Measure> Measures {
			get;
		}


		public Key Key {
			get {
				return workspace.Key;
			}
		}


		public Alphabet Alphabet {
			get;
			set;
		}


		public double AlphabetStrength { 
			get; 
			set; 
		}

		public List<NoteWithAttackPoint> AllNotes {
			get {
				List<NoteWithAttackPoint> notes = new List<NoteWithAttackPoint>();
				foreach (Measure m in Measures) {
					int time = 0;
					for (int i = 0; i < m.rhythm.notes.Count; i++) {
						Note n = m.rhythm.notes[i];
						notes.Add(new NoteWithAttackPoint(n, time));
						time += n.duration;
					}
				}
				return notes;
			}
		}

		public List<int> AttackPoints {
			get {
				List<Note> notes = new List<Note>();
				foreach (NoteWithAttackPoint nwap in AllNotes) {
					notes.Add(new Note(nwap.duration, nwap.tiedBefore, nwap.tiedAfter, nwap.midiInfo));
				}
				Rhythm r = new Rhythm(notes);
				return r.AttackPoints;
			}
		}

		public bool IncludesLocation(int p) {
			return p >= MinLocation && p <= MaxLocation;
		}


		public abstract int LengthInMeasures { get; }

		public bool OverlapsRange(int minMeasure, int maxMeasure) {
			 //(StartA <= EndB) And (EndA >= StartB)

			// Ex: this group (A) is m. 2-4.  input range: 0-1.  output: 2 <= 1 && 4 >= 0   => false
											//input range: 0-2: output: 2 <= 2 && 4 >= 0  -> true
											//             5-6: output: 2 <= 6 && 4 >= 5 -> false
											//             4-6: output: 2 <= 6 && 4 >= 4 -> true
											//             3-5: output: 2 <= 5 && 4 >= 3 -> true
											//             1-3: output: 2 <= 3 && 4 >= 1 -> true

			return (this.MinLocation <= maxMeasure) && (this.MaxLocation >= minMeasure);
		}


        public bool Overlaps(GroupElement ge2) {
            if (this.MinLocation == ge2.MinLocation)
                return true;

            if (this.MinLocation < ge2.MinLocation) {
                return (this.MaxLocation >= ge2.MinLocation);
            } else {
                return (ge2.MaxLocation >= this.MinLocation);
            }
        }


		/// <summary>
		/// Compute the happiness of this item, from 0 (unhappy) to 100 (nirvana)
		/// </summary>
		/// <returns></returns>
		/// <param name="weight">The weight to use for this component, when considering multiple happiness components. 
		/// Weight based on "importance" of structure.</param>
		abstract public double ComputeHappiness(out double weight);

		/// <summary>
		/// Returns true if this groupelement has the excact same range (in measures) of the given groupelement.
		/// </summary>
		/// <param name="parent"></param>
		/// <returns></returns>
		public abstract bool MatchesRangeOf(GroupElement target);

		#region Alphabet Choice

		/// <summary>
		/// Returns the likely alphabets for this group, with a score from 0 to 100 representing the probability that each alphabet is correct.
		/// Alphabets are chosen based on looking at longer notes on stronger beats.
		/// </summary>
		/// <returns></returns>
		public List<Tuple<Alphabet, float>> GetAlphabetsWithLikelihoods() {
			List<Tuple<Alphabet, float>> results = new List<Tuple<Alphabet, float>>();
			int[] attackPointLevels = Measures[0].GetAttackPointLevels();

			// Score all possible alphabets.
			float totalScore = 0;
			foreach (Alphabet a in possibleGroupAlphabets) {
				// set up scoring for each pitch in the alphabet.
				Dictionary<int, float> scores = new Dictionary<int, float>();	// (pitch class, score) Scores between 0 and 1.
				foreach (ScaleDegree sd in a)
					scores[Key.GetScaleDegreePitchClass(sd)] = 0;

				// Tally the 1) duration and 2) beat strength for each pitch in the group, if it's found in the alphabet.
				List <NoteWithAttackPoint> notes = AllNotes;
				foreach (NoteWithAttackPoint n in notes) {
					int dur = Math.Min(n.duration, 16);	// 1-16
					int attack = n.AttackPoint;
					int level = attackPointLevels[attack];	// stength from 0-4 of beat position.
                    if (n.midiInfo == null)
                        continue;
                    int pc = n.midiInfo.PitchClass;
					if (scores.ContainsKey(pc))
						scores[pc] += (dur / 16.0f) * Constants.ALPHABET_SCORING_DURATION_WEIGHT + (level / 4.0f) * Constants.ALPHABET_SCORING_LEVEL_WEIGHT;
				}

				// Bonus for ending group in chord (just add additional salience to final note).
				if (notes.Count > 0) {
					NoteWithAttackPoint n = notes[notes.Count - 1];
                    if (n.midiInfo != null) {
                        int pc = n.midiInfo.PitchClass;
                        if (scores.ContainsKey(pc)) {
                            int dur = Math.Min(n.duration, 16);	// 1-16
                            scores[pc] += (dur / 16.0f) * Constants.ALPHABET_SCORING_FINAL_NOTE_WEIGHT;
                        }
                    }
				}

				// Sum the scores over all notes in the alphabet to get alphabet score.
				float score = 0;
				foreach (ScaleDegreeWithStability sd in a) {
					// TODO: add in small score for non-stable notes too.
					score += sd.Stability * scores[Key.GetScaleDegreePitchClass(sd)];
				}

				// Bonus for starting piece on tonic chord.
				if (this.MinLocation == 0 && a.RootScaleDegree.IsTonic)
					score *= 5;

			
				//score = 100.0f * (score / 7);
				results.Add(Tuple.Create(a, score));
				totalScore += score;
			}

			// Normalize results.
			List<Tuple<Alphabet, float>> resultsNormalized = new List<Tuple<Alphabet, float>>();
			foreach (Tuple<Alphabet, float> x in results) {
				resultsNormalized.Add(Tuple.Create(x.Item1, 100.0f * x.Item2/totalScore));
			}

			return resultsNormalized;
		}

		#endregion


    }
}
