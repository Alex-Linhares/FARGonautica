using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using MusicPrimitives;

namespace RhythmCat
{
	/// <summary>
	/// Represents an ordered collection of ScaleDegrees within one octave as defined in the SeekWell project.  
	/// </summary>
	/// 

	public class Alphabet : IEnumerable, ICloneable
	{
		private List<ScaleDegreeWithStability> scaleDegrees;


		#region Properties
		/// <summary>
		/// Returns the number of items in the alphabet.
		/// </summary>
		public int Count {
			get {
				return scaleDegrees.Count;
			}
		}

		public string Name {
			get;
			protected set;
		}

		public int RootPitchClass {
			get;
			protected set;
		}

		public ScaleDegree RootScaleDegree {
			get;
			protected set;
		}
	
		public ScaleDegreeWithStability this[int index] {
			get {
				if (index < 0 || index >= Count)
					throw new IndexOutOfRangeException("Alphabet: index " + index + " is out-of-range.");

				return scaleDegrees[index];
			}
		}

		public bool StepCollection {
			get;
			private set;
		}

		#endregion

		#region Public methods

		#region Constructor

		public Alphabet() {
			scaleDegrees = new List<ScaleDegreeWithStability>();
		}

		#endregion

		public bool Contains(ScaleDegree sd) {
			foreach (ScaleDegree x in scaleDegrees)
				if (x.Number == sd.Number && x.Alteration == sd.Alteration)
					return true;
			return false;
		}

		public bool Contains(ScaleDegree sd, out ScaleDegreeWithStability found) {
			foreach (ScaleDegreeWithStability x in scaleDegrees)
				if (x.Number == sd.Number && x.Alteration == sd.Alteration) {
					found = x;
					return true;
				}
			found = null;
			return false;
		}

		/// <summary>
		/// Add a new ScaleDegree to the alphabet. ScaleDegrees must be added in the correct order; the order of Add calls determines alphabet adjacency.
		/// </summary>
		/// <param name="sd"></param>
		protected void Add(ScaleDegreeWithStability sd) {
			scaleDegrees.Add(sd);
		}


		public int GetIndexOfScaleDegree(ScaleDegree sd) {
			for (int i = 0; i < scaleDegrees.Count; i++) {
				ScaleDegree x = scaleDegrees[i];
				if (x.Number == sd.Number && x.Alteration == sd.Alteration)
					return i;
			}
			return -1;
		}

		/// <summary>
		/// Return the distance from n1 up to n2 in this alphabet and given key.
		/// Returns -1 if notes are not both in the alphabet.
		/// </summary>
		/// <param name="n1"></param>
		/// <param name="n2"></param>
		/// <param name="k"></param>
		/// <returns></returns>
		public int AlphabetDistance(Note n1, Note n2, Key k) {
			ScaleDegree s1 = n1.GetScaleDegree(k);
			ScaleDegree s2 = n1.GetScaleDegree(k);
			if (s1 == null || s2 == null)
				return -1;

			// What is distance in alphabet?
			int dist = 0;
			// Go up until we find note2.
			int idx1 = this.GetIndexOfScaleDegree(s1);
			int sdIndex = idx1;
			int octave = n1.GetOctave();

			Pitch p = k.GetScaleDegreePitch(this[sdIndex], octave);
			Pitch pOld = p;

			while (p.MidiNumber < n2.Midi) {
				sdIndex++;
				if (sdIndex >= this.Count)
					sdIndex = 0;
				dist++;
				p = k.GetScaleDegreePitch(this[sdIndex], octave);
				if (p.MidiNumber < pOld.MidiNumber) {
					octave++;
					p.Octave++;
				}

				pOld = p;
			}

			if (p.MidiNumber != n2.Midi) // overshot
				return -1;
			else
				return dist;
		}

		public bool HasSameContentAs(Alphabet alphabet) {
			if (this.Count != alphabet.Count)
				return false;
			for (int i = 0; i < this.Count; i++) {
				if (this[i].Number != alphabet[i].Number || this[i].Alteration != alphabet[i].Alteration)
					return false;
			}
			return true;
		}


		// Finds the scale degree in the alphabet. Returns stability.
		public bool isStable(ScaleDegree sd) {
			if (sd == null)
				return false;
			foreach (ScaleDegreeWithStability sds in scaleDegrees) {
				if (sds.Alteration == sd.Alteration && sds.Number == sd.Number)
					return sds.IsStable;
			}
			throw new Exception("ScaleDegree '" + sd.ToString() + "' not found in alphabet: " + this.ToString());
		}

		#endregion

		#region Static

		#region Static Data
		static List<Alphabet> CMajorAlphabets;
		static List<Alphabet> AMinorAlphabets;
		#endregion

		#region Static Constructor
		static Alphabet() {
			// Set up static alphabets.
			Key keyCmajor = new Key();
			Key keyAminor = new Key(0, KeyMode.Minor);
			CMajorAlphabets = new List<Alphabet>();
			AMinorAlphabets = new List<Alphabet>();

			// Set up staticC major alphabets.
			Alphabet CMajorScale = GetScaleAlphabet(keyCmajor);
			GenerateAlphabetsForScale(keyCmajor, CMajorScale, CMajorAlphabets, false);

			// Set up static A minor alphabets.
			Alphabet AMinorScale = GetScaleAlphabet(keyAminor);
			Alphabet AMelodicMinorScale = GetScaleAlphabet(keyAminor, false, true);
			Alphabet AHarmonicMinorScale = GetScaleAlphabet(keyAminor, true, false);
			
			GenerateAlphabetsForScale(keyAminor, AMinorScale, AMinorAlphabets, false);
			GenerateAlphabetsForScale(keyAminor, AMelodicMinorScale, AMinorAlphabets, true);
			GenerateAlphabetsForScale(keyAminor, AHarmonicMinorScale, AMinorAlphabets, true);
		}
		#endregion

		#region Static private functions
		private static void GenerateAlphabetsForScale(Key key, Alphabet scale, List<Alphabet> alphabetList, bool checkPreviousAlphabets) {
			alphabetList.Add(scale);

			// Add all the triads and 7th chords in A melodic minor (skipping duplicates from earlier).
			foreach (ScaleDegree sd in scale.scaleDegrees) {
				Pitch p = key.GetScaleDegreePitch(sd, 4);
				Note n = new Note(0, false, false, new MidiInfo(p.MidiNumber, p.Accidental));
				if (checkPreviousAlphabets) {
					AddIfNew(alphabetList, GetTriadAlphabetForRoot(n, key, scale));
					AddIfNew(alphabetList, GetSeventhAlphabetForRoot(n, key, scale));
				} else {
					alphabetList.Add(GetTriadAlphabetForRoot(n, key, scale));
					alphabetList.Add(GetSeventhAlphabetForRoot(n, key, scale));
				}
			}
		}

		private static void AddIfNew(List<Alphabet> alphabetList, Alphabet alphabet) {
			// Look for pre-existing copy of this alphabet
			foreach (Alphabet a2 in alphabetList) {
				if (a2.HasSameContentAs(alphabet))
					return;
			}
			alphabetList.Add(alphabet);
		}
		#endregion

		#region Static methods to generate alphabets
		/// <summary>
		///  Returns an alphabet for a scale in the given key.
		/// </summary>
		/// <param name="bass"></param>
		/// <returns></returns>
		public static Alphabet GetScaleAlphabet(Key k, bool harmonicMinor = false, bool melodicMinor = false) {
			Alphabet a = new Alphabet();

			for (int i = 1; i < 8; i++) {
				Alteration alteration = Alteration.None;
				if (k.Mode == KeyMode.Minor) {
					if (i == 3 || (i == 6 && !melodicMinor) || (i == 7 && !harmonicMinor && !melodicMinor)) {
						alteration = Alteration.Lowered;
					}
				}
				int stability;
				switch (i) {
					case 1:
						stability = 3;	//root
						break;
					case 3:
						stability = 1;	//3rd
						break;
					case 5: 
						stability = 2;	//dominant
						break;
					default:
						stability = 0;
						break;
				}
				a.Add(new ScaleDegreeWithStability(i, alteration, stability));
			}

			a.Name = k.ToString();
			a.RootScaleDegree = new ScaleDegree(1, Alteration.None);
			a.RootPitchClass = k.GetScaleDegreePitchClass(a.RootScaleDegree);
			a.StepCollection = true;
			return a;
		}


		/// <summary>
		///  Returns an alphabet for a triad on the given root in the given key.
		/// </summary>
		/// <param name="bass"></param>
		/// <returns></returns>
		static public Alphabet GetTriadAlphabetForRoot(Note bass, Key k, Alphabet aScale) {
			Alphabet a = GetStackedTriadAlphabetForBass(bass, k, aScale, 2);
			return a;
		}

		/// <summary>
		///  Returns an alphabet for a 7th chord on the given root in the given key.
		/// </summary>
		/// <param name="bass"></param>
		/// <returns></returns>
		static public Alphabet GetSeventhAlphabetForRoot(Note bass, Key k, Alphabet aScale) {
			Alphabet a = GetStackedTriadAlphabetForBass(bass, k, aScale, 3);
			return a;
		}

		/// <summary>
		///  Returns an alphabet for a stacked triadic style chord on the given root in the given key.
		/// </summary>
		/// <param name="bass"></param>
		/// <param name="numStackedThirds">Set to 2 for a normal triad, 3 for a 7th, 4 for a ninth, etc.</param>
		/// <returns></returns>
		static public Alphabet GetStackedTriadAlphabetForBass(Note bass, Key k, Alphabet aScale, int numStackedThirds) {
			int bassIdx = -1;
			for (int i = 0; i < aScale.Count; i++) {
				ScaleDegree sd = aScale[i];
				if ((bass.Midi - k.GetScaleDegreePitch(sd, 0).MidiNumber) % 12 == 0)
					bassIdx = i;
			}
			if (bassIdx == -1) {
				throw new Exception("Can't compute bass scale degree");
			}

			Alphabet a = GetStackedTriadAlphabetForScaleDegree(k, aScale, numStackedThirds, bassIdx + 1);
			return a;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="aScale">Source scale alphabet</param>
		/// <param name="numStackedThirds">Set to 2 for a normal triad, 3 for a 7th, 4 for a ninth, etc.</param>
		/// <param name="scaleDegree">1 to 7</param>
		/// <returns></returns>
		private static Alphabet GetStackedTriadAlphabetForScaleDegree(Key k, Alphabet aScale, int numStackedThirds, int scaleDegree) {
			Alphabet aTriad = new Alphabet();

			for (int offset = 0; offset <= numStackedThirds * 2; offset += 2) {
				int stability;
				switch (offset) {
					case 0:
						stability = 3; // root;
						break;
					case 4:
						stability = 2; // 5th
						break;
					default:
						stability = 1;	// other thirds
						break;
				}
				ScaleDegreeWithStability sd = new ScaleDegreeWithStability(aScale[(scaleDegree - 1 + offset) % aScale.Count], stability);
				aTriad.Add(sd);
			}

			aTriad.Name = k.GetScaleDegreePitch(aScale[scaleDegree - 1], 4).ToString();
			
			// Get quality of first interval.
			int pc1 = k.GetScaleDegreePitchClass(aTriad[0]);
			int pc2 = k.GetScaleDegreePitchClass(aTriad[1]) + 12;
			int diff = (pc2 - pc1) % 12;
			if (diff == 3)
				aTriad.Name += "m";

/*			if (numStackedThirds == 2)
				//aTriad.Name += " triad";
			else
*/
			if (numStackedThirds > 2)
				aTriad.Name += (numStackedThirds * 2 + 1).ToString();  //7, 9, etc. for 7th, 9th chords
			aTriad.RootScaleDegree = new ScaleDegree(scaleDegree, Alteration.None);
			aTriad.RootPitchClass = k.GetScaleDegreePitchClass(aTriad.RootScaleDegree);

			aTriad.StepCollection = false;
			return aTriad;
		}

		/// <summary>
		/// Returns a major scale, with stable goals marked on the diven degree and its triad members above it.
		/// </summary>
		/// <param name="key"></param>
		/// <param name="degree">From 1 to 7</param>
		/// <returns></returns>
		static public Alphabet GetMajorScaleAlphabetWithTriadOnDegree(Key key, int degree) {
			Alphabet aScale = GetScaleAlphabet(key);
			Alphabet aStable = GetStackedTriadAlphabetForScaleDegree(key, aScale, 2, degree);

			// Adjust stabilty
			foreach (ScaleDegreeWithStability sd in aScale) {
				ScaleDegreeWithStability member;
				if (aStable.Contains(sd, out member))
					sd.Stability = member.Stability;
				else
					sd.Stability = 0;
			}
			//aScale.Name += ":" + aStable.Name;
			aScale.Name = aStable.Name;
			aScale.RootScaleDegree = aStable.RootScaleDegree;
			aScale.RootPitchClass = aStable.RootPitchClass;
			return aScale;
		}

		static public Alphabet GetAlphabetFromScaleWithTriadOnDegree(Key key, Alphabet scale, ScaleDegree sd) {
			Alphabet a = (Alphabet)scale.Clone();

			// record the index of the root.
			int rootDegree = -1;
			for (int i = 0; i < a.Count; i++) {
				ScaleDegreeWithStability sds = a[i];
				if (sds.Number == sd.Number && sds.Alteration == sd.Alteration) {
					rootDegree = i;
					break;
				}
			}

			// Assign stability for each alphabet member.
			for (int j = 0; j < a.Count; j++) {
				ScaleDegreeWithStability sds = a[(rootDegree + j) % a.Count];	// start at the root, and go up 7 scale degrees (with mod to wrap around)
				int stability;
				switch (j) {
					case 0:
						stability = 3; // root;
						break;
					case 2:
						stability = 1; // 3rd
						break;
					case 3:
						stability = 2; // 5th
						break;
					default:
						stability = 0;	// other thirds
						break;
				}
				sds.Stability = stability;
			}

			a.Name = key.GetScaleDegreePitch(sd, 4).ToString();


			// Get quality of first third.
			int pc1 = key.GetScaleDegreePitchClass(a[rootDegree]);
			int pc2 = key.GetScaleDegreePitchClass(a[(rootDegree + 2) % a.Count]) + 12;
			int diff = (pc2 - pc1) % 12;
			if (diff == 3)
				a.Name += "m";

			//a.Name += " triad";
			a.RootScaleDegree = sd;
			a.RootPitchClass = key.GetScaleDegreePitchClass(sd);
			return a;		
		}


		#endregion

		#region Static methods to select alphabets

		/// <summary>
		/// Given a soure alphabet list, filters out the ones which are consistent.
		/// </summary>
		/// <param name="scaleDegrees"></param>
		/// <returns></returns>
		public static List<Alphabet> GetConsistentAlphabetsForScaleDegrees(List<Alphabet> sourceAlphabets, List<ScaleDegree> scaleDegrees) {
			List<Alphabet> alphabets = new List<Alphabet>();

			foreach (Alphabet a in sourceAlphabets) {
				// Make sure each note exists in the alphabet.
				bool allExist = true;
				foreach (ScaleDegree sd in scaleDegrees) {
					if (!a.Contains(sd)) {
						allExist = false;
						break;
					}
				}
				if (allExist)
					alphabets.Add(a);
			}

			return alphabets;
		}

		/// <summary>
		/// Returns either C Major or A minor alphabets, as requested by the given KeyMode.
		/// </summary>
		/// <param name="sourceAlphabets"></param>
		/// <param name="scaleDegrees"></param>
		/// <param name="mode"></param>
		/// <returns></returns>
		public static List<Alphabet> GetConsistentAlphabetsForScaleDegrees(List<ScaleDegree> scaleDegrees, KeyMode mode) {
			if (mode == KeyMode.Major)
				return GetConsistentAlphabetsForScaleDegrees(CMajorAlphabets, scaleDegrees);
			else
				return GetConsistentAlphabetsForScaleDegrees(AMinorAlphabets, scaleDegrees);
		}

		#endregion

		#endregion

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			foreach (ScaleDegree sd in scaleDegrees) {
				sb.Append(sd.ToString());
				sb.Append(' ');
			}
			return sb.ToString();
		}
		#region IEnumerable Members

		public IEnumerator GetEnumerator()
		{
			return scaleDegrees.GetEnumerator();
		}

		#endregion




		public object Clone() {
			Alphabet a = new Alphabet();
			foreach (ScaleDegreeWithStability sd in this) {
				a.Add((ScaleDegreeWithStability)sd.Clone());
			}
			a.Name = Name;
			a.RootScaleDegree = RootScaleDegree;
			a.RootPitchClass = RootPitchClass;
			a.StepCollection = StepCollection;
			return a;
		}

	}
}
