using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using MusicPrimitives;

namespace RhythmCat {

	public class Rhythm : ICloneable {
		/// <summary>
		/// Notes in this measure, in order.
		/// </summary>
		public List<Note> notes;
	
		protected Dictionary<int, Note> notesByAttackPoint;	// links attack points (including rests) to notes.

        /// <summary>
        ///  Kept in sorted order. Rests are not included; only actual sounding attack points are listed.
        /// </summary>
        public List<int> AttackPoints {
            get;
            protected set;
        }

        /// <summary>
        ///  kept in sorted order. Includes all points where a note starts, including rests, and 
        ///  disregarding ties (ties are ignored and rearticulated according to this; used for drawing.
        /// </summary>
        public List<int> AttackPointsIncludingRests {
            get;
            protected set;
        }

		protected string measureStr;

		/// <summary>
		/// In 16ths.
		/// </summary>
		public int TotalDuration {
			get; private set;
		}

		public bool ContainsAttackPoint(int attackPoint) {
			return AttackPoints.Contains(attackPoint);
		}

		public bool ContainsNoteEvent(int attackPoint) {
			return notesByAttackPoint.Keys.Contains(attackPoint);
		}

		public Rhythm() {
			this.notes = new List<Note>();
            this.AttackPoints = new List<int>();
			this.notesByAttackPoint = new Dictionary<int, Note>();
		}

		public Rhythm(List<Note> notes) {
			this.notes = notes;

			resetAttackPoints();
		}

		/// <summary>
		/// Set up the attack point list and update note last-status.
		/// </summary>
		protected void resetAttackPoints() {
            this.AttackPoints = new List<int>();
            this.AttackPointsIncludingRests = new List<int>();
            this.notesByAttackPoint = new Dictionary<int, Note>();
			int time = 0;
			foreach (Note note in notes) {
				if (!note.isRest && !note.tiedBefore)
                    this.AttackPoints.Add(time);
                this.AttackPointsIncludingRests.Add(time);
                notesByAttackPoint[time] = note;
				time += note.duration;
				note.isLastInMeasure = false;
			}
			this.TotalDuration = time;

			if (notes.Count > 0)
				notes[notes.Count-1].isLastInMeasure = true;
		}


		/// <summary>
		/// Regular expression used to parse rhythms from strings.
		/// </summary>
		private static Regex regexRhythmMeasure = new Regex(@"
			#^									# be sure to match start of token, so Bbb works, etc.
			(?<entry>
			  (?<duration>s|e|e\.|q|q\.|h|h\.|w)			 # 1) duration
			  (?<pitch>[a-gA-GrR])?							 # 2) note name, or r/R for rest
			  (?<accidental>\#|b|x|bb)?						 # 3) optional accidental
			  (?<octave>\d)?								 # 4) optional octave
			  (?<tie>_)?									 # 5) optional tie to next note
			  \s*											# 6) optional space separates notes
			)+
			$                                # as above, also match end of token
			", RegexOptions.IgnorePatternWhitespace | RegexOptions.IgnoreCase);

		private static Regex regexRhythmNote = new Regex(@"
			#^									# be sure to match start of token, so Bbb works, etc.
			  (?<duration>s|e|e\.|q|q\.|h|h\.|w)           # 1) duration
			  (?<pitch>[a-gA-GrR])?						   # 2) optional note name, or r/R for rest
			  (?<accidental>\#|b|x|bb)?						 # 3) optional accidental
			  (?<octave>\d)?								 # 4) optional octave
			  (?<tie>_)?									 # 5) optional tie to next note
			$                                # as above, also match end of token
			", RegexOptions.IgnorePatternWhitespace | RegexOptions.IgnoreCase);

		private static readonly int[] pitchOffsets = { 9, 11, 0, 2, 4, 5, 7 };    // # half steps above C, where index is the # white keys above A.

		public Rhythm(string measureStr, bool incomingTie, out bool tieToNextMeasure, ref int defaultOctave) {
			this.measureStr = measureStr;
			this.notes = new List<Note>();
			
			// Parse the measure into notes.
			Match m = regexRhythmMeasure.Match(measureStr);
			if (!m.Success)
				throw new ArgumentException("Measure '" + measureStr + "' not parsed.");

			// Parse each note.
			bool tiedAfter = false;
			bool tiedBefore = incomingTie;
			foreach (Capture c in m.Groups["entry"].Captures) {
				string note = c.ToString();

				// Now we have to reparse, which is silly.
				Match m2 = regexRhythmMeasure.Match(note);
				if (!m2.Success)
					throw new ArgumentException("Note '" + note + "' not parsed.");

				string durationStr = m2.Groups["duration"].Value.ToUpper();
				string tie = m2.Groups["tie"].Value;
				string pitchStr = m2.Groups["pitch"].Value;
				string accidental = m2.Groups["accidental"].Value;
				string octaveString = m2.Groups["octave"].Value;
				bool isRest = (pitchStr == "r" || pitchStr == "R");
				tiedAfter = (tie != "");
				int duration;

				switch (durationStr) {
					case "Q":
						duration = 4;
						break;
					case "Q.":
						duration = 6;
						break;
					case "H":
						duration = 8;
						break;
					case "H.":
						duration = 12;
						break;
					case "W":
						duration = 16;
						break;
					case "E":
						duration = 2;
						break;
					case "E.":
						duration = 3;
						break;
					case "S":
						duration = 1;
						break;
					default:
						throw new ArgumentException("Unknown rhythm: " + durationStr);
				}

				char pitch = ' ';
				if (pitchStr != "")
					pitch = pitchStr[0];

				int userOctave = (octaveString.Length == 0) ? defaultOctave : int.Parse(octaveString);
				defaultOctave = userOctave;

				if (isRest) {
					notes.Add(new Rest(duration, tiedBefore, tiedAfter));
				} else {
					if (pitchStr != "") {
						// Calculate the # of white keys above A.
						int whiteKeyOffset = (pitch >= 'a' && pitch <= 'g') ? (int)(pitch - 'a') :
																		   (int)(pitch - 'A');
						// Convert to # half steps above C with a lookup table.
						int pitchOffset = pitchOffsets[whiteKeyOffset];

						int accidentalOffset;
						Accidental accidentalEnum;
						switch (accidental) {
							case "#":
								accidentalOffset = 1;
								accidentalEnum = Accidental.Sharp;
								break;
							case "b":
								accidentalOffset = -1;
								accidentalEnum = Accidental.Flat;
								break;
							case "x":
								accidentalOffset = 2;
								accidentalEnum = Accidental.DoubleSharp;
								break;
							case "bb":
								accidentalOffset = -2;
								accidentalEnum = Accidental.DoubleFlat;
								break;
							default:
								accidentalOffset = 0;
								accidentalEnum = Accidental.Natural;
								break;
						}

						int midi = (userOctave + 1) * 12 + pitchOffset + accidentalOffset;

						notes.Add(new Note(duration, tiedBefore, tiedAfter, new MidiInfo(midi, accidentalEnum)));
					} else {
						notes.Add(new UnpitchedNote(duration, tiedBefore, tiedAfter));
					}
				}
				
				tiedBefore = tiedAfter;
			}

			tieToNextMeasure = tiedAfter;
			resetAttackPoints();
		}


		public int Count {
			get {
                return AttackPoints.Count;
			}
		}

		public int this[int index] {
            get { return AttackPoints[index]; }
		}

		/// <summary>
		/// Returns the duration of the note beginning at the given point (both are expressed in 16th notes)
		/// </summary>
		/// <param name="attackPoint"></param>
		/// <returns></returns>
		public int GetDurationIncludingTiesAfterByAttackPoint(int attackPoint) {
			if (!notesByAttackPoint.ContainsKey(attackPoint))
				throw new ArgumentException("Not Found: " + attackPoint.ToString());

			return notesByAttackPoint[attackPoint].duartionIncludingTiesAfter;
		}

		public int GetDurationByAttackPoint(int attackPoint) {
			if (!notesByAttackPoint.ContainsKey(attackPoint))
				throw new ArgumentException("Not Found: " + attackPoint.ToString());

			return notesByAttackPoint[attackPoint].duration;

		}

		public Note GetNoteByAttackPoint(int attackPoint) {
			if (!notesByAttackPoint.ContainsKey(attackPoint))
				throw new ArgumentException("Not Found: " + attackPoint.ToString());

			return notesByAttackPoint[attackPoint];
		}


		public List<int> Durations {
			get {
				List<int> durations = new List<int>();
                for (int i = 0; i < AttackPoints.Count - 1; i++) {
                    durations.Add(AttackPoints[i + 1] - AttackPoints[i]);
				}

				return durations;
			}
		}

		public void AddNote(Note note) {
			if (notes.Count > 0)
				notes[notes.Count - 1].isLastInMeasure = false;
			note.isLastInMeasure = true;

			notes.Add(note);			
			resetAttackPoints();
		}



		/// <summary>
		/// Parse an entire string of measures of rhythms.
		/// </summary>
		/// <param name="input"></param>
		/// <returns></returns>
		static public List<Measure> ParseRhythmMeasures(Workspace workspace, string input, out int upbeatOffset, out Key key) {
			List<Measure> measures = new List<Measure>();
			int measureNumber = 0;
			bool incomingTie = false;
			bool tieToNextMeasure;
			int defaultOctave = 4;


			// First extract and parse the key from the start of the string. For no key, assume C major.
			// Format should be like 
			// "C: QC QD QE QF | QD QE QF QG" for C major
			// "Dm: QD QE QF QD | QD QE QF QG" for D minor
			string[] tokens = input.Split(':');
			string inputNoKey;
			if (tokens.Length > 1) {
				string keyString = tokens[0];
				key = new Key(keyString);
				inputNoKey = input.Substring(keyString.Length+1);
			} else {
				key = new Key(); // C major
				inputNoKey = input;
			}

			foreach (string measureStr in inputNoKey.Split(new char[] { '|' }, StringSplitOptions.RemoveEmptyEntries)) {
				Rhythm rhythm = new Rhythm(measureStr.Trim(), incomingTie, out tieToNextMeasure, ref defaultOctave);

				Measure m = new Measure(workspace, rhythm);
				m.number = measureNumber++;
				measures.Add(m);

				// Apply any final tie to the start of the next measure.
				incomingTie = tieToNextMeasure;
			}

			// Now go through backwards, updating note total durations including ties.

			int totaldur = 0;
			for (int i = measures.Count - 1; i >= 0; i--) {
				Measure m = measures[i];
				for (int j = m.rhythm.notes.Count-1; j >= 0; j--) {
					Note n = m.rhythm.notes[j];

					if (n.tiedAfter) {
						totaldur += n.duration;
					} else {
						totaldur = n.duration;
					}
					n.duartionIncludingTiesAfter = totaldur;
				}
			}

			// Compute upbeat offset
			if (measures.Count < 2)
				upbeatOffset = 0;
			else {
				upbeatOffset = measures[0].rhythm.TotalDuration;
				if (upbeatOffset == measures[1].rhythm.TotalDuration)
					upbeatOffset = 0;
			}

			return measures;
		}

		static public List<Measure> ShiftRhythmMeasures(Workspace workspace, List<Measure> measures, int upbeatDuration) {
			int measureNumber = 0; 
			List<Measure> shifted = new List<Measure>();
			
			// If no upbeat, return a copy of original.
			if (upbeatDuration == 0) {
				return new List<Measure>(measures);
			}
				
			
			// Determine duration of typical measures. Assumes uniform! 
			if (measures.Count < 2)
				throw new Exception("Not enough measures to perform upbeat shift!");

			int measureLength = measures[1].rhythm.TotalDuration;

			Measure currentMeasure = new Measure(workspace);
			currentMeasure.number = measureNumber++;
			int curMeasureDurRemaining = measureLength;

			//int curTime = 0;
			
			foreach (Measure m in measures) {
				foreach (Note n in m.rhythm.notes) {
					// Skip notes that have incoming ties.
					if (n.tiedBefore)
						continue;
					int durRemaining = n.duartionIncludingTiesAfter;
					//int nextAttack = curTime + durRemaining;
					

					bool tieBefore = false; //todo

					// Now, figure out how to split up this note so that it fits the measure length.
					while (durRemaining > 0) {
						int dur = Math.Min(durRemaining, curMeasureDurRemaining);
						durRemaining -= dur;
						curMeasureDurRemaining -= dur;
						bool tieAfter = (durRemaining > 0);
						Note newNote = new Note(dur, tieBefore, tieAfter, n.midiInfo);
						tieBefore = false;
						currentMeasure.rhythm.AddNote(newNote);

						if (curMeasureDurRemaining == 0) {
							shifted.Add(currentMeasure);
							currentMeasure = new Measure(workspace);
							currentMeasure.number = measureNumber++;
							
							curMeasureDurRemaining = measureLength;

							if (durRemaining > 0)
								tieBefore = true;
						}						
					}

				}
			}

			if (currentMeasure.rhythm.notes.Count > 0)
				shifted.Add(currentMeasure);

			return shifted;
		}


		public object Clone() {
			Rhythm r = new Rhythm();

			foreach (Note note in this.notes)
				r.notes.Add((Note)note.Clone());	// no need to sort, so add directly.

			r.resetAttackPoints();
			r.TotalDuration = TotalDuration;

			return r;
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			/*foreach (int i in attackPoints) {
				sb.Append(i);
				sb.Append(' ');
			}*/
            foreach (int i in AttackPoints) {
				Note n = GetNoteByAttackPoint(i);
				sb.Append(i);
				if (n.midiInfo != null)
					sb.Append(n.midiInfo.GetName());
				sb.Append(' ');
			}
			return sb.ToString();
		}


		public void ClearNotes()
		{
			notes.Clear();
			resetAttackPoints();
		}
	}
}
