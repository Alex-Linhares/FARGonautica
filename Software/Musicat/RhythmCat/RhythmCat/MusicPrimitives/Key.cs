using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;


namespace MusicPrimitives {
	/// <summary>
	/// Represents the current key signature.  Supports major and minor mode and standard circle-of-fifths key signatures.
	/// </summary>
	public class Key {

		/// <summary>
		/// Ranges from -7 to + 7.  This represents the number of flats or sharps in the key signature, indicated by a 
		/// circle-of-fifths traversal given by the value.  fifths = 0 represents the key of C major/a minor.  Positive 
		/// integers represent sharps, so that +1 represents G/e, while -1 represents F/d.
		/// </summary>
		private int fifths;

		/// <summary>
		/// The mode may be either Major or Minor.
		/// </summary>
		private KeyMode mode;


		/// <summary>
		/// Constructor creates C Major by default.
		/// </summary>
		public Key() {

		}

		/// <summary>
		/// Constructor based on fifths (MIDI-style key declaration) and major/minor mode.
		/// </summary>
		/// <param name="fifths">0 = C, 1 = G, -1 = F, etc.</param>
		/// <param name="mode">Major or Minor.</param>
		public Key(int fifths, KeyMode mode) {
			Fifths = fifths;
			Mode = mode;
		}

		/// <summary>
		///  Initialize key from a string.
		/// </summary>
		/// <param name="keyString">Format should be like 
		/// "C: QC QD QE QF | QD QE QF QG" for C major
		/// "Ebm: QEb QF QGb QF | QEb QF QGb QAb" for Eb minor</param>
		public Key(string keyString) {
			Match m = Regex.Match(keyString, @"([A-G])([b#])?(m)?");

			char name = m.Groups[1].Value[0];
			Accidental accidental = Accidental.Natural;
			if (m.Groups[2].Value != "") {
				if (m.Groups[2].Value == "b")
					accidental = Accidental.Flat;
				else
					accidental = Accidental.Sharp;
			}
			Pitch p = new Pitch(name, 4, accidental);

			if (m.Groups[3].Value == "m")
				mode = KeyMode.Minor;
			else
				mode = KeyMode.Major;
			
			string keyName = p.ToString();	// TODO: all the parsing of accidental above, etc isn't necessary if we just use the string anyway.
			switch (keyName) {
				#region Silly Case Statements to set "fifths"
	
				case "C":
					fifths = 0;
					break;
				case "G":
					fifths = 1;
					break;
				case "D":
					fifths = 2;
					break;
				case "A":
					fifths = 3;
					break;
				case "E":
					fifths = 4;
					break;
				case "B":
					fifths = 5;
					break;
				case "F#":
					fifths = 6;
					break;
				case "C#":
					fifths = 7;
					break;
				case "G#":
					fifths = 8;
					break;
				case "D#":
					fifths = 9;
					break;
				case "A#":
					fifths = 10;
					break;
				case "E#":
					fifths = 11;
					break;
				case "B#":
					fifths = 12;
					break;
				case "F":
					fifths = -1;
					break;
				case "Bb":
					fifths = -2;
					break;
				case "Eb":
					fifths = -3;
					break;
				case "Ab":
					fifths = -4;
					break;
				case "Db":
					fifths = -5;
					break;
				case "Gb":
					fifths = -6;
					break;
				case "Cb":
					fifths = -7;
					break;
				case "Fb":
					fifths = -8;
					break;
				default:
					throw new ArgumentException("Unknown Key: " + keyName);
				#endregion
			}
		}

		public int Fifths {
			get {
				return fifths;
			}
			set {
				if (value < -7 || value > 7) {
					throw new ArgumentOutOfRangeException("Fifths", value, "Fifths must be between -7 and 7.");
				}
				fifths = value;
			}
		}
		public KeyMode Mode {
			get {
				return mode;
			}
			set {
				if (!Enum.IsDefined(typeof(KeyMode), value)) {
					throw new ArgumentOutOfRangeException("KeyMode", value, "The mode must be either Major or Minor.");
				}
				mode = value;
			}
		}
		/// <summary>
		/// Returns the name of the key, such as "Ab Major", "E Minor", or "F# Major"
		/// </summary>
		public override string ToString() {
			// Get the name of the tonic and append the mode.
			Pitch tonic = GetScaleDegreePitch(new ScaleDegree(), 4);

			return tonic.ToString() + " " + mode.ToString();
		}

		/// <summary>
		/// Returns an example Pitch in the given key on the given scale degree, in the given octave.
		/// </summary>
		/// <param name="degree">The scale degree to create.</param>
		/// <param name="octave">The c-based octave in which to create the Pitch instance.</param>
		/// <returns>The example pitch on the requested scale degree.</returns>
		public Pitch GetScaleDegreePitch(ScaleDegree degree, int octave) {

			int numFifths;
			bool progressUp;
			PitchInterval fifth;
			PitchInterval degreeInterval;
			numFifths = Math.Abs(Fifths);
			progressUp = (Fifths > 0);

			fifth = new PitchInterval(7);

			// First, find the tonic of the key.
			// Start on middle C and loop through the circle of fifths.
			Pitch tonic = new Pitch(35, Accidental.Natural);
			for (int i = 0; i < numFifths; i++) {
				if (progressUp) {
					tonic += fifth;
					tonic.FlipEnharmonic(true, true); // The first argument is true because B# and E# are not allowed.
				} else {
					tonic -= fifth;
					tonic.FlipEnharmonic(i < 6, false); // The first argument is used because Cb is allowed (fifths = -7; i=6);
				}
			}

			// Second, add the scale degree to the tonic note.
			// Convert the scale degree to a pitch interval.
			degreeInterval = new PitchInterval();
			switch ((degree.Number - 1) % 7) {
				case 0:
					degreeInterval.Interval = 0;
					break;
				case 1:
					degreeInterval.Interval = 2;
					break;
				case 2:
					degreeInterval.Interval = 4;
					break;
				case 3:
					degreeInterval.Interval = 5;
					break;
				case 4:
					degreeInterval.Interval = 7;
					break;
				case 5:
					degreeInterval.Interval = 9;
					break;
				case 6:
					degreeInterval.Interval = 11;
					break;
			}
			// Add back in the # octaves.
			degreeInterval += new PitchInterval(12 * ((degree.Number - 1) / 7));
			Pitch result = tonic + degreeInterval;

			// Pick the correct enharmonic version (assuming no alterations). We use sharps if we are going up on the circle of fifths.
			// Natural notes like B are used instead of Cb until we have 6 accidentals. This is a special mixed case where we need to 
			// use one normal "white key" and one sharp "white key": both B natural and E# are in the key signature in F# major. Similarly, 
			// in Gb major both Cb and F natural are in the key signature.
			// Once we have 7 accidentals, we don't use any natural white keys.
			if (numFifths < 6)
				result.FlipEnharmonic(true, progressUp);
			else if (numFifths > 6)
				result.FlipEnharmonic(false, progressUp);
			else if (fifths == 6) // F# Major.
			{
				if (degree.Number == 4) // B natural special case.
					result.FlipEnharmonic(true, true);
				else
					result.FlipEnharmonic(false, true);
			} else // Gb Major.
			{
				if (degree.Number == 7) // F natural special case.
					result.FlipEnharmonic(true, false);
				else
					result.FlipEnharmonic(false, false);
			}

			// Set the alteration (this function picks the right enharmonic version if the pitch is already set right for the no alteration case.)
			result.ApplyAlteration(degree.Alteration);

			// Finally, set the octave.
			result.Octave = octave;
			return result;
		}

		public int GetScaleDegreePitchClass(ScaleDegree sd) {
			// Get the midi # and do mod 12.
			int midi = GetScaleDegreePitch(sd, 0).MidiNumber;
			return midi % 12;
		}
	}// END CLASS DEFINITION Key
}
