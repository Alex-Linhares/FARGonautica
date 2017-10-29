using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	public class MidiInfo : ICloneable {
		public int midi;
		public Accidental Accidental { get; set; }

		// Optional attributes for more-than-midi representation.
		private int whiteKeyNoteNumber;

		/// <summary>
		/// Extra info; might be -1 for unset.
		/// </summary>
		public int WhiteKeyNoteNumber { get; set;}

		public MidiInfo(int midi, Accidental accidental) {
			this.midi = midi;
			this.Accidental = accidental;
			this.UserOctave = -1;        //undefined
			this.whiteKeyNoteNumber = -1;	//undefined
		}



		public object Clone() {
			MidiInfo p = new MidiInfo(midi, Accidental);
			p.UserOctave = UserOctave;
			p.whiteKeyNoteNumber = whiteKeyNoteNumber;

			return p;
		}

		
        /// <summary>
        /// This is the user-specified octave from the constructor, if specified. This differs from MIDI octave for notes like Cb.
        /// </summary>
		public int UserOctave {
			get;
			set;
		}

		/// <summary>
		/// Converts the MIDI # to a name (using sharps). No perception involved.
		/// </summary>
		public String GetName() {
			if (midi == -1)
				return "r";

			switch (midi % 12) {
				case 0:
					return "C";
				case 1:
					return "C#";
				case 2:
					return "D";
				case 3:
					return "D#";
				case 4:
					return "E";
				case 5:
					return "F";
				case 6:
					return "F#";
				case 7:
					return "G";
				case 8:
					return "G#";
				case 9:
					return "A";
				case 10:
					return "A#";
				case 11:
					return "B";
				default:
					return "ERROR";
			}
		}

		/// <summary>
		/// Converts the MIDI # to an octave. No perception involved.
		/// </summary>
		public int GetOctave() {
			return midi / 12 - 1;
		}

		public Pitch GetPitch(Key k) {
			if (whiteKeyNoteNumber > -1)
				return new Pitch(whiteKeyNoteNumber, Accidental);
			else
				return new Pitch(midi, k);
		}

		/// <summary>
		/// Guesses the scale degree for this note given a key.
		/// </summary>
		/// <param name="k"></param>
		/// <returns></returns>
		public ScaleDegree GetScaleDegree(Key k) {

			Alphabet a = Alphabet.GetScaleAlphabet(k);
			foreach (ScaleDegree sd in a) {
				if (this.midi % 12 == k.GetScaleDegreePitch(sd, 0).MidiNumber % 12)
					return sd;
			}

			return null;
		}

		public override string ToString() {
			return GetName() + GetOctave().ToString();
		}

		public int PitchClass {
			get {
				// Just do mod 12 on midi #.
				return midi % 12;
			}
		}
	}
}
