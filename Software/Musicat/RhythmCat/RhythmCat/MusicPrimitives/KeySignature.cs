using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using RhythmCat;

namespace MusicPrimitives {
	/// <summary>
	/// Summary description for KeySignature.
	/// </summary>
	public class KeySignature : IEnumerable {
		private Key key;
		private ArrayList pitchClasses;

		/// <summary>
		/// Create a new key signature for the given key.
		/// </summary>
		/// <param name="k"></param>
		public KeySignature(Key k) {
			// Record the key.
			key = k;

			// Calcualate the key signature elements.
			InitializeSignature();
		}

		public Key Key {
			get {
				return key;
			}
		}

		public int Count {
			get {
				return pitchClasses.Count;
			}
		}

		public PitchClass this[int index] {
			get {
				if (index < 0 || index >= Count)
					throw new IndexOutOfRangeException("KeySignature: index " + index + " is out-of-range.");

				return (PitchClass)pitchClasses[index];
			}
		}

		private void InitializeSignature() {
			// Count the # of sharps in the key signature (a negative # means flats.)
			int sharps = key.Fifths;

			// Add 3 flats for minor.
			if (key.Mode == KeyMode.Minor)
				sharps -= 3;

			int count = Math.Abs(sharps);
			pitchClasses = new ArrayList(count);

			// Find the starting accidental: F# for sharps or Bb for flats.
			PitchClass pc;
			Pitch tmpPitch;
			if (sharps > 0)
				pc = new PitchClass(3, Accidental.Sharp);
			else
				pc = new PitchClass(6, Accidental.Flat);

			// Move up or down the line of fifths to generate new accidentals.
			PitchInterval fifth = new PitchInterval(7);
			for (int i = 0; i < count; i++) {
				// Add the current pitchclass to the list.
				pitchClasses.Add(pc);

				// Convert the pitch class to a real pitch for processing.
				tmpPitch = new Pitch(pc.COffset, pc.Accidental);

				// Move by a fifth.
				if (sharps > 0)
					tmpPitch += fifth;
				else
					tmpPitch -= fifth;

				// Pick the right enharmonic version: don't use accidentals like B# on white keys until the 6th accidental.
				tmpPitch.FlipEnharmonic(i < 4, sharps > 0);

				// Convert back to a pitch class.
				pc = new PitchClass(tmpPitch.Number % 7, tmpPitch.Accidental);
			}

		}

		public override string ToString() {
			string s = "";

			foreach (PitchClass pc in this)
				s += pc.ToString() + " ";

			if (s.Length > 0)
				return s.Substring(0, s.Length - 1);
			else
				return "";
		}


		#region IEnumerable Members

		public IEnumerator GetEnumerator() {
			return pitchClasses.GetEnumerator();
		}

		#endregion
	}
}
