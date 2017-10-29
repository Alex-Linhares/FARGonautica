using System;
using System.Collections.Generic;
using System.Text;
using RhythmCat;

namespace MusicPrimitives {
	/// <summary>
	/// Represents a pitch class; that is, a pitch name such as "C" with no mention of the octave involved.
	/// </summary>
	public class PitchClass {
		private int cOffset;
		private Accidental accidental;

		/// <summary>
		/// Create a new pitchclass object using a cOffset.
		/// </summary>
		/// <param name="cOffset">0 for C, 1 for D, up to 6 for B.</param>
		/// <param name="accidental"></param>
		public PitchClass(int cOffset, Accidental accidental) {
			COffset = cOffset;
			Accidental = accidental;
		}

		/// <summary>
		/// Create a new pitchclass object using a note name.
		/// </summary>
		/// <param name="name">'A', 'B', 'C', 'D', 'E', 'F', or 'G'</param>
		/// <param name="accidental"></param>
		public PitchClass(char name, Accidental accidental) {
			switch (name) {
				case 'A':
					COffset = 5;
					break;
				case 'B':
					COffset = 6;
					break;
				case 'C':
					COffset = 0;
					break;
				case 'D':
					COffset = 1;
					break;
				case 'E':
					COffset = 2;
					break;
				case 'F':
					COffset = 3;
					break;
				case 'G':
					COffset = 4;
					break;
				default:
					throw new ArgumentOutOfRangeException("name", name, "PitchClass name must be between A and G");
			}
			Accidental = accidental;
		}

		/// <summary>
		/// Get or set the cOffset: 0 for C, 1 for D, ... 6 for B.
		/// </summary>
		public int COffset {
			get {
				return cOffset;
			}
			set {
				if (value < 0 || value > 6) {
					throw new ArgumentOutOfRangeException("PitchClass", value, "COffset must be between 0 and 6.");
				}
				cOffset = value;
			}
		}

		/// <summary>
		/// Gets or sets the accidental for the pitchclass, leaving the cOffset unaffected.
		/// </summary>
		public Accidental Accidental {
			get {
				return accidental;
			}
			set {
				if (!Enum.IsDefined(typeof(Accidental), value)) {
					throw new ArgumentOutOfRangeException("Accidental", value, "The given accidental was not recognized.");
				}
				accidental = value;
			}
		}

		public char Name {
			get {
				Pitch p = new Pitch(cOffset, accidental);
				return p.Name;
				//				// Add the offset from A to ASCII 'A' to get the return character.
				//				return (char)((int)'A' + (cOffset + 2) % 7);
			}
		}

		public override string ToString() {
			Pitch p = new Pitch(cOffset, accidental);
			return p.ToString();
		}


	}
}
