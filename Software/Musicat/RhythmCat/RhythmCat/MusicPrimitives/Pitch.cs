// Static Model
using System;

namespace MusicPrimitives { 
	/// <summary>
	/// A pitch represents a particular frequency in the 12-tone system.  Each pitch is made up of a
	/// note letter name and an accidental.
	/// </summary>
	public class Pitch {
		/// <summary>The note number used here corresponds to a natural, white key note.  Each note number can be 
		/// combined with one of the 5 accidentals to represent one of 5 different pitches.  Natural note number 0 
		/// corresponds to MIDI 0, C in octave -1.  Middle C is natural note number 35, MIDI 60, C4.  G4 is natural 
		/// note number 39, C5 is note number 42, etc.
		/// </summary>
		private int number;

		private Accidental accidental;

// PUBLIC METHODS.
		
		/// <summary>
		/// Default constructor: Creates the note # 0, C(-1) natural, MIDI 0.
		/// </summary>
		public Pitch() {
	
		}

		/// <summary>
		/// Constructor based on the NoteNumber and accidental.
		/// </summary>
		/// <param name="noteNumber">The internal "Natural Note Number"</param>
		/// <param name="accidental">The accidental to apply to the note.</param>
		public Pitch(int noteNumber, Accidental accidental) {
			number = noteNumber;
			Accidental = accidental;
		}

		/// <summary>
		/// Conctructor most suitable for human use: takes the note name, octave, and accidental.
		/// </summary>
		/// <param name="name">Uppercase character such as 'C' or 'A'.</param>
		/// <param name="octave">Octave must be at least -1.  C4 is middle C (octave = 4).</param>
		/// <param name="accidental">The accidental to apply to the given note.</param>
		public Pitch(char name, int octave, Accidental accidental) {
			int cOffset;	// 0 for C, 1 for D, ..., 5 for A, 6 for B

			Accidental = accidental;
			cOffset = GetCOffset(name);
			number = (octave + 1) * 7 + cOffset;
		}

		/// <summary>
		/// Creates a note based on the given MIDI number, using only natural notes or 1 accidental at most.
		/// </summary>
		/// <param name="midiNumber">The MIDI number.</param>
		/// <param name="preferSharps">If True, sharps are used for the black keys.  Otherwise flats are used.</param>
		public Pitch(int midiNumber, bool preferSharps) {
			InitPitch(midiNumber, preferSharps);
		}

		/// <summary>
		/// Creates a note based on the given MIDI number, using only natural notes or 1 accidental at most.
		/// </summary>
		/// <param name="midiNumber">The MIDI number.</param>
		/// <param name="preferSharps">If True, sharps are used for the black keys.  Otherwise flats are used.</param>
		private void InitPitch(int midiNumber, bool preferSharps)
		{
			char name = 'X';
			int offset;

			// First set the C-based octave.
			Octave = (midiNumber / 12) - 1;
			// Get the offset within the octave.
			offset = midiNumber % 12;
			// Allow for going below 0; this is used for PitchClass calculations.
			if (offset < 0)
				offset += 12; // Correct for the mod operator % returning a negative result for negative numbers.

			// Assume natural.
			accidental = Accidental.Natural;			
			// The note name and accidental are defined in a lookup table:
			switch (offset) 
			{
				case 0:
					name = 'C';		
					break;

				case 1:
					if (preferSharps) 
					{
						name = 'C';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'D';
						accidental = Accidental.Flat;
					}
					break;
			
				case 2:
					name = 'D';		
					break;

				case 3:
					if (preferSharps) 
					{
						name = 'D';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'E';
						accidental = Accidental.Flat;
					}
					break;

				case 4:
					name = 'E';		
					break;
			
				case 5:
					name = 'F';		
					break;

				case 6:
					if (preferSharps) 
					{
						name = 'F';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'G';
						accidental = Accidental.Flat;
					}
					break;

				case 7:
					name = 'G';		
					break;

				case 8:
					if (preferSharps) 
					{
						name = 'G';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'A';
						accidental = Accidental.Flat;
					}
					break;

				case 9:
					name = 'A';		
					break;

				case 10:
					if (preferSharps) 
					{
						name = 'A';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'B';
						accidental = Accidental.Flat;
					}
					break;

				case 11:
					name = 'B';		
					break;
			}

			// Finally set the note name.
			Name = name;
		}

		/// <summary>
		/// Creates a best guess note for the given MIDI number and current key signature.  Notes in the given
		/// key are prefered, notes with one accidental away from the key signature are prefered next, and
		/// lastly notes with 2 or more differences in accidental are prefered. 
		/// </summary>
		/// <example>
		/// For example, in Eb Major, the midi note 68 at G#/Ab would be selected to be Ab instead of G#.  Similarly, 
		/// in Cb major, the midi note 34 would turn into Cb instead of B-natural.
		/// </example>
		/// <param name="midiNumber"></param>
		/// <param name="key"></param>
		public Pitch(int midiNumber, Key key) {
			// TODO
			bool preferSharps;

			// For now, just use flats for flat keys and sharps for sharp keys.
			if (key.Fifths >=0)
				preferSharps = true;
			else
				preferSharps = false;

			InitPitch(midiNumber, preferSharps);
		}

		/// <summary>
		/// Gets or sets the natural note number for this note.  The accidental is unaffected.
		/// </summary>
		public int Number {
			get {
				return number;
			}
			set {
				number = value;
			}
		}

		/// <summary>
		/// Gets or sets the accidental for the note, leaving the natural note number unaffected.
		/// </summary>
		public Accidental Accidental {
			get {
				return accidental;
			}
			set {
				if (!Enum.IsDefined(typeof(Accidental), value)){
					throw new ArgumentOutOfRangeException("Accidental", value, "The given accidental was not recognized.");
				}
				accidental = value;
			}
		}

		/// <summary>
		/// Gets the MIDI number for the note.  The only way to create a note based on MIDI number is to use the constructor.
		/// </summary>
		public int MidiNumber {
			get {	
				// Calculate the octave, and then calculate the MIDI C note for this octave.  Calculate
				// the offset from C in half-steps, add in the accidental offset, and add this to MIDI C.

				int midiC;
				int offset;
				int accidentalOffset;

				midiC = (Octave + 1) * 12;

				// Use a lookup table to get the offset from C to any of the 7 notes.
				switch (number % 7) {
					case 0:
						offset = 0;
						break;
					case 1:
						offset = 2;
						break;
					case 2:
						offset = 4;
						break;
					case 3:
						offset = 5;
						break;
					case 4:
						offset = 7;
						break;
					case 5:
						offset = 9;
						break;
					case 6:
						offset = 11;
						break;
					default:
						throw new Exception();
				}

				switch (accidental) {
					case Accidental.Natural:
						accidentalOffset = 0;
						break;
					case Accidental.Sharp:
						accidentalOffset = 1;
						break;
					case Accidental.Flat:
						accidentalOffset = -1;
						break;
					case Accidental.DoubleSharp:
						accidentalOffset = 2;
						break;
					case Accidental.DoubleFlat:
						accidentalOffset = -2;
						break;
					default:
						throw new Exception();
				}

				return midiC + offset + accidentalOffset;			
			}
		}

		/// <summary>
		/// The Name is the 1-character note name, one of: A, B, C, D, E, F, G.
		/// </summary>
		/// <remarks>
		/// This method preserves the current Octave and accidental, modifying the note name within the octave.
		/// Note that everything from Cbb to Bx is considered to be in the same Octave.
		/// </remarks>
		public char Name {
			get {
				// Add the offset from A to ASCII 'A' to get the return character.
				return (char)((int)'A' + GetAOffset());
			}
			set {
				// Record the current C-based octave, get the note name offset, and add to the lower octave 
				// base C note.
				int cOffset;
				int cOctave;

				// Record the offset from the note C.
				cOffset = GetCOffset(value);
				// Use the note number + 2, DIV 7 followed by a linear transform to get the octave.
				cOctave = number / 7;
				// Compute the new number using the note name.  Notice that cOctave * 7 here gives a different
				// result than number * 7 above, because x/7 takes the integer part.
				number = cOctave * 7 + cOffset;
			}
		}


		/// <summary>
		/// Select a particular enharmonic version of this pitch. If forceNatural is true, then C is prefered over B# or Dbb, for instance.
		/// If useSharps is true, then sharp or double-sharp are used; otherwise flat/double-flat are used.
		/// </summary>
		/// <param name="forceNatural"></param>
		/// <param name="sharp"></param>
		public void FlipEnharmonic(bool allowNatural, bool useSharps)
		{
			// Get the offset within the octave.
			int offset = MidiNumber % 12;
			
			// Pick the name later.
			char name = 'X';
	
			// The note name and accidental are defined in a lookup table:
			switch (offset) 
			{
				case 0:
					if (allowNatural) 
					{
						name = 'C';		
						accidental = Accidental.Natural;		
					}
					else if (useSharps)
					{	
						name = 'B';
						accidental = Accidental.Sharp;
						Octave -= 1;
					}
					else
					{
						name = 'D';
						accidental = Accidental.DoubleFlat;
					}
					break;

				case 1:
					if (useSharps) 
					{
						name = 'C';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'D';
						accidental = Accidental.Flat;
					}
					break;
			
				case 2:
					if (allowNatural) 
					{
						name = 'D';	
						accidental = Accidental.Natural;		
					}	
					else if (useSharps)
					{	
						name = 'C';
						accidental = Accidental.DoubleSharp;
					}
					else
					{
						name = 'E';
						accidental = Accidental.DoubleFlat;
					}
					break;

				case 3:
					if (useSharps) 
					{
						name = 'D';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'E';
						accidental = Accidental.Flat;
					}
					break;

				case 4:
					if (allowNatural) 
					{
						name = 'E';	
						accidental = Accidental.Natural;		
					}	
					else if (useSharps)
					{	
						name = 'D';
						accidental = Accidental.DoubleSharp;
					}
					else
					{
						name = 'F';
						accidental = Accidental.Flat;
					}
					break;
			
				case 5:
					if (allowNatural) 
					{
						name = 'F';	
						accidental = Accidental.Natural;		
					}	
					else if (useSharps)
					{	
						name = 'E';
						accidental = Accidental.Sharp;
					}
					else
					{
						name = 'G';
						accidental = Accidental.DoubleFlat;
					}
					break;

				case 6:
					if (useSharps) 
					{
						name = 'F';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'G';
						accidental = Accidental.Flat;
					}
					break;

				case 7:
					if (allowNatural) 
					{
						name = 'G';	
						accidental = Accidental.Natural;		
					}	
					else if (useSharps)
					{	
						name = 'F';
						accidental = Accidental.DoubleSharp;
					}
					else
					{
						name = 'A';
						accidental = Accidental.DoubleFlat;
					}	
					break;

				case 8:
					if (useSharps) 
					{
						name = 'G';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'A';
						accidental = Accidental.Flat;
					}
					break;

				case 9:
					if (allowNatural) 
					{
						name = 'A';	
						accidental = Accidental.Natural;		
					}	
					else if (useSharps)
					{	
						name = 'G';
						accidental = Accidental.DoubleSharp;
					}
					else
					{
						name = 'B';
						accidental = Accidental.DoubleFlat;
					}						
					break;

				case 10:
					if (useSharps) 
					{
						name = 'A';
						accidental = Accidental.Sharp;
					} 
					else 
					{
						name = 'B';
						accidental = Accidental.Flat;
					}
					break;

				case 11:
					if (allowNatural) 
					{
						name = 'B';	
						accidental = Accidental.Natural;		
					}	
					else if (useSharps)
					{	
						name = 'A';
						accidental = Accidental.DoubleSharp;
					}
					else
					{
						name = 'C';
						accidental = Accidental.Flat;
						Octave += 1;
					}
					break;
			}

			// Finally set the note name.
			Name = name;
		}

		/// <summary>
		/// Apply the given alteration to the current pitch.
		/// </summary>
		/// <param name="alt"></param>
		public void ApplyAlteration(Alteration alt)
		{
			switch (alt)
			{
				case Alteration.Raised:
					switch (accidental)
					{
						case Accidental.DoubleFlat:
							accidental = Accidental.Flat;
							break;
						case Accidental.Flat:
							accidental = Accidental.Natural;
							break;
						case Accidental.Natural:
							accidental = Accidental.Sharp;
							break;
						case Accidental.Sharp:
							accidental = Accidental.DoubleSharp;
							break;
						case Accidental.DoubleSharp:
							InitPitch(MidiNumber+1, true);
							FlipEnharmonic(false, true);
							break;
					}
					break;

				case Alteration.Lowered:
					switch (accidental)
					{
						case Accidental.DoubleFlat:
							InitPitch(MidiNumber-1, false);
							FlipEnharmonic(false, false);							
							break;
						case Accidental.Flat:
							accidental = Accidental.DoubleFlat;
							break;
						case Accidental.Natural:
							accidental = Accidental.Flat;
							break;
						case Accidental.Sharp:
							accidental = Accidental.Natural;
							break;
						case Accidental.DoubleSharp:
							accidental = Accidental.Sharp;
							break;
					}
					break;

				case Alteration.Augmented:
					switch (accidental)
					{
						case Accidental.DoubleFlat:
							accidental = Accidental.Natural;
							break;
						case Accidental.Flat:
							accidental = Accidental.Sharp;
							break;
						case Accidental.Natural:
							accidental = Accidental.DoubleSharp;
							break;
						case Accidental.Sharp:
							InitPitch(MidiNumber+2, true);
							FlipEnharmonic(false, true);							
							break;
						case Accidental.DoubleSharp:
							InitPitch(MidiNumber+2, true);
							FlipEnharmonic(false, true);							
							break;
					}
				break;

				case Alteration.Diminished:
					switch (accidental)
					{
						case Accidental.DoubleFlat:
							InitPitch(MidiNumber-2, false);
							FlipEnharmonic(false, true);	
							break;
						case Accidental.Flat:
							InitPitch(MidiNumber-2, false);
							FlipEnharmonic(false, true);	
							break;
						case Accidental.Natural:
							accidental = Accidental.DoubleFlat;
							break;
						case Accidental.Sharp:
							accidental = Accidental.Flat;
							break;
						case Accidental.DoubleSharp:
							accidental = Accidental.Natural;
							break;
					}
				break;
			}
		}

		/// <summary>
		/// Return a string representation of the pitch, such as C, Ab, or G#.
		/// </summary>
		/// <returns></returns>
		public override string ToString()
		{
			string s = "" + Name;

			switch (accidental)
			{
				case Accidental.Sharp:
					s += "#";
					break;
				case Accidental.Flat:
					s += "b";
					break;
				case Accidental.DoubleSharp:
					s += "x";
					break;
				case Accidental.DoubleFlat:
					s += "bb";
					break;
			}

			return s;
		}

		/// <summary>
		/// Octave 4 encompasses middle C through B natural a 7th above middle C.  The 
		/// lowest MIDI note, 0, is Octave -1, C natural.  The highest MIDI note, 127, is G, 
		/// Octave 8.
		/// </summary>
		public int Octave {
			get {
				// Use the note numer DIV 7 followed by a linear transform to get the octave.
				return (number / 7) - 1;
			}
			set {
				// Figure out the current note offset within the octave (D=1, E=2, etc.).
				int cOffset = number % 7;
				// Then do a linear transform on the octave, multiply the octave # by 7, and add the offset back.
				number = (value + 1) * 7 + cOffset;
			}
		}

		public static Pitch operator+(Pitch p, PitchInterval pi) {
			// Prefer to call the new pitch a sharp if the base pitch is a sharp or natural; otherwise prefer flats.
			bool preferSharp = (p.accidental == Accidental.Natural || p.accidental == Accidental.Sharp || p.accidental == Accidental.DoubleSharp);
			// Increment the pitch by doing math with MIDI numbers.
			return new Pitch(p.MidiNumber + pi.Interval, preferSharp);
		}

		public static Pitch operator-(Pitch p, PitchInterval pi) {
			// Prefer to call the new pitch a sharp if the base pitch is a sharp or natural; otherwise prefer flats.
			bool preferSharp = (p.accidental == Accidental.Natural || p.accidental == Accidental.Sharp || p.accidental == Accidental.DoubleSharp);
			// Increment the pitch by doing math with MIDI numbers.
			return new Pitch(p.MidiNumber - pi.Interval, preferSharp);
		}



// PRIVATE METHODS.


		/// <summary>
		/// Get the A-based offset for the current note.  A = 0, B = 1, C = 2, etc.
		/// </summary>
		/// <returns></returns>
		private int GetAOffset() {
			// Calculates the note number+2 mod 7 to get the offset from A.
			// This effectively rotates so that A = 0, B = 1, C = 2, etc.
			return (number + 2) % 7;
		}

		/// <summary>
		/// Get the C-based offset for the current note.  C = 0, D = 1, E = 2, etc.
		/// </summary>
		/// <returns></returns>
		private int GetCOffset(char name) {
			
			int cOffset;
			
			// Calculates the offset of the given name string.
			switch (name){
				case 'C':
					cOffset = 0;
					break;
				case 'D':
					cOffset = 1;
					break;
				case 'E':
					cOffset = 2;
					break;
				case 'F':
					cOffset = 3;
					break;
				case 'G':
					cOffset = 4;
					break;
				case 'A':
					cOffset = 5;
					break;
				case 'B':
					cOffset = 6;
					break;
				default:
					throw new ArgumentOutOfRangeException("name", name, "Note name (" + name + ") invalid. Must be an uppercase letter between 'A' and 'G'");
			}
			return cOffset;
		}

	}// END CLASS DEFINITION Pitch
}