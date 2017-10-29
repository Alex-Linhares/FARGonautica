// Static Model

using System;
using System.Text;


namespace MusicPrimitives 
{

	/// <summary>
	/// Represents one of the degrees of a scale.  For example, in G major, the 5th scale degree is D.  Note that the scale degree 
	/// number must be between 1 and 7.  Also, scale degrees may include an alteration, so that in C minor, the 3rd, lowered scale
	/// degree is Eb.  Note that even though the lowered third is indicated in the key signature, the scale
	/// degree object must still include the lowered 3rd, etc.
	/// </summary>
	public class ScaleDegree
	{
		/// <summary>
		/// A number of 1 represents the tonic of the key.  5 represents the 5th, etc.  The number is restricted to the range 1 - 7.
		/// </summary>
		protected int number;

		protected Alteration alteration;

		/// <summary>
		/// Initializes the ScaleDegree to the tonic.
		/// </summary>
		public ScaleDegree()
		{
			number = 1;
			alteration = Alteration.None;
		}

		/// <summary>
		/// Initializes the new scaledegree to the given # and alteration.
		/// </summary>
		/// <param name="number">Must be between 1=tonic and 7=subtonic.</param>
		/// <param name="alteration">The pitch modification to apply to the scale degree.</param>
		public ScaleDegree(int number, Alteration alteration) {
			Number = number;
			Alteration = alteration;
		}

		public int Number
		{
			get
			{
				return number;
			}
			set
			{
				if (value < 1 || value > 7){
					throw new ArgumentOutOfRangeException("Number", value, "Number must be between 1 and 7.");
				}
				number = value;
			}
		}

		public Alteration Alteration
		{
			get
			{
				return alteration;
			}
			set
			{
				if (!Enum.IsDefined(typeof(Alteration), value)){
					throw new ArgumentOutOfRangeException("Alteration", value, "The given alteration was not recognized.");
				}
				alteration = value;
			}
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			sb.Append(Number.ToString());
			string accidental = "";
			switch (alteration) {
				case MusicPrimitives.Alteration.None:
					break;
				case MusicPrimitives.Alteration.Lowered:
					accidental = "b";
					break;
				case MusicPrimitives.Alteration.Raised:
					accidental = "#";
					break;
				case MusicPrimitives.Alteration.Diminished:
					accidental = "-";
					break;
				case MusicPrimitives.Alteration.Augmented:
					accidental = "+";
					break;
			}
			sb.Append(accidental);
			return sb.ToString();

		}

		public bool IsTonic {
			get {
				return number == 1 && alteration == MusicPrimitives.Alteration.None;
			}
		}
	}// END CLASS DEFINITION ScaleDegree


	public class ScaleDegreeWithStability : ScaleDegree, ICloneable {
		public bool IsStable {
			get {
				return Stability > 0;
			}
		}

		/// <summary>
		/// range 0-3.
		/// 0 = not stable, 1 = somewhat stable, 2 = more stable, 3 = most stable.
		/// </summary>
		public int Stability {
			get;
			set;
		}

		public ScaleDegreeWithStability(int number, Alteration alteration, int stability)
			: base(number, alteration) {
				Stability = stability;
		}

		public ScaleDegreeWithStability(ScaleDegree sd, int stability)
			: base(sd.Number, sd.Alteration) {
				Stability = stability;
		}

		public object Clone() {
			return new ScaleDegreeWithStability(number, alteration, Stability);
		}
	}
}