using System;

namespace MusicPrimitives
{

	/// <summary>
	/// Class representing a pitch interval.  This is simply a number of half-steps.
	/// TODO: Maybe this needs to be a "smarter" interval: an interval + quality (aug 2nd vs. dim 3rd, etc.)
	/// </summary>
	public class PitchInterval
	{
		private int interval;
	
		
		/// <summary>
		/// The default interval is 0.
		/// </summary>
		public PitchInterval()
		{
		}

		/// <summary>
		/// Create a pitch interval.
		/// </summary>
		/// <param name="interval">The number of the half-steps (+ or -) in the interval.</param>
		public PitchInterval(int interval){
			Interval = interval;
		}

		public int Interval{
			get {
				return interval;
			}
			set {
				interval = value;
			}
		}

		public static PitchInterval operator+(PitchInterval p1, PitchInterval p2){
			return new PitchInterval(p1.interval + p2.interval);
		}
	}
}
