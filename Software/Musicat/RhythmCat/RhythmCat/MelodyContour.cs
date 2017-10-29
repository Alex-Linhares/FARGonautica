using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	public enum ContourDirection {
		Up,
		Down,
		Same
	}

	public class ContourTransition {
		public ContourDirection Direction { get; private set; }
		public int HalfSteps { get; private set; }

		public ContourTransition(ContourDirection direction, int halfSteps) {
			this.Direction = direction;
			this.HalfSteps = halfSteps;
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			switch (Direction) {
				case ContourDirection.Up:
					sb.Append('U');
					sb.Append(HalfSteps.ToString());
					break;
				case ContourDirection.Down:
					sb.Append('D');
					sb.Append(HalfSteps.ToString());
					break;
				case ContourDirection.Same:
					sb.Append('-');
					break;
				default:
					throw new ArgumentException("Broken enum in contour transition.");
			}

			return sb.ToString();
		}

		public string ToStringSimple() {
			switch (Direction) {
				case ContourDirection.Up:
					if (HalfSteps > 2)
						return "U";
					else
						return "u";
				case ContourDirection.Down:
					if (HalfSteps > 2)
						return "D";
					else
						return "d";
				case ContourDirection.Same:
					return "-";
				default:
					throw new ArgumentException("Broken enum in contour transition.");
			}
		}
	}

	public class MelodyContour {

		public int Length {
			get {
				return transitions.Count;
			}
		}

		public List<ContourTransition> transitions { get; private set; }

		public MelodyContour() {
			transitions = new List<ContourTransition>();
		}

		/// <summary>
		/// Generate a contour from a Group
		/// </summary>
		/// <param name="group"></param>
		public MelodyContour(GroupElement ge) {
			transitions = new List<ContourTransition>();
			// Get list of notes in group, removing all rests.

			// Flatten group.
			List<Measure> measures = ge.Measures;
			List<Note> notes = Measure.FlattenToNotesNoRests(measures);

			for (int i = 0; i < notes.Count - 1; i++) {
				AddTransition(notes[i], notes[i + 1]);
			}
		}

		public ContourTransition AddTransition(ContourDirection direction, int halfSteps) {
			ContourTransition ct = new ContourTransition(direction, halfSteps); 
			transitions.Add(ct);
			return ct;
		}

		public ContourTransition AddTransition(Note n1, Note n2) {
			// Skip rest-to-rest transitions.
			if (n1.isRest || n2.isRest)
				throw new ArgumentException("Must not have rests in contour creation.");
			
			ContourDirection dir;

			int diff = n1.Midi - n2.Midi;
			if (diff > 0)
				dir = ContourDirection.Down;
			else if (diff < 0)
				dir = ContourDirection.Up;
			else
				dir = ContourDirection.Same;

			ContourTransition ct = new ContourTransition(dir, Math.Abs(diff));
			transitions.Add(ct);
			return ct;
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			foreach (ContourTransition ct in transitions) {
				sb.Append(ct.ToString());
				sb.Append(" ");
			}

			if (sb.Length > 0)
				return sb.ToString(0, sb.Length-1);
			else
				return sb.ToString();
		}

		public string ToStringSimple() {
			StringBuilder sb = new StringBuilder();
			foreach (ContourTransition ct in transitions) {
				sb.Append(ct.ToStringSimple());
			}
			
			return sb.ToString();
		}

		/// <summary>
		/// Compute the similarity between this contour and the given contour.
		/// </summary>
		/// <param name="mc2"></param>
		/// <returns></returns>
		public float ComputeSimilarity(MelodyContour mc2) {

			string strC1 = this.ToStringSimple();
			string strC2 = mc2.ToStringSimple();
			int lenMax = Math.Max(strC1.Length, strC2.Length);
			if (lenMax == 0)
				return 0;

			int distNormal = Utilities.EditDistance(strC1, strC2);

			string strC2Inverted = InvertContourString(strC2);
			int distInverted = Utilities.EditDistance(strC1, strC2Inverted) + Constants.INVERT_CONTOUR_COST;

			int dist = Math.Min(distNormal, distInverted);

			return 100f * (((float)lenMax - dist) / lenMax);
		}

		
		private string InvertContourString(string s) {
			StringBuilder sb = new StringBuilder();
			foreach (char c in s) {
				switch (c) {
					case '-':
						sb.Append('-');
						break;
					case 'u':
						sb.Append('d');
						break;
					case 'U':
						sb.Append('D');
						break;
					case 'd':
						sb.Append('u');
						break;
					case 'D':
						sb.Append('U');
						break;
					default:
						throw new Exception("Unknown contour character: " + c);
				}
			}

			return sb.ToString();
		}
	}
}
