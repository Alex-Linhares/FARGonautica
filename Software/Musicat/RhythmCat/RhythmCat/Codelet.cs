using System;
using System.Collections.Generic;
using System.Text;

namespace RhythmCat
{
	public abstract class Codelet : ICloneable
	{

		protected Coderack coderack;
		protected Workspace workspace;
		protected Slipnet slipnet;

		protected double rawUrgency;

		protected string name;

		public string Name
		{
			get { return name; }
		}

		const double decay_parameter = 1;  //// Tabletop used: 2;

		public double Urgency
		{
			get
			{
				// Return adjusted urgency as in Tabletop.
				// adj = raw / p^G. p is a decay parameter, and G is the generation number
				return (double)rawUrgency / Math.Pow(decay_parameter, generation);
			}
			set { rawUrgency = value; }
		}
		protected Codelet parent;

		private int postTime; // time codelet was added to coderack.

		public int PostTime
		{
			get { return postTime; }
			set { postTime = value; }
		}

		/// <summary>
		/// Number of timesteps codelet has been on coderack.
		/// </summary>
		public int Age
		{
			get { return workspace.CurrentTime - postTime; }
		}

		private int generation;   // 0 for original, 1 for a child of a gen 0, 2 for a grandchild, etc.

		public int Generation
		{
			get { return generation; }
		}

		/// <summary>
		/// The parent codelet that added this one to the coderack (if any).
		/// </summary>
		public Codelet Parent
		{
			get { return parent; }
		}

		public Codelet(string name, double urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
		{
			if (urgency < 0 || urgency > 100) {
			//	throw new ArgumentOutOfRangeException("Urgency for new codelet invalid");
			}
			this.name = name;
			this.rawUrgency = urgency;
			this.parent = parent;
			this.coderack = coderack;
			this.workspace = workspace;
			this.slipnet = slipnet;

			if (parent == null)
				generation = 0;
			else
				generation = parent.generation + 1;

		}

		abstract public void Run();


		public override string ToString()
		{
			return name + ": urgency=" + Urgency + " Generation=" + generation.ToString();
		}

		#region ICloneable Members

		public object Clone()
		{
			return base.MemberwiseClone();
		}

		#endregion
	}
}
