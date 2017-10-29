using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace RhythmCat
{
	public static class Utilities
	{

		public static string newline = System.Environment.NewLine;
		public static Random rand = null;



		static Utilities()
		{
			InitRandom();
		}


		public static void InitRandom()
		{
			rand = new Random();
			int seed = rand.Next();
			rand = new Random(seed);
		}

		public static void InitRandom(int seed) {
			rand = new Random(seed);
		}


		public static List<string> DisabledCodelets()
		{
			List<string> disabled = new List<string>();

			//foreach (string s in Settings.Default.DisabledCodelets)
			//    disabled.Add(s);
			//return disabled;

			//disabled.Add("Promotion");
			//disabled.Add("Demotion");
			////disabled_codelet_names.Add("Linear Grouping");
			//disabled.Add("SuggestLowerGroup");
			//disabled.Add("Find Similar Group");
			//disabled.Add("Generate Expectation");
			////disabled_codelet_names.Add("MetaGrouping");

			//disabled.Add("Destroy Downbeat");
			//disabled.Add("Create Downbeat");

			////disabled.Add("Find Contour Motif");
			return disabled;
		}

		public static void Log(string s)
		{
			Console.WriteLine(s);
		}

		public static void LogtoFile(string s) {
			try {
				using (TextWriter tw = new StreamWriter(Constants.LOGFILE_PATH, true)) {
					tw.WriteLine(s);
				}
			} catch (IOException) {
				// ignore.
			}
		}

		/// <summary>
		/// This function does the inverse sigmoid, adds the given amount, and reapplies sigmoid.
		/// </summary>
		/// <param name="originalValue">A value in the range 0 to 100.</param>
		/// <param name="amountToAdd">Value to add along the x axis.</param>
		/// <param name="m">1 / Sigmoid steepness. 20 is typical.</param>
		/// <returns></returns>
		public static double AddAlongSigmoid(double originalValue, double amountToAdd, double m)
		{
			double y = originalValue;
			//double w = amountToAdd;

			// Scale y to 0-1 and then do inverse sigmoid.
			//double temp = 2.0 / (y / 100.0 + 1.0) - 1.00;
			double temp = 1.0 / (y / 100.0 + 1.0);

			if (temp < 0.00000001)
				return 100;

			double x = -m * Math.Log(temp);
			x += amountToAdd;

			// Resigmoid and rescale to 0-100.
			//return 100.0 * (2.0 / (1.0 + Math.Exp(-x / m)) - 1.0);
			return 100.0 * (1.0 / (1.0 + Math.Exp(-x / m)));
		}


		public struct ObjectValuePair
		{
			public Object obj;
			public double value;

			public ObjectValuePair(Object obj, double value)
			{
				this.obj = obj;
				this.value = value;
			}
		}

		public static Object PickItemWeighted(List<ObjectValuePair> pairs)
		{
			// Compute total value.
			double s = 0;
			foreach (ObjectValuePair p in pairs) {
				if (p.value < 0) {
#if DEBUG
					System.Diagnostics.Debugger.Break();
#endif
					throw new ArgumentOutOfRangeException("Weights must be nonnegative in PickItemWeighted. weight = " + p.value.ToString());
				}
					s += p.value;
			}

			Object picked = null;

			double r = Utilities.rand.NextDouble() * s;
			double current = 0;
			foreach (ObjectValuePair p in pairs)
			{
				current += p.value;
				if (r < current + 0.00000001)
				{
					picked = p.obj;
					break;
				}
			}

			return picked;
		}

		public static ObjectValuePair PickItemWeightedReturnPair(List<ObjectValuePair> pairs)
		{
			// Compute total value.
			double s = 0;
			foreach (ObjectValuePair p in pairs) {
				if (p.value < 0)
					throw new ArgumentOutOfRangeException("Weights must be nonnegative in PickItemWeightedReturnPair. weight = " + p.value.ToString());
				s += p.value;
			}
			ObjectValuePair picked = new ObjectValuePair();

			double r = Utilities.rand.NextDouble() * s;
			double current = 0;
			foreach (ObjectValuePair p in pairs)
			{
				current += p.value;
				if (r < current + 0.00000001)
				{
					picked = p;
					break;
				}
			}

			return picked;
		}

		public static Object PickItemWeighted(List<Tuple<object, float>> tuples) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();
			foreach (Tuple<object, float> x in tuples)
				pairs.Add(new Utilities.ObjectValuePair(x.Item1, x.Item2));

			return PickItemWeighted(pairs);
		}


		public static ObjectValuePair PickItemWeightedReturnPair(List<Tuple<object, float>> tuples) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();
			foreach (Tuple<object, float> x in tuples)
				pairs.Add(new Utilities.ObjectValuePair(x.Item1, x.Item2));

			return PickItemWeightedReturnPair(pairs);
		}

		public static int EditDistance(string s, string t)
		{
			int m = s.Length;
			int n = t.Length;

			// d is a table with m+1 rows and n+1 columns 

			int[,] d = new int[m + 1, n + 1];

			for (int i = 0; i < m + 1; i++)
				d[i, 0] = i; //deletion
			for (int j = 0; j < n + 1; j++)
				d[0, j] = j; //insertion

			for (int j = 1; j < n + 1; j++)
			{
				for (int i = 1; i < m + 1; i++)
				{
					if (s[i - 1] == t[j - 1])
						d[i, j] = d[i - 1, j - 1];
					else
						d[i, j] = Min3(d[i - 1, j] + 1,  // deletion
									  d[i, j - 1] + 1,  // insertion
									  d[i - 1, j - 1] + 1 // substitution
									  );
				}
			}

			return d[m, n];
		}

		static int Min3(int a, int b, int c)
		{
			return Math.Min(a, Math.Min(b, c));
		}

		public static T PickItem<T>(List<T> list)
		where T : class {
			if (list.Count == 0)
				return null;
			return list[rand.Next(list.Count)];
		}


		public static double SigmoidSquash(double score) {
			double x = score / 100;
			return 100.0 * ((2.0 / (1.0 + Math.Exp(-2*x)))-1);
		}



		/// <summary>
		/// Given two rival structures of any sort, this function decides if structure 1 will "beat" structure 2. Returns true if structure 1 wins.
		/// </summary>
		/// <param name="strength1">Strength (0 to 100) of structure 1</param>
		/// <param name="strength2">Strength (0 to 100) of structure 2</param>
		/// <returns></returns>
		public static bool FightItOut(double strength1, double strength2, double temperature) {
			double s1 = AdjustedStrength(strength1, temperature);
			double s2 = AdjustedStrength(strength2, temperature);

			double total = s1 + s2;
			double r = Utilities.rand.NextDouble() * total;
			return r < s1;
		}

		// For a strength between 0 and 100.
		// Enhances differences as the temperature falls. Makes differences less important as temp goes above 85.
		public static double AdjustedStrength(double strength, double temperature) {
			return 100 * Math.Pow(strength / 100, (115 - temperature) / 30);
		}

		public static ObjectValuePair PickLargestItemReturnPair(List<ObjectValuePair> pairs) {
			double max = double.NegativeInfinity;
			ObjectValuePair argMax = new ObjectValuePair();
			foreach (ObjectValuePair p in pairs) {
				if (p.value > max) {
					max = p.value;
					argMax = p;
				}
			}
			return argMax;
		}
	}
}
