using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public struct IntPair {
		public int x;
		public int y;

		public IntPair(int x, int y) {
			this.x = x;
			this.y = y;
		}

		//		public override int GetHashCode() {
		//		return base.GetHashCode();
		//}

		public override string ToString() {
			return "(" + x.ToString() + "-" + y.ToString() + ")";
		}
	}

	public class IntMapping : ICloneable {
		public List<IntPair> mapPairs;

		private static Dictionary<IntPair, List<IntMapping>> computedIntMaps;
		private static int a, b;

		public IntMapping() {
			this.mapPairs = new List<IntPair>();
		}

		public override string ToString() {
			StringBuilder sb = new StringBuilder();
			foreach (IntPair p in mapPairs)
				sb.Append(p);
			return sb.ToString();
		}


		static public List<IntMapping> getAllMaps(int sizeLHS, int sizeRHS) {
			a = sizeLHS;
			b = sizeRHS;

			computedIntMaps = new Dictionary<IntPair, List<IntMapping>>();
			List<IntMapping> results = genMaps(1, 1);
			foreach (IntMapping submap in results) {
				IntPair x = new IntPair(0, 0);
				submap.mapPairs.Insert(0, x);
			}

			return results;
		}

		static private List<IntMapping> genMaps(int min_i, int min_j) {
			IntPair parameters = new IntPair(min_i, min_j);
			List<IntMapping> resultMaps = new List<IntMapping>();
			if (computedIntMaps.ContainsKey(parameters))
				return computedIntMaps[parameters];

			// base case: end with final link pair
			if (min_i == a && min_j == b) {
				resultMaps.Add(new IntMapping());
				return resultMaps;
			}

			if (min_i < a && a > b) {
				List<IntMapping> subMaps = genMaps(min_i + 1, min_j);
				foreach (IntMapping submap in subMaps) {
					IntPair x = new IntPair(min_i, min_j - 1);
					IntMapping newMap = (IntMapping)submap.Clone();
					newMap.mapPairs.Insert(0, x);
					resultMaps.Add(newMap);
				}
			}

			if (min_j < b && a < b) {
				List<IntMapping> subMaps = genMaps(min_i, min_j + 1);
				foreach (IntMapping submap in subMaps) {
					IntPair x = new IntPair(min_i - 1, min_j);
					IntMapping newMap = (IntMapping)submap.Clone();
					newMap.mapPairs.Insert(0, x);
					resultMaps.Add(newMap);
				}
			}

			if (min_i < a && min_j < b) {
				List<IntMapping> subMaps = genMaps(min_i + 1, min_j + 1);
				foreach (IntMapping submap in subMaps) {
					IntMapping newMap = (IntMapping)submap.Clone();
					IntPair x = new IntPair(min_i, min_j);
					newMap.mapPairs.Insert(0, x);
					resultMaps.Add(newMap);
				}
			}

			computedIntMaps[parameters] = resultMaps;
			return resultMaps;
		}


		#region ICloneable Members

		public object Clone() {
			IntMapping map = new IntMapping();
			foreach (IntPair p in this.mapPairs)
				map.mapPairs.Add(p);
			return map;
		}

		#endregion
	}
}
