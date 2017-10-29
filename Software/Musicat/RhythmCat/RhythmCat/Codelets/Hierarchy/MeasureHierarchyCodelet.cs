using System;
using System.Collections.Generic;
using System.Text;

namespace RhythmCat.Codelets {
	[Codelet("Grouping", CodeletAttribute.CodeletWorkType.Create, 10, true)]
	public class MeasureHierarchyCodelet : Codelet {

		/// <summary>
		/// The measure to try to score (imagining a barline BEFORE measure). If -1, we'll pick randomly from the workspace.
		/// </summary>
		private int measureIndex;

		//private const int MAX_HEIGHT = 5;

		/// <summary>
		/// Current barline index heard in the input stream.
		/// </summary>

		int curMaxMeasure;

		#region Constants


		const bool INIT_RANDOM = false;
		const int MAX_HEIGHT = Constants.BARLINE_MAX_HEIGHT;
		const int MODIFICATION_WINDOW_SIZE = 3;
		const int NUM_CODELETS_PER_BARLINE = 150;
		const int NUM_MEASURES = 34;

		const float ALPHA = 0.4f;		// leaarning rate: amount of remaining distance to move strength towards target
		const float ALPHA_ZERO_REDUCTION = 0.8f;		// leaarning rate: amount of remaining distance to move strength towards target

		#endregion


		public MeasureHierarchyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Measure Hierarchy", urgency, parent, coderack, workspace, slipnet) {
			measureIndex = -1;
		}

		/// <summary>
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="group"></param>
		public MeasureHierarchyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, int measureIndex)
			: base("Measure Hierarchy", urgency, parent, coderack, workspace, slipnet) {

			this.measureIndex = measureIndex;
		}

		public override void Run() {
			List<Measure> measures = workspace.measures;
			curMaxMeasure = measures.Count;

			if (measures.Count < 1)
				return;

			// If the measure is undefined, pick one randomly.
			if (measureIndex == -1) {
				measureIndex = Utilities.rand.Next(curMaxMeasure - MODIFICATION_WINDOW_SIZE, curMaxMeasure+1);
			}

			if (measureIndex < 0)
				measureIndex = 0;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, measureIndex);


			while (measureIndex >= workspace.barlines.Count) {
				float nextStr = ComputeNewBarlineStrength();
				workspace.barlineStrengths.Add(nextStr);
				workspace.barlines.Add((int)nextStr);
			}
			/*
				// TODO: Score barline...
				// Find out sizes/types of groups starting/ending here.
				// TODO

				int score1 = 0;
				int score2 = 0;

				// Score: try all heights.

			*/

			// Pick a random measure in the window
					
				// Clamp measure 0 to MAX_HEIGHT.
				if (measureIndex == 0) {
					workspace.barlineStrengths[0] = MAX_HEIGHT;
					workspace.barlines[0] = MAX_HEIGHT;
					return;
				}
			
				switch (Utilities.rand.Next(8)) {
					case 0:
                        MoveHeightTowardsStrength(measureIndex);
						break;
					case 1:
						DecreaseStrengthTowardsZero(measureIndex);
						break;
					case 2:
						ApplyBoostToStrength(measureIndex);
						break;
					case 3:
						//IncreaseStrengthForZeroBlocks(i);
						break;
					case 4:
						AddNoiseToStrength(measureIndex);
						break;
					case 5:
						//InfluenceOtherBarlines(i);
						break;
					case 6:
						//InfluenceFutureBarline(curMaxMeasure);
						break;
					case 7:
						MakeShortBarlineAfterTall(measureIndex);	
						break;
				}
			
				//MoveHeightTowardsStrength(measureIndex);
					
				
		}


		private float ComputeNewBarlineStrength() {
			int measureIndex = workspace.barlines.Count;
			if (measureIndex == 0) {
				return MAX_HEIGHT;
			} else {
				if (workspace.barlines[measureIndex - 1] > 0) {
					return 0;
				} else {
					//Move backwards, noticing only higher heights, until we hit a gap of size >1. Then fill gap from bottom.
					int curHeight = 0;
					for (int i = measureIndex - 1; i >= 0; i--) {
						int gap = workspace.barlines[i] - curHeight;
						if (gap == 1) {
							curHeight++;
						} else if (gap > 1) {
							return curHeight + 1;
						}
					}
				}

			}
			return MAX_HEIGHT;	// throw exception TODO
		}

        private void MakeShortBarlineAfterTall(int i) {
            if (i + 2 >= workspace.barlines.Count)
                return;

            if (workspace.barlines[i] > 1 && i < curMaxMeasure - 1) {
                // Try to make a height 1 barline in 2 measures.
                float diff = workspace.barlines[i + 2] - 1;
                workspace.barlineStrengths[i + 2] -= diff * ALPHA;
            }
        }

		private void AddNoiseToStrength(int i) {
			// Add noise.
			float magnitude = 0.2f;
			workspace.barlineStrengths[i] += magnitude * (float)(Utilities.rand.NextDouble() - 0.5f);
		}

		private void IncreaseStrengthForZeroBlocks(int i) {
			// Increase strength if heights of both neighbors are 0 and this is 0 too.
			if (workspace.barlines[i] == 0) {
				if (i > 0) {
					if (i < curMaxMeasure) {
						if (workspace.barlines[i - 1] == 0 || workspace.barlines[i + 1] == 0)
							workspace.barlineStrengths[i] += 1;
					} else {
						if (workspace.barlines[i - 1] == 0)
							workspace.barlineStrengths[i] += 1;
					}
				} else {
					if (workspace.barlines[i + 1] == 0)
						workspace.barlineStrengths[i] += 1;
				}
			}
		}

		private void ApplyBoostToStrength(int i) {
			// Boost strength if boost is here and needed.
			int barheight;
			if (BoostsContainsBarline(i, out barheight)) {
				// Make sure this barline is higher than neighbors.

				//if (boost) {
				float diff = workspace.barlineStrengths[i] - barheight;
				if (diff < 0)
					workspace.barlineStrengths[i] -= diff * ALPHA;
				//}
			}
		}

        bool BoostsContainsBarline(int i, out int barheight) {
            barheight = 0;
            // Find largest group ending at barline.
            Group argmax = null;
            int max = -1;
            foreach (Group g in workspace.groups) {
                if (g.MaxLocation == i + 1 && g.LengthInMeasures > max) {
                    max = g.LengthInMeasures;
                    argmax = g;
                }
            }
            if (argmax != null) {
                int level = argmax.Level;
                if (level > 1) {
                    barheight = level;
                    return true;
                }
            }
            return false;
        }

		private void DecreaseStrengthTowardsZero(int i) {
			// Decrease strength towards 0 if either neighbors is non-0.
			if (i > 0) {
                if (i < workspace.barlines.Count - 1) {
					if (workspace.barlines[i - 1] > 0 || workspace.barlines[i + 1] > 0)
						workspace.barlineStrengths[i] *= ALPHA_ZERO_REDUCTION;
				} else {
					if (workspace.barlines[i - 1] > 0)
						workspace.barlineStrengths[i] *= ALPHA_ZERO_REDUCTION;
				}
			} else {
				if (workspace.barlines[i + 1] > 0)
					workspace.barlineStrengths[i] *= ALPHA_ZERO_REDUCTION;
			}
		}

		private void InfluenceOtherBarlines(int i) {
			// Go back until the prev. barline with this height, and expect repetition of height, +1!, in the future at same period.
			int curHeight = workspace.barlines[i];
			if (curHeight == 0)
				return;

			int period = ComputeRecentPeriod(i);

			if (period > 1) {
				int nextMeasure = i + period;
				if (nextMeasure <= curMaxMeasure) {
					// Move strength towards curHeight+1, if it is too low.
					float diff2 = workspace.barlineStrengths[nextMeasure] - (curHeight + 1);
					if (diff2 < 0) {
						workspace.barlineStrengths[nextMeasure] -= diff2 * ALPHA;
					}
				}

				// Between any two close barlines of the same height (>0), we expect to find one measure of length height-1.
				// Look for a measure that qualifies.
				if (curHeight > 0) {
					bool found = true;
					for (int j = i - 1; j >= i - period; j--) {
						if (workspace.barlines[j] == curHeight - 1) {
							found = true;
							break;
						}
					}
					if (!found) {
						// Nothing in between of correct height. 
						// Perturb a random measure in between, and move it halfway to desired pt.
						int m = Utilities.rand.Next(i - period + 1, i);
						float diff3 = workspace.barlineStrengths[m] - curHeight;
						workspace.barlineStrengths[m] -= diff3 * ALPHA;
					}

				}
			}
		}

		int ComputeRecentPeriod(int i) {
			int curHeight = workspace.barlines[i];
			int period = -1;
			for (int j = i - 1; j >= 0; j--)
				if (workspace.barlines[j] == curHeight)
					period = i - j;
			return period;
		}

		private void InfluenceFutureBarline() {
			// Pick a random height.
			int h = Utilities.rand.Next(1, MAX_HEIGHT + 1);

			int m = -1;

			// Pick the last measure with this height.
			for (int i = curMaxMeasure; i >= 0; i--) {
				if (workspace.barlines[i] == h) {
					m = i;
					break;
				}
			}
			if (m==-1)
				return;

			int recentPeriod = ComputeRecentPeriod(m);


			if (recentPeriod > 1) {
				int next = m + recentPeriod;
				
				// If we have an intervening barline of larger height, reset to its "phase"
				for (int i = m+1; i < next; i++) {
					if (next > curMaxMeasure)
						return;
					if (workspace.barlines[i] >= h) {
						// Reset the "phase" of this height to this larger height.
						next = i + recentPeriod;
					}
				}

				if (next <= curMaxMeasure) {
					float diff = workspace.barlineStrengths[next] - h;
					// If too low, increase. 
					if (diff < 0)
						workspace.barlineStrengths[next] -= diff * ALPHA;
				}

			}

			
		}

		#region Helper Functions
		private void MoveHeightTowardsStrength(int i) {
			// Find difference between the barline's strength and its current height.

			float diff = workspace.barlines[i] - workspace.barlineStrengths[i];

			// If diff > .5, move towards each other.
			if (diff > 0.5) {
				workspace.barlines[i]--;
				//workspace.barlineStrengths[i] += ALPHA * diff;
			} else if (diff < -0.5) {
				workspace.barlines[i]++;
				//workspace.barlineStrengths[i] += ALPHA;
			}
		}

		#endregion

	}
}
