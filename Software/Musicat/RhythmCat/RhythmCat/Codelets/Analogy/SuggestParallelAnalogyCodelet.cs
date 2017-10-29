using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	/// <summary>
	/// Makes a possible analogy.
	/// Looks for an existing analogy between groups with a gap in between them. 
	/// </summary>
	[Codelet("Create", CodeletAttribute.CodeletWorkType.Create, 40, true)]
	public class SuggestParallelAnalogyCodelet : Codelet {

		/// <summary>
		/// The group to examine. If none given, we select randomly.
		/// </summary>
		private Analogy sourceAnalogy;


		public SuggestParallelAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Suggest Parallel Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which analogy to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public SuggestParallelAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy sourceAnalogy)
			: base("Suggest Parallel Analogy", urgency, parent, coderack, workspace, slipnet) {

			this.sourceAnalogy = sourceAnalogy;
		}


		public override void Run() {
			if (sourceAnalogy == null) {
				// Pick an expected group to start from.
				sourceAnalogy = workspace.PickRandomGapAnalogyByRecencyAndStrengthAndSizeAndGapSize();
			}

			if (sourceAnalogy == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, sourceAnalogy.LHS.MinLocation, sourceAnalogy.LHS.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, sourceAnalogy.RHS.MinLocation, sourceAnalogy.RHS.MaxLocation);

			// Look in two places for parallel analogies: one crossing the LHS of the given analogy (RHS of new in the gap),
			// and one crossing the RHS (the LHS will be in the gap)
			int sizeRHS = sourceAnalogy.RHS.LengthInMeasures;
			int sizeLHS = sourceAnalogy.LHS.LengthInMeasures;
			int gapMin = sourceAnalogy.LHS.MaxLocation + 1;
			int gapMax = sourceAnalogy.RHS.MinLocation - 1;

			// Make sure gap is large enough to hold something of appropriate size.
			if (gapMax - gapMin < Math.Min(sizeLHS, sizeRHS))
				return;

			// First try to find a group inside the gap, of the same size as the RHS.
			GroupElement geGap = FindGroupElementInRange(gapMin, gapMax, sizeRHS);
			if (geGap != null) {
				// Now look for an element of same size as LHS, to the left of the original LHS.
				// We find the difference in starting pos between the element in the gap and the RHS, and look at that offset to the left to find an element.
				// If none found, send grouping codelets.
				int offset = sourceAnalogy.RHS.MinLocation - geGap.MinLocation;
				GroupElement geLHS = FindGroupElementStartingNearPointWithMax(sourceAnalogy.LHS.MinLocation - offset, sourceAnalogy.LHS.MinLocation-1, sizeLHS);	// start point, max, size
				if (geLHS != null) {
					// Try to form analogy.
					CreateAnalogyCodelet cac = new CreateAnalogyCodelet(100, this, coderack, workspace, slipnet, geLHS, geGap);
					coderack.AddCodelet(cac);
				} else {
					// Send group scouts to find something in that area.
					SendGroupScoutsToArea(sourceAnalogy.LHS.MinLocation - offset, sizeLHS);	// min, size
				}
			} else {
				// Send group scouts to look in the gap.	
			}

			// Second try to find a group inside the gap, of the same size as the LHS.
			geGap = FindGroupElementInRange(gapMin, gapMax, sizeLHS);
			if (geGap != null) {
				// Now look for an element of same size as RHS, to the right of the original RHS.
				// We find the difference in starting pos between the element in the gap and the RHS, and look at that offset to the left to find an element.
				// If none found, send grouping codelets.
				int offset = geGap.MinLocation - sourceAnalogy.LHS.MaxLocation;
				GroupElement geRHS = FindGroupElementStartingNearPointWithMin(sourceAnalogy.RHS.MaxLocation + offset, sourceAnalogy.RHS.MaxLocation+1, sizeRHS);	// start point, min, size
				if (geRHS != null) {
					// Try to form analogy.
					CreateAnalogyCodelet cac = new CreateAnalogyCodelet(100, this, coderack, workspace, slipnet, geGap, geRHS);
					coderack.AddCodelet(cac);
				} else {
					// Send group scouts to find something in that area.
					SendGroupScoutsToArea(sourceAnalogy.RHS.MaxLocation + offset, sizeRHS);	// min, size
				}
			}
		}

		

		/// <summary>
		/// Find a group element strictly in the given range, close to the given size.
		/// </summary>
		/// <param name="gapMin"></param>
		/// <param name="gapMax"></param>
		/// <param name="sizeRHS"></param>
		/// <returns></returns>
		private GroupElement FindGroupElementInRange(int min, int max, int size) {
			if (size == 1) {
				// return a measure
				int n = Utilities.rand.Next(min, max + 1);
				return workspace.measures[n];
			}
			if (workspace.groups.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Group group in workspace.groups) {
				if (group.MinLocation < min || group.MaxLocation > max)
					continue;

				double sizeScore =  Math.Max(0, 1.0 - Math.Abs(group.LengthInMeasures - size) / (double)size);

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(group, sizeScore);
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
		}

		/// <summary>
		/// Finds a Group Element near the given starting location, with an absolute maximum, and approx desired size.
		/// </summary>
		/// <param name="startPoint"></param>
		/// <param name="max"></param>
		/// <param name="size"></param>
		/// <returns></returns>
		private GroupElement FindGroupElementStartingNearPointWithMax(int startPoint, int max, int size) {
			if (startPoint < 0)
				startPoint = 0;

			if (size == 1) {
				// return a measure
				if (startPoint <= workspace.MaxLocation)
					return workspace.measures[startPoint];
				else
					return null;
			}
			if (workspace.groups.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Group group in workspace.groups) {
				if (group.MaxLocation > max)
					continue;

				double sizeScore =  Math.Max(0, 1.0 - Math.Abs(group.LengthInMeasures - size) / (double)size);
				double locScore = Math.Max(0, 1.0 - Math.Abs(group.MinLocation - startPoint) / (double)size);
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(group, sizeScore * locScore);
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
		}

		/// <summary>
		/// Finds a Group Element near the given starting location, with an absolute minimum, and approx desired size.
		/// </summary>
		/// <param name="startPoint"></param>
		/// <param name="max"></param>
		/// <param name="size"></param>
		/// <returns></returns>
		private GroupElement FindGroupElementStartingNearPointWithMin(int startPoint, int min, int size) {
			if (size == 1) {
				// return a measure
				if (startPoint <= workspace.MaxLocation)
					return workspace.measures[startPoint];
				else
					return null;
			}
			if (workspace.groups.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Group group in workspace.groups) {
				if (group.MinLocation < min)
					continue;

				double sizeScore =  Math.Max(0, 1.0 - Math.Abs(group.LengthInMeasures - size) / (double)size);
				double locScore = Math.Max(0, 1.0 - Math.Abs(group.MinLocation - startPoint) / (double)size);
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(group, sizeScore * locScore);
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
		}


		// Creates group scout codelets for a group starting near position, of given size.
		private void SendGroupScoutsToArea(int position, int size) {
            if (size < 3) {
                if (size < 2)
                    return;
                if (position < 0 || position + 1 >= workspace.measures.Count)
                    return;

                ProximityGrouperCodelet pgc = new ProximityGrouperCodelet(
                    40, this, coderack, workspace, slipnet,
                    workspace.measures[position],
                    workspace.measures[position + 1]);
                coderack.AddCodelet(pgc);

                // Try to find measure link.
                MeasureLink link = workspace.FindMeasureLink(position, position + 1);
                if (link != null) {
                    MeasureSamenessGrouperCodelet mgc = new MeasureSamenessGrouperCodelet(
                        60, this, coderack, workspace, slipnet,
                        link);
                    coderack.AddCodelet(mgc);
                }

                return;
            }

            
            int range = size / 2;
            for (int pos = position - range; pos <= position + range; pos++) {
                
                // Set urgency higher for an exact fit.
                int u = 30;
                if (pos == position)
                    u = 100;

                Group g1=null, g2=null;
                bool foundBoth = false;
                // Try to divide into 2 subelements: find existing groups in range.
                for (int split = pos + 2; split <= pos + size - 2; split++) {
                    foundBoth = TryToFindGroups(pos, split - 1, split, pos + size - 1, out g1, out g2);

                    if (!foundBoth && pos == position) {
                        if (g1 == null) {
                            SendGroupScoutsToArea(pos, split - pos);
                        } else {
                            SendGroupScoutsToArea(split, pos+size-split+1);
                        }
                    }

                    if (foundBoth)
                        break;
                }

                if (!foundBoth)
                    continue;
                
                MetaGrouperCodelet mgc = new MetaGrouperCodelet(u, this, coderack, workspace, slipnet, g1, g2);
                coderack.AddCodelet(mgc);
            }
		}

        private bool TryToFindGroups(int a1, int a2, int b1, int b2, out Group g1, out Group g2) {
            bool result = true;
            GroupElement ge1 = workspace.FindGroupElement(a1, a2);
            if (ge1 is Group)
                g1 = (Group)ge1;
            else {
                g1 = null;
                result = false;
            }

            GroupElement ge2 = workspace.FindGroupElement(b1, b2);
            if (ge2 is Group)
                g2 = (Group)ge2;
            else {
                g2 = null;
                result = false;
            }

            return result;
        }
	}
}