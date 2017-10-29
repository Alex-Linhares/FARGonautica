using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	/*public class MetaLink {
		public MeasureLink linkSource;
		public MeasureLink linkDest;
		public float strength;

		public MetaLink(MeasureLink linkSource, MeasureLink linkDest, float strength) {
			this.linkSource = linkSource;
			this.linkDest = linkDest;
			this.strength = strength;
		}
	}*/


	public class Analogy {
		public List<Relationship> relationships;
		public GroupElement LHS, RHS;
        protected Workspace workspace;

        public int CreationTime {
            get;
            protected set;
        }

        public int WorkspaceAge {
            get {
                return workspace.CurrentTime - CreationTime;
            }
        }

		public const float ANALOGY_BEST_RELATIONSHIP_STRENGTH_THRESHOLD = 70;

		public Analogy(GroupElement LHS, GroupElement RHS, Workspace workspace) {
			relationships = new List<Relationship>();
			this.LHS = LHS;
			this.RHS = RHS;
            this.workspace = workspace;
            CreationTime = workspace.CurrentTime;
		}


		public int MinLocation {
			get {
				return LHS.MinLocation;
			}
		}
	
		public int MaxLocation {
			get {
				return RHS.MaxLocation;
			}
		}

		/// <summary>
		/// Is there a gap between the LHS and RHS of at least one measure?
		/// </summary>
		public bool HasGap {
			get {
				return (GapSize > 0);
			}
		}
		
		/// <summary>
		/// Returns the distance between the LHS and RHS, in measures.
		/// </summary>
		public int GapSize {
			get {
				return RHS.MinLocation - LHS.MaxLocation - 1;
			}
		}

		/// <summary>
		/// The length is the sum of the LHS and RHS lengths.
		/// </summary>
		public int LengthInMeasures {
			get {
				return LHS.LengthInMeasures + RHS.LengthInMeasures;
			}
		}


		public int TotalNumberOfElements {
			get {
				int num = 0;
				if (LHS.hasChildren)
					num += ((Group)LHS).GroupElements.Count;
				if (RHS.hasChildren)
					num += ((Group)RHS).GroupElements.Count;
				return num;
			}
		}

		/// <summary>
		/// Returns the hierarchical level of this element. Measures are level 0. 1st-order groups and level 1, etc.
		/// </summary>
		virtual public int Level {
			get {
				return Math.Min(LHS.Level, RHS.Level) + 1;
			}
		}

		public float SumRelationshipStrength {
			get {
				float sum = 0;
				foreach (Relationship r in relationships) {
					sum += r.Strength;
				}
				return sum;
			}
		}

		virtual public float Strength {
			get {
				// Separate into rhythm/melody parts.
				float sumR = 0;
				float sumM = 0;
				int countR = 0;
				int countM = 0;

				// Must have at least one strong relationship that motivates this analogy. Otherwise, it's too weak to exist.
				float max = float.NegativeInfinity;
				foreach (Relationship r in relationships) {
					if (r.RhythmType) {
						sumR += r.Strength;
						countR++;
					} else {
						sumM += r.Strength;
						countM++;
					}

					if (max < r.Strength) {
						max = r.Strength;
					}
				}

				if (max < ANALOGY_BEST_RELATIONSHIP_STRENGTH_THRESHOLD)
					return 0;

				// Analogy scoring has 3 components:
				// 1. Size
				// 2. Completeness of mapping
				// 3. Strength of component relationships.


				// 1. Size.
				
				// Get base 2 log of size (where size is total # of measures, LHS + RHS.)
				float lgSize = (float)Math.Log(LengthInMeasures, 2);  // 1m LHS, 1m RHS -> lgSize = 1.  4m. to 4m: size3. 8m to 8 m: size 4. etc.
				float sizeScore = (lgSize - 1) * 25;
				if (sizeScore < 0)
					sizeScore = 0;
				else if (sizeScore > 100)
					sizeScore = 100;

				// 2. Completeness of mapping

				// Get unmapped components pairs.
				List<GroupElement> unmappedComponents = GetUnmappedComponents();

				// TODO: Bonus for RHS being more conclusive than LHS.
				// Check for an ending groupreason....

				int numComponents = NumComponents;
				int numUnmapped = unmappedComponents.Count;
				int numMapped = numComponents - numUnmapped;

				float completenessScore = 100 * ((float)numMapped) / numComponents;


				// 3. Strength of subcomponents.

				float avgR = (countR > 0) ? sumR / countR : 0;
				float avgM = (countM > 0) ? sumM / countM : 0;

				float averageRelationshipScore = (avgM * Constants.WEIGHT_MELODY + avgR * Constants.WEIGHT_RHYTHM);

                // 4. Age of analogy (in coderack time).
                // age 0 = just created.
                // age 100 = super-old: anything that has remained for NUM_MEASURES_UNTIL_ANALOGIES_OLD or more
                int maxAge = Constants.NUM_CODELETS_PER_SIXTEENTH * 16 * Constants.NUM_MEASURES_UNTIL_ANALOGIES_OLD;
                float ageCeiling = Math.Min(this.WorkspaceAge, maxAge);
                float ageScore = 100 * (ageCeiling / maxAge);

				// Weighted sum.
                float wSum = averageRelationshipScore * Constants.ANALOGY_SCORE_WEIGHT_RELATIONSHIPS +
                    completenessScore * Constants.ANALOGY_SCORE_WEIGHT_COMPLETENESS +
                    sizeScore * Constants.ANALOGY_SCORE_WEIGHT_SIZE +
                    ageScore * Constants.ANALOGY_SCORE_WEIGHT_AGE; 

				return wSum;
			}
		}

		public double ComputeHappiness(out double weight) {
			double strength = this.Strength;

			// Weight: weight higher for large analogies being happy, lower for small analogies.
			weight = (LHS.LengthInMeasures + RHS.LengthInMeasures) / 4;

			return strength;
		}


		public int NumComponents {
			get {
				return GetAllComponents().Count;
			}
		}

		public int NumUnmappedComponents {
			get {
				return GetUnmappedComponents().Count;
			}
		}


		private List<GroupElement> GetAllComponents() {
			List<GroupElement> elements = new List<GroupElement>();
			elements.Add(LHS);
			elements.Add(RHS);
			
			if (LHS.hasChildren)
				elements.AddRange(((Group)LHS).GroupElements);
			
			if (RHS.hasChildren)
				elements.AddRange(((Group)RHS).GroupElements);
			
			/*List<GroupElement> elements = new List<GroupElement>();
			if (LHS.hasChildren)
				elements.AddRange(((Group)LHS).GroupElements);
			else
				elements.Add(LHS);
			if (RHS.hasChildren)
				elements.AddRange(((Group)RHS).GroupElements);
			else
				elements.Add(RHS);*/

			return elements;
		}


		private List<GroupElement> GetUnmappedComponents() {
			List<GroupElement> elements = GetAllComponents();

			foreach (Relationship r in relationships) {
				if (elements.Contains(r.LHS))
					elements.Remove(r.LHS);
				if (elements.Contains(r.RHS))
					elements.Remove(r.RHS);			
			}
			return elements;
		}


		/// <summary>
		/// Adds the given relationship to the analogy, Only one relationships of a particular type is allowed.
		/// If the relationships conflicts with any previous relationship, the previous is removed and replaced with the new one.
		/// </summary>
		/// <param name="reason"></param>
		private void AddRelationship(Relationship relationship) {
			List<Relationship> conflicts = FindConflictingRelationships(relationship);
            foreach (Relationship r in conflicts) {
                // Never overwrite a nice Identical relationship with a Similarity relationship.
                if (EquivalentRelationships(relationship, r)) {
                    if (relationship is RelationshipSimilar && r is RelationshipIdentical)
                        return;
                    if (relationship is RelationshipStartSimilar && r is RelationshipStartIdentical)
                        return;
                }
                RemoveRelationship(r);
            }

			relationships.Add(relationship);
		}

		/// <summary>
		/// Tries to add the given relationship to the analogy, only one relationships of a particular type is allowed.
		/// Requires relationships to conform to a valid left-to-right mapping!
		/// If the relationships conflicts with any previous relationship, the previous is removed and replaced with the new one.
		/// </summary>
		/// <param name="reason"></param>
		public bool TryToAddRelationship(Relationship relationship) {

			// Check for special case of LHS->RHS relationship, instead of subcomponent relationship.
			if (relationship.LHS == LHS && relationship.RHS == RHS) {
                // This is fine; add it. (Note: replaces any previous relationship of same type).
				AddRelationship(relationship);
				return true;
			}

			List<IntMapping> maps = IntMapping.getAllMaps(LHS.Count, RHS.Count);
			// Make sure this relationship is valid in terms of the possible mappings that can occur left-to-right.
			// It has to show up in one of the consistent maps.
			List<IntMapping> consistentMaps = GetConsistentMaps(maps);
			bool found = false;
			int leftIdx = relationship.LHS.IndexInParent;
			int rightIdx = relationship.RHS.IndexInParent;
			foreach (IntMapping map in consistentMaps) {
				foreach (IntPair pair in map.mapPairs) {
					if (pair.x == leftIdx && pair.y == rightIdx) {
						found = true;
						break;
					}
				}
				if (found)
					break;
			}
			// If not found, don't add.
			if (!found)
				return false;

			// No problems. Add it.
			AddRelationship(relationship);
			return true;
		}

		public void CloneRelationshipsFrom(Analogy a) {
			foreach (Relationship r in a.relationships) {
				relationships.Add(r.DeepCopyUsingNewGroupsInAnalogy(r, a));
			}
		}


		/// <summary>
		/// Returns all left-to-right subelement mappings consistent with the current state of relationships in the analogy.
		/// </summary>
		/// <param name="maps"></param>
		/// <returns></returns>
		private List<IntMapping> GetConsistentMaps(List<IntMapping> maps) {
			List<IntMapping> consistentMaps = new List<IntMapping>(maps);
			foreach (Relationship r in relationships) {
				int idxLeft = r.LHS.IndexInParent;
				int idxRight = r.RHS.IndexInParent;
				IntPair p = new IntPair(idxLeft, idxRight);

				// Find invalid maps.
				List<IntMapping> invalid = new List<IntMapping>();
				foreach (IntMapping map in consistentMaps) {
					if (!map.mapPairs.Contains(p))
						invalid.Add(map);
				}
				// Remove.
				foreach (IntMapping map in invalid)
					consistentMaps.Remove(map);
			}
			return consistentMaps;
		}


		public List<Relationship> FindConflictingRelationships(Relationship r) {
			List<Relationship> conflicting = new List<Relationship>();
			foreach (Relationship r2 in relationships) {
				if (ConflictingRelationships(r, r2)) {
					conflicting.Add(r2);
				}
			}
			return conflicting;
		}

		private bool EquivalentRelationships(Relationship r1, Relationship r2) {
			if (r1.LHS == r2.LHS && r1.RHS == r2.RHS) {
				if (r1.IsEquivalentTypeTo(r2))
					return true;
			}
			return false;
		}

		private bool ConflictingRelationships(Relationship r1, Relationship r2) {
			if (r1.LHS == r2.LHS || r1.RHS == r2.RHS) {
				if (r1.IsEquivalentTypeTo(r2))
					return true;
			}
			return false;
		}


		public void RemoveRelationship(Relationship r) {
			relationships.Remove(r);
		}

		public List<Group> GetAllSubGroups() {
			List<Group> subgroups = new List<Group>();
			AddAllSubgroups(this.LHS, subgroups);
			AddAllSubgroups(this.RHS, subgroups);
			return subgroups;
		}

		private void AddAllSubgroups(GroupElement ge, List<Group> subgroups) {
			if (ge is Group) {
				Group g = (Group)ge;
				subgroups.Add(g);
				foreach (GroupElement ge2 in g.GroupElements)
					AddAllSubgroups(ge2, subgroups);
			} 
		}

		public List<GroupElement> GetAllSubElements() {
			List<GroupElement> subelements = new List<GroupElement>();
			AddAllSubelements(this.LHS, subelements);
			AddAllSubelements(this.RHS, subelements);
			return subelements;
		}

		private void AddAllSubelements(GroupElement ge, List<GroupElement> subelements) {
			if (ge is Group) {
				Group g = (Group)ge;
				subelements.Add(g);
				foreach (GroupElement ge2 in g.GroupElements)
					AddAllSubelements(ge2, subelements);
			} else {
				subelements.Add(ge);
			}
		}

		public override string ToString() {
			string lhs = LHS.FormLabel;
			string rhs = RHS.FormLabel;

			if (lhs == null)
				lhs = "m." + (LHS.MinLocation+1).ToString() + "-" + (LHS.MaxLocation+1).ToString();
			if (rhs == null)
				rhs = "m." + (RHS.MinLocation+1).ToString() + "-" + (RHS.MaxLocation+1).ToString();

			return lhs + "<--->" + rhs + " (" + this.Strength.ToString() + ")";
		}

		public bool LinksTheseTwo(GroupElement ge1, GroupElement ge2) {
			return ((LHS == ge1 && RHS == ge2) || (LHS == ge2 && RHS == ge1));
		}

		public Group GetParent() {
			if (LHS.hasParent && RHS.hasParent && LHS.parentGroup == RHS.parentGroup) {
				Group parent = LHS.parentGroup;
				if (parent.MinLocation == LHS.MinLocation && parent.MaxLocation == RHS.MaxLocation &&
					LHS.MaxLocation + 1 == RHS.MinLocation) {
					return parent;
				}
			}
			return null;
		}

		public bool IncludesLocation(int p) {
			return LHS.IncludesLocation(p) || RHS.IncludesLocation(p);
		}



		public void GetUnmappedElements(out List<GroupElement> unmappedLeft, out List<GroupElement> unmappedRight) {
			unmappedLeft = new List<GroupElement>();
			unmappedRight = new List<GroupElement>();
			if (LHS.hasChildren) {
				unmappedLeft.AddRange(((Group)LHS).GroupElements);
				foreach (Relationship r in relationships) {
					// Remove each mapped item from the unmapped list.
					if (unmappedLeft.Contains(r.LHS))
						unmappedLeft.Remove(r.LHS);
				}
				// Now the unmapped list is accurate for the left.
			}
			if (RHS.hasChildren) {
				unmappedRight.AddRange(((Group)RHS).GroupElements);
				foreach (Relationship r in relationships) {
					// Remove each mapped item from the unmapped list.
					if (unmappedRight.Contains(r.RHS))
						unmappedRight.Remove(r.RHS);
				}
				// Now the unmapped list is accurate for the right.
			}
		}


		/// <summary>
		/// Searches the subgrounp tree and returns a match based on measure #s, not pointers. 
		/// </summary>
		/// <param name="target"></param>
		/// <returns></returns>
		public GroupElement FindSubGroupEquivalentToGroup(GroupElement target) {
			GroupElement found = SearchForEquivalentGroupElement(target, this.LHS);

			if (found != null)
				return found;

			return SearchForEquivalentGroupElement(target, this.RHS);
		}

		private GroupElement SearchForEquivalentGroupElement(GroupElement target, GroupElement parent) {
			if (target.MatchesRangeOf(parent))
				return parent;
			if (parent is Group) {
				Group p = (Group)parent;
				foreach (GroupElement ge in p.GroupElements) {
					GroupElement result = SearchForEquivalentGroupElement(target, ge);
					if (result != null)
						return result;
				}
			}
			return null;
		}
	}
}
