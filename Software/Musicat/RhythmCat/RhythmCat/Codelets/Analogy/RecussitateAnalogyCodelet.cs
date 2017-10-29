using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	//[Codelet("Analogy", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class RecussitateAnalogyCodelet : Codelet {

		/// <summary>
		/// The stored analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;
		
		public RecussitateAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Recussitate Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks one randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public RecussitateAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Analogy analogy)
			: base("Recussitate Analogy", urgency, parent, coderack, workspace, slipnet) {

			this.analogy = analogy;
		}

		public override void Run() {

			if (analogy == null) {
				analogy = workspace.structureCollection.PickAnalogy();
			}

			if (analogy == null)
				return;

			// Check if it already exists in the workspace.
			if (workspace.FindEquivalentAnalogy(analogy) != null)
				return;

			// Construct the new LHS and RHS, using existing groups in the workspace if they are there. If groups don't exist, try to add.
			// Also find conflicts.
			GroupElement lhs, rhs;
			bool foundLHS, foundRHS;
			List<Group> conflictGroups = new List<Group>();
			lhs = ProcessAnalogyComponent(analogy.LHS, out foundLHS, conflictGroups);
			rhs = ProcessAnalogyComponent(analogy.RHS, out foundRHS, conflictGroups);

			// Find conflict Analogies.
			List<Analogy> conflictAnalogies = FindConflictAnalogies(conflictGroups);

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, analogy.LHS.MinLocation, analogy.LHS.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, analogy.RHS.MinLocation, analogy.RHS.MaxLocation);

			// Evaluate current workspaces conflict measures vs. the proposed recussitated version, in terms of happiness.
			
			// Find conflict measures.
			HashSet<int> conflictMeasures = UnionGroupRanges(conflictGroups);
			
			// Eval happiness for original version.
			double happinessOld = EvalHappiness(conflictMeasures);

			// Swap out groups/analogies and reeval happiness for new proposed version.
			workspace.DropGroups(conflictGroups);
			workspace.DropAnalogies(conflictAnalogies);

			// Put in the new pile of stuff.
			if (!foundLHS)
				workspace.AddGroupAndSubgroupsNoChecks(lhs);
			if (!foundRHS)
				workspace.AddGroupAndSubgroupsNoChecks(rhs);
	
			Analogy newAnalogy = new Analogy(lhs, rhs, workspace);
			workspace.AddAnalogy(newAnalogy);
			newAnalogy.CloneRelationshipsFrom(analogy);

			// Eval.
			double happinessNew = EvalHappiness(conflictMeasures);

			if (Utilities.FightItOut(happinessNew, happinessOld, workspace.Temperature)) {
				// New one won!

				// Update stats.
				workspace.structureCollection.analogyCompetitionStats[analogy].AddWin();
				
				// Reify all temporary groups.
				if (newAnalogy.LHS is TemporaryGroup) {
					Group realLHS = ((TemporaryGroup)lhs).MakeRealGroup();
					newAnalogy.LHS = realLHS;
				}

				if (newAnalogy.RHS is TemporaryGroup) {
					Group realRHS = ((TemporaryGroup)rhs).MakeRealGroup();
					newAnalogy.RHS = realRHS;
				}

				// Remove left-over relationship stuff from dropped /analogoes
				workspace.CompleteDropGroups(conflictGroups);
				workspace.CompleteDropAnalogies(conflictAnalogies);

			} else {
				// Old one won! revert.
				// Update stats.
				workspace.structureCollection.analogyCompetitionStats[analogy].AddLoss();

				workspace.DropGroupAndSubgroupsNoChecks(lhs);
				workspace.DropGroupAndSubgroupsNoChecks(rhs);
				workspace.DropAnalogyNoChecks(newAnalogy);

				workspace.AddGroupsNoChecks(conflictGroups);
				workspace.AddAnalogiesNoChecks(conflictAnalogies);
			}

			
		}

		private double EvalHappiness(HashSet<int> measures) {
			double total = 0;
			foreach (int i in measures)
				total += workspace.HappinessForMeasure(i);
			return total;
		}

		/// <summary>
		/// Returns all analogies which have one of the given groups as a subcomponent.
		/// </summary>
		/// <param name="conflictGroups"></param>
		/// <returns></returns>
		private List<Analogy> FindConflictAnalogies(List<Group> conflictGroups) {
			List<Analogy> conflicts = new List<Analogy>();

			foreach (Analogy a in workspace.analogies) {
				List<GroupElement> sub = a.GetAllSubElements();
				foreach (GroupElement ge in sub) {
					if (ge is Group) {
						if (conflictGroups.Contains((Group)ge)) {
							conflicts.Add(a);
							break;
						}
					}
				}
			}
			return conflicts;
		}

		private HashSet<int> UnionGroupRanges(List<Group> groupList) {
			HashSet<int> measures = new HashSet<int>();
			foreach (Group g in groupList) {
				for (int i = g.MinLocation; i < g.MaxLocation; i++) {
					measures.Add(i);
				}
			}
			return measures;
		}

		private GroupElement ProcessAnalogyComponent(GroupElement component, out bool foundExisting, List<Group> conflicts) {
			GroupElement processedComponent;
			if (component is Group) {
				// Look for existing LHS.
				Group foundLHSGroup = workspace.FindEquivalentGroup((Group)component, false);
				if (foundLHSGroup != null) {
					processedComponent = foundLHSGroup;
					foundExisting = true;
				} else {
					// Copy substructure recursively... but don't add groups to workspace yet; we'll do that atomically later.
					processedComponent = TemporaryCopyExistingGroupOrFindExisting((Group)component);
					foundExisting = false;
				}
				// Look for conflicts and add to list.
				if (!foundExisting) {
					List<Group> conflictsHere = workspace.FindConflictingGroups((Group)component);

					// Add conflists to list, skipping duplicates.
					foreach (Group gConflict in conflictsHere) {
						if (!conflicts.Contains(gConflict))
							conflicts.Add(gConflict);
					}
				}
			} else {
				// For a measure, no need to clone.
				processedComponent = component;
				foundExisting = true;
			}
			return processedComponent;
		}

		private Group TemporaryCopyExistingGroupOrFindExisting(Group g) {

			Group tempG = workspace.FindEquivalentGroup(g, false);
			if (tempG != null) {
				return tempG;
			}

			tempG = new TemporaryGroup(workspace);

			foreach (GroupElement ge in g.GroupElements) {
				if (ge is Group) {
					tempG.AddGroupElement(TemporaryCopyExistingGroupOrFindExisting((Group)ge));
				} else {
					tempG.AddGroupElement(ge);
				}
			}

			foreach (GroupReason r in g.Reasons) {
				tempG.AddGroupReason(r);
				r.ReplaceGroup(tempG);
			}
			foreach (GroupPenaltyReason r in g.PenaltyReasons) {
				tempG.AddGroupPenaltyReason(r);
				r.ReplaceGroup(tempG);
			}	
					
			return tempG;
		}

		private Relationship FightItOut(List<Relationship> fighting) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < fighting.Count; i++) {
				Relationship r = fighting[i];

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(r, r.Strength);
				pairs.Add(pair);
			}

			return (Relationship)Utilities.PickItemWeighted(pairs);
		}

		private Relationship FightItOut(Relationship r1, Relationship r2) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			pairs.Add(new Utilities.ObjectValuePair(r1, r1.Strength));
			pairs.Add(new Utilities.ObjectValuePair(r2, r2.Strength));

			return (Relationship)Utilities.PickItemWeighted(pairs);
		}


	}
}
