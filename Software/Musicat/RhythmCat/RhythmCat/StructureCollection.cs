using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {

	public class CompetitionStats {
		public int NumCompetitions { get; private set; }
		public int NumWins { get; private set; }

		public void AddWin() {
			NumCompetitions++;
			NumWins++;
		}
	
		public void AddLoss() {
			NumCompetitions++;
		}


		public double PercentWins {
			get {
				return ((float)NumWins) / NumCompetitions;
			}
		}
	}


	public class StructureCollection {
        protected Workspace workspace;
		protected List<Analogy> analogies;
		protected List<Group> groups;

		public Dictionary<Analogy, CompetitionStats> analogyCompetitionStats = new Dictionary<Analogy, CompetitionStats>();
		public Dictionary<Group, CompetitionStats> groupCompetitionStats = new Dictionary<Group, CompetitionStats>();

		public int NumAnalogyWinsTotal {
			get {
				int total = 0;
				foreach (CompetitionStats cs in analogyCompetitionStats.Values)
					total += cs.NumWins-1;
	
				return total;
			}
		}


		public int NumAnalogyFightsTotal {
			get {
				int total = 0;
				foreach (CompetitionStats cs in analogyCompetitionStats.Values)
					total += cs.NumCompetitions-1;

				return total;
			}
		}

		public StructureCollection(Workspace workspace) {
            this.workspace = workspace;
            analogies = new List<Analogy>();
			groups = new List<Group>();
			analogyCompetitionStats = new Dictionary<Analogy, CompetitionStats>();
			groupCompetitionStats = new Dictionary<Group, CompetitionStats>();
		}

		/// <summary>
		/// Adds a strong analogy to the list, cloning in memory any sub analogies/relationships necessary. i.e. we duplicate the tree structure.
		/// </summary>
		/// <param name="a"></param>
		public void AddAnalogy(Analogy a) {
			// First check if it already exists.
			Analogy found = FindAnalogy(a);

			// If it exists, update (by adding relationships, not taking away)
			if (found != null) {
				UpdateAnalogy(found, a);
				return;
			}

			// Otherwise, clone and store new version.
			GroupElement lhs = a.LHS.DeepCopy();
			GroupElement rhs = a.RHS.DeepCopy();

			Analogy newAnalogy = new Analogy(lhs, rhs, workspace);

			// Clone analogy relationships.
			newAnalogy.CloneRelationshipsFrom(a);

			analogies.Add(newAnalogy);
			analogyCompetitionStats[newAnalogy] = new CompetitionStats();
			analogyCompetitionStats[newAnalogy].AddWin();	// start with default win.
		}

		private void UpdateAnalogy(Analogy toUpdate, Analogy newerAnalogy) {
			foreach (Relationship r in newerAnalogy.relationships) {
				toUpdate.TryToAddRelationship(r);	// TODO might destroy old relationships... not sure we want this............ might need to have them compete too...
			}
		}

		/// <summary>
		/// Checks if analogy exists (in terms of have the same measure span and same (recursive) structure of groups)
		/// </summary>
		/// <param name="a"></param>
		/// <returns></returns>
		public Analogy FindAnalogy(Analogy toFind) {
			foreach (Analogy a in analogies) {
				// Verify LHS and RHS are the same measures.
				if (Group.VerifySameGroupStructures(toFind.LHS, a.LHS) && Group.VerifySameGroupStructures(toFind.RHS, a.RHS))
					return a;
			}
			return null;
		}

		

		/// <summary>
		/// Adds group to the collection, unless one with the same range and same subelements exists.
		/// </summary>
		/// <param name="g"></param>
		public void AddGroup(Group g) {
			// Make sure group doens't already exist.
			if (FindExistingGroup(g) != null)
				return;
			
			Group newGroup = (Group)g.DeepCopy();
			groups.Add(newGroup);

			groupCompetitionStats[newGroup] = new CompetitionStats();
			groupCompetitionStats[newGroup].AddWin();	// start with default win.
		}

		public Group FindExistingGroup(Group g) {
			foreach (Group g2 in groups)
				if (GroupsHaveSameStructure(g, g2))
					return g2;
			return null;
		}

		private bool GroupsHaveSameStructure(GroupElement ge1, GroupElement ge2) {
			if (!ge2.MatchesRangeOf(ge1))
				return false;

			// Check for idential substructure
			if (ge1 is Measure && ge2 is Measure)
				return true;
			if (!(ge1 is Group && ge2 is Group))
				return false;
			Group g1 = (Group)ge1;
			Group g2 = (Group)ge2;

			if (g1.Count != g2.Count)
				return false;
			for (int i = 0; i < g1.Count; i++) 
				if (!GroupsHaveSameStructure(g1.GroupElements[i], g2.GroupElements[i]))
					return false;

			return true;
		}


		/// <summary>
		/// Returns a random analogy from the collection.
		/// Weights it by win/loss stats
		/// </summary>
		/// <returns></returns>
		public Analogy PickAnalogy() {
			if (analogies.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < analogies.Count; i++) {
				Analogy a = analogies[i];

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(analogies[i], analogyCompetitionStats[a].PercentWins);
				pairs.Add(pair);
			}

			return (Analogy)Utilities.PickItemWeighted(pairs);
		}
	}
}
