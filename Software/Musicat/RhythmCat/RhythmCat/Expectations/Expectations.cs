using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RhythmCat {
	public class Expectations {
		private Workspace workspace;

		private int offset;

		public int Offset {
			get { return offset; }
		}

		private Group sourceGroup;

		public Group SourceGroup {
			get { return sourceGroup; }
		}

		public List<ExpectedGroup> groups;
		public List<ExpectedMeasure> measures;
		public List<ExpectedAnalogy> analogies;
		public List<ExpectedMeasureLink> links;

		public Expectations(Workspace workspace) {
			this.workspace = workspace;

			groups = new List<ExpectedGroup>();
			measures = new List<ExpectedMeasure>();
			analogies = new List<ExpectedAnalogy>();
			links = new List<ExpectedMeasureLink>();
			offset = 0;
		}
		
		/*public ExpectedGroup AddGroup(List<ExpectedMeasure> measures, float expectedStrength) {
			ExpectedGroup g = new ExpectedGroup(expectedStrength);

			return g;
		}*/

		public ExpectedMeasure GetExpectedMeasure(int idx) {
			foreach (ExpectedMeasure m in measures)
				if (m.Location == idx)
					return m;
			return null;
		}

		public void CleanExpectations() {
			int time = workspace.measures.Count - 1;

			// Measures.
			List<ExpectedMeasure> mToDelete = new List<ExpectedMeasure>(); 
			foreach (ExpectedMeasure m in measures) {
				if (m.MaxLocation < time)
					mToDelete.Add(m);
			}

			foreach (ExpectedMeasure m in mToDelete)
				measures.Remove(m);

			// Groups.
			List<ExpectedGroup> gToDelete = new List<ExpectedGroup>(); 
			foreach (ExpectedGroup g in groups) {
				if (g.MaxLocation < time)
					gToDelete.Add(g);
			}

			foreach (ExpectedGroup g in gToDelete)
				groups.Remove(g);


			// Analogies.
			List<ExpectedAnalogy> aToDelete = new List<ExpectedAnalogy>();
			foreach (ExpectedAnalogy a in analogies) {
				if (a.MaxLocation < time)
					aToDelete.Add(a);
			}

			foreach (ExpectedAnalogy a in aToDelete)
				analogies.Remove(a);


			// Links.
			List<ExpectedMeasureLink> lToDelete = new List<ExpectedMeasureLink>();
			foreach (ExpectedMeasureLink link in links) {
				if (link.MostRecentMeasure.Location < time)
					lToDelete.Add(link);
			}

			foreach (ExpectedMeasureLink link in lToDelete)
				links.Remove(link);

		}

		public void RemoveAllExpectations() {
			groups = new List<ExpectedGroup>();
			measures = new List<ExpectedMeasure>();
			analogies = new List<ExpectedAnalogy>();
			links = new List<ExpectedMeasureLink>();
		}

		public ExpectedGroup GenerateExpectedGroupBasedOn(Group group, int offset, float expectationStrength, bool compete) {
			// First check other competing expectation groups. Delete them and add this one if we win.
			// Note that ALL other expectation groups are considered "competing". i.e. only 1 set of expectations can be
			// active at one time.

			double str = group.ComputeStrength();
			// Look for conflicts and resolve.

			if (compete) {
				/*
				foreach (Group g2 in groups) {
					if (Utilities.FightItOut(g2.ComputeStrength(), str, workspace.Temperature)) {
						// Lost fight.
						return null;
					}
				}*/
				ExpectedGroup eGroup = FindLargestExpectationGroup();
				if (eGroup != null) {
					if (Utilities.FightItOut(eGroup.ComputeStrength(), str, workspace.Temperature)) {
						// Lost fight.
						return null;
					}
				}

				// Won all fights. Clean out expectations.
				RemoveAllExpectations();
				this.offset = offset;
				this.sourceGroup = group;
			}

			
			// Copy the group structure, recursively.
			ExpectedGroup expectedGroup = MakeNewExpectedGroup(group.MinLocation + offset, group.MaxLocation + offset, group.Level, group.ComputeStrength());

			
			foreach (GroupElement e in group.GroupElements) {
				if (e is Measure)
					GenerateExpectedMeasureBasedOn((Measure)e, offset, group.ComputeStrength());
				else {
					GenerateExpectedGroupBasedOn((Group)e, offset, expectationStrength, false);
				}
					
			}

			return expectedGroup;
		}

		public ExpectedGroup FindLargestExpectationGroup() {
			int maxSize = -1;
			ExpectedGroup argmax = null;
			foreach (ExpectedGroup g2 in groups) {
				int size = g2.LengthInMeasures;
				if (size > maxSize) {
					maxSize = size;
					argmax = g2;
				}
			}
			return argmax;
		}



		private void GenerateExpectedMeasureBasedOn(Measure m, int offset, double strength) {
			ExpectedMeasure em = new ExpectedMeasure(m, m.Location + offset, (float)strength);	//TODO: deal with any existing conflicts............???
			measures.Add(em);
		}

		private ExpectedGroup MakeNewExpectedGroup(int minLocation, int maxlocation, int level, double strength) {
			ExpectedGroup g = new ExpectedGroup(workspace, minLocation, maxlocation, level, (float)strength);
			groups.Add(g);		// TODO: deal with any existing conflicts............???
			return g;
		}

		public ExpectedMeasureLink MakeNewExpectedMeasureLink(MeasureLink sourceLink, int offset, float strength) {
			int location1 = sourceLink.m1.number;
			int location2 = sourceLink.m2.number;

			ExpectedMeasure em1 =  workspace.expectations.GetExpectedMeasure(location1+offset);
			ExpectedMeasure em2 =  workspace.expectations.GetExpectedMeasure(location2+offset);

			ExpectedMeasureLink expectedLink = new ExpectedMeasureLink(sourceLink, em1, em2, strength);
			links.Add(expectedLink);
			return expectedLink;
		}

		public ExpectedMeasureLink MakeNewExpectedMeasureLink(int location1, int location2, int offset, float strength) {
			ExpectedMeasure em1 = workspace.expectations.GetExpectedMeasure(location1 + offset);
			ExpectedMeasure em2 = workspace.expectations.GetExpectedMeasure(location2 + offset);

			ExpectedMeasureLink expectedLink = new ExpectedMeasureLink(null, em1, em2, strength);
			links.Add(expectedLink);
			return expectedLink;
		}

		public ExpectedAnalogy MakeNewExpectedAnalogy(Group group, ExpectedGroup expectedGroup, float expectationStrength, int level) {
			ExpectedAnalogy a = new ExpectedAnalogy(group, expectedGroup, workspace, expectationStrength, level);
			analogies.Add(a);
			return a;
		}


		public ExpectedGroup PickRandomGroupWithConditions(int startTime, int level) {
			List<ExpectedGroup> now = new List<ExpectedGroup>();

			foreach (ExpectedGroup g in groups) {
				if (g.MinLocation == startTime && g.Level == level)
					now.Add(g);
			}

			return Utilities.PickItem<ExpectedGroup>(now);
		}

		public ExpectedMeasureLink PickRandomLinkWithEndTime(int time) {
			List<ExpectedMeasureLink> now = new List<ExpectedMeasureLink>();

			foreach (ExpectedMeasureLink link in links) {
				if (link.MostRecentMeasure.MinLocation == time)
					now.Add(link);
			}

			return Utilities.PickItem<ExpectedMeasureLink>(now);
		}

		public ExpectedAnalogy PickRandomAnalogyWithMaxEndTime(int time) {
			List<ExpectedAnalogy> valid = new List<ExpectedAnalogy>();

			foreach (ExpectedAnalogy a in analogies) {
				if (a.MaxLocation <= time)
					valid.Add(a);
			}

			return Utilities.PickItem<ExpectedAnalogy>(valid);
		}

		public ExpectedGroup FindGroupByLocation(int min, int max) {
			foreach (ExpectedGroup eg in groups) {
				if (eg.MinLocation == min && eg.MaxLocation == max)
					return eg;
			}
			return null;
		}
	}
}



