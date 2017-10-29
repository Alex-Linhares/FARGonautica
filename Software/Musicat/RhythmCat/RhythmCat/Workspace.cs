using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using MusicPrimitives;

namespace RhythmCat
{
	public class WorkspaceEventArgs : EventArgs
	{
		public Workspace workspace;

		public WorkspaceEventArgs(Workspace workspace)
		{
			this.workspace = workspace;
		}
	}
	public class Workspace
	{

		private delegate void CloseFormsDelegate();

		bool GUIenabled;

		public List<Measure> measuresInput;      // the "True" inputs (including future)
		public List<Measure> measuresInputShifted;      // the "True" inputs (including future), shifted by upbeat
		public List<Measure> measures;           // the perceived measures (transformed by upbeat, if present)
		public List<Measure> measuresDrawing;    // the perceived measures, no upbeat transform
		public StructureCollection structureCollection;	// The good, big, stored structures (i.e. TableTop Workspace (not worldview))

		public int upbeatOffset { get; set; }

		public bool HasUpbeat {
			get {
				return upbeatOffset > 0;
			}
		}

		public List<MeasureLink> measureLinks;
		public List<Group> groups;

		private int initialMelodyIndex = 0;

		/// <summary>
		/// Index i gives the barline just BEFORE measure i.
		/// index 0 is for the barline before the 1st measure. index 1 is the barline between the 1st and 2nd measures, etc.
		/// </summary>
		public List<int> barlines;
		public List<float> barlineStrengths;
		public List<Analogy> analogies;
		public List<Relationship> relationships;
		public Expectations expectations;
		public List<Tuple<int, Codelet>> codeletAttentionHistory;

		public Key Key {
			get;
			set;
		}

		private double totalDrawTime;	// duration of all draw commands, in milliseconds
		private int numDraws;

		public double AvgDrawTimeMs {
			get {
				return totalDrawTime / numDraws;
			}
		}

		private Slipnet slipnet;
		private Coderack coderack;

		public Coderack Coderack {
			get { return coderack; }
			set { coderack = value; }
		}

		public MainForm frmMain;
		public WorkspaceForm frmWorkspace;
		public GroupScoresForm frmGroupScores;
		public AnalogyScoresForm frmAnalogyScores;
		public CoderackForm frmCoderack;

		private int programRunNum;

		public Slipnet Slipnet {
			get { return slipnet; }
			set { slipnet = value; }
		}

		/// <summary>
		/// Time in units of # run codelets.
		/// </summary>
		protected int currentTime;

		/// <summary>
		/// Time in units of # run codelets.
		/// </summary>
		public int CurrentTime
		{
			get { return currentTime; }
			set { currentTime = value; }
		}

		public double Temperature { get; set; }


		public event EventHandler<WorkspaceEventArgs> Update;
		public event EventHandler<EventArgs> Close;

		public List<GroupElement> GroupElements {
			get {
				List<GroupElement> elements = new List<GroupElement>();
				foreach (Measure m in measures)
					elements.Add(m);
				foreach (Group g in groups)
					elements.Add(g);
				return elements;
			}
		}

		/// <summary>
		/// Look for bad (garbage) relationships, and also remove from analogies.
		/// </summary>
		public void CollectGarbage() {
			List<GroupElement> elements = GroupElements;
			List<Relationship> garbage = new List<Relationship>();

			foreach (Relationship r in relationships) {
				if (!(elements.Contains(r.LHS) && elements.Contains(r.RHS))) {
					garbage.Add(r);
				}
			}

			List<Analogy> garbageAnalogies = new List<Analogy>();
			foreach (Relationship r in garbage) {
				relationships.Remove(r);
				foreach (Analogy a in analogies) {
					if (a.relationships.Contains(r)) {
						a.RemoveRelationship(r);
						if (a.relationships.Count == 0)
							garbageAnalogies.Add(a);
					}
				}
				foreach (Analogy a in garbageAnalogies)
					analogies.Remove(a);
			}
		}

		/// <summary>
		/// Removes old expectations.
		/// </summary>
		public void CleanExpectations() {
			// TODO.
			throw new NotImplementedException();

		}


		public Workspace(int programRunNum, bool GUIenabled, int curMelodyIndex)
		{
			this.programRunNum = programRunNum;
			this.GUIenabled = GUIenabled;
			this.initialMelodyIndex = curMelodyIndex;
			this.Key = new MusicPrimitives.Key();
			measuresInput = new List<Measure>();
			measuresInputShifted = new List<Measure>();
			measures = new List<Measure>();
			measuresDrawing = new List<Measure>();
			measureLinks = new List<MeasureLink>();
			barlines = new List<int>();
			barlineStrengths = new List<float>();
			analogies = new List<Analogy>();
			relationships = new List<Relationship>();
			groups = new List<Group>();
			expectations = new Expectations(this);
			codeletAttentionHistory = new List<Tuple<int, Codelet>>();
			structureCollection = new StructureCollection(this);
			
			Temperature = 50;

			//CreateTheForms()
			//while (frmMain == null || frmWorkspace == null)
			//	Thread.SpinWait(5);

			//mainForm.SetText("WSC");
		}

		~Workspace() {
			if (GUIenabled) {
				CloseForms();
			}
		}

		/// <summary>
		/// Must be called after all forms, links are wired up in the main program, here the workspace needs to know the coderack pointer.
		/// </summary>
		public void CreateTheForms() {
			Thread thread = new Thread(new ThreadStart(CreateForms));
			thread.Start();

			while (frmMain == null || frmWorkspace == null || frmCoderack == null)
				Thread.SpinWait(5);
		}


		private void CreateForms() {

			frmMain = new MainForm();
			frmWorkspace = new WorkspaceForm(initialMelodyIndex);
			frmWorkspace.SetRunNumber(programRunNum);
			frmWorkspace.Show();

			frmGroupScores = new GroupScoresForm(this);
			frmGroupScores.Show();

			frmAnalogyScores = new AnalogyScoresForm(this);
			frmAnalogyScores.Show();

			frmCoderack = new CoderackForm(coderack);
			frmCoderack.Show();

			frmWorkspace.closeEvent += frmWorkspace_closeEvent; //new EventHandler<EventArgs>(frmWorkspace_closeEvent);
			frmMain.closeEvent += frmMain_closeEvent; // new EventHandler<EventArgs>(frmMain_closeEvent);

			Application.Run(frmMain);

		}

		public void UnsubscribeFormEventHandlers() {
			frmWorkspace.closeEvent -= frmWorkspace_closeEvent;
			frmMain.closeEvent -= frmMain_closeEvent;
		}

		public void Draw(Coderack coderack) {
			if (GUIenabled) {
				try {
					// Measure drawing time.
					DateTime start = DateTime.Now;

					// Draw.
					frmMain.DrawWorkspace(this);
					frmWorkspace.Draw(this, slipnet, coderack);
					frmGroupScores.Draw();
					frmAnalogyScores.Draw();
					frmCoderack.Draw();
					//while (frmCoderack.Updating) {
					//	Thread.Sleep(5);
					//}

					// Measure end time.
					TimeSpan duration =  DateTime.Now - start;
					numDraws++;
					totalDrawTime += duration.TotalMilliseconds;
					Console.WriteLine("Avg draw time (ms) = {0}\tCur = {1}", totalDrawTime/numDraws, duration.TotalMilliseconds);

					// Re-average sometimes, putting in 5 copies of prev. average to smooth over the transition. This keeps us current.
					if (numDraws == 20) {
						double oldAvg = totalDrawTime/numDraws;
						numDraws = 5;
						totalDrawTime = numDraws * oldAvg;
					}

				} catch (Exception) {

				}
			}
		}

		/*
		/// <summary>
		/// Raise an event to let viewers update their views.
		/// </summary>
		public void UpdateView() {
			if (GUIenabled) {
				// Verify we have subsubscribed event handlers.
				if (Update != null) {
					WorkspaceEventArgs ea = new WorkspaceEventArgs(this);
					Update(this, ea);
				}
				try {
					frmGroupScores.Draw();
					frmCoderack.Draw();

					while (frmCoderack.Updating) {
						Thread.Sleep(5);
					}
				} catch (Exception) {

				}
			}
		}

		*/

		void frmMain_closeEvent(object sender, EventArgs e) {
			if (Close != null) {
				Close(this, e);
			}
			//frmCoderack.Close();
			//frmGroupScores.Close();
		}

		void frmWorkspace_closeEvent(object sender, EventArgs e) {
			if (Close != null) {
				Close(this, e);
			}
			frmCoderack.Close();
			frmGroupScores.Close();
			frmAnalogyScores.Close();
			frmMain.Close();
		}

		public void CloseForms() {
			if (frmCoderack.InvokeRequired) {
				frmCoderack.Invoke(new CloseFormsDelegate(CloseForms));
			} else {
				frmCoderack.Close();
				frmGroupScores.Close();
				frmWorkspace.Close();
				frmAnalogyScores.Close();
				frmMain.Close();
			}
		}



		public virtual void Reset()
		{

		}

		


		/// <summary>
		/// Return the index of the maximum measure, inclusive (excluding expectations).
		/// </summary>
		public int MaxLocation {
			get {
				return measures.Count - 1;
			}
		}





		public Measure PickRandomMeasureByRecency() {
			if (measures.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < measures.Count; i++ ) {
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(measures[i], ComputeRecencyScore(measures[i]));
				pairs.Add(pair);
			}

			return (Measure)Utilities.PickItemWeighted(pairs);
		}


		private double ComputeRecencyScore(GroupElement ge) {
			if (ge is Measure)
				return ComputeRecencyMultiplier(ge.IndexInParent);
			else
				return ComputeRecencyMultiplier(((Group)ge).MaxLocation);
		}

		private double ComputeRecencyScore(MeasureLink link) {
			return ComputeRecencyMultiplier(link.MostRecentMeasure.IndexInParent);
		}

		private double ComputeRecencyScore(Relationship r) {
			return ComputeRecencyMultiplier(r.RHS.MaxLocation);
		}

		private double ComputeRecencyScore(Analogy a) {
			return ComputeRecencyMultiplier(a.RHS.MaxLocation);
		}

		private const int PRESENT_TIME_RECENCY_WINDOW_MEASURES = 4;
		private const double RECENCY_WINDOW_EXPONENT = 1.5;

		private int ComputeDistanceInPast(int location) {
			return Math.Max(1, measures.Count - (location + PRESENT_TIME_RECENCY_WINDOW_MEASURES - 1));
		}

		private double ComputeRecencyMultiplier(int location) {
			int distanceInPast = ComputeDistanceInPast(location);
			return Math.Pow(distanceInPast, -RECENCY_WINDOW_EXPONENT);
		}

		/// <summary>
		/// Returns 1 if totally unmapped, 0 if all elements are mapped.
		/// </summary>
		/// <param name="a"></param>
		/// <returns></returns>
		private double ComputeIncompletenessScore(Analogy a) {
			List<GroupElement> unmappedLeft, unmappedRight;
			a.GetUnmappedElements(out unmappedLeft, out unmappedRight);

			int totalElements = a.TotalNumberOfElements;

			return (unmappedLeft.Count + unmappedRight.Count) / (double)totalElements;
		}

		public MeasureLink PickRandomMeasureLinkByRecencyAndStrength() {
			if (measureLinks.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (MeasureLink link in measureLinks) {
				double score = link.strength * ComputeRecencyScore(link);

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(link, score);
				pairs.Add(pair);
			}

			return (MeasureLink)Utilities.PickItemWeighted(pairs);
		}


		public Relationship PickRandomRelationshipByRecencyAndStrength() {
			if (relationships.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Relationship relationship in relationships) {
				double recency = measures.Count - relationship.RHS.Location;

				double score = relationship.Strength * ComputeRecencyScore(relationship);

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(relationship, score);
				pairs.Add(pair);
			}

			return (Relationship)Utilities.PickItemWeighted(pairs);
		}


        public RelationshipMelodyContour PickRandomContourRelationshipByRecencyAndStrength() {
            if (relationships.Count == 0)
                return null;

            List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

            foreach (Relationship relationship in relationships) {
                if (relationship is RelationshipMelodyContour) {
                    double recency = measures.Count - relationship.RHS.Location;

                    double score = relationship.Strength * ComputeRecencyScore(relationship);

                    Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(relationship, score);
                    pairs.Add(pair);
                }
            }

            return (RelationshipMelodyContour)Utilities.PickItemWeighted(pairs);
        }



        public RelationshipTransposition PickRandomTranspositionRelationshipByRecencyAndStrength() {
            if (relationships.Count == 0)
                return null;

            List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

            foreach (Relationship relationship in relationships) {
                if (relationship is RelationshipTransposition) {
                    double recency = measures.Count - relationship.RHS.Location;

                    double score = relationship.Strength * ComputeRecencyScore(relationship);

                    Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(relationship, score);
                    pairs.Add(pair);
                }
            }

            return (RelationshipTransposition)Utilities.PickItemWeighted(pairs);
        }    


		public override string ToString() {
			StringBuilder sb = new StringBuilder();

			sb.AppendLine("Original Input:");
			foreach (Measure m in measuresInput)
				sb.AppendLine(m.ToString());

			sb.AppendLine("\nMeasures:");
			foreach (Measure m in measures)
				sb.AppendLine(m.Label + ": " + m.ToString());

			sb.AppendLine("\nMeasure Links:");

			measureLinks.Sort();
			foreach (MeasureLink ml in measureLinks)
				sb.AppendLine(ml.ToString());

			sb.AppendLine("\nAnalogies:");
			foreach (Analogy a in analogies)
				sb.AppendLine(a.ToString());

			return sb.ToString();
		}

		/// <summary>
		/// Adds a link between two measures of given strength. If it exits already, the strength is updated.
		/// Strength between 0 and 100.
		/// </summary>
		/// <param name="measure1"></param>
		/// <param name="measure2"></param>
		/// <param name="similarity"></param>
		/// <returns></returns>
		public MeasureLink AddMeasureLink(Measure measure1, Measure measure2, float similarity) {
			// Look for existing link.
			MeasureLink l = null;

			foreach (MeasureLink x in measureLinks) {
				if ((x.m1 == measure1 && x.m2 == measure2) || (x.m1 == measure2 && x.m2 == measure1)) {
					l = x;
					break;
				}
			}

			if (l == null) {
				l = new MeasureLink(measure1, measure2, similarity);
				measureLinks.Add(l);
				// TODO:
				// For now, also add a parallel "relationship" between the measures. Later we will remove separate MeasueLinks entirely.
				Relationship r;
				if (similarity > 99.99)
					r = new RelationshipIdentical(measure1, measure2, similarity);
				else
					r = new RelationshipSimilar(measure1, measure2, similarity);
				AddRelationship(r);
			} else
				l.strength = similarity;
			return l;
		}

		public void AddRelationship(Relationship r) {
			// Check for duplicate.
			foreach (Relationship r2 in relationships) {
				if (r2.LHS == r.LHS && r2.RHS == r.RHS && r.GetType() == r2.GetType()) {
					// Found duplicate; just update strength.
					r2.Strength = r.Strength;
					return;
				}
			}
			relationships.Add(r);
		}


		public MeasureLink PickRandomMeasureLinkByRecency() {
			if (measureLinks.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < measureLinks.Count; i++) {
				MeasureLink link = measureLinks[i];
				int location = link.MostRecentMeasure.number;
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(measureLinks[i], ComputeRecencyScore(link));
				pairs.Add(pair);
			}

			return (MeasureLink)Utilities.PickItemWeighted(pairs);
		}

		public MeasureLink PickRandomMeasureLinkByOldest() {
			if (measureLinks.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < measureLinks.Count; i++) {
				MeasureLink link = measureLinks[i];
				int location = link.MostRecentMeasure.number;
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(measureLinks[i], 1.0 - ComputeRecencyScore(link));
				pairs.Add(pair);
			}

			return (MeasureLink)Utilities.PickItemWeighted(pairs);
		}

		/// <summary>
		/// Add analogy unless it already exists.
		/// </summary>
		/// <param name="a"></param>
		public bool AddAnalogy(Analogy a) {
			if (a.relationships.Count > 0) {
				foreach (Analogy a2 in analogies) {
					if (a2.relationships.Contains(a.relationships[0]))
						return false;
				}
			}
			analogies.Add(a);

			// Remove any old relationship for this analogy.
			List<Relationship> toDelete = new List<Relationship>();
			foreach (Relationship r in relationships) {
				if (r.LHS == a.LHS && r.RHS == a.RHS)
					toDelete.Add(r);
			}
			foreach (Relationship r in toDelete)
				relationships.Remove(r);

			// Also add a "relationship" to link the groups involved.
			RelationshipAnalogy rs = new RelationshipAnalogy(a);
			relationships.Add(rs);
			return true;
		}

		public Group CreateAndAddGroup(GroupElement ge1, GroupElement ge2) {
			Group g = new Group(this, ge1, ge2);
			groups.Add(g);
			return g;
		}

		/// <summary>
		/// Adds the group only if it survives all conflicts; fights with all overlaps!
		/// </summary>
		/// <param name="tmpG"></param>
		/// <returns></returns>
		public Group AddGroup(TemporaryGroup tmpG) {
			double str = tmpG.ComputeStrength();

			List<Group> toRemove;
			bool cancelAdd = TestGroupAddIsPossible(tmpG, str, out toRemove);
			if (cancelAdd)
				return null;

			try {
				// The group survived all fights. Destroy overlapping groups.
				DestroyGroupsTopDown(toRemove);
			} catch (Exception e) {
				Utilities.LogtoFile(e.ToString());

				System.Diagnostics.Debugger.Break();
			}

			// Add the new group.
			Group realGroup = tmpG.MakeRealGroup();
			groups.Add(realGroup);
			return realGroup;
		}

		private void DestroyGroupsTopDown(List<Group> toRemove) {
			// First sort by level so we destroy top-level groups first.
			toRemove.Sort((a, b) => -a.Level.CompareTo(b.Level));
			foreach (Group g2 in toRemove)
				BreakGroup(g2);
		}

		private bool TestGroupAddIsPossible(Group g, double str, out List<Group> toRemove) {
			bool cancelAdd = false;

			// Look for conflicts and resolve.
			toRemove = new List<Group>();
			foreach (Group g2 in groups) {
				// Check for duplication.
				if (g.MinLocation == g2.MinLocation && g.MaxLocation == g2.MaxLocation) {
					// Check if all subcomponents are the same
					bool hasAll = true;
					foreach (GroupElement ge2 in g.GroupElements) {
						if (!g2.GroupElements.Contains(ge2)) {
							hasAll = false;
							break;
						}
					}
					if (hasAll) {
						cancelAdd = true;
						break;
					}
				}

				//if (g.Level == g2.Level) {
				if (g.Overlaps(g2) && !g.ContainsSubGroup(g2)) {
					if (FightItOut(g2.ComputeStrength(), str)) {
						// Lost fight.
						cancelAdd = true;
						break;
					}
					// The overlapping group lost fight. Prepare to destroy it.
					toRemove.Add(g2);
				}
				//}
			}

			// Verify groups aren't too old to break.
			foreach (Group g2 in toRemove) {
				if (g2.IsPermanent) {
					cancelAdd = true;
					break;
				}
			}

			if (!cancelAdd) {
				if (GUIenabled) {
					if (toRemove.Count > 0) {
						Console.WriteLine("Destroying " + toRemove.Count.ToString() + " groups. New strength: " + str.ToString());
						Console.Write("Old Strengths: ");
						foreach (Group g2 in toRemove)
							Console.Write(g2.ComputeStrength().ToString() + ", ");
						Console.Write("  ");
					}
					Console.WriteLine();
				}
			}
			return cancelAdd;
		}


		public bool AddSequence(Sequence sequence, double str) {
			//double str = sequence.ComputeStrength();
			
			List<Group> toRemove;
			bool cancelAdd = TestGroupAddIsPossible(sequence, str, out toRemove);
			if (cancelAdd)
				return false;

			// The group survived all fights. Destroy overlapping groups.
			DestroyGroupsTopDown(toRemove);

            // Repair child-parent relationships.
            foreach (GroupElement ge in sequence.GroupElements) {
                ge.parentGroup = sequence;
            }

			// Add the sequence.
			groups.Add(sequence);
			return true;
		}

		
		public List<Group> FindConflictingGroups(Group newGroup) {//, out bool foundIdentical) {
			//foundIdentical = false;
			List<Group> conflicts = new List<Group>();
			foreach (Group g2 in groups) {
				/*// Check for duplication.
				if (newGroup.MinLocation == g2.MinLocation && newGroup.MaxLocation == g2.MaxLocation) {
					// Check if all subcomponents are the same
					if (Group.VerifySameGroupStructures(newGroup, g2)) {
						foundIdentical = true;
						return new List<Group>();
					}
				}
				*/
				if (newGroup.Overlaps(g2) && !newGroup.ContainsSubGroup(g2))
					conflicts.Add(g2);
			}
			return conflicts;
		}

		public Group AttemptExtensionRight(Group g) {
			TemporaryGroup tmpG = new TemporaryGroup(this);
			foreach (GroupElement ge in g.GroupElements)
				tmpG.AddGroupElement(ge);
			tmpG.AddGroupElement(measures[g.MaxLocation + 1]);	// extend right 1 measure
			foreach (GroupReason r in g.Reasons) {
				if (!(r is GroupReasonEndBeforeGap || r is GroupReasonEndBeforeLeap)) {
					tmpG.AddGroupReason(r);		// TODO: clone reason?
				}
			}
					


			double str = tmpG.ComputeStrength(); 
			List<Group> toRemove = new List<Group>();
			foreach (Group g2 in groups) {
				if (g2 == g) // skip current (unextended) group
					continue;
				// Check for duplication.
				if (tmpG.MinLocation == g2.MinLocation && tmpG.MaxLocation == g2.MaxLocation) {
					// Check if all subcomponents are the same
					bool hasAll = true;
					foreach (GroupElement ge2 in tmpG.GroupElements) {
						if (!g2.GroupElements.Contains(ge2)) {
							hasAll = false;
							break;
						}
					}
					if (hasAll)
						return null;
				}

				//if (tmpG.Level == g2.Level) {
				if (tmpG.Overlaps(g2) && !tmpG.ContainsSubGroup(g2)) {
					if (FightItOut(g2.ComputeStrength(), str))
						// Lost fight.
						return null;
					// The overlapping group lost fight. Prepare to destroy it.
					toRemove.Add(g2);
				}
				//}
			}
			if (GUIenabled) {
				if (toRemove.Count > 0) {
					Console.WriteLine("Destroying " + toRemove.Count.ToString() + " groups. New strength: " + str.ToString());
					Console.Write("Old Strengths: ");
					foreach (Group g2 in toRemove)
						Console.Write(g2.ComputeStrength().ToString() + ", ");
					Console.Write("  ");
				}
				Console.WriteLine();
			}

			// The group survived all fights. Destroy overlapping groups.
			// First sort by level so we destroy top-level groups first.
			toRemove.Sort((a, b) => -a.Level.CompareTo(b.Level));
			
			// Make sure gropus to break aren't too old.
			foreach (Group g2 in toRemove)
				if (g2.IsPermanent)
					return null;
			
			foreach (Group g2 in toRemove)
				BreakGroup(g2);
			// Remove old version.
			BreakGroup(g);
			// Add the new group.
			Group realGroup = tmpG.MakeRealGroup();
			groups.Add(realGroup);
			return realGroup;
		}

		/// <summary>
		/// Remove a group and update its children's parent references.
		/// <returns>True if break is successful, false otherwise.</returns>
		/// </summary>
		public bool BreakGroup(Group group) {
			if (group.hasParent)
				throw new Exception("Logic: can't break group that has a parent");

			// Only break if it's not too old.
			if (group.IsPermanent)
				return false;

			foreach (GroupElement ge in group.GroupElements)
				ge.parentGroup = null;
			
			// Remove analogy references for any analogy this group is part of.
			// Destroy anaolgies where this is a critical LHS or RHS..
			List<Analogy> toDestroy = new List<Analogy>();
			foreach (Analogy a in analogies) {
				if (a.LHS == group || a.RHS == group) {
					toDestroy.Add(a);
					continue;
				}

				List<Relationship> toDestroyRelatinoships = new List<Relationship>();
				foreach (Relationship r in a.relationships) {
					if (r.LHS == group || r.RHS == group)
						toDestroyRelatinoships.Add(r);
				}
				foreach (Relationship r in toDestroyRelatinoships) {
					a.RemoveRelationship(r);
				}
			}

			// Destroy invalid analogies.
			foreach (Analogy a in toDestroy) {
				analogies.Remove(a);
			}
			
			groups.Remove(group);
			return true;
		}

		public Group PickRandomGroupByRecencyAndStrength() {
			if (groups.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Group group in groups) {
				double recency = measures.Count - group.MaxLocation;

				double score = group.ComputeStrength() * ComputeRecencyScore(group);	// TODO: scale differently? seems like a very steep curve.

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(group, score);
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
		}


		public Group PickRandomGroupByStrength() {
			if (groups.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Group group in groups) {
				double score = group.ComputeStrength();
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(group, score);
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
		}



		public GroupElement PickRandomGroupElementByRecencyAndStrength() {
			List<GroupElement> elements = GroupElements;

			if (elements.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < elements.Count; i++) {
				GroupElement e = elements[i];

				double strength;
				if (e is Group)
					strength = ((Group)e).ComputeStrength();
				else
					strength = 100;
				
				double score = strength * ComputeRecencyScore(e);	// TODO: scale differently? seems like a very steep curve.

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(e, score);
				pairs.Add(pair);
			}

			return (GroupElement)Utilities.PickItemWeighted(pairs);
		}


		public Group PickRandomGroupByRecency() {
			if (groups.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Group group in groups) {
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(group, ComputeRecencyScore(group));
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
		}





		public void BreakMeasureLink(MeasureLink link) {
			measureLinks.Remove(link);
			// Find a related "Relationship" and remove it.
			Relationship toRemove = null;
			foreach (Relationship r in relationships) {
				if (r.LHS == link.m1 && r.RHS == link.m2) {
					toRemove = r;
					break;
				}
			}
			if (toRemove != null)
				relationships.Remove(toRemove);
		}

		public List<MeasureLink> GetOtherLinksForMeasure(Measure measure, MeasureLink link) {
			List<MeasureLink> otherLinks = new List<MeasureLink>();

			foreach (MeasureLink l in measureLinks) {
				if (l == link)
					continue;
				if (l.m1 == measure || l.m2 == measure)
					otherLinks.Add(l);
			}
			return otherLinks;
		}


		public void TakeScreenshot(int measureNum) {
			frmWorkspace.TakeScreenshot(measureNum);
		}

		public GroupElement PickRandomGroupElementByRecency() {
			List<GroupElement> elements = GroupElements;

			if (elements.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < elements.Count; i++) {
				GroupElement e = elements[i];
				
				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(elements[i],
					1.0 / ((measures.Count - e.Location) * (measures.Count - e.Location)));
				pairs.Add(pair);
			}

			return (GroupElement)Utilities.PickItemWeighted(pairs);
		}



		
		
		/// <summary>
		/// Given two rival structures of any sort, this function decides if structure 1 will "beat" structure 2. Returns true is structure 1 wins.
		/// </summary>
		/// <param name="strength1">Strength (0 to 100) of structure 1</param>
		/// <param name="strength2">Strength (0 to 100) of structure 2</param>
		/// <returns></returns>
		public bool FightItOut(double strength1, double strength2) {
			return Utilities.FightItOut(strength1, strength2, Temperature);
		}

		public string GetNextFormLabel(int level) {
			char baseChar;

			switch (level % 4) {
				case 0:
					baseChar = 'a';
					break;
				case 1:
					baseChar = 'A';
					break;
				case 2:
					baseChar = 'α';
					break;
				case 3:
					baseChar = 'Γ';
					break;
				default:
					throw new NotImplementedException();
			}

			List<GroupElement> elements = GetElementsAtLevel(level);

			// Try to find a free letter.
			string proposedLabel = "";
			bool free = true;
			for (int i = 0; i < 64; i++) {
				proposedLabel = "" + ((char)((int)baseChar + i));
				free = true;
				foreach (GroupElement e in elements) {
					if (e.FormLabel == proposedLabel) {
						free = false;
						break;
					}
				}
				if (free)
					break;
			}
			if (!free)
				throw new Exception("couldn't find free label letter");
			return proposedLabel;
		}

		public List<GroupElement> GetElementsAtLevel(int level) {
			List<GroupElement> list = new List<GroupElement>();

			foreach (GroupElement e in GroupElements) {
				if (e.Level == level)
					list.Add(e);
			}
			return list;
		}

		public Group PickRandomGroupAdjacentTo(Group g) {
			int prevEndMeasure = g.MinLocation - 1;
			int nextFirstMeasure = g.MaxLocation + 1;

			// Find groups fitting either criterion.
			List<Group> candidateGroups = new List<Group>();
			foreach (Group g2 in groups)
				if (g2.MaxLocation == prevEndMeasure || g2.MinLocation == nextFirstMeasure)
					candidateGroups.Add(g2);
			
			return Utilities.PickItem(candidateGroups);
		}


		public Group PickRandomGroupByAdjacencyAndSize(Group sourceGroup) {
			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < groups.Count; i++) {
				Group g = groups[i];

				if (g == sourceGroup)
					continue;

				// Compute closeness in time and closeness in size.

				// ratio is 0 for same size, 0.25 for a 25% difference, 1.0 for a 100% difference, et
				double sizeRatio = Math.Abs(g.LengthInMeasures - sourceGroup.LengthInMeasures) / (double)sourceGroup.LengthInMeasures;

				if (sizeRatio > 0.5)
					continue;

				// Scale to range 0-1, where 1 is perfect, 0 is 50% different size (bad)
				double sizeRatioScore = 1.0 - (sizeRatio / 0.5);

				// Compute closeness.
				int dist = sourceGroup.DistanceToGroup(g);

				// Skip overlapping groups.
				if (dist < 1)
					continue;
				// Scale to range 0-1, where 1 is distance 1 (next-door neighbor) and 0 is 2x length of group 1 away.
				double distNormed = (dist-1) / (double)sourceGroup.LengthInMeasures;	// 0 indicates neighbors, 1 indicates 1 grp len away, etc
				// Cap distance at 2x.
				if (distNormed > 2)
					distNormed = 2;
				double closenessScore = (2 - distNormed) / 2;
				double score = sizeRatioScore *  closenessScore;

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(g, score);
				pairs.Add(pair);
			}

			return (Group)Utilities.PickItemWeighted(pairs);
			//
		}

		public void WeakenOldLinks() {
			float multiplier = 1 - Constants.LINKS_PERCENT_FADE;
			List<MeasureLink> toRemove = new List<MeasureLink>();
			List<Relationship> toRemoveR = new List<Relationship>();

			// Weaken links.
			for (int i = 0; i < measureLinks.Count; i++) {
				MeasureLink link = measureLinks[i];
				// Leave recent links alone!
				if (MaxLocation - link.MostRecentMeasure.Location < Constants.NUM_MEASURES_OF_PERFECT_MEMORY)
					continue;

				link.strength *= multiplier;
				if (link.strength < 10)
					toRemove.Add(link);
			}

			// Weaken relationships.
			for (int i = 0; i < relationships.Count; i++) {
				Relationship r = relationships[i];

				// Leave recent relationships alone!
				if (MaxLocation - r.RHS.MaxLocation < Constants.NUM_MEASURES_OF_PERFECT_MEMORY)
					continue;

				// Is this a relationship analogy? If so, skip.
				if (r is RelationshipAnalogy)
					continue;

				// Is relationship used?
				bool used = false;
				foreach (Analogy a in analogies)
					if (a.relationships.Contains(r)) {
						used = true;
						break;
					}
				if (!used)
					r.Strength *= multiplier;
			}

			if (GUIenabled)
				Console.WriteLine("Removing {0} weak links and {1} relationships", toRemove.Count, toRemoveR.Count);

			foreach (MeasureLink link in toRemove)
				measureLinks.Remove(link);
			foreach (Relationship r in toRemoveR)
				relationships.Remove(r);
		}

		public Analogy PickRandomAnalogyByRecency() {
			if (analogies.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < analogies.Count; i++) {
				Analogy a = analogies[i];

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(analogies[i], ComputeRecencyScore(a));
				pairs.Add(pair);
			}

			return (Analogy)Utilities.PickItemWeighted(pairs);
		}


		public Analogy PickRandomAnalogyByRecencyAndStrengthAndSize() {
			if (analogies.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < analogies.Count; i++) {
				Analogy a = analogies[i];

				double strength = a.Strength;
				double recency = ComputeRecencyScore(a);
				int size = a.LengthInMeasures;

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(analogies[i], strength * recency * size);
				pairs.Add(pair);
			}

			return (Analogy)Utilities.PickItemWeighted(pairs);
		}

		/// <summary>
		/// Returns an analogy with a gap between the LHS and RHS.
		/// Prefers analogies with gap size similar to LHS or RHS size.
		/// </summary>
		/// <returns></returns>
		public Analogy PickRandomGapAnalogyByRecencyAndStrengthAndSizeAndGapSize() {
			if (analogies.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < analogies.Count; i++) {
				Analogy a = analogies[i];

				if (!a.HasGap)
					continue;

				double targetGap= (a.LHS.LengthInMeasures + a.RHS.LengthInMeasures)/2.0;
				double gapScore = (a.GapSize > targetGap) ? targetGap / a.GapSize : a.GapSize / targetGap;
			

				double strength = a.Strength;
				double recency = ComputeRecencyScore(a);
				int size = a.LengthInMeasures;

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(analogies[i], strength * recency * size * gapScore);
				pairs.Add(pair);
			}

			return (Analogy)Utilities.PickItemWeighted(pairs);
		}


		/// <summary>
		/// Picks analogies that are more recent, and also have more missing relationships (unmapped elements), and are bigge!r
		/// </summary>
		/// <returns></returns>
		public Analogy PickRandomAnalogyByRecencyAndIncompletenessAndSize() {
			if (analogies.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			for (int i = 0; i < analogies.Count; i++) {
				Analogy a = analogies[i];

				double incompleteness = ComputeIncompletenessScore(a);
				double recency = ComputeRecencyScore(a);
				int size = a.LengthInMeasures;	// TODO!!!! remove

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(analogies[i], incompleteness * recency * size);
				pairs.Add(pair);
			}

			return (Analogy)Utilities.PickItemWeighted(pairs);
		}

		/// <summary>
		/// Tries to compute similarity between two structures.
		/// At the base, if they are both measures, just does a measure-based similarlty computation.
		/// Otherwise, resorts to analogies between the structures.
		/// If no analogy is found, posts codelets to try to build one.
		/// </summary>
		/// <param name="ge1"></param>
		/// <param name="ge2"></param>
		/// <param name="callingCodelet"></param>
		/// <returns></returns>
		public float ComputeSimilarity(GroupElement ge1, GroupElement ge2, Codelet callingCodelet, Coderack coderack) {
			if (ge1 is Measure && ge2 is Measure) {
				return ((Measure)ge1).ComputeRhythmicSimilarity((Measure)ge2);
			} else {
				// Look for an analogy between the groups.
				foreach (Analogy a in analogies)
					if (a.LHS == ge1 && a.RHS == ge2)
						return a.Strength;
				// No analogy found. Try to make one.
				CreateAnalogyCodelet cac = new CreateAnalogyCodelet(90, callingCodelet, coderack, this, slipnet, ge1, ge2);
//				coderack.AddCodelet(cac, 3);	//Post 3 copies! this is important...(WHY? becase we weren't specifying ge1 and ge2?)
				coderack.AddCodelet(cac);	//Post 3 copies! this is important...(WHY?)
				return 0;
			}
		}

		/// <summary>
		/// Tries to compute similarity between two structures, but only look at Start!
		/// Start is defined as the 1st half. For groups, if this is > 8 beats, just use the first 8 beats.
		/// Uses rhythmic similarity.
		/// </summary>
		/// <param name="ge1"></param>
		/// <param name="ge2"></param>
		/// <param name="callingCodelet"></param>
		/// <returns></returns>odel
		public float ComputeSimilarityStartOnly(GroupElement ge1, GroupElement ge2) {
			
			// Get a list of attack points.
			List<int> attacks1full = ge1.AttackPoints;
			List<int> attacks2full = ge2.AttackPoints;

			// Get max time of ge, and find halfway point.
			int time1 = 0;
			int time2 = 0;
			foreach (Note n in ge1.AllNotes)
				time1 += n.duration;
			foreach (Note n in ge2.AllNotes)
				time2 += n.duration;

			int maxTime = Math.Min(32, Math.Max(time1 / 2, time2 / 2));

			List<int> attacks1 = new List<int>();
			List<int> attacks2 = new List<int>();

			foreach (int i in attacks1full)
				if (i <= maxTime)
					attacks1.Add(i);
				else
					break;

			foreach (int i in attacks2full)
				if (i <= maxTime)
					attacks2.Add(i);
				else
					break;

				return ComputeRhythmicSimilarity(attacks1, attacks2);
		}


		private float ComputeRhythmicSimilarity(List<int> attackPoints1, List<int> attackPoints2) {
			int diff = ComputeSymmetricDifference(attackPoints1, attackPoints2);
			int totalAttacks = attackPoints1.Count + attackPoints2.Count;

			return 100 * (1.0f - ((float)diff / totalAttacks));
		}

		private static int ComputeSymmetricDifference(List<int> list, List<int> list2) {
			return CountMissingInts(list, list2) + CountMissingInts(list2, list);
		}

		private static int CountMissingInts(List<int> list, List<int> list2) {
			int count = 0;
			foreach (int i in list) {
				if (!list2.Contains(i))
					count++;
			}
			return count;
		}



		/// <summary>
		/// Find the closest parent analogy involving this groupElement.
		/// TODO: this seems ambiguous, if 2 analogies use the same group...
		/// </summary>
		/// <returns></returns>
		public List<Analogy> FindAllEnclosingAnalogies(GroupElement ge) {
			List<Analogy> enclosing = new List<Analogy>();
			foreach (Analogy a in analogies) {
				List<GroupElement> elements = a.GetAllSubElements();
				if (elements.Contains(ge))
					enclosing.Add(a);
			}

			return enclosing;
		}

		public GroupElement PickRandomGroupElementRightOf(GroupElement ge) {
			int nextFirstMeasure = ge.MaxLocation + 1;

			// Find groups starting there.
			List<GroupElement> candidateGroups = new List<GroupElement>();
			foreach (GroupElement ge2 in GroupElements)
				if (ge2.MinLocation == nextFirstMeasure)
					candidateGroups.Add(ge2);

			return Utilities.PickItem(candidateGroups);
		}

		public GroupElement PickRandomGroupElementAdjacentTo(GroupElement ge) {
			int prevEndMeasure = ge.MinLocation - 1;
			int nextFirstMeasure = ge.MaxLocation + 1;

			// Find groups fitting either criterion.
			List<GroupElement> candidateGroups = new List<GroupElement>();
			foreach (GroupElement ge2 in GroupElements)
				if (ge2.MaxLocation == prevEndMeasure || ge2.MinLocation == nextFirstMeasure)
					candidateGroups.Add(ge2);

			return Utilities.PickItem(candidateGroups);
		}

		public int MaxPossibleTime {
			get;
			set;
		}




		/*
		/// <summary>
		/// Figures out, for a given hierarchy level, which range of measures we can reasonably consider to do stuff to in the workspace.
		/// </summary>
		/// <param name="minMeasure"></param>
		/// <param name="maxMeasure"></param>
		/// <param name="hierarchyLevel">Level 0 = measures, level 1 = 1st order groups, level 2= metagroups, etc.</param>
		public void GetWorkingRange(int hierarchyLevel, out int minMeasure, out int maxMeasure) {

			

		}*/

		/// <summary>
		/// Computes the happiness for the given measure. 
		/// </summary>
		/// <param name="measure"></param>
		/// <returns></returns>
		public double HappinessForMeasure(int measure) {
			return HappinessForRange(measure, measure);
		}

		/// <summary>
		/// Computes the happiness for the given range of measures (inclusive). 
		/// </summary>
		/// <param name="minMeasure"></param>
		/// <param name="maxMeasure"></param>
		/// <returns></returns>
		public double HappinessForRange(int minMeasure, int maxMeasure) {
			// Find all the structures that overlap these measures at all.
			double sum = 0;
			double sumWeights = 0;
			foreach (GroupElement ge in GroupElements) {
				if (ge.OverlapsRange(minMeasure, maxMeasure)) {
					double weight;
					double happiness = ge.ComputeHappiness(out weight);
					sum += happiness * weight;
					sumWeights += weight;

					// TODO? Find largest-level group in range.
					
				}
			}

			foreach (Analogy a in analogies) {
				if (a.LHS.OverlapsRange(minMeasure, maxMeasure) || a.RHS.OverlapsRange(minMeasure, maxMeasure)) {
					double weight;
					double happiness = a.ComputeHappiness(out weight);
					sum += happiness * weight;
					sumWeights += weight;
					
					// TODO: Add in other factors?

				}

			}

			if (sumWeights < 0.0001)
				return 0;
			return sum / sumWeights;
		}


		public double GetHappinessStandardWindow(int measureIndex, out int minMeasure, out int maxMeasure) {
			minMeasure = Math.Max(0, measureIndex - Constants.HAPPINESS_WINDOW_SIZE / 2);
			maxMeasure = Math.Min(measureIndex + Constants.HAPPINESS_WINDOW_SIZE / 2, this.measures.Count - 1);
			return this.HappinessForRange(minMeasure, maxMeasure);
		}


		public Group PickGroupInRange(int minMeasure, int maxMeasure) {
			List<Group> groupList = new List<Group>();
			foreach (Group g in groups) {
				if (g.OverlapsRange(minMeasure, maxMeasure))
					groupList.Add(g);
			}

			return Utilities.PickItem<Group>(groupList);
		}

		public MeasureLink PickMeasureLinkInRange(int minMeasure, int maxMeasure) {
			List<MeasureLink> linkList = new List<MeasureLink>();
			foreach (MeasureLink link in measureLinks) {
				if (link.m1.OverlapsRange(minMeasure, maxMeasure) || link.m2.OverlapsRange(minMeasure, maxMeasure))
					linkList.Add(link);
			}

			return Utilities.PickItem<MeasureLink>(linkList);
		}

		public Measure PickMeasureInRange(int minMeasure, int maxMeasure) {
			List<Measure> measureList = new List<Measure>();
			for (int i = minMeasure; i <= maxMeasure; i++)
				measureList.Add(measures[i]);

			return Utilities.PickItem<Measure>(measureList);
		}

		public GroupElement PickGroupElementInRange(int minMeasure, int maxMeasure) {
			List<GroupElement> geList = new List<GroupElement>();
			foreach (GroupElement ge in GroupElements) {
				if (ge.OverlapsRange(minMeasure, maxMeasure))
					geList.Add(ge);
			}
			return Utilities.PickItem<GroupElement>(geList);
		}


		public int CountLinksToMeasure(int index, out double avgLinkStrength, out double maxLinkStrength) {
			double totalStrength = 0;
			maxLinkStrength = 0;
			int count = 0;
			foreach (MeasureLink link in measureLinks) {
				if (link.Involves(measures[index])) {
					count++;
					totalStrength += link.strength;
					if (link.strength > maxLinkStrength)
						maxLinkStrength = link.strength;
				}
			}
			if (totalStrength == 0)
				avgLinkStrength = 0;
			else
				avgLinkStrength = totalStrength / count;
			return count;
		}

		public int CountGroupsInvolvingMeasure(int i, out double avgGroupStrength, out double maxGroupStrength) {
			double totalStrength = 0;
			maxGroupStrength = 0;
			int count = 0;
			foreach (Group group in groups) {
				if (group.OverlapsRange(i, i)) {
					count++;
					double groupStrength = group.ComputeStrength();
					totalStrength += groupStrength;
					if (groupStrength > maxGroupStrength)
						maxGroupStrength = groupStrength;
				}
			}
			if (totalStrength == 0)
				avgGroupStrength = 0;
			else
				avgGroupStrength = totalStrength / count;
			return count;
		}

		public int CountAnalogiesInvolvingMeasure(int i, out double avgAnalogyStrength, out double maxAnalogyStrength) {
			double totalStrength = 0;
			maxAnalogyStrength = 0;
			int count = 0;
			foreach (Analogy analogy in analogies) {
				if (analogy.IncludesLocation(i)) {
					count++;
					double analogyStrength = analogy.Strength;
					totalStrength += analogyStrength;
					if (analogyStrength > maxAnalogyStrength)
						maxAnalogyStrength = analogyStrength;
				}
			}
			if (totalStrength == 0)
				avgAnalogyStrength = 0;
			else
				avgAnalogyStrength = totalStrength / count;
			return count;
		}

		/// <summary>
		/// Records history of codelet attention for a single measure.
		/// </summary>
		/// <param name="codelet"></param>
		/// <param name="measureIndex"></param>
		public void RecordCodeletAttentionHistory(Codelet codelet, int measureIndex) {
			// Keep track of all the places where codelets showed attention to the workspace.
			// TODO: prune to last N datapoints to save memory.
			codeletAttentionHistory.Add(new Tuple<int, Codelet>(measureIndex, codelet));
		}

		/// <summary>
		/// Records history of codelet attention, for a given range.
		/// </summary>
		/// <param name="codelet"></param>
		/// <param name="measureIndexStart"></param>
		/// <param name="measureIndexEnd"></param>
		public void RecordCodeletAttentionHistory(Codelet codelet, int measureIndexStart, int measureIndexEnd) {
			// Keep track of all the places where codelets showed attention to the workspace.
			// TODO: prune to last N datapoints to save memory.
			for (int i = measureIndexStart; i <= measureIndexEnd; i++ )
				codeletAttentionHistory.Add(new Tuple<int, Codelet>(i, codelet));
		}

		// Try to find a groupelement with the given coordinates.
		public GroupElement FindGroupElement(int startLocation, int endLocation) {
			if (startLocation == endLocation) {
				if (startLocation < measures.Count)
					return measures[startLocation];
			}

			foreach (Group g in groups) {
				if (g.MinLocation == startLocation && g.MaxLocation == endLocation)
					return g;
			}

			return null;

		}



		public Analogy PickRandomAnalogyInGroup(Group group) {
			List<Analogy> valid = new List<Analogy>();
			int min = group.MinLocation;
			int max = group.MaxLocation;

			foreach (Analogy a in analogies) {
				if (a.MinLocation >= min && a.MaxLocation <= max)
					valid.Add(a);
			}
			return Utilities.PickItem<Analogy>(valid);
		}

		/// <summary>
		/// Starts at the given location and works backwards, finding each barline height's normal distance until another of the same height.
		/// </summary>
		/// <param name="startExpectationLocation"></param>
		/// <returns></returns>
		public int[] FindPreviousBarlineDistances(int startExpectationLocation) {
			try {
				// First find the max barline height up to this location.
				int max = -1;
				for (int i = 0; i <= startExpectationLocation; i++)
					if (barlines[i] > max)
						max = barlines[i];

				// Make an array to hold all barline heights up to max. Initiailize each one's expected length to -1.
				int[] heights = new int[max + 1];
				for (int i = 0; i <= max; i++)
					heights[i] = -1;

				// Loop backwards from RHS, filling up heights as needed.
				for (int rhs = startExpectationLocation; rhs > 0; rhs--) {
					// Get this barline height, and check if we already computed its length backwards.
					int h = barlines[rhs];
					if (h < 0) {
						// Still need to compute barlines. fail.
						return null;
						
					}
					if (heights[h] > -1)
						continue;
					for (int i = rhs - 1; i >= 0; i--) {
						// If we reached the same or taller barline than we started on, stop here.
						if (barlines[i] >= h) {
							heights[h] = rhs - i;	// set the distance between these bars.
							break;
						}
					}
					// If we found no greater barline yet, it's an error (we should have hit the start at 0 and finished.
					if (heights[h] == -1)
						throw new Exception("Logic error in barline distance computation");
				}

				return heights;
			} catch (Exception e) {
				MessageBox.Show(e.ToString(), "DEBUG");
				return null;
			}
		}


		public int maxPitchMidi {
			get {
				int max = -1;

				foreach (Measure m in measuresInput) {
					foreach (Note n in m.rhythm.notes) {
						if (n.Midi > max)
							max = n.Midi;
					}
				}
				return max;
			}
		}


		public object FindEquivalentAnalogy(Analogy target) {
			foreach (Analogy a in analogies) {
				if (a.LHS.MinLocation == target.LHS.MinLocation && a.LHS.MaxLocation == target.LHS.MaxLocation &&
					a.RHS.MinLocation == target.RHS.MinLocation && a.RHS.MaxLocation == target.RHS.MaxLocation)
					return a;
			}
			return null;
		}


		public Group FindEquivalentGroup(Group target, bool ignoreSubstructure) {
			foreach (Group g in groups) {
				if (g.MinLocation == target.MinLocation && g.MaxLocation == target.MaxLocation) {
					if (ignoreSubstructure)
						return g;
					else
						// Make sure all subgroups have correct start/end points.
						if (Group.VerifySameGroupStructures(g, target))
							return g;
				}
			}
			return null;
		}

		public void DropGroups(List<Group> groupsToDrop) {
			foreach (Group g in groupsToDrop) {
				if (groups.Contains(g))
					groups.Remove(g);
			}
		}

		public void DropAnalogies(List<Analogy> analogiesToDrop) {
			foreach (Analogy a in analogiesToDrop) {
				if (analogies.Contains(a))
					analogies.Remove(a);
			}
		}

		public void AddGroupAndSubgroupsNoChecks(GroupElement ge) {
			if (!(ge is Group))
				return;
			groups.Add((Group)ge);
			foreach (GroupElement ge2 in ((Group)ge).GroupElements)
				AddGroupAndSubgroupsNoChecks(ge2);

		}

		public void AddGroupsNoChecks(List<Group> groupsToAdd) {
			groups.AddRange(groupsToAdd);
		}

		public void AddAnalogiesNoChecks(List<Analogy> analogiesToAdd) {
			analogies.AddRange(analogiesToAdd);
		}

		public void DropAnalogyNoChecks(Analogy a) {
			analogies.Remove(a);
		}

		public void DropGroupAndSubgroupsNoChecks(GroupElement ge) {
			if (ge is Group) {
				groups.Remove((Group)ge);
				foreach (GroupElement ge2 in ((Group)ge).GroupElements) {
								DropGroupAndSubgroupsNoChecks(ge2);
				} 
			}
		}

		public void CompleteDropGroups(List<Group> groupsToDrop) {
			foreach (Group group in groupsToDrop) {
				foreach (Analogy a in analogies) {
					List<Relationship> toDestroyRelatinoships = new List<Relationship>();
					foreach (Relationship r in a.relationships) {
						if (r.LHS == group || r.RHS == group)
							toDestroyRelatinoships.Add(r);
					}
					foreach (Relationship r in toDestroyRelatinoships) {
						a.RemoveRelationship(r);
					}
				}
			}
		}

		public void CompleteDropAnalogies(List<Analogy> analogiesToDrop) {
			// TODO.

			// Maybe nothing to do!
			return;
		}

		public Relationship PickRandomRelationshipByWeaknessAndAge() {
			if (relationships.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Relationship relationship in relationships) {
				double recency = measures.Count - relationship.RHS.Location;

				double score = (100-relationship.Strength) * 1.0/ComputeRecencyScore(relationship);

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(relationship, score);
				pairs.Add(pair);
			}

			return (Relationship)Utilities.PickItemWeighted(pairs);
		}

		public Analogy PickRandomAnalogyByWeaknessAndAge() {
			if (analogies.Count == 0)
				return null;

			List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

			foreach (Analogy analogy in analogies) {
				double recency = measures.Count - analogy.RHS.Location;

				double score = (100 - analogy.Strength) * 1.0 / ComputeRecencyScore(analogy);

				Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(analogy, score);
				pairs.Add(pair);
			}

			return (Analogy)Utilities.PickItemWeighted(pairs);
		}


		public bool verifyConsistent() {
			
			// Check for duplicate groups.
			for (int i = 0; i < groups.Count; i++ ) {
				Group g = groups[i];
				for (int j = i + 1; j < groups.Count; j++) {
					Group g2 = groups[j];

					if (g.MinLocation == g2.MinLocation && g.MaxLocation == g2.MaxLocation)
						return false;
				}

			}
			return true;
		}

		public string GetGroupsString() {
			StringBuilder sb = new StringBuilder();

			foreach (Group g in groups) {
				sb.AppendLine(g.ToString());
			}

			return sb.ToString();
		}

		public GroupElement GetPreviousGroupElement(GroupElement e) {
			// Search for element at same level.
			// If not found, try lower levels, then higher levels.
			for (int i = e.Level; i >= 0; i--) {
				GroupElement result = SearchAtLevelForPreviousGroupElement(e, i);
				if (result != null)
					return result;
			}

			// Try searching 2 levels higher.
			for (int i = e.Level+1; i <= e.Level+2; i++) {
				GroupElement result = SearchAtLevelForPreviousGroupElement(e, i);
				if (result != null)
					return result;
			}

			return null;
		}

		private GroupElement SearchAtLevelForPreviousGroupElement(GroupElement e, int level) {
			GroupElement result = null;
			List<GroupElement> sameLevel = GetElementsAtLevel(level);
			foreach (GroupElement e2 in sameLevel) {
				if (e2.MaxLocation == e.MinLocation - 1)
					result = e2;
				break;
			}
			return result;
		}

		public MeasureLink FindMeasureLink(int measureIndex1, int measureIndex2) {
			foreach (MeasureLink link in measureLinks) {
				if ((link.m1.Location == measureIndex1 && link.m2.Location == measureIndex2) ||
					(link.m1.Location == measureIndex2 && link.m2.Location == measureIndex1))

					return link;
			}
			return null;
		}


    }
}
