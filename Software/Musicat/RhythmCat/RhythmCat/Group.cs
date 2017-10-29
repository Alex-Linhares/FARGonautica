using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	public class Group : GroupElement {

		#region Members

		protected List<GroupElement> groupElements;
		protected List<GroupReason> reasons; // reasons I'm a group are listed here!
		protected List<GroupPenaltyReason> penaltyReasons; // reasons I'm a group are listed here!

		#endregion

		#region Properties

		public List<GroupElement> GroupElements {
			get { return groupElements; }
		}

		/// <summary>
		/// Location 0 = 1st measure. 1 = 2nd measure. etc.
		/// </summary>
		public override int Location {
			get {
				return MaxLocation;
			}
		}

		public override int LengthInMeasures {
			get {
				return MaxLocation - MinLocation + 1;
			}
		}

		public override int Level {
			get {
				int max = -1;
				foreach (GroupElement ge in groupElements) {
					if (ge.Level + 1 > max)
						max = ge.Level + 1;
				}
				return max;
			}
		}

		public override int MinLocation {
			get {
				// Return the minimum measure number of the children.
				int min = int.MaxValue;

				foreach (GroupElement ge in groupElements) {
					int m = ge.MinLocation;
					if (m < min) {
						min = m;
					}
				}

				return min;
			}
		}

		public override int MaxLocation {
			get {
				// Return the maximum measure number of the children.
				int max = int.MinValue;

				foreach (GroupElement ge in groupElements) {
					int m = ge.MaxLocation;
					if (m > max) {
						max = m;
					}
				}

				return max;
			}
		}


		public float midPoint {
			get {
				return ((float)MinLocation + MaxLocation) / 2.0f;
			}
		}

		public override int Count {
			get {
				return groupElements.Count;		// TODO: for now, measures have count=0; this means they have no children..
			}
		}


		public override List<Measure> Measures {
			get {
				List<Measure> measures = new List<Measure>();
				foreach (GroupElement ge in groupElements) {
					measures.AddRange(ge.Measures);
				}
				return measures;
			}
		}

		
		public List<GroupReason> Reasons {
			get {
				return reasons;
			}
		}

		public IEnumerable<GroupPenaltyReason> PenaltyReasons {
			get {
				return penaltyReasons;
			}
		}

		/// <summary>
		/// This is true if the group is too old to break.
		/// </summary>
		public bool IsPermanent {
			get {
				/*
				// Is the group earlier than the previous big barline? 
				int maxHeightIdx = GetIndexOfHighestBarlineAfterGroup();

				if (maxHeightIdx < 1)
					return false;

				// Get the period
				int period = ComputeRecentPeriod(maxHeightIdx);

				if (LengthInMeasures < period)
					return true;
				else
					return false;
				*/

				// The age at which a group become permanent is given by the MAX
				// of the given constant factor and the group's own length...
				// We might want to break a 6-measure long group after 5 more measures have passed, because
				// we might need to find a better competitor of similar size!
				return workspace.MaxLocation - MaxLocation >
					Math.Max(LengthInMeasures / 4, Constants.NUM_MEASURES_UNTIL_GROUPS_UNBREAKABLE);


				//return workspace.MaxLocation - MaxLocation > Constants.NUM_MEASURES_UNTIL_GROUPS_UNBREAKABLE;
			}
		}

		#endregion

		#region Constructors

		public Group(Workspace workspace)
			: base(workspace) {
			Init();
		}

		public Group(Workspace workspace, GroupElement ge1, GroupElement ge2)
			: base(workspace) {
			Init();
			AddGroupElement(ge1);
			AddGroupElement(ge2);
		}

		public Group(Workspace workspace, GroupElement ge)
			: base(workspace) {
			Init();
			AddGroupElement(ge);
		}

		/// <summary>
		/// Copy constructor; makes a deep copy of this group. Doesn't add to workspace; just copies it and all its subcomponents.
		/// </summary>
		/// <param name="g"></param>
		public Group(Group g)
			: base(g.workspace) {
			Init();

			//groupElements
			foreach (GroupElement ge in g.groupElements)
				groupElements.Add(ge.DeepCopy());

			foreach (GroupReason reason in g.reasons)
				reasons.Add(reason.DeepCopy(g));

			foreach (GroupPenaltyReason penalty in g.penaltyReasons)
				penaltyReasons.Add(penalty.DeepCopy(g));
		}

	

		#region Private Init

		private void Init() {
			groupElements = new List<GroupElement>();
			reasons = new List<GroupReason>();
			penaltyReasons = new List<GroupPenaltyReason>();
			this.hasChildren = true;
		}

		#endregion

		#endregion

		#region Public Functions

			#region Modify Group

		public void AddGroupElement(GroupElement ge) {
			ge.parentGroup = this;
			groupElements.Add(ge);
		}

		/// <summary>
		/// Adds the given reason-for-grouping to a group. Only one reason of a particular type is allowed.
		/// If the reason conflicts with a previous reason, the previous is removed and replaced with the new reason.
		/// TODO: consider removing lowest-scoring, and keeping only highest-scoring reason....
		/// 
		/// </summary>
		/// <param name="reason"></param>
		public void AddGroupReason(GroupReason reason) {
			GroupReason reasonToRemove = null;

			Type reasonType = reason.GetType();
			foreach (GroupReason r in reasons) {
				Type t2 = r.GetType();

				if (reasonType.IsAssignableFrom(t2)) {
					// Conflict found.
					reasonToRemove = r;
					break;
				}
			}
			if (reasonToRemove != null)
				reasons.Remove(reasonToRemove);

			reasons.Add(reason);
		}

		/// <summary>
		/// Adds the given penalty-for-grouping to a group. Only one penalty of a particular type is allowed.
		/// If the reason conflicts with a previous reason, the previous is removed and replaced with the new reason.
		/// TODO: consider removing lowest-scoring, and keeping only highest-scoring reason....
		/// 
		/// </summary>
		/// <param name="reason"></param>
		public void AddGroupPenaltyReason(GroupPenaltyReason reason) {
			GroupPenaltyReason reasonToRemove = null;

			Type reasonType = reason.GetType();
			foreach (GroupPenaltyReason r in penaltyReasons) {
				Type t2 = r.GetType();

				if (reasonType.IsAssignableFrom(t2)) {
					// Conflict found.
					reasonToRemove = r;
					break;
				}
			}
			if (reasonToRemove != null)
				penaltyReasons.Remove(reasonToRemove);

			penaltyReasons.Add(reason);
		}

		#endregion

			#region Compute Strength, Happiness etc

		public virtual double ComputeStrength() {
			double score = 0;

			// Add reasons.
			foreach (GroupReason reason in reasons) {
				score += reason.ComputeReasonScore();
			}
			// Subtract penalties
			foreach (GroupPenaltyReason penalty in penaltyReasons) {
				score -= penalty.ComputePenaltyScore();
			}


			// Add strength with age.
			score += GetAgeStrengthBonus();

			score = Utilities.SigmoidSquash(score);

			// Validate range.
			if (score < 0)
				score = 0;
			else if (score > 100)
				throw new ArgumentOutOfRangeException("score is out of range for group" + this.ToString());

			return score;
		}

		override public double ComputeHappiness(out double weight) {
			double strength = ComputeStrength();

			// Weight: weight higher for small groups being happy, lower for larger groups.
			weight = 1.0 / Level;

			return strength;
		}

		public double GetAgeStrengthBonus() {
			return 10 * (workspace.measures.Count - 1 - this.MaxLocation);
		}

		#endregion

			#region Range, Overlap, Containment, Subgroups, Group Structure

		public override bool MatchesRangeOf(GroupElement target) {
			if (!(target is Group))
				return false;

			return MinLocation == target.MinLocation && MaxLocation == target.MaxLocation;
		}

		public bool ContainsSubGroup(GroupElement subgroup) {
			if (subgroup == this)
				return false;
			return ContainsSubGroupRecur(this, subgroup);
		}

		private bool ContainsSubGroupRecur(GroupElement ge, GroupElement subgroup) {
			if (!(ge is Group))
				return false;
			foreach (GroupElement ge2 in ((Group)ge).GroupElements) {
				if (ge2 == subgroup)
					return true;
				if (ContainsSubGroupRecur(ge2, subgroup))
					return true;
			}
			return false;
		}


		/// <summary>
		/// Returns distance from end of leftmost group to start of rightmost group. May be 0 or negative, if overlapping.
		/// </summary>
		/// <param name="g"></param>
		/// <returns></returns>
		public int DistanceToGroup(Group g) {
			Group left, right;

			if (g.MinLocation < MinLocation) {
				left = g;
				right = this;
			} else {
				left = this;
				right = g;
			}
			return right.MinLocation - left.MaxLocation;
		}

		public static bool VerifySameGroupStructures(GroupElement ge1, GroupElement ge2) {
			// Verify same type.
			if ((ge1 is Group && !(ge2 is Group)) ||
				(!(ge1 is Group) && ge2 is Group))
				return false;
			// Measures.
			if (!(ge1 is Group)) {
				return ge1.Location == ge2.Location;
			}
			// Groups.
			Group g1 = (Group)ge1;
			Group g2 = (Group)ge2;
			if (g1.Count != g2.Count)
				return false;

			// Verify all elements
			for (int i = 0; i < g1.Count; i++) {
				if (!VerifySameGroupStructures(g1.GroupElements[i], g2.GroupElements[i]))
					return false;
			}

			return true;
		}


		/// <summary>
		/// Recursively gets all subgroups (skipping individual measures). 
		/// </summary>
		/// <returns></returns>
		public List<Group> GetAllSubgroups() {
			List<Group> subggroups = new List<Group>();

			foreach (GroupElement ge in groupElements) {
				if (ge is Group) {
					subggroups.Add((Group)ge);
					subggroups.AddRange(((Group)ge).GetAllSubgroups());
				}
			}
			return subggroups;
		}



		#endregion

			#region Deep Copy

		public override GroupElement DeepCopy() {
			Group g = new Group(this);
			return g;
		}

		#endregion

			#region Barline Heights

		private int GetIndexOfHighestBarlineAfterGroup() {
			int max = -1;
			int argmax = -1;
			for (int i = MaxLocation; i <= workspace.MaxLocation && i+1 < workspace.barlines.Count; i++) {
				int height = workspace.barlines[i + 1];
				if (height > max) {
					max = height;
					argmax = i;
				}
			}
			return argmax;
		}

		int ComputeRecentPeriod(int i) {
			int curHeight = workspace.barlines[i];
			int period = -1;
			for (int j = i - 1; j >= 0; j--)
				if (workspace.barlines[j] == curHeight)
					period = i - j;
			return period;
		}

		#endregion

			#region Larson Musical Forces

				#region Expectaton Vector

		// Returned list contains one value for each note, computed with simple single-level model
		public List<float> GetNoteExpectednessLarson(Key key, Alphabet alphabet) {
			List<NoteWithAttackPoint> notes = this.AllNotes;

			List<float> noteExpectednessLarson = new List<float>(notes.Count);

			Note n1 = null;
			Note n2 = null;
			for (int i = 0; i < notes.Count; i++) {
				Note n3 = notes[i];
				int G = 0, I = 0;
				float M = 0;

				// Require at least 2 notes for Gravity and Magnetism forces, all 3 required for Inertia.
                if (n2 != null) {
                    int n2MIDI = n2.Midi;
                    ScaleDegree sd2 = n2.GetScaleDegree(key);
                    if (sd2 != null) {
                        int diff2 = n2MIDI - n3.Midi;
                        // Gravity.

                        if (diff2 > 0 && (sd2 == null || !sd2.IsTonic))
                            G = 1;

                        // Magnetism.
                        // Is previous note a stable note? If so, no prediction.
                        if (!alphabet.isStable(sd2)) {
                            // Not stable. Compute the magnetism based on the distance in half-steps to the nearest goals from the 2nd pitch.

                            // Go up until we find a stable note.
                            int stableAboveMidi = -1;
                            int stableBelowMidi = -1;

                            for (int midi = n2MIDI + 1; ; midi++) {
                                ScaleDegree sdx = new Note(-1, false, false, new MidiInfo(midi, Accidental.Natural)).GetScaleDegree(key);
                                if (sdx != null) {
                                    if (alphabet.isStable(sdx)) {
                                        stableAboveMidi = midi;
                                        break;
                                    }
                                }
                            }
                            for (int midi = n2MIDI - 1; ; midi--) {
                                ScaleDegree sdx = new Note(-1, false, false, new MidiInfo(midi, Accidental.Natural)).GetScaleDegree(key);
                                if (sdx != null) {
                                    if (alphabet.isStable(sdx)) {
                                        stableBelowMidi = midi;
                                        break;
                                    }
                                }
                            }

                            int distAbove = stableAboveMidi - n2MIDI;
                            int distBelow = n2MIDI - stableBelowMidi;

                            float mag = 1.0f / (distAbove * distAbove) - 1.0f / (distBelow * distBelow);
                            float magMag = Math.Abs(mag); // magnitude of magnetism

                            // Test direction
                            if (diff2 * mag > 0)
                                // Going in opposite direction (because diff2 is positive when going down and mag is positive when going up.
                                M = -magMag;
                            else
                                // Going in same direction.
                                M = magMag;
                        }

                        // Inertia.
                        if (n1 != null) {
                            int diff1 = n1.Midi - n2.Midi;
                            if (diff1 != 0)
                                I = (diff1 * diff2 > 0) ? 1 : -1;
                        }
                    }
                }

				/// Compute force of expecting this note.
				float force = G * Constants.LARSON_WEIGHT_G + I * Constants.LARSON_WEIGHT_I + M * Constants.LARSON_WEIGHT_M;
				noteExpectednessLarson.Add(force);
				//throw new Exception("Force computed: " + force.ToString());

				n1 = n2;
				n2 = n3;
			}
			return noteExpectednessLarson;
		}

		#endregion


		#endregion
	  #endregion

		#region ToString

		public override string ToString() {
			StringBuilder sb = new StringBuilder();

			sb.AppendFormat("[{0}-{1}]\t#components: {2}", MinLocation, MaxLocation, groupElements.Count);
			if (IsPermanent)
				sb.AppendFormat("\tPERM");
			sb.AppendFormat("\tStrength: {0}", ComputeStrength());
			return sb.ToString();
		}
		#endregion
		
	}
}
