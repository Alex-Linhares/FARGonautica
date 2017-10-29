using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Grouper", CodeletAttribute.CodeletWorkType.Create, 20, true)]
	public class MakeTranspositionGroupCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The group elements to examine. If none given, we select randomly.
		/// </summary>
        private RelationshipTransposition rt;


		public MakeTranspositionGroupCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Transposition Grouper", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which groups to examine. 
		/// Otherwise, it picks randomly.
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public MakeTranspositionGroupCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, RelationshipTransposition rt)
			: base("Transposition Grouper", urgency, parent, coderack, workspace, slipnet) {

			this.rt = rt;
		}



		public override void Run() {
		

            if (rt == null) {
                rt = workspace.PickRandomTranspositionRelationshipByRecencyAndStrength();
            }

            if (rt == null)
                return;

            if (!workspace.relationships.Contains(rt))
                return;

            // Verify adjacency.
            if (rt.LHS.MaxLocation + 1 != rt.RHS.MinLocation)
                return;

            // Add to attention history.
            workspace.RecordCodeletAttentionHistory(this, rt.LHS.MinLocation, rt.RHS.MaxLocation);

            // Check for existing group.
            foreach (Group g in workspace.groups) {
                if (g.GroupElements.Count == 2) {
                    if (g.GroupElements[0] == rt.LHS && g.GroupElements[1] == rt.RHS)
                        return;
                }
            }

    		double r = Utilities.rand.NextDouble() * 100;
            
			// Make tmp group.
            TemporaryGroup tmpG = new TemporaryGroup(workspace, rt.LHS, rt.RHS);
			float score = 75;

			tmpG.AddGroupReason(new GroupReasonNumberOfSubelements(tmpG, 75));
            tmpG.AddGroupReason(new GroupReasonComponentsIdentical(tmpG));


			// Group if the score is strong enough.
			if (r < score) {
				// Check for conflicts, and decide whether to kill conflicting.
				workspace.AddGroup(tmpG);
			}

		}
	}
}

