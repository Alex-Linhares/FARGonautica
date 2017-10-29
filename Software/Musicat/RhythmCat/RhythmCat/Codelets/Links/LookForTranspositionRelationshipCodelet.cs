using System;
using System.Collections.Generic;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
    [Codelet("Linker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
    public class LookForTranspositionRelationshipCodelet : Codelet {

        private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;
        private const int NUM_SCALE_DEGREES = 7;

        /// <summary>
        /// The relationship to examine. If none given, we select randomly.
        /// Used as a starting point to look for a transposition relationship (a more specific type of contour relationship)
        /// </summary>
        private RelationshipMelodyContour cr;
 

        public LookForTranspositionRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
            : base("Look for Transposition Relationship", urgency, parent, coderack, workspace, slipnet) {

        }

        /// <summary>
        /// Use this constructer to tell the codelet which measure to examine. 
        /// Otherwise, it picks one randomly.
        /// </summary>
        /// <param name="urgency"></param>
        /// <param name="parent"></param>
        /// <param name="coderack"></param>
        /// <param name="workspace"></param>
        /// <param name="slipnet"></param>
        /// <param name="notes"></param>
        public LookForTranspositionRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
            RelationshipMelodyContour cr)
            : base("Look for Transposition Relationship", urgency, parent, coderack, workspace, slipnet) {
            this.cr = cr;
        }

        public override void Run() {
            if (cr == null) {
                cr = workspace.PickRandomContourRelationshipByRecencyAndStrength();
            }

            if (cr == null)
                return;

            if (!workspace.relationships.Contains(cr))
                return;
          
            // Add to attention history.
            workspace.RecordCodeletAttentionHistory(this, cr.LHS.MinLocation, cr.LHS.MaxLocation);
            workspace.RecordCodeletAttentionHistory(this, cr.RHS.MinLocation, cr.RHS.MaxLocation);

            // We have a contour relationship. Check for same # notes.
            GroupElement ge1 = cr.LHS;
            GroupElement ge2 = cr.RHS;

            if (ge1.AllNotes.Count != ge2.AllNotes.Count)
                return;

            // Ignore the trivial transposition of 0 or 1 note.
            if (ge1.AllNotes.Count < 2)
                return;

            // Get PCs in C.
            Key k = new Key();
            List<ScaleDegree> sds1 = GetScaleDegrees(ge1.AllNotes, k);
            List<ScaleDegree> sds2 = GetScaleDegrees(ge2.AllNotes, k);

            // Calculate transposition.
            int tranposition = 0;
            for (int i = 0; i < sds1.Count; i++) {
                ScaleDegree sd1 = sds1[i];
                ScaleDegree sd2 = sds2[i];
                
                // skip rests if they are both rests.
                if (sd1 == null && sd2 == null)
                    continue;
                // if just one is a rest, cancel.
                if (sd1 == null || sd2 == null)
                    return;
                tranposition = sds2[i].Number - sds1[i].Number;
                break;
            }
 

            if (tranposition < 0)
                tranposition += NUM_SCALE_DEGREES;

            for (int i = 1; i < sds1.Count; i++) {
                ScaleDegree sd1 = sds1[i];
                ScaleDegree sd2 = sds2[i];

                // skip rests if they are both rests.
                if (sd1 == null && sd2 == null)
                        continue;
                // if just one is a rest, cancel.
                if (sd1 == null || sd2 == null) {
                        return;
                }

                int diff = sd2.Number - sd1.Number;
                if (diff < 0)
                    diff += NUM_SCALE_DEGREES;

                if (diff != tranposition)
                    return;
            }

            workspace.AddRelationship(new RelationshipTransposition(ge1, ge2, 100));
        }

        protected List<ScaleDegree> GetScaleDegrees(List<NoteWithAttackPoint> notes, Key k) {
            List<ScaleDegree> sds = new List<ScaleDegree>();

            foreach (NoteWithAttackPoint n in notes)
                sds.Add(n.GetScaleDegree(k));

            return sds;
        }
    }
}
