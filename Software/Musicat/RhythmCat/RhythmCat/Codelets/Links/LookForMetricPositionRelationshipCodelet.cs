using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
    [Codelet("Linker", CodeletAttribute.CodeletWorkType.Create, 20, true)]
    public class LookForMetricPositionRelationshipCodelet : Codelet {

        private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


        /// <summary>
        /// The group elements to examine. If none given, we select randomly.
        /// </summary>
        private Group g1;
        private Group g2;


        public LookForMetricPositionRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
            : base("Look for Mertric Position Relationship", urgency, parent, coderack, workspace, slipnet) {

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
        public LookForMetricPositionRelationshipCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
            Group g1, Group g2)
            : base("Look for Mertric Position Relationship", urgency, parent, coderack, workspace, slipnet) {
            this.g1 = g1;
            this.g2 = g2;
        }

        public override void Run() {
            if (g1 == null) {
                g1 = workspace.PickRandomGroupByRecency();
            }

            if (g1 == null)
                return;

            if (g2 == null) {
                g2 = workspace.PickRandomGroupByRecency();
            }

            if (g2 == null || g2 == g1)
                return;

            // Reorder in time if out-of-order. m1 comes first.
            if (g1.Location > g2.Location) {
                Group tmp = g1;
                g1 = g2;
                g2 = tmp;
            }

            // Make sure groups are non-overlapping.
            if (g1.MaxLocation >= g2.MinLocation)
                return;

            // Add to attention history.
            workspace.RecordCodeletAttentionHistory(this, g1.MinLocation, g1.MaxLocation);
            workspace.RecordCodeletAttentionHistory(this, g2.MinLocation, g2.MaxLocation);

            // Compute strength, based on the similarity of surrounding barlink thicknesses.

            // Find whether first group is up against a strong barline on the left or the right.
            int barlineStrBefore1, barlineStrAfter1, barlineStrBefore2, barlineStrAfter2;
            bool strongBefore1, strongBefore2;

            if (g2.MaxLocation + 1 >= workspace.barlines.Count)
                return;

            barlineStrBefore1 = workspace.barlines[g1.MinLocation];
            barlineStrAfter1 = workspace.barlines[g1.MaxLocation+1];
            barlineStrBefore2 = workspace.barlines[g2.MinLocation];
            barlineStrAfter2 = workspace.barlines[g2.MaxLocation + 1];

            strongBefore1 = (barlineStrBefore1 > barlineStrAfter1);
            strongBefore2 = (barlineStrBefore2 > barlineStrAfter2);

            // Must match in butting-up against strong left or right.
            if (strongBefore1 != strongBefore2)
                return;

          
            int magnitude;
            int dist;

            if (strongBefore1) {
                magnitude = Math.Min(barlineStrBefore1, barlineStrBefore2);
                dist = Math.Abs(barlineStrBefore1 - barlineStrBefore2) - 1; // allow 1  different for free
            } else {
                magnitude = Math.Min(barlineStrAfter1, barlineStrAfter2);
                dist = Math.Abs(barlineStrAfter1 - barlineStrAfter2) - 1;
            }
            if (dist < 0)
                dist = 0;

            // Max magnitude = 5.

            float strength = Math.Min((100.0f * (magnitude - dist)) / (Constants.BARLINE_MAX_HEIGHT-1), 100);
            double r = Utilities.rand.NextDouble() * 100;

            if (r < strength) {
                workspace.AddRelationship(new RelationshipMetricPosition(g1, g2, strength));
            }

        }
    }
}
