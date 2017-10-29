using System;
using System.Collections.Generic;
using System.Text;

namespace RhythmCat
{
    public struct ConceptInstance
    {
        public ConceptNode node;
        public double strength;

        public ConceptInstance(ConceptNode node, double strength)
        {
            this.node = node;
            this.strength = strength;
        }
    }
}
