using System;
using System.Collections.Generic;
using System.Text;

namespace RhythmCat
{
    public class ConceptLink
    {

        public enum ConceptLinkType
        {
            Normal,
            Opposite
        }

        private ConceptNode nodeA;

        public ConceptNode NodeA
        {
            get { return nodeA; }
            set { nodeA = value; }
        }
        private ConceptNode nodeB;

        public ConceptNode NodeB
        {
            get { return nodeB; }
            set { nodeB = value; }
        }

        private int strength;

        /// <summary>
        /// From 0(weakest/nonexistant) to 100 (perfect connection).
        /// </summary>
        public int Strength
        {
            get { return strength; }
            set { strength = value; }
        }
        private int elasticity;

        public int Elasticity
        {
            get { return elasticity; }
            set { elasticity = value; }
        }

        private bool opposite;

        public bool Opposite
        {
            get { return opposite; }
            set { opposite = value; }
        }

        public ConceptLink(ConceptNode node1, ConceptNode node2, ConceptLinkType linkType, int strength, int elasticity)
        {
            this.nodeA = node1;
            this.nodeB = node2;
            this.strength = strength;
            this.elasticity = elasticity;
            opposite = (linkType == ConceptLinkType.Opposite);
        }

        public void SpreadActivation(ConceptNode src, float amount)
        {
            ConceptNode dest = GetDestinationNode(src);

            int opp = (opposite) ? -1 : 1;  // multiple activation by -1 if it's an opposite link

            dest.IncreaseIncomingActivation(opp * amount * strength / 100.0f);  // Strength = 100-distance (in Melanie's terms)
        }

        public override string ToString()
        {
            string typeStr = (opposite) ? "<. . .>" : "<-->";
            return nodeA.Name + typeStr + nodeB.Name + "(" + strength + ", " + elasticity + ")";
        }

        /// <summary>
        /// Finds the other end of the link, given one end node.
        /// </summary>
        /// <param name="src"></param>
        /// <returns></returns>
        public ConceptNode GetDestinationNode(ConceptNode src)
        {
            if (nodeA == src)
                return nodeB;
            if (nodeB == src)
                return nodeA;
            throw new ArgumentException("Src node not part of the link");
        }
    }
}
