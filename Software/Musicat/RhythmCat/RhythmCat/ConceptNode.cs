using System;
using System.Collections.Generic;
using System.Text;

namespace RhythmCat
{
    public class ConceptNode : IComparable<ConceptNode>
    {

        protected const int DEFAULT_POSTED_CODELET_URGENCY = 30;

        /// <summary>
        /// A list of links of which this node is a member.
        /// </summary>
        private List<ConceptLink> links;

        /// <summary>
        /// Cache to avoid recomputing.
        /// </summary>
        private List<ConceptNode> _linkedNodes;

        /// <summary>
        /// Cache to avoid recomputing.
        /// </summary>
        private List<ConceptNode> _linkedOppositeNodes;

        /// <summary>
        /// The name of the concept node.
        /// </summary>
        private string name;

        public virtual string Name
        {
            get { return name; }
            set { name = value; }
        }


        private bool differenceBasedSimilarity;

        /// <summary>
        /// True for nodes where percived value needs to be compared to others with a difference. I.e. pitch height/frequency or tension-level.
        /// False for nodes were value indicates strength instead of position along a scale, such as "Tonic" or "GroupStart".
        /// </summary>
        public bool DifferenceBasedSimilarity
        {
            get { return differenceBasedSimilarity; }
            set { differenceBasedSimilarity = value; }
        }


        private bool useInSimilarity;

        /// <summary>
        /// True for nodes to be used in similiarty computations.
        /// False for nodes like Immediacy.
        /// </summary>
        public bool UseInSimilarity
        {
            get { return useInSimilarity; }
            set { useInSimilarity = value; }
        }




        public virtual void PostCodelets(Coderack coderack, Workspace workspace, Slipnet slipnet)
        {
        }

        /// <summary>
        /// ID # of this node.
        /// </summary>
        protected int id;

        public int Id
        {
            get { return id; }
        }


        /// <summary>
        /// Current activation of this node. range = 0 - 100. 0 = none, 100 = full.
        /// </summary>
        protected float activation;

        public float Activation
        {
            get { return activation; }
            set { activation = value; }
        }

        /// <summary>
        /// Incoming activation of this node; used to store it temporarily during a global slipnet update.
        /// </summary>
        private float incomingActivation;

        private int conceptualDepth;

        public int ConceptualDepth
        {
            get { return conceptualDepth; }
            set { conceptualDepth = value; }
        }

        public List<ConceptNode> GetLinkedNodes()
        {
            if (_linkedNodes != null)
                return _linkedNodes;

            _linkedNodes = new List<ConceptNode>();
            foreach (ConceptLink link in links)
            {
                if (link.NodeA == this)
                    _linkedNodes.Add(link.NodeB);
                else
                    _linkedNodes.Add(link.NodeA);
            }
            return _linkedNodes;
        }


        public List<ConceptNode> GetLinkedOppositeNodes()
        {
            if (_linkedOppositeNodes != null)
                return _linkedOppositeNodes;

            _linkedOppositeNodes = new List<ConceptNode>();
            foreach (ConceptLink link in links)
            {
                if (link.Opposite)
                {
                    if (link.NodeA == this)
                        _linkedOppositeNodes.Add(link.NodeB);
                    else
                        _linkedOppositeNodes.Add(link.NodeA);
                }
            }
            return _linkedOppositeNodes;
        }

        public ConceptNode()
        {
            throw new System.NotImplementedException();
        }

        public ConceptNode(string name, int id, int conceptualDepth, bool differenceBasedSimilarity, bool useInSimilarity, float activation)
        {
            this.id = id;
            this.conceptualDepth = conceptualDepth;
            this.activation = activation;
            this.differenceBasedSimilarity = differenceBasedSimilarity;
            this.useInSimilarity = useInSimilarity;

            if (name == null)
                this.name = this.Name;
            else
                this.name = name;


            links = new List<ConceptLink>();
        }

        public void AddLink(ConceptLink link)
        {
            links.Add(link);
        }

        public override string ToString()
        {
            return name + "(" + conceptualDepth + ") - " + activation.ToString("#.###");
        }

        /// <summary>
        /// Sends activation to all linked neighbors. 
        /// Implemented in such a way that cycles don't cause infinite loops: we pool the incoming
        /// activation for each node and then add it in without doing a 2nd round of updating in the same iteration.
        /// </summary>
        public void SpreadActivation(float amount)
        {
            foreach (ConceptLink link in links)
            {
                link.SpreadActivation(this, amount);
            }
        }

        /// <summary>
        /// Decay: remove (100-conceptual depth) percent of current activation.
        /// </summary>
        public void DecayActivation(List<ConceptInstance> instances)
        {
            // Find the total strength of instances of this node.

            double total = 0;
            double maxStr = Double.NegativeInfinity;
            int numInstanceObjects = 0;
            foreach (ConceptInstance ci in instances)
            {
                if (ci.node == this)
                {
                    numInstanceObjects++;
                    total += ci.strength;

                    if (ci.strength > maxStr)
                        maxStr = ci.strength;
                }
            }
            double avgStr = total / numInstanceObjects;

            double minMultiplier = Math.Max(maxStr / 100, conceptualDepth / 100.0f);

            //            activation *= conceptualDepth/100.0f;

            // Keep miltiplier < 100%.
            if (minMultiplier > 0.95)
                minMultiplier = 0.95;

            // Decay.
            activation *= (float)minMultiplier;

            // keep activation >= avg strength.
            if (activation < avgStr && avgStr < 0.90)
                activation = (float)avgStr;
        }

        public void IncreaseIncomingActivation(float amount)
        {
            incomingActivation += amount;
        }

        /// <summary>
        /// Apply the incoming activation and then clear the incoming buffer.
        /// </summary>
        public void ApplyIncomingActivation()
        {
            IncreaseActivation(incomingActivation);
            incomingActivation = 0;
        }

        public void IncreaseActivation(float amount)
        {
            activation += amount;
            if (activation > 100) activation = 100;
            if (activation < 0) activation = 0;
        }


        #region IComparable<ConceptNode> Members

        public int CompareTo(ConceptNode other)
        {
            if (activation > other.activation)
                return -1;
            else if (activation < other.activation)
                return 1;
            return 0;
        }

        #endregion
    }
}
