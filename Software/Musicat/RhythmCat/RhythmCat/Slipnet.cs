using System;
using System.Collections.Generic;
using System.Text;
using System.IO;


namespace RhythmCat
{
    public class SlipnetEventArgs : EventArgs
    {
        public Slipnet slipnet;

        public SlipnetEventArgs(Slipnet slipnet)
        {
            this.slipnet = slipnet;
        }
    }

    public class Slipnet : IEnumerable<ConceptNode>
    {

        private Coderack coderack;
        private Workspace workspace;

        private const int DEFAULT_CONCEPTUAL_DEPTH = 50;
        private const int DEFAULT_ACTIVATION = 49;
        private const bool DISPLAY_DEBUG_INFO = false;

        private int nextID = 0;
        private int motifIdOffset;

        public int MotifIdOffset
        {
            get { return motifIdOffset; }
        }

        public event EventHandler<SlipnetEventArgs> Update;

        protected List<ConceptNode> nodes;

        public List<ConceptNode> Nodes
        {
            get { return nodes; }
            set { nodes = value; }
        }
        protected List<ConceptLink> links;

        public List<ConceptLink> Links
        {
            get { return links; }
            set { links = value; }
        }

        protected bool newNodesAdded;

        public bool NewNodesAdded
        {
            get { return newNodesAdded; }
            set { newNodesAdded = value; }
        }


        private Dictionary<string, ConceptNode> nodeNameDictionary;


        //Viz: http://www.codeproject.com/KB/miscctrl/quickgraph.aspx
        //GLEE: http://research.microsoft.com/research/downloads/Details/c927728f-8872-4826-80ee-ecb842d10371/Details.aspx


        public Slipnet(Coderack coderack, Workspace workspace)
        {
            this.coderack = coderack;
            this.workspace = workspace;

            Reset();

            Initialize();
        }

        /// <summary>
        /// Updates the slipnet, as in Mitchell Appendix B, pg. 72 and 254
        /// </summary>
        public void UpdateActivations(List<ConceptInstance> instances)
        {

            // 1. Apply all incoming activations from the previous 15 codelet runs & clear.
            foreach (ConceptNode node in nodes)
                node.ApplyIncomingActivation();

            // 2a. Spread activations. 
            foreach (ConceptNode node in nodes)
                node.SpreadActivation(node.Activation);
            // 2b. Apply all incoming activations from step 2a and clear.
            foreach (ConceptNode node in nodes)
                node.ApplyIncomingActivation();

            // 3. Activation decay based on conceptual depth.
            foreach (ConceptNode node in nodes)
            {
                node.DecayActivation(instances);
            }

            // 4a. Each node gets a chance to become fully active, which results in firing activation to neighbors.
            foreach (ConceptNode node in nodes)
            {
                // Probability of becoming fully active is activation/100, but only if at least 50% already.
                if (node.Activation >= 50)
                {
                    if (Utilities.rand.NextDouble() * 100 < node.Activation)
                    {
                        if (DISPLAY_DEBUG_INFO)
                            Console.WriteLine("Full Activation Spike: " + node.ToString());
                        node.Activation = 100;
                        node.SpreadActivation(node.Activation);

                        node.PostCodelets(coderack, workspace, this);
                    }
                }
            }

            // 4b. Apply activations spread from step 4a.
            foreach (ConceptNode node in nodes)
                node.ApplyIncomingActivation();

            // Raise the viewer update event.
            UpdateView();
        }

        /// <summary>
        /// Sets up the slipnet with the default nodes.
        /// </summary>
        public void Initialize()
        {
            /*// Make a node for each of the N abstract features.
            for (int i = 0; i < Constants.NUM_FEATURES; i++)
            {
                // Add a new node.
                ConceptNode node = new ConceptNode("Concept" + i.ToString(), i, DEFAULT_CONCEPTUAL_DEPTH, true, true, DEFAULT_ACTIVATION);
                AddNode(node);

            }
			*/

            // Record first Motif ID offset.
            motifIdOffset = nextID;

            // Set up the node dictionary.
            nodeNameDictionary = new Dictionary<string, ConceptNode>();
            foreach (ConceptNode n in nodes)
                nodeNameDictionary[n.Name] = n;
        }

        public void AddLink(string node1, string node2, ConceptLink.ConceptLinkType linkType, int strength, int elasticity)
        {
            // First search for nodes; add if they don't exist.
            ConceptNode n1, n2;

            n1 = FindNodeOrNull(node1);
            n2 = FindNodeOrNull(node2);

            if (n1 == null)
            {
                n1 = new ConceptNode(node1, getNewNodeID(), DEFAULT_CONCEPTUAL_DEPTH, false, true, DEFAULT_ACTIVATION);
                AddNode(n1);
            }
            if (n2 == null)
            {
                n2 = new ConceptNode(node2, getNewNodeID(), DEFAULT_CONCEPTUAL_DEPTH, false, true, DEFAULT_ACTIVATION);
                AddNode(n1);
            }

            // Make the new link.
            ConceptLink link = new ConceptLink(n1, n2, linkType, strength, elasticity);

            // Add the link to each node.
            n1.AddLink(link);
            n2.AddLink(link);

            // Keep the links list in sync too.
            links.Add(link);
        }

        public void AddNode(ConceptNode node)
        {
            nodes.Add(node);
            if (nodeNameDictionary != null)
                nodeNameDictionary[node.Name] = node;
            newNodesAdded = true;
        }

        public void RemoveNode(ConceptNode node)
        {
            nodes.Remove(node);
            if (nodeNameDictionary != null)
                nodeNameDictionary.Remove(node.Name);
            newNodesAdded = true;
        }

        /// <summary>
        /// Returns the given named node, or exception if not found.
        /// </summary>
        /// <param name="nodeName"></param>
        /// <returns></returns>
        public ConceptNode FindNode(string nodeName)
        {
            return nodeNameDictionary[nodeName];
        }

        /// <summary>
        /// Returns the given named node, or null if not found.
        /// </summary>
        /// <param name="nodeName"></param>
        /// <returns></returns>
        public ConceptNode FindNodeOrNull(string nodeName)
        {
            foreach (ConceptNode n in nodes)
                if (n.Name == nodeName)
                    return n;
            return null;
        }

        /// <summary>
        /// Returns the link from nodeA to nodeB. Note: does not return a link if specified in the reverse order.
        /// </summary>
        /// <param name="nodeA"></param>
        /// <param name="nodeB"></param>
        /// <returns></returns>
        public ConceptLink FindLink(string nodeA, string nodeB)
        {
            foreach (ConceptLink cl in this.links)
                if (cl.NodeA.Name == nodeA && cl.NodeB.Name == nodeB)
                    return cl;

            return null;
        }

        public void Reset()
        {
            nodes = new List<ConceptNode>(100);
            links = new List<ConceptLink>(100);

            nextID = 1;
        }

        public int getNewNodeID()
        {
            //bool found;
            //do {
            //    found == false;
            //    foreach (ConceptNode n in nodes)
            //        if (n.Id == nextID) {
            //            found == true;
            //            break;
            //        }
            //    // If we already used this ID, increment and try again.
            //    if (found == true)
            //        nextID++;
            //} while (found == true);

            // TODO: if reloading LTM from a file, we have to resent nextID.

            // For now, just use it and increment.
            return nextID++;
        }

        /// <summary>
        /// Raise an event to let viewers update their views.
        /// </summary>
        public void UpdateView()
        {
            // Verify we have subsubscribed event handlers.
            if (Update != null)
            {
                SlipnetEventArgs ea = new SlipnetEventArgs(this);
                Update(this, ea);
            }
        }

        #region IEnumerable<ConceptNode> Members

        public IEnumerator<ConceptNode> GetEnumerator()
        {
            return nodes.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #endregion

        internal int GetARandomConcept() {
            // Do roulette wheel selection on concepts based on activation.

            //return Utilities.rand.Next(nodes.Count);

            List<Utilities.ObjectValuePair> pairs = new List<Utilities.ObjectValuePair>();

            foreach (ConceptNode node in nodes) {
                Utilities.ObjectValuePair pair = new Utilities.ObjectValuePair(node, Math.Max(5, node.Activation));
				pairs.Add(pair);
            }

            return ((ConceptNode)Utilities.PickItemWeighted(pairs)).Id;

        }

        public override string ToString() {
            StringBuilder sb = new StringBuilder();

            foreach (ConceptNode node in nodes) {
                sb.AppendLine(node.ToString());
            }
            return sb.ToString();
        }
    }
}
