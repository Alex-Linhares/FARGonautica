using System;
using System.Collections.Generic;
using System.Text;


namespace RhythmCat {
	[Codelet("Breaker", CodeletAttribute.CodeletWorkType.Destroy, 20, true)]
	public class RelationshipBreakerCodelet : Codelet {

		//private const float ACTIVATION_INCREASE_FOR_FEATURE = 20;


		/// <summary>
		/// The relationship to examine. If none given, we select randomly.
		/// </summary>
		private Relationship relationship;


		public RelationshipBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Relationship Breaker", urgency, parent, coderack, workspace, slipnet) {

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
		public RelationshipBreakerCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet, Relationship relationship)
			: base("Relationship Breaker", urgency, parent, coderack, workspace, slipnet) {
				this.relationship = relationship;
		}

		public override void Run() {
			if (relationship == null) {
				relationship = workspace.PickRandomRelationshipByWeaknessAndAge();
			}

			if (relationship == null)
				return;

			// Add to attention history.
			workspace.RecordCodeletAttentionHistory(this, relationship.LHS.MinLocation);
			workspace.RecordCodeletAttentionHistory(this, relationship.LHS.MaxLocation);
			workspace.RecordCodeletAttentionHistory(this, relationship.RHS.MinLocation);
			workspace.RecordCodeletAttentionHistory(this, relationship.RHS.MaxLocation);

			
			// verify rel is unused.
			foreach (Analogy a in workspace.analogies) {
				if (a.relationships.Contains(relationship))
					return;
			}

			double r = Utilities.rand.NextDouble() * 100;

			if (r > relationship.Strength) {
				workspace.relationships.Remove(relationship);
			}
		}
	}
}
