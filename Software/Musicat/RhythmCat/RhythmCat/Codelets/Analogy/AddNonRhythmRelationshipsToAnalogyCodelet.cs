using System;
using System.Collections.Generic;
using System.Text;
using RhythmCat.Codelets;


namespace RhythmCat {
	[Codelet("Scout", CodeletAttribute.CodeletWorkType.Examine, 30, true)]
	public class AddNonRhythmRelationshipsToAnalogyCodelet : Codelet {


		/// <summary>
		/// The analogy to examine. If none given, we select randomly.
		/// </summary>
		private Analogy analogy;

		public AddNonRhythmRelationshipsToAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet)
			: base("Add Nonrhythm Relationships To Analogy", urgency, parent, coderack, workspace, slipnet) {

		}

		/// <summary>
		/// Use this constructer to tell the codelet which group to examine. 
		/// Otherwise, it picks one randoml.y
		/// </summary>
		/// <param name="urgency"></param>
		/// <param name="parent"></param>
		/// <param name="coderack"></param>
		/// <param name="workspace"></param>
		/// <param name="slipnet"></param>
		/// <param name="notes"></param>
		public AddNonRhythmRelationshipsToAnalogyCodelet(int urgency, Codelet parent, Coderack coderack, Workspace workspace, Slipnet slipnet,
			Analogy analogy)
			: base("Add Nonrhythm Relationships To Analogy", urgency, parent, coderack, workspace, slipnet) {
			this.analogy = analogy;
		}

		public override void Run() {
			if (analogy == null) {
				analogy = workspace.PickRandomAnalogyByRecency();
			}

			if (analogy == null)
				return;

			if (!workspace.analogies.Contains(analogy))
				return;

			if (!(analogy.LHS is Group && analogy.RHS is Group))
				return;

			List<Group> lhsGroups = ((Group)analogy.LHS).GetAllSubgroups();
			lhsGroups.Add((Group)analogy.LHS);
			List<Group> rhsGroups = ((Group)analogy.RHS).GetAllSubgroups();
			rhsGroups.Add((Group)analogy.RHS);


			// Look for relationships and try to add to analogy. 
			foreach (Relationship r in workspace.relationships) {
				// Skip rhythm-type relationships.
				if (r.RhythmType)
					continue;

				if (!(r.LHS is Group && r.RHS is Group))
					continue;
				if (lhsGroups.Contains((Group)r.LHS) && rhsGroups.Contains((Group)r.RHS)) {
					analogy.TryToAddRelationship(r);

					// Add to attention history.
					workspace.RecordCodeletAttentionHistory(this, r.LHS.MinLocation, r.LHS.MaxLocation);
					workspace.RecordCodeletAttentionHistory(this, r.RHS.MinLocation, r.RHS.MaxLocation);
				}
			}
		}
	}
}
