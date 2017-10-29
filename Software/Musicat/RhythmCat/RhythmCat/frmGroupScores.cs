using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace RhythmCat {
	public partial class GroupScoresForm : Form {

		private bool updating;

		public class ViewGroupEventArgs : EventArgs {
			public Group g;

			public ViewGroupEventArgs(Group g) {
				this.g = g;
			}
		}


		//public event EventHandler<ViewGroupEventArgs> viewGroupEvent;
		

		private Workspace workspace;
		private List<ButtonAndGroup> buttonData;


		public GroupScoresForm(Workspace workspace) {
			updating = false;
			this.workspace = workspace;
			InitializeComponent();
		}

		private void DrawGroups(bool force = false) {
			try {
				// Add buttons to the form for each codelet type.
				if (!force) {
					if (updating)
						return;

					// Only redraw every 10 codelets.
					if ((workspace != null) && (workspace.CurrentTime % 45 != 0 || workspace.CurrentTime >= workspace.MaxPossibleTime))
						return;
				}
				lock (this) {
					updating = true;

					if (buttonData != null) {
						foreach (ButtonAndGroup bag in buttonData) {
							this.Controls.Remove(bag.button);
						}
					}

					buttonData = new List<ButtonAndGroup>();

					int x = 0; int y = 0;

					// Sort groups by start time.
					List<Group> sorted = new List<Group>(workspace.groups);
					sorted.Sort((g1, g2) => {
						if (g1.MinLocation > g2.MinLocation)
							return 1;
						else if (g1.MinLocation < g2.MinLocation)
							return -1;
						else if (g1.MaxLocation > g2.MaxLocation)
							return 1;
						else if (g1.MaxLocation < g2.MaxLocation)
							return -1;
						return 0;
					});

					foreach (Group g in workspace.groups) {

						// Make the text for the button
						string label = GroupString(g, false);

						// Add a button
						Button b = new Button();
						b.Left = x;
						b.Width = 200;
						b.Top = y;
						b.Text = label;
						b.Click += new EventHandler(b_Click);
						this.Controls.Add(b);



						buttonData.Add(new ButtonAndGroup(b, g));

						// Spacing.
						y += b.Height + 12;
						// Go down. If too far, go right and back to top.
						if (y + b.Height + 12 > this.Height) {
							y = 0;
							x += b.Width + 6;
						}

					}
					updating = false;
				}
			} catch (Exception e) {
				//MessageBox.Show("Exception in frmGroupScroes!: " + e.ToString());
				Console.WriteLine("Exception in frmGroupScroes!: " + e.ToString());
			}
		
		}

		private static string GroupString(Group g, bool verbose) {
			StringBuilder sb = new StringBuilder();
			sb.Append("m.");
			sb.Append(g.MinLocation+1);
			sb.Append('-');
			sb.Append(g.MaxLocation+1);
			
			sb.Append(" (");
			foreach (Measure m in g.Measures) {
				sb.Append(m.FormLabel);
			}
			sb.Append(")");
			sb.Append(" -- ");
			if (verbose)
				sb.Append("Score: ");
			sb.Append(g.ComputeStrength());
			string label = sb.ToString();
			return label;
		}

		void b_Click(object sender, EventArgs e) {
			// Find this button's data.
			foreach (ButtonAndGroup bag in buttonData) {
				if (sender == bag.button) {
					// List group reasons.
					StringBuilder sb = new StringBuilder();
					sb.Append("Group: ");
					sb.AppendLine(GroupString(bag.group, true));
					sb.AppendLine();
					sb.AppendLine("Scoring:");
					sb.Append("Age bonus: ");
					sb.AppendLine(bag.group.GetAgeStrengthBonus().ToString());
					sb.AppendLine();
					foreach (GroupReason r in bag.group.Reasons) {
						sb.AppendLine(r.ToString());
					}
					sb.AppendLine("Penalties:");
					foreach (GroupPenaltyReason p in bag.group.PenaltyReasons) {
						sb.AppendLine(p.ToString());
					}
					sb.AppendLine();
					sb.Append("# subcomponents: ");
					sb.AppendLine(bag.group.GroupElements.Count.ToString());

					for (int i = 0; i < bag.group.GroupElements.Count; i++) {
						GroupElement ge = bag.group.GroupElements[i];
						sb.Append("Component #");
						sb.Append(i+1);
						sb.Append(": ");
						if (ge is Group)
							sb.AppendLine(GroupString((Group)ge, false));
						else {
							sb.Append("m.");
							sb.AppendLine((ge.Location+1).ToString());
						}
						//sb.AppendLine();
					}

					MessageBox.Show(sb.ToString());
				}
			}
		}


		private void frmGroupScores_Paint(object sender, PaintEventArgs e) {
			DrawGroups();
		}

		public struct ButtonAndGroup {
			public Button button;
			public Group group;

			public ButtonAndGroup(Button button, Group group) {
				this.button = button;
				this.group = group;
			}
		}

		private delegate void DrawDelegate();

		public void Draw() {
			if (updating)
				return;

			if (this.InvokeRequired)
				this.Invoke(new DrawDelegate(Draw));
			else
				this.Refresh();
		}

		private void btnRefresh_Click(object sender, EventArgs e) {
			DrawGroups(true);
		}

		private void GroupScoresForm_Load(object sender, EventArgs e) {
			this.Size = Settings.Default.groupScoresSize;
			this.Location = Settings.Default.groupScoresLoc;
		}

		private void GroupScoresForm_FormClosing(object sender, FormClosingEventArgs e) {
			Settings.Default.groupScoresSize = this.Size;
			Settings.Default.groupScoresLoc = this.Location;

			Settings.Default.Save();
		}

	}
}
