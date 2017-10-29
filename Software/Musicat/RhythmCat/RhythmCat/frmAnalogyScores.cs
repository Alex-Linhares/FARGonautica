using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace RhythmCat {
	public partial class AnalogyScoresForm : Form {

		private bool updating;

		public class ViewGroupEventArgs : EventArgs {
			public Analogy a;

			public ViewGroupEventArgs(Analogy a) {
				this.a = a;
			}
		}


		//public event EventHandler<ViewGroupEventArgs> viewGroupEvent;


		private Workspace workspace;
		private List<ButtonAndAnalogy> buttonData;


		public AnalogyScoresForm(Workspace workspace) {
			updating = false;
			this.workspace = workspace;
			InitializeComponent();
		}

		private void DrawAnalogies(bool force = false) {
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
						foreach (ButtonAndAnalogy bag in buttonData) {
							this.Controls.Remove(bag.button);
						}
					}

					buttonData = new List<ButtonAndAnalogy>();

					int x = 0; int y = 0;

					// Sort analogies by start time.
					List<Analogy> sorted = new List<Analogy>(workspace.analogies);
					sorted.Sort((a1, a2) => {
						if (a1.MinLocation > a2.MinLocation)
							return 1;
						else if (a1.MinLocation < a2.MinLocation)
							return -1;
						else if (a1.MaxLocation > a2.MaxLocation)
							return 1;
						else if (a1.MaxLocation < a2.MaxLocation)
							return -1;
						return 0;
					});

					foreach (Analogy a in workspace.analogies) {

						// Make the text for the button
						string label = AnalogyString(a, false);

						// Add a button
						Button b = new Button();
						b.Left = x;
						b.Width = 200;
						b.Top = y;
						b.Text = label;
						b.Click += new EventHandler(b_Click);
						this.Controls.Add(b);



						buttonData.Add(new ButtonAndAnalogy(b, a));

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
				//MessageBox.Show("Exception in frmAnalogyScroes!: " + e.ToString());
				Console.WriteLine("Exception in frmAnalogyScroes!: " + e.ToString());
			}

		}

		private static string AnalogyString(Analogy a, bool verbose) {
			StringBuilder sb = new StringBuilder();
			/*sb.Append("m.");
			sb.Append(g.MinLocation + 1);
			sb.Append('-');
			sb.Append(g.MaxLocation + 1);

			sb.Append(" (");
			foreach (Measure m in g.Measures) {
				sb.Append(m.FormLabel);
			}
			sb.Append(")");*/

			sb.Append(a.ToString());



			//sb.Append(" -- ");
			//if (verbose)
			//	sb.Append("Score: ");
			//sb.Append(g.ComputeStrength());
			return sb.ToString();
		}

		void b_Click(object sender, EventArgs e) {
			// Find this button's data.
			foreach (ButtonAndAnalogy bag in buttonData) {
				if (sender == bag.button) {
					// List analogy reasons.
					StringBuilder sb = new StringBuilder();
					sb.Append("Analogy: ");
					sb.AppendLine(AnalogyString(bag.analogy, true));
					sb.AppendLine();
					sb.AppendLine("Scoring:");
					sb.Append("Size bonus: TODO");
					//sb.AppendLine(bag.analogy.GetAgeStrengthBonus().ToString());
					sb.AppendLine();
					//foreach (GroupReason r in bag.analogy.Reasons) {
					//	sb.AppendLine(r.ToString());
					//}
					
					//TOOD: draw all scoring stuff.
					sb.AppendLine("# Unmapped components: " + bag.analogy.NumUnmappedComponents.ToString());
					sb.AppendLine("# relationships: " + bag.analogy.relationships.Count);
					sb.AppendLine("sum relationship strengths: " + bag.analogy.SumRelationshipStrength.ToString());

					// Draw relationships
					sb.AppendLine("\nRelationships:");
					foreach (Relationship r in bag.analogy.relationships) {
						sb.AppendLine(r.ToString());
					}

					MessageBox.Show(sb.ToString());
				}
			}
		}


		private void frmAnalogyScores_Paint(object sender, PaintEventArgs e) {
			DrawAnalogies();
		}

		public struct ButtonAndAnalogy {
			public Button button;
			public Analogy analogy;

			public ButtonAndAnalogy(Button button, Analogy analogy) {
				this.button = button;
				this.analogy = analogy;
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
			DrawAnalogies(true);
		}

		private void AnalogyScoresForm_Load(object sender, EventArgs e) {
			this.Size = Settings.Default.groupScoresSize;
			this.Location = Settings.Default.groupScoresLoc;
		}

		private void AnalogyScoresForm_FormClosing(object sender, FormClosingEventArgs e) {
			Settings.Default.groupScoresSize = this.Size;
			Settings.Default.groupScoresLoc = this.Location;

			Settings.Default.Save();
		}

	}
}
