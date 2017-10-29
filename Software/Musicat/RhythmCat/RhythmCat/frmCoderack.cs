using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace RhythmCat {
	public partial class CoderackForm : Form {

		/// <summary>
		/// Tracks all teh codelet types and urgencies. This also lets us get the list of all codelets
		/// that have ever been active, even if some are missing now, so that the graph is consistent.
		/// </summary>
		private SortedDictionary<string, double> codeletUrgencies;

		private Coderack coderack;

		private bool updating;

		public bool Updating {
			get { return updating; }
		}

		public class ViewCodeletEventArgs : EventArgs {
			public string codeletName;

			public ViewCodeletEventArgs(string codeletName) {
				this.codeletName = codeletName;
			}
		}


		//public event EventHandler<ViewCodeletEventArgs> viewCodeletEvent;


		//private List<ButtonAndGroup> buttonData;


		public CoderackForm(Coderack coderack) {
			updating = false;
			this.coderack = coderack;
			this.codeletUrgencies = new SortedDictionary<string, double>();
			InitializeComponent();
		}

		private void DrawCodelets(Graphics gr, bool force = false) {

			try {
				// Add buttons to the form for each codelet type.
				if (coderack == null)
					return;

				if (!force) {
					if (updating)
						return;

				}
				lock (this) {
					updating = true;
					/*
					if (buttonData != null) {
						foreach (ButtonAndGroup bag in buttonData) {
							this.Controls.Remove(bag.button);
						}
					}

					buttonData = new List<ButtonAndGroup>();
					*/

					int y = 0;

					//Compute urgency data to graph.
					// Clear out urgency sums.
					List<string> names = new List<string>(codeletUrgencies.Keys);
					foreach (string name in names)
						codeletUrgencies[name] = 0;
					double totalUrgency = 0;

					// Iterate over all active codelet names and tally urgencies.
					lock (coderack) {
						foreach (Codelet c in coderack) {
							totalUrgency += c.Urgency;
							if (codeletUrgencies.ContainsKey(c.Name))
								codeletUrgencies[c.Name] += c.Urgency;
							else
								codeletUrgencies[c.Name] = c.Urgency;
						}
					}

					// Draw each codelet type name and urgency value + bar.
					int xBar = 200;
					int xBuffer = 10;
					int yBuffer = 2;
					foreach (KeyValuePair<string, double> codeletUrgency in codeletUrgencies) {
						gr.DrawString(codeletUrgency.Key, SystemFonts.DefaultFont, Brushes.Black, xBuffer, y);

						gr.FillRectangle(Brushes.BlueViolet, xBar, y + yBuffer, (int)((this.Width - xBar + xBuffer) * codeletUrgency.Value / totalUrgency), SystemFonts.DefaultFont.Height - yBuffer*2);

						// Spacing.
						y += SystemFonts.DefaultFont.Height;
					}



					
				}
				updating = false;
			} catch (Exception e) {
				//MessageBox.Show("Exception in frmGroupScroes!: " + e.ToString());
				Console.WriteLine("Exception in frmCoderack!: " + e.ToString());
			}

		}


/*		public struct ButtonAndGroup {
			public Button button;
			public Group group;

			public ButtonAndGroup(Button button, Group group) {
				this.button = button;
				this.group = group;
			}
		}
		*/
		private delegate void DrawDelegate();

		public void Draw() {
			if (updating)
				return;

			if (this.InvokeRequired)
				this.Invoke(new DrawDelegate(Draw));
			else
				this.Refresh();
		}

		private void CoderackForm_Paint(object sender, PaintEventArgs e) {
			DrawCodelets(e.Graphics);
		}

		private void btnRefresh_Click_1(object sender, EventArgs e) {
			this.Refresh();
		}

		private void CoderackForm_Load(object sender, EventArgs e) {
			this.Size = Settings.Default.coderackSize;
			this.Location = Settings.Default.coderackLoc;
		}

		private void CoderackForm_FormClosing(object sender, FormClosingEventArgs e) {
			Settings.Default.coderackSize = this.Size;
			Settings.Default.coderackLoc = this.Location;

			Settings.Default.Save();
		}
	}
}
