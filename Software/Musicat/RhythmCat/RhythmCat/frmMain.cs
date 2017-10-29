using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace RhythmCat {


	public partial class MainForm : Form {

		public event EventHandler<EventArgs> closeEvent;


		public MainForm() {
			InitializeComponent();
		}


		private delegate void SetTextDelegate(string s);
		private delegate void DrawWorkspaceDelegate(Workspace w);

		public void SetText(string s) {
			if (this.labelTxt.InvokeRequired) {
				this.labelTxt.Invoke(new SetTextDelegate(SetText), s);
			} else {
				labelTxt.Text = s;
			}
		}

		public void DrawWorkspace(Workspace w) {
			if (this.labelTxt.InvokeRequired) {
				this.labelTxt.Invoke(new DrawWorkspaceDelegate(DrawWorkspace), w);
			} else {
				labelTxt.Text = w.ToString();
			}

		}

		private void MainForm_Paint(object sender, PaintEventArgs e) {

		}

		private void MainForm_FormClosing(object sender, FormClosingEventArgs e) {
			if (closeEvent != null) {
				closeEvent(this, new EventArgs());
			}
			Settings.Default.mainSize = this.Size;
			Settings.Default.mainLoc = this.Location;

			Settings.Default.Save();
		}

		private void MainForm_Load(object sender, EventArgs e) {
			this.Size = Settings.Default.mainSize;
			this.Location = Settings.Default.mainLoc;
		}
	}
}
