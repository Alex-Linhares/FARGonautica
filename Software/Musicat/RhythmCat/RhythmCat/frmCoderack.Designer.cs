namespace RhythmCat {
	partial class CoderackForm {
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing) {
			if (disposing && (components != null)) {
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent() {
			this.btnRefresh = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// btnRefresh
			// 
			this.btnRefresh.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnRefresh.Location = new System.Drawing.Point(303, 572);
			this.btnRefresh.Name = "btnRefresh";
			this.btnRefresh.Size = new System.Drawing.Size(75, 23);
			this.btnRefresh.TabIndex = 1;
			this.btnRefresh.Text = "Refresh";
			this.btnRefresh.UseVisualStyleBackColor = true;
			this.btnRefresh.Click += new System.EventHandler(this.btnRefresh_Click_1);
			// 
			// CoderackForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(390, 607);
			this.Controls.Add(this.btnRefresh);
			this.DoubleBuffered = true;
			this.Name = "CoderackForm";
			this.ShowIcon = false;
			this.Text = "Coderack";
			this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.CoderackForm_FormClosing);
			this.Load += new System.EventHandler(this.CoderackForm_Load);
			this.Paint += new System.Windows.Forms.PaintEventHandler(this.CoderackForm_Paint);
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Button btnRefresh;
	}
}