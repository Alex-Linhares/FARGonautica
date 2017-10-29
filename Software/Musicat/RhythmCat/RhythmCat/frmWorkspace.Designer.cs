namespace RhythmCat {

	public class MyPanel : System.Windows.Forms.Panel {
		public MyPanel() {
			this.DoubleBuffered = true;
		}
	}

	partial class WorkspaceForm {
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(WorkspaceForm));
            this.grpControls = new System.Windows.Forms.GroupBox();
            this.chkChords = new System.Windows.Forms.CheckBox();
            this.lblNumResurrectedAnalogies = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.lblNumBigAnalogies = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.barDetail = new System.Windows.Forms.TrackBar();
            this.cboExamples = new System.Windows.Forms.ComboBox();
            this.lblCodeletsKilled = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.lblAvgTimeToRunCodelet = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.chkExpectations = new System.Windows.Forms.CheckBox();
            this.btnPause = new System.Windows.Forms.Button();
            this.txtInputMeasures = new System.Windows.Forms.TextBox();
            this.btnRestart = new System.Windows.Forms.Button();
            this.label3 = new System.Windows.Forms.Label();
            this.lblCodeletNum = new System.Windows.Forms.Label();
            this.trackbarSpeed = new System.Windows.Forms.TrackBar();
            this.nudAnalogyNumber = new System.Windows.Forms.NumericUpDown();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.btnScreenshot = new System.Windows.Forms.Button();
            this.lblSlipnet = new System.Windows.Forms.Label();
            this.pnlMain = new RhythmCat.MyPanel();
            this.grpControls.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.barDetail)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.trackbarSpeed)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudAnalogyNumber)).BeginInit();
            this.SuspendLayout();
            // 
            // grpControls
            // 
            this.grpControls.Controls.Add(this.chkChords);
            this.grpControls.Controls.Add(this.lblNumResurrectedAnalogies);
            this.grpControls.Controls.Add(this.label9);
            this.grpControls.Controls.Add(this.lblNumBigAnalogies);
            this.grpControls.Controls.Add(this.label7);
            this.grpControls.Controls.Add(this.label5);
            this.grpControls.Controls.Add(this.barDetail);
            this.grpControls.Controls.Add(this.cboExamples);
            this.grpControls.Controls.Add(this.lblCodeletsKilled);
            this.grpControls.Controls.Add(this.label6);
            this.grpControls.Controls.Add(this.lblAvgTimeToRunCodelet);
            this.grpControls.Controls.Add(this.label4);
            this.grpControls.Controls.Add(this.chkExpectations);
            this.grpControls.Controls.Add(this.btnPause);
            this.grpControls.Controls.Add(this.txtInputMeasures);
            this.grpControls.Controls.Add(this.btnRestart);
            this.grpControls.Controls.Add(this.label3);
            this.grpControls.Controls.Add(this.lblCodeletNum);
            this.grpControls.Controls.Add(this.trackbarSpeed);
            this.grpControls.Controls.Add(this.nudAnalogyNumber);
            this.grpControls.Controls.Add(this.label2);
            this.grpControls.Controls.Add(this.label1);
            this.grpControls.Controls.Add(this.btnScreenshot);
            this.grpControls.Controls.Add(this.lblSlipnet);
            this.grpControls.Dock = System.Windows.Forms.DockStyle.Right;
            this.grpControls.Location = new System.Drawing.Point(711, 0);
            this.grpControls.Name = "grpControls";
            this.grpControls.Size = new System.Drawing.Size(332, 684);
            this.grpControls.TabIndex = 25;
            this.grpControls.TabStop = false;
            // 
            // chkChords
            // 
            this.chkChords.AutoSize = true;
            this.chkChords.Location = new System.Drawing.Point(41, 199);
            this.chkChords.Name = "chkChords";
            this.chkChords.Size = new System.Drawing.Size(89, 17);
            this.chkChords.TabIndex = 48;
            this.chkChords.Text = "Show Chords";
            this.chkChords.UseVisualStyleBackColor = true;
            this.chkChords.CheckedChanged += new System.EventHandler(this.chkChords_CheckedChanged);
            // 
            // lblNumResurrectedAnalogies
            // 
            this.lblNumResurrectedAnalogies.AutoSize = true;
            this.lblNumResurrectedAnalogies.Location = new System.Drawing.Point(207, 293);
            this.lblNumResurrectedAnalogies.Name = "lblNumResurrectedAnalogies";
            this.lblNumResurrectedAnalogies.Size = new System.Drawing.Size(13, 13);
            this.lblNumResurrectedAnalogies.TabIndex = 47;
            this.lblNumResurrectedAnalogies.Text = "0";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(41, 293);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(138, 13);
            this.label9.TabIndex = 46;
            this.label9.Text = "# resurrected big analogies:\r\n";
            // 
            // lblNumBigAnalogies
            // 
            this.lblNumBigAnalogies.AutoSize = true;
            this.lblNumBigAnalogies.Location = new System.Drawing.Point(207, 273);
            this.lblNumBigAnalogies.Name = "lblNumBigAnalogies";
            this.lblNumBigAnalogies.Size = new System.Drawing.Size(13, 13);
            this.lblNumBigAnalogies.TabIndex = 45;
            this.lblNumBigAnalogies.Text = "0";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(41, 273);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(114, 13);
            this.label7.TabIndex = 44;
            this.label7.Text = "# stored big analogies:\r\n";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(290, 65);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(34, 13);
            this.label5.TabIndex = 43;
            this.label5.Text = "Detail";
            // 
            // barDetail
            // 
            this.barDetail.Location = new System.Drawing.Point(289, 79);
            this.barDetail.Maximum = 100;
            this.barDetail.Name = "barDetail";
            this.barDetail.Orientation = System.Windows.Forms.Orientation.Vertical;
            this.barDetail.Size = new System.Drawing.Size(45, 448);
            this.barDetail.TabIndex = 42;
            this.barDetail.TickFrequency = 10;
            this.barDetail.TickStyle = System.Windows.Forms.TickStyle.None;
            this.barDetail.Value = 100;
            this.barDetail.Scroll += new System.EventHandler(this.barDetail_Scroll);
            // 
            // cboExamples
            // 
            this.cboExamples.FormattingEnabled = true;
            this.cboExamples.Location = new System.Drawing.Point(40, 102);
            this.cboExamples.MaxDropDownItems = 50;
            this.cboExamples.Name = "cboExamples";
            this.cboExamples.Size = new System.Drawing.Size(228, 21);
            this.cboExamples.TabIndex = 41;
            this.cboExamples.SelectedIndexChanged += new System.EventHandler(this.cboExamples_SelectedIndexChanged);
            // 
            // lblCodeletsKilled
            // 
            this.lblCodeletsKilled.AutoSize = true;
            this.lblCodeletsKilled.Location = new System.Drawing.Point(206, 248);
            this.lblCodeletsKilled.Name = "lblCodeletsKilled";
            this.lblCodeletsKilled.Size = new System.Drawing.Size(13, 13);
            this.lblCodeletsKilled.TabIndex = 40;
            this.lblCodeletsKilled.Text = "0";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(41, 248);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(88, 13);
            this.label6.TabIndex = 39;
            this.label6.Text = "% codelets killed:";
            // 
            // lblAvgTimeToRunCodelet
            // 
            this.lblAvgTimeToRunCodelet.AutoSize = true;
            this.lblAvgTimeToRunCodelet.Location = new System.Drawing.Point(207, 223);
            this.lblAvgTimeToRunCodelet.Name = "lblAvgTimeToRunCodelet";
            this.lblAvgTimeToRunCodelet.Size = new System.Drawing.Size(13, 13);
            this.lblAvgTimeToRunCodelet.TabIndex = 38;
            this.lblAvgTimeToRunCodelet.Text = "0";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(41, 223);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(157, 13);
            this.label4.TabIndex = 37;
            this.label4.Text = "Avg. time on coderack until run:";
            // 
            // chkExpectations
            // 
            this.chkExpectations.AutoSize = true;
            this.chkExpectations.Checked = true;
            this.chkExpectations.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkExpectations.Location = new System.Drawing.Point(41, 176);
            this.chkExpectations.Name = "chkExpectations";
            this.chkExpectations.Size = new System.Drawing.Size(117, 17);
            this.chkExpectations.TabIndex = 36;
            this.chkExpectations.Text = "Show Expectations";
            this.chkExpectations.UseVisualStyleBackColor = true;
            this.chkExpectations.CheckedChanged += new System.EventHandler(this.chkExpectations_CheckedChanged);
            // 
            // btnPause
            // 
            this.btnPause.Location = new System.Drawing.Point(27, 36);
            this.btnPause.Name = "btnPause";
            this.btnPause.Size = new System.Drawing.Size(75, 23);
            this.btnPause.TabIndex = 35;
            this.btnPause.Text = "Go";
            this.btnPause.UseVisualStyleBackColor = true;
            this.btnPause.Click += new System.EventHandler(this.btnPause_Click);
            // 
            // txtInputMeasures
            // 
            this.txtInputMeasures.Location = new System.Drawing.Point(122, 127);
            this.txtInputMeasures.Name = "txtInputMeasures";
            this.txtInputMeasures.Size = new System.Drawing.Size(146, 20);
            this.txtInputMeasures.TabIndex = 34;
            this.txtInputMeasures.Text = resources.GetString("txtInputMeasures.Text");
            // 
            // btnRestart
            // 
            this.btnRestart.Location = new System.Drawing.Point(193, 150);
            this.btnRestart.Name = "btnRestart";
            this.btnRestart.Size = new System.Drawing.Size(75, 23);
            this.btnRestart.TabIndex = 33;
            this.btnRestart.Text = "Restart";
            this.btnRestart.UseVisualStyleBackColor = true;
            this.btnRestart.Click += new System.EventHandler(this.btnRestart_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(164, 73);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(56, 13);
            this.label3.TabIndex = 32;
            this.label3.Text = "Codelet #:";
            // 
            // lblCodeletNum
            // 
            this.lblCodeletNum.AutoSize = true;
            this.lblCodeletNum.Location = new System.Drawing.Point(226, 73);
            this.lblCodeletNum.Name = "lblCodeletNum";
            this.lblCodeletNum.Size = new System.Drawing.Size(13, 13);
            this.lblCodeletNum.TabIndex = 31;
            this.lblCodeletNum.Text = "0";
            // 
            // trackbarSpeed
            // 
            this.trackbarSpeed.Location = new System.Drawing.Point(102, 16);
            this.trackbarSpeed.Maximum = 50;
            this.trackbarSpeed.Name = "trackbarSpeed";
            this.trackbarSpeed.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.trackbarSpeed.Size = new System.Drawing.Size(194, 45);
            this.trackbarSpeed.TabIndex = 30;
            this.trackbarSpeed.Value = 25;
            this.trackbarSpeed.Scroll += new System.EventHandler(this.trackbarSpeed_Scroll);
            // 
            // nudAnalogyNumber
            // 
            this.nudAnalogyNumber.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.nudAnalogyNumber.Location = new System.Drawing.Point(102, 67);
            this.nudAnalogyNumber.Name = "nudAnalogyNumber";
            this.nudAnalogyNumber.Size = new System.Drawing.Size(56, 26);
            this.nudAnalogyNumber.TabIndex = 29;
            this.nudAnalogyNumber.Click += new System.EventHandler(this.nudAnalogyNumber_ValueChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(38, 74);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(48, 13);
            this.label2.TabIndex = 28;
            this.label2.Text = "Analogy:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(64, 18);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(38, 13);
            this.label1.TabIndex = 27;
            this.label1.Text = "Speed";
            // 
            // btnScreenshot
            // 
            this.btnScreenshot.Location = new System.Drawing.Point(41, 135);
            this.btnScreenshot.Name = "btnScreenshot";
            this.btnScreenshot.Size = new System.Drawing.Size(75, 23);
            this.btnScreenshot.TabIndex = 26;
            this.btnScreenshot.Text = "Screenshot";
            this.btnScreenshot.UseVisualStyleBackColor = true;
            this.btnScreenshot.Click += new System.EventHandler(this.btnScreenshot_Click);
            // 
            // lblSlipnet
            // 
            this.lblSlipnet.AutoSize = true;
            this.lblSlipnet.Location = new System.Drawing.Point(37, 316);
            this.lblSlipnet.Name = "lblSlipnet";
            this.lblSlipnet.Size = new System.Drawing.Size(49, 13);
            this.lblSlipnet.TabIndex = 25;
            this.lblSlipnet.Text = "lblSlipnet";
            // 
            // pnlMain
            // 
            this.pnlMain.BackColor = System.Drawing.Color.White;
            this.pnlMain.Dock = System.Windows.Forms.DockStyle.Left;
            this.pnlMain.Location = new System.Drawing.Point(0, 0);
            this.pnlMain.Name = "pnlMain";
            this.pnlMain.Size = new System.Drawing.Size(1043, 684);
            this.pnlMain.TabIndex = 1;
            this.pnlMain.Paint += new System.Windows.Forms.PaintEventHandler(this.pnlMain_Paint);
            // 
            // WorkspaceForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1043, 684);
            this.Controls.Add(this.grpControls);
            this.Controls.Add(this.pnlMain);
            this.DoubleBuffered = true;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "WorkspaceForm";
            this.Text = "Workspace";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.WorkspaceForm_FormClosing);
            this.Load += new System.EventHandler(this.WorkspaceForm_Load);
            this.Resize += new System.EventHandler(this.WorkspaceForm_Resize);
            this.grpControls.ResumeLayout(false);
            this.grpControls.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.barDetail)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.trackbarSpeed)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nudAnalogyNumber)).EndInit();
            this.ResumeLayout(false);

		}

		#endregion

        private MyPanel pnlMain;
        private System.Windows.Forms.GroupBox grpControls;
        private System.Windows.Forms.CheckBox chkChords;
        private System.Windows.Forms.Label lblNumResurrectedAnalogies;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label lblNumBigAnalogies;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TrackBar barDetail;
        private System.Windows.Forms.ComboBox cboExamples;
        private System.Windows.Forms.Label lblCodeletsKilled;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label lblAvgTimeToRunCodelet;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.CheckBox chkExpectations;
        private System.Windows.Forms.Button btnPause;
        private System.Windows.Forms.TextBox txtInputMeasures;
        private System.Windows.Forms.Button btnRestart;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label lblCodeletNum;
        private System.Windows.Forms.TrackBar trackbarSpeed;
        private System.Windows.Forms.NumericUpDown nudAnalogyNumber;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button btnScreenshot;
        private System.Windows.Forms.Label lblSlipnet;
	}
}