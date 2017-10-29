using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace RhythmCat {
	public partial class WorkspaceForm : Form {

		Dictionary<string, string> melodyExamples;		// key = name, value = melody
		int curMelodyindex = 0;

		public WorkspaceForm(int initialMelodyIndex) {
			InitializeComponent();
			DoubleBuffered = true;

			melodyExamples = new Dictionary<string,string>();

			// Load combolist.
			using (TextReader tr = File.OpenText(Constants.MELODY_PATH)) {
				string line;
				string name = "";
				string melody;
				bool nameLine = true;
				while ((line = tr.ReadLine()) != null) {
					if (line.Trim() == "") {
						nameLine = true;
						continue;
					}
					if (nameLine) {
						name = line.Trim();
						nameLine = false;
					} else {
						melody = line.Trim();
						cboExamples.Items.Add(name);
						melodyExamples[name] = melody;
					}
				}

				cboExamples.Sorted = true;

				cboExamples.SelectedIndex = initialMelodyIndex;
				curMelodyindex = initialMelodyIndex;
			}
		}
		
		private static Color[] colorList = { Color.Green, Color.Blue, Color.Red, Color.Orange, Color.LightGreen, Color.LightBlue, Color.Pink, Color.DarkOrchid };
		private const float MIN_STRENGTH_FOR_COLOR = 50;

		Workspace workspace;
		int programRunNum;

		private const float measureBorderX = 10;
		private const float measureHeight = 80;
		private const float textOffsetX = 5;
		private const float textOffsetY = 5;
		private const float LINK_HEIGHT = 40;
		private const float symbolOffsetY = 30;
		private const float groupOffsetY = 6;
		private const float NumHeightsVerticalHappiness = 4.5f;

		#region Events


		private delegate void SetCodeletNumDelegate(int num);
		private delegate void SetTextDelegate(string s);
		private delegate void TakeScreenshotDelegate(int programRunNum);
		private delegate void DrawDelegate(Workspace w, Slipnet s, Coderack c);

		public class SetProgramSpeedEventArgs : EventArgs {
			public int delay;

			public SetProgramSpeedEventArgs(int delay) {
				this.delay = delay;
			}
		}
		public class SetDetailLevelEventArgs : EventArgs {
			public int detail;

			public SetDetailLevelEventArgs(int detail) {
				this.detail = detail;
			}
		}
		
		public event EventHandler<SetProgramSpeedEventArgs> setProgramSpeedEvent;

		public event EventHandler<SetDetailLevelEventArgs> setDetailLevelEvent;


		public class RestartEventArgs : EventArgs {
			public string inputMeasureText;

			public RestartEventArgs(string inputMeasureText) {
				this.inputMeasureText = inputMeasureText;
			}
		}
		public event EventHandler<RestartEventArgs> restartEvent;
		public event EventHandler<EventArgs> pauseEvent;
		public event EventHandler<EventArgs> closeEvent;


		#endregion

		#region External updating
		
		public void SetRunNumber(int num) {
			programRunNum = num;
		}

		public void SetCodeletNum(int num) {
			if (this.lblCodeletNum.InvokeRequired) {
				this.lblCodeletNum.Invoke(new SetCodeletNumDelegate(SetCodeletNum), num);
			} else {
				lblCodeletNum.Text = num.ToString();
			}
		}

		public void SetText(string s) {
			if (this.lblSlipnet.InvokeRequired) {
				this.lblSlipnet.Invoke(new SetTextDelegate(SetText), s);
			} else {
				lblSlipnet.Text = s;
			}
		}
		#endregion


		/*private float _detail;
		private float DetailLevel {
			get {
				return _detail * workspace.measuresInput.Count;

			}
			set {
				_detail = value;
			}
		}*/

		private static float DetailLevel { get; set; }

		#region Draw Main

		public void Draw(Workspace w, Slipnet s, Coderack c) {
			if (this.lblSlipnet.InvokeRequired) {
				this.lblSlipnet.Invoke(new DrawDelegate(Draw), w, s, c);
			} else {
				workspace = w;
				lblSlipnet.Text = s.ToString();
				lblAvgTimeToRunCodelet.Text = c.AverageCodeletAgeAtRun.ToString("0.0");
				lblCodeletsKilled.Text = c.PercentCodeletsKilled.ToString("0.0");
				lblNumBigAnalogies.Text = workspace.structureCollection.analogyCompetitionStats.Count.ToString();
				lblNumResurrectedAnalogies.Text = workspace.structureCollection.NumAnalogyWinsTotal + "/" + workspace.structureCollection.NumAnalogyFightsTotal;
				this.Refresh();
			}
		}

		private void pnlMain_Paint(object sender, PaintEventArgs e) {           
			if (workspace == null)
				return;

			Bitmap bmp = new Bitmap(pnlMain.Width, pnlMain.Height);
			Graphics graphics = Graphics.FromImage(bmp);
			DrawWorkspaceToContext(graphics, pnlMain.Width, pnlMain.Height, workspace, (int)nudAnalogyNumber.Value, chkExpectations.Checked, chkChords.Checked, bmp);

			e.Graphics.DrawImageUnscaled(bmp, 0, 0);
		}

		public static void DrawWorkspaceToContext(Graphics graphics, int width, int height, Workspace workspace, int analogyNumber, bool drawExpectations, bool drawChords, Bitmap bitmap) {

			graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;

			#region Analogy/group Colors
			// Assign analogy/group colors.
			/*
			// First sort analogies by order, from left-to-right, in workspace.
			List<Analogy> sortedAnalogies = new List<Analogy>(workspace.analogies);
			sortedAnalogies.Sort((a, b) => {
				if (a.MinLocation > b.MinLocation)
					return 1;
				else if (a.MinLocation < b.MinLocation)
					return -1;
				else if (a.MaxLocation > b.MaxLocation)
					return 1;
				else if (a.MaxLocation < b.MaxLocation)
					return -1;
				return 0;
			});*/


			List<Analogy> sortedAnalogies = workspace.analogies;			// just link to it; we aren't sorting anymore. TODO: remove "sorted" reference

			Dictionary<int, Color> analogyColorsByLength = MakeColorListBySize(workspace.analogies);
			Dictionary<int, Color> objectColorsByLength = MakeColorListBySize(workspace.analogies, workspace.groups);
			
			List<Color> analogyColors = new List<Color>();
			Dictionary<GroupElement, Color> groupColors = new Dictionary<GroupElement, Color>();

			// Color all groups gray by default if we selected a particular analogy
			if (analogyNumber > 0) {
				foreach (Group g in workspace.groups) {
					groupColors[g] = Color.Gray;
				}
			} else {
				// Otherwise color based on size by default.
				foreach (Group g in workspace.groups) {
					if (g.ComputeStrength() >= MIN_STRENGTH_FOR_COLOR)
						groupColors[g] = objectColorsByLength[g.LengthInMeasures];
					else
						groupColors[g] = Color.Gray;
				}
			}

			for (int i = 0; i < sortedAnalogies.Count; i++) {
				Analogy a = sortedAnalogies[i];
/*			for (int i = 0; i < sortedAnalogies.Count; i++) {
				Analogy a = sortedAnalogies[i];
				*/

				// Color the analogy gray unless we selected it or selected "0".
				Color c = Color.Gray;
				if (analogyNumber == 0 || (analogyNumber - 1 == i)) {
					//c = colorList[i % colorList.Count()];
					if (a.Strength >= MIN_STRENGTH_FOR_COLOR)
						//c = analogyColorsByLength[a.LengthInMeasures];
						c = objectColorsByLength[a.LengthInMeasures];
					else
						c = Color.DarkGray;

					//Group parent = a.GetParent();
					//if (parent != null)
					//	groupColors[parent] = c;
					groupColors[a.LHS] = c;
					groupColors[a.RHS] = c;
				}
				analogyColors.Add(c);
			}
			#endregion

			int n = workspace.measures.Count;
			if (workspace.HasUpbeat)
				n++;
			//int n = workspace.measures.Count;
			
			int numMeasuresIncludingExpectations = n;
			if (drawExpectations) {
				if (workspace.expectations.groups.Count > 0) {
					foreach (ExpectedGroup g in workspace.expectations.groups) {
						if (g.MaxLocation >= numMeasuresIncludingExpectations)
							numMeasuresIncludingExpectations = g.MaxLocation + 1;
					}
				}
				if (workspace.expectations.measures.Count > 0) {
					foreach (ExpectedMeasure m in workspace.expectations.measures) {
						if (m.Location >= numMeasuresIncludingExpectations)
							numMeasuresIncludingExpectations = m.Location + 1;
					}
				}
			}
			// show at least 4 measures.
			float measureWidth = GetMeasureWidth(numMeasuresIncludingExpectations, width);

			// Draw happiness meter rectangle.
			Pen happyPen = Pens.Black;
			float x, y;
			GetMeasureUpperLeft(height, measureWidth, 0, out x, out y);

			// Draw "attention" dots/squares for each measure.
			//DrawAttentionDots(e, x, y, measureWidth, workspace);
			DrawAttentionSquares(graphics, x, y, measureWidth, numMeasuresIncludingExpectations, workspace);

			// Draw measure boxes.
			for (int i = 0; i < n; i++) {
				Measure workspaceMeasure = null;
				if (i < workspace.measures.Count)
					workspaceMeasure = workspace.measures[i];
				else if (workspace.measures.Count > 0)
					workspaceMeasure = workspace.measures[workspace.measures.Count-1];

				if (workspaceMeasure != null)
					if (i < workspace.measuresDrawing.Count)
						DrawMeasure(graphics, workspace, height, measureWidth, i, workspace.measuresDrawing[i], workspaceMeasure, drawChords);
			}

			// Draw barlines.
			for (int i = 0; i < workspace.barlines.Count; i++) {
				DrawBarline(graphics, workspace, height, measureWidth, i);
			}

			// Draw links.
			foreach (MeasureLink link in workspace.measureLinks) {
				if (link.strength >= DetailLevel) {
					DrawMeasureLink(graphics, height, measureWidth, link, workspace);
				}
			}


			// Draw exepctations.
			if (drawExpectations) {
				// Draw expected groups.
				foreach (ExpectedGroup eg in workspace.expectations.groups) {
					if (eg.ComputeStrength() * eg.LengthInMeasures >= DetailLevel) {
						DrawExpectationGroup(graphics, workspace, height, measureWidth, eg);
					}
				}
			

				// Draw expected links.
				foreach (ExpectedMeasureLink link in workspace.expectations.links) {
					if (link.strength >= DetailLevel) {
						DrawMeasureLink(graphics, height, measureWidth, link, workspace, true);
					}
				}
			
				// Draw expected analogies.
				foreach (ExpectedAnalogy analogy in workspace.expectations.analogies) {
					if (analogy.Strength * analogy.LengthInMeasures >= DetailLevel) {
						DrawAnalogy(graphics, workspace, height, measureWidth, analogy, Color.Purple, true);
					}
				}
			}

			List<Analogy> dispalyedAnalogies = ComputeDisplayedAnalogies(analogyNumber, sortedAnalogies);

			// Draw groups.
			foreach (Group g in workspace.groups) {
				// Is this group the LHS or RHS of a displayed analogy? If so,
				// display even if too weak for the detail level.
				if (ShouldDisplayGroup(g, dispalyedAnalogies)) {
					Color? c = null;
					if (groupColors.ContainsKey(g))
						c = groupColors[g];
					DrawGroup(graphics, workspace, height, measureWidth, g, drawChords, c);
				}
			}

			// Draw analogies.
			for (int i = 0; i < sortedAnalogies.Count; i++) {
				if (dispalyedAnalogies.Contains(sortedAnalogies[i]))
					DrawAnalogy(graphics, workspace, height, measureWidth, sortedAnalogies[i], analogyColors[i]);
			}

			// Draw melody contour relationships.
			/*if (analogyNumber == 0) {
				foreach (Relationship r in workspace.relationships) {
					if (!r.RhythmType)
						DrawRelationship(graphics, workspace, height, measureWidth, r, Color.Pink);
				}
			}*/

			// Happiness box.
			
			// Compute where top of box should start.
			//float happinessBoxTopMax = NumHeightsVerticalHappiness * measureHeight;

			int BOTTOM_GAP = 10;
			int TOP_GAP = 10;

			float happinessBoxTopMax = height - measureHeight - BOTTOM_GAP - 13;

			// Find the bottom of the staff.
			GetMeasureUpperLeft(height, measureWidth, 0, out x, out y);
			float happinessBoxTopStart = y + measureHeight + TOP_GAP;
			float happinessBoxTop = happinessBoxTopMax;

			Rectangle rect = new Rectangle(0, 0, width, height);

			BitmapData bmd = bitmap.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadOnly, bitmap.PixelFormat);
			int PixelSize=4;
 
			// Search for the first row of pixels that is empty (so no arcs cross it)
			unsafe {
				for (int yy = (int)happinessBoxTopStart; yy < happinessBoxTopMax; yy++) {
					byte* row = (byte*)bmd.Scan0 + (yy * bmd.Stride);
					bool empty = true;
					for (int xx = 0; xx < width; xx++) {
						byte pixelB = row[xx * PixelSize];
						byte pixelG = row[xx * PixelSize + 1];
						byte pixelR = row[xx * PixelSize + 2];
						byte pixelA = row[xx*PixelSize+3];
						if (pixelA > 0) {// && (pixelB != 255 || pixelG != 255 || pixelR != 255)) {
							empty = false;
							break;
						}
					}
					if (empty) {
						happinessBoxTop = yy + TOP_GAP;
						break;
					} else {
						;
					}
				}
			}
			bitmap.UnlockBits(bmd);

			DrawHappinessRectangleOutline(graphics, height, n, measureWidth, happyPen, x, happinessBoxTop);

			// Draw measure happinesses.
			for (int i = 0; i < n; i++) {
				Measure workspaceMeasure = null;
				if (i < workspace.measures.Count)
					workspaceMeasure = workspace.measures[i];
				else if (workspace.measures.Count > 0)
					workspaceMeasure = workspace.measures[workspace.measures.Count - 1];

				if (workspaceMeasure != null)
					if (i < workspace.measuresDrawing.Count)
						DrawMeasureHappiness(graphics, workspace, height, measureWidth, i, workspace.measuresDrawing[i], workspaceMeasure, drawChords, happinessBoxTop);
			}

		}

		private static void DrawMeasureHappiness(Graphics graphics, Workspace workspace, int height, float measureWidth, int i, Measure measure, Measure workspaceMeasure, bool drawChords, float happinessBoxTop) {
			// Draw "Happiness" graph below.
			int minMeasure;
			int maxMeasure;
			double happiness = workspace.GetHappinessStandardWindow(i, out minMeasure, out maxMeasure);

			Pen happyPen = Pens.DarkGreen;
			Brush happyBrush = Brushes.LightGreen;

			if (happiness < 75) {
				happyPen = Pens.YellowGreen;
				happyBrush = Brushes.LightGreen;
			}
			if (happiness < 50) {
				happyPen = Pens.DarkOrange;
				happyBrush = Brushes.Orange;
			}
			if (happiness < 25) {
				happyPen = Pens.DarkRed;
				happyBrush = Brushes.Red;
			}

			float measureWidthNoBorder = GetMeasureWidthNoBorder(measureWidth);

			float x, y;

			GetMeasureUpperLeft(height, measureWidth, minMeasure, out x, out y);

			// Offset by pickup beats.
			float dummy = 0;
			AdjustXForUpbeats(workspace, measureWidth, ref x, ref dummy);

			float xExtension = (workspace.HasUpbeat) ? measureBorderX : 0;

			graphics.FillRectangle(happyBrush, x + 1, happinessBoxTop + 1 + (float)(measureHeight
									- (happiness / 100 * measureHeight)), xExtension + measureWidth * (maxMeasure - minMeasure + 1) - 2 - (measureWidth - measureWidthNoBorder), 10 - 2);
			graphics.DrawRectangle(happyPen, x, happinessBoxTop + (float)(measureHeight
									- (happiness / 100 * measureHeight)), xExtension + measureWidth * (maxMeasure - minMeasure + 1) - (measureWidth - measureWidthNoBorder), 10);
		}

		private static void DrawHappinessRectangleOutline(Graphics graphics, int height, int n, float measureWidth, Pen happyPen, float x, float y) {
			
			graphics.DrawRectangle(happyPen, x, y, measureWidth * n - (measureWidth - GetMeasureWidthNoBorder(measureWidth)), measureHeight + 10);
		}

		private static List<Analogy> ComputeDisplayedAnalogies(int analogyNumber, List<Analogy> sortedAnalogies) {
			List<Analogy> displayed = new List<Analogy>();
			for (int i = 0; i < sortedAnalogies.Count; i++) {
				// Only draw selected analogy, unless set to 0.
				if (analogyNumber > 0 && analogyNumber - 1 != i)
					continue;
				Analogy a = sortedAnalogies[i];
				if (a.Strength * a.LengthInMeasures >= DetailLevel)
					displayed.Add(a);
			}
			return displayed;
		}

		private static bool ShouldDisplayGroup(Group g, List<Analogy> displayedAnalogies) {
			foreach (Analogy a in displayedAnalogies)
				if (a.LHS == g || a.RHS == g)
					return true;

			if (g.ComputeStrength() * g.LengthInMeasures >= DetailLevel)
				return true;

			return false;
		}

		/// <summary>
		/// Makes a Dictionary of colors for each object's size. (Analogies are the size of the sum of LHS and RHS).
		/// </summary>
		/// <param name="analogies"></param>
		/// <param name="?"></param>
		/// <returns></returns>
		private static Dictionary<int, Color> MakeColorListBySize(List<Analogy> analogies) {
			List<Color> colors = new List<Color>(analogies.Count);

			// Make a list of all distinct lengths of groups/analogies.
			// Sort the list.
			// Divide in half. Assign long half to vivid colors, short half to pastels.
			// Color analogies by the length of the longer group in that analogy's LHS/RHS pair.
			SortedDictionary<int, bool> lengths = new SortedDictionary<int, bool>();

			foreach (Analogy a in analogies) {
				if (a.Strength >= MIN_STRENGTH_FOR_COLOR)
					lengths[a.LengthInMeasures] = true;
			}

			return AssignColorsForLengths(lengths);
		}

		/// <summary>
		/// Makes a Dictionary of colors for each object's size, including groups.
		/// </summary>
		/// <param name="analogies"></param>
		/// <param name="?"></param>
		/// <returns></returns>
		private static Dictionary<int, Color> MakeColorListBySize(List<Analogy> analogies, List<Group> groups) {
			List<Color> colors = new List<Color>(analogies.Count);

			// Make a list of all distinct lengths of groups/analogies.
			// Sort the list.
			// Divide in half. Assign long half to vivid colors, short half to pastels.
			// Color analogies by the length of the longer group in that analogy's LHS/RHS pair.
			SortedDictionary<int, bool> lengths = new SortedDictionary<int, bool>();

			foreach (Analogy a in analogies) {
				if (a.Strength >= MIN_STRENGTH_FOR_COLOR)
					lengths[a.LengthInMeasures] = true;
			}

			foreach (Group g in groups) {
				if (g.ComputeStrength() >= MIN_STRENGTH_FOR_COLOR) 
					lengths[g.LengthInMeasures] = true;
			}

			return AssignColorsForLengths(lengths);
		}

		private static Dictionary<int, Color> AssignColorsForLengths(SortedDictionary<int, bool> lengths) {
			// Find the midpoint.
			int mid = lengths.Keys.Count / 2;

			// Assign a color for each length.
			Dictionary<int, Color> lengthColors = new Dictionary<int, Color>();
			for (int i = 0; i < mid; i++) {
				int length = lengths.ElementAt(i).Key;
				if (mid <= ColorList.pastel.Length)
					lengthColors[length] = ColorList.pastel[ColorList.pastel.Length - mid + i];	// Assign color from pastel list, making sure to use the end of the first.
				else
					lengthColors[length] = ColorList.pastel[i % ColorList.pastel.Length];	// Assign color from pastel list, wrapping around.
			}
			for (int i = mid; i < lengths.Keys.Count; i++) {
				int length = lengths.ElementAt(i).Key;
				if (lengths.Keys.Count - mid <= ColorList.vivid.Length)
					lengthColors[length] = ColorList.vivid[ColorList.vivid.Length - lengths.Keys.Count + i];	// Assign color from vivid list, making sure to use the end of the first.
				else
					lengthColors[length] = ColorList.vivid[(i - mid) % ColorList.vivid.Length];	// Assign color from vivid list, wrapping around.
			}

			return lengthColors;
		}

		

		#endregion

		#region Drawing Functions

		#region NonExpectations

		private void DrawAttentionDots(Graphics graphics, float x, float y, float measureWidth, Workspace workspace) {
			Brush br = Brushes.DarkOrange;
			float radius = measureWidth / 20;

			int numTotalDots = workspace.codeletAttentionHistory.Count;
			for (int i = Math.Max(0, numTotalDots - Constants.NUM_ATTENTION_HISTORY_DOTS); i < numTotalDots; i++) {
				Tuple<int, Codelet> tuple = workspace.codeletAttentionHistory[i];

				int measureIdx = tuple.Item1;
				//Codelet c = tuple.Item2;

				// Pick a random x cordinate to draw.
				float offset = (float)Utilities.rand.NextDouble() * measureWidth;

				// Draw circle.
				graphics.FillEllipse(br, x + measureWidth * measureIdx + offset, y - 40, radius, radius);
			}

		}

		private static void DrawAttentionSquares(Graphics graphics, float x, float y, float measureWidth,
		int numMeasuresIncludingExpectations, Workspace workspace) {

			try {
				int[] counter = new int[numMeasuresIncludingExpectations*2];	// buffer in case some expectation groups are missing now.
				int maxMeasureIdx = -1;

				int numTotalDots = workspace.codeletAttentionHistory.Count;
				for (int i = Math.Max(0, numTotalDots - Constants.NUM_ATTENTION_HISTORY_DOTS); i < numTotalDots; i++) {
					Tuple<int, Codelet> tuple = workspace.codeletAttentionHistory[i];

					int measureIdx = tuple.Item1;
					//Codelet c = tuple.Item2;

					// Increment the counter for this measure.
					counter[measureIdx]++;

					if (measureIdx > maxMeasureIdx)
						maxMeasureIdx = measureIdx;
				}
				// Draw rectangles.
				float measureWidthNoBorder = GetMeasureWidthNoBorder(measureWidth);

				for (int i = 0; i <= maxMeasureIdx; i++) {
					// Chose brush color based on count of attention in this measure.
					float attentionPercent = (float)(counter[i]) / Constants.NUM_ATTENTION_HISTORY_DOTS;
					Color c = Color.FromArgb((int)(attentionPercent * 255), Color.DarkOrange);
					Brush br = new SolidBrush(c);
					// Draw rectangle.
					graphics.FillRectangle(br, x + measureWidth * i, y, measureWidthNoBorder, measureHeight);
				}
			} catch (Exception ex) {
				MessageBox.Show(ex.ToString());
			}
		}



		private static void DrawBarline(Graphics graphics, Workspace workspace, int height, float measureWidth, int i) {
			float x;
			float y;
			GetMeasureUpperLeft(height, measureWidth, i, out x, out y);
			x -= measureBorderX;
			if (workspace.HasUpbeat)
				x += measureWidth;

			int thickness = workspace.barlines[i];
			Pen p = new Pen(Color.Black, thickness * 2);
			graphics.DrawLine(p, x, y, x, y + measureHeight*0.45f);
		}

		private static void DrawMeasure(Graphics graphics, Workspace workspace, int height, float measureWidth, int i, Measure m, Measure mWorkspace, bool drawChord) {
			float x;
			float y;
			GetMeasureUpperLeft(height, measureWidth, i, out x, out y);
			float measureWidthNoBorder = GetMeasureWidthNoBorder(measureWidth);
			graphics.DrawRectangle(Pens.DarkBlue, x, y, measureWidthNoBorder, measureHeight);

			// Draw text inside.
			//graphics.DrawString(m.ToString(), SystemFonts.DefaultFont, Brushes.Black, x + textOffsetX, y + textOffsetY, StringFormat.GenericTypographic);
			Font labelFont = new Font(SystemFonts.DefaultFont.FontFamily, 20);
			graphics.DrawString(m.FormLabel, labelFont, Brushes.Black, x + textOffsetX, y + measureHeight + textOffsetY, StringFormat.GenericTypographic);
			
			// Draw attack points.
			// Check for pickup measure.
			int offset = 0;
			//int numSixteenthsInMeasure = m.rhythm.TotalDuration;  // caused weirdness while notes were added.
			int numSixteenthsInMeasure = workspace.measuresInput[1].rhythm.TotalDuration;
			if (workspace.upbeatOffset > 0 && i == 0) {
				offset = workspace.upbeatOffset;
				numSixteenthsInMeasure = workspace.measuresInput[1].rhythm.TotalDuration;
			} else
				offset = numSixteenthsInMeasure;
			
			for (int j = 0; j < numSixteenthsInMeasure; j++) {
				float featureWidth = ((float)measureWidthNoBorder) / numSixteenthsInMeasure;

				float symbolX = x + (j + numSixteenthsInMeasure - offset) * featureWidth;
				float symbolY = y + symbolOffsetY;

				float yRadius =  featureWidth * 3 / 4;
				if (m.rhythm.ContainsNoteEvent(j)) {
					int dur = m.rhythm.GetDurationByAttackPoint(j);

					// Look for pitch.
					Note note = m.rhythm.GetNoteByAttackPoint(j);

                    if (note.isRest)
                        continue;

					if (note.midiInfo != null) {
						// Adjust notehead height based on midi pitch dist from middle C.
						int midiDist = note.Midi - 60;
						float halfStepHeight;

						if (workspace.maxPitchMidi > 60)
							halfStepHeight = (symbolOffsetY) / (workspace.maxPitchMidi - 60);
						else
							halfStepHeight = yRadius/2;

						symbolY -= midiDist * halfStepHeight;
					}

					// Draw solid or filled notehead.
					if (dur > 7)
						graphics.DrawEllipse(Pens.Black, symbolX, symbolY, featureWidth, yRadius);
					else
						graphics.FillEllipse(Brushes.Black, symbolX, symbolY, featureWidth, yRadius);

					// Check for dotted rhythm
					if (dur == 3 || dur == 6 || dur == 12) {
						// Dotted!
						float width = featureWidth / 3;

						graphics.FillEllipse(Brushes.Black, symbolX + featureWidth * 1.5f, symbolY + featureWidth * 3 / 8 - 2, width, width);
					}

					// Draw stem.
					Note n = m.rhythm.GetNoteByAttackPoint(j);

					// Stems for less-than-whole-note durations.
					if (n.duration < 16) {
						float stemX = symbolX + featureWidth;
						float stemYbottom = symbolY + yRadius / 2;
						float stemYtop = stemYbottom - measureHeight / 3.2f;

						graphics.DrawLine(Pens.Black, stemX, stemYbottom, stemX, stemYtop);

						// Draw flags.
						// 1 flag for less than quarter notes
						if (n.duration < 4) {
							graphics.DrawLine(Pens.Black, stemX, stemYtop, stemX+yRadius/2, stemYtop+yRadius/1.5f);

							// 2 flags for 16th notes.
							if (n.duration == 1) {
								graphics.DrawLine(Pens.Black, stemX, stemYtop + measureHeight / 10, stemX + yRadius / 2, stemYtop + measureHeight / 10 + yRadius / 1.5f);
							}
						}
					}


					// Check for tie after.
					if (n.tiedAfter) {
						// Draw tie.
						float x1,y1,x2,y2,xm,ym;
						x1 = symbolX + featureWidth/2 + 1;
						y1 = symbolY + featureWidth;
						x2 = symbolX + dur * featureWidth + featureWidth / 2 - 1;
						y2 = y1;

						if (n.isLastInMeasure)
							x2 += measureBorderX*2;

						xm = (x1+x2)/2;
						ym = y1 + featureWidth/4;
						PointF[] points = { new PointF(x1, y1), new PointF(xm, ym), new PointF(x2, y2) };
						graphics.DrawCurve(Pens.Black, points, 0.8f);
					}

				}
			}

			// Draw alphabet.
			if (mWorkspace.Alphabet != null && drawChord) {
				Font labelFont2 = new Font(SystemFonts.DefaultFont.FontFamily, 8);
				graphics.DrawString(mWorkspace.Alphabet.Name, labelFont2, Brushes.Black, x, y - 40, StringFormat.GenericTypographic);
			}

			
			// Draw text as boxes and filled circles.
			/*for (int j = 0; j < Constants.NUM_FEATURES; j++) {
				float featureWidth = measureWidthNoBorder / Constants.NUM_FEATURES;

				float symbolX = x + j * featureWidth;
				float symbolY = y + symbolOffsetY;


				switch (workspace.measures[i].features[j]) {
					case Feature.On:
						graphics.FillEllipse(Brushes.Green, symbolX, symbolY, featureWidth, featureWidth);
						break;
					case Feature.Off:
						graphics.FillRectangle(Brushes.DarkRed, symbolX+2, symbolY + featureWidth*1.5f, featureWidth-4, featureWidth-4);
						break;
				}
			}*/
		}






		private static void DrawAnalogy(Graphics graphics, Workspace workspace, int height, float measureWidth, Analogy a, Color color, bool expectation = false) {
			if (a.Strength < 50)
				return;

			float pos1;
			float pos2;
			GetGroupElementPositions(a.LHS, a.RHS, out pos1, out pos2);

			float x1, y1, x2, y2;
			GetMeasureBottomMiddle(height, measureWidth, pos1, out x1, out y1);
			GetMeasureBottomMiddle(height, measureWidth, pos2, out x2, out y2);

			AdjustXForUpbeats(workspace, measureWidth, ref x1, ref x2);

			y1 = GetGroupBottom(height, a.LHS.LengthInMeasures);
			y2 = GetGroupBottom(height, a.RHS.LengthInMeasures);
				
			//float linkDistance = Math.Abs(pos2 - pos1);
			float linkImportance = a.Level;
			//float width = 2 * (a.Strength - 50) / 100 * 6            *1;
			float width = a.Strength * a.Strength / 10000.0f * 6.0f;
			//float width = a.Strength * a.Strength / 10000.0f * 6.0f * a.LengthInMeasures / 8.0f;
			float W = Math.Max(width * 1.2f, 10);
			float H = width;
			PointF midpoint = GetLinkBottomMidpoint(x1, y1, x2, linkImportance*1.8f);
			PointF[] points = { new PointF(x1, y1), midpoint, new PointF(x2, y2) };
			Pen p = new Pen(color, width);
			if (expectation) {
				p.DashStyle = System.Drawing.Drawing2D.DashStyle.Dash;
			}
			p.Color = Color.FromArgb((int)(255 * (a.Strength / 100)), p.Color);

			//p.StartCap = System.Drawing.Drawing2D.LineCap.Flat;

			// Set clipping region so the rotated end-of-line doesn't stick above the group line.
			//p.EndCap = System.Drawing.Drawing2D.LineCap.Flat;
			Region oldclip = graphics.Clip;
			RectangleF bottomRegion = oldclip.GetBounds(graphics);
			bottomRegion.Y = y1;
			bottomRegion.Height = bottomRegion.Height - y1;
			graphics.Clip = new Region(bottomRegion);

			// Draw!!!
			graphics.DrawCurve(p, points, 0.5f);
			// Reset clipping region.
			graphics.Clip = oldclip;

			// Now draw the river-delta smooth connector.
			y1 += H;
			float xStart = x1 + H * (float)Math.Tan(Math.Atan2(midpoint.X, midpoint.Y));
			// start
			PointF[] pointsDelta = { new PointF(xStart, y1), new PointF(x1+0.29f*W, y1-0.71f*H), 
									 new PointF(x1+W, y1-H), new PointF(x1-W, y1-H),
									 new PointF(x1-0.29f*W, y1-0.71f*H) };
			Brush b = new SolidBrush(p.Color);
			graphics.FillClosedCurve(b, pointsDelta, System.Drawing.Drawing2D.FillMode.Alternate,1.0f);
			
			//end
			float xStart2 = x2 - H * (float)Math.Tan(Math.Atan2(midpoint.X, midpoint.Y));
			PointF[] pointsDelta2 = { new PointF(xStart2, y1), new PointF(x2+0.29f*W, y1-0.71f*H), 
									 new PointF(x2+W, y1-H), new PointF(x2-W, y1-H),
									 new PointF(x2-0.29f*W, y1-0.71f*H) };
			b = new SolidBrush(p.Color);
			graphics.FillClosedCurve(b, pointsDelta2, System.Drawing.Drawing2D.FillMode.Alternate, 1.0f);


			foreach (Relationship r in a.relationships) {
				DrawRelationship(graphics, workspace, height, measureWidth, r, color);
			}
		}

		private static void DrawGroup(Graphics graphics, Workspace workspace, int height, float measureWidth, Group g, bool drawChord, Color? color) {
			float x1, x2;
			int m1 = g.MinLocation;
			int m2 = g.MaxLocation;
			x1 = m1 * measureWidth;// +measureBorderX;
			x2 = (m2 + 1) * measureWidth;

			AdjustXForUpbeats(workspace, measureWidth, ref x1, ref x2);

			int numMeasures = m2 - m1 + 1;

			float groupHeight = GetGroupHeight(numMeasures);
			float groupTop = GetGroupTop(height, groupHeight);

			Pen p = null;
			if (color == null) {
				switch (g.Level % 3) {
					case 0:
						p = Pens.Purple;
						break;
					case 1:
						p = Pens.Green;
						break;
					case 2:
						p = Pens.Brown;
						break;
				}
			} else {
				p = new Pen((Color)color);
			}
			p.Color = Color.FromArgb((int)(255 * (g.ComputeStrength() / 100)), p.Color);
			
			graphics.DrawEllipse(p, x1, groupTop, x2 - x1, groupHeight);

			if (g is Sequence)
				DrawSequence(graphics, (Sequence)g, height, measureWidth);

			// Draw alphabet.
			if (g.Alphabet != null && drawChord) {
				Font labelFont = new Font(SystemFonts.DefaultFont.FontFamily, 12);
				graphics.DrawString(g.Alphabet.Name, labelFont, Brushes.Black, ((x2+x1)/2) - 20, groupTop - 80 - (g.Level * 20), StringFormat.GenericTypographic);
			}
		}

		private static void DrawSequence(Graphics graphics, Sequence s, int height,float measureWidth) {
			Pen p = new Pen(Color.Blue, ((float)s.ComputeStrength() / 100 * 4));
			for (int i = 0; i < s.SequenceElements.Count-1; i++) {
				GroupElement ge1 = s.SequenceElements[i];
				GroupElement ge2 = s.SequenceElements[i+1];		

				float groupHeight1 = GetGroupHeight(ge1.LengthInMeasures);
				float groupTop1 = GetGroupTop(height, groupHeight1);
				float groupHeight2 = GetGroupHeight(ge2.LengthInMeasures);
				float groupTop2 = GetGroupTop(height, groupHeight2);

				float topMid = Math.Min(groupTop1, groupTop2) - measureHeight/2;
				float loc1 = 0.5f + (ge1.MaxLocation + ge1.MinLocation) / 2.0f;
                float loc2 = 0.5f + (ge2.MaxLocation + ge2.MinLocation) / 2.0f;

				float x1, x2, xmid;
				x1 = loc1 * measureWidth;
				x2 = loc2 * measureWidth;
				xmid = (x1+x2)/2;

				

				graphics.DrawLine(p, x1, groupTop1, xmid, topMid);
				graphics.DrawLine(p, xmid, topMid, x2, groupTop2);
			}
		}

		private static void AdjustXForUpbeats(Workspace workspace, float measureWidth, ref float x1, ref float x2) {
			if (workspace.HasUpbeat) {
				if (workspace.measuresInput.Count > 1) {
					int dur = workspace.measuresInput[1].rhythm.TotalDuration;
					float dx = measureWidth * ((float)dur - workspace.upbeatOffset) / dur - measureBorderX;

					x1 += dx;
					x2 += dx;
				}
			}
		}

		private static float GetGroupTop(float height, float groupHeight) {
			//return 50 + (height - measureHeight) * .25f + measureHeight / 2 - groupOffsetY - groupHeight / 2;
			return 50 + (height - measureHeight) * .25f + measureHeight / 2 - groupOffsetY - groupHeight / 2;
		}
		private static float GetGroupHeight(int numMeasures) {
			float multiplier;

			if (numMeasures < 2)
				multiplier = 0.5f;
			else
				multiplier = numMeasures - 1.0f;

			return measureHeight * multiplier / 4;
		}

		private static void GetGroupElementPositions(GroupElement ge1, GroupElement ge2, out float pos1, out float pos2) {
			if (ge1 is Measure)
				pos1 = ge1.Location;
			else {
				pos1 = ((Group)ge1).midPoint;
			}
			if (ge2 is Measure)
				pos2 = ge2.Location;
			else {
				pos2 = ((Group)ge2).midPoint;
			}
		}

		private static void DrawMeasureLink(Graphics graphics, int height, float measureWidth, MeasureLink link, Workspace workspace, bool expectation = false) {
			if (link.strength < 25)
				return;

			// If link exists in an analogy's "Relationship". don't draw.
			if (!expectation) {
				foreach (Analogy a in workspace.analogies) {
					foreach (Relationship r in a.relationships) {
						if (r.LHS == link.m1 && r.RHS == link.m2)
							return;
					}
				}
			}
			int m1 = link.m1.number;
			int m2 = link.m2.number;

			float x1, y1, x2, y2;
			GetMeasureUpperMiddle(height, measureWidth, m1, out x1, out y1);
			GetMeasureUpperMiddle(height, measureWidth, m2, out x2, out y2);

			AdjustXForUpbeats(workspace, measureWidth, ref x1, ref x2);

			float linkDistance = Math.Abs(m2 - m1);

			PointF[] points = { new PointF(x1, y1), GetLinkMidpoint(x1, y1, x2, linkDistance/2), new PointF(x2, y2) };
			Pen p;
			if (expectation) {
				p = new Pen(ColorList.COLOR_MAUVE, (link.strength - 50) / 100 * 4);
				p.DashStyle = System.Drawing.Drawing2D.DashStyle.Dash;
			} else {
				p = new Pen(ColorList.COLOR_AZURE, (link.strength - 50) / 100 * 4);
			}
			p.Color = Color.FromArgb((int)(255 * (link.strength / 100)), p.Color);
			//p.CustomEndCap.
			graphics.DrawCurve(p, points, 0.5f);
		}

		private static void DrawRelationship(Graphics graphics, Workspace workspace, int height, float measureWidth, Relationship r, Color color) {
			float pos1;
			float pos2;
			GetGroupElementPositions(r.LHS, r.RHS, out pos1, out pos2);

			float x1, y1, x2, y2;
			GetMeasureBottomMiddle(height, measureWidth, pos1, out x1, out y1);
			GetMeasureBottomMiddle(height, measureWidth, pos2, out x2, out y2);

			AdjustXForUpbeats(workspace, measureWidth, ref x1, ref x2);

			float linkDistance = Math.Abs(pos2 - pos1);
			//float linkImportance = r.LHS.Measures.Count;

			// Link to group ovals, not reactangles, for group relationships.
			if (r.LHS is Group) {
				y1 = GetGroupBottom(height, r.LHS.LengthInMeasures);
			}
			if (r.RHS is Group) {
				y2 = GetGroupBottom(height, r.RHS.LengthInMeasures);
			}

			if (!r.RhythmType) {
				y1 += 20;
				y2 += 20;
				color = Color.Violet;
			}

			PointF[] points = { new PointF(x1, y1), GetLinkBottomMidpoint(x1, y1, x2, linkDistance/1.5f), new PointF(x2, y2) };
			Pen p = new Pen(color, (r.Strength - 50) / 100);
			p.DashStyle = System.Drawing.Drawing2D.DashStyle.Dot;
			graphics.DrawCurve(p, points, 0.5f);
		}

		private static float GetGroupBottom(int height,int numMeasures) {
			float groupHeight = GetGroupHeight(numMeasures);
			float groupTop = GetGroupTop(height, groupHeight);
			return groupTop + groupHeight;
		}
		#endregion

		#region Expectations
		
		private static void DrawExpectationGroup(Graphics graphics, Workspace workspace, int height, float measureWidth, ExpectedGroup g) {
			float x1, x2;
			int m1 = g.MinLocation;
			int m2 = g.MaxLocation; 
			x1 = m1 * measureWidth;// +measureBorderX;
			x2 = (m2 + 1) * measureWidth;

			AdjustXForUpbeats(workspace, measureWidth, ref x1, ref x2);

			int numMeasures = m2 - m1;
			if (numMeasures < 1)
				numMeasures = 1;

			float groupHeight = measureHeight * numMeasures / 4;
			Pen p = new Pen(Color.Purple, 2);
			p.DashStyle = System.Drawing.Drawing2D.DashStyle.Dash;
			p.Color = Color.FromArgb((int)(255 * (g.ComputeStrength() / 100)), p.Color);

			graphics.DrawEllipse(p, x1, 50 + (height-measureHeight) * .25f + measureHeight/2 - groupOffsetY - groupHeight / 2, x2 - x1, groupHeight);
		}

		#endregion

		#endregion

		#region Utility Functions

		private static PointF GetLinkMidpoint(float x1, float y1, float x2, float linkDistance) {
			return new PointF((x1 + x2) / 2, y1 - LINK_HEIGHT * linkDistance);
		}

		/// <summary>
		/// Used for underneath-links (relationships and analogies)
		/// </summary>
		/// <param name="x1"></param>
		/// <param name="y1"></param>
		/// <param name="x2"></param>
		/// <param name="heightUnits">Gives the importance of the link -- converts to height.</param>
		/// <returns></returns>
		private static PointF GetLinkBottomMidpoint(float x1, float y1, float x2, float heightUnits) {
			return new PointF((x1 + x2) / 2, y1 + LINK_HEIGHT * heightUnits);
		}

		private static float GetMeasureWidthNoBorder(float measureWidth) {
		   return measureWidth - 2 * measureBorderX;
	}

		private static float GetMeasureWidth(int n, int width) {
			int numMeasuresVisible = Math.Max(4, n);
			float measureWidth = width / (float)numMeasuresVisible;
			return measureWidth;
		}

		private static void GetMeasureUpperLeft(int height, float measureWidth, int i, out float x, out float y) {
			x = i * measureWidth + measureBorderX;
			y = (height - measureHeight) * 0.25f + 50;
		}

		private static void GetMeasureUpperMiddle(int height, float measureWidth, int i, out float x, out float y) {
			x = (i + 0.5f) * measureWidth;
			y = (height - measureHeight) * 0.25f + 50;
		}


		private static void GetMeasureBottomMiddle(int height, float measureWidth, float i, out float x, out float y) {
			x = (i + 0.5f) * measureWidth;
			y = (height - measureHeight) * 0.25f + measureHeight + 50;
		}

		#endregion

		#region Screenshot

		private void btnScreenshot_Click(object sender, EventArgs e) {
			TakeScreenshot(this.workspace.measures.Count);
		}

		public void TakeScreenshot(int measureNum) {

			if (this.pnlMain.InvokeRequired) {
				this.pnlMain.Invoke(new TakeScreenshotDelegate(TakeScreenshot), measureNum);
			} else {
				string filepath = string.Format(Constants.SCREENSHOT_PATH + @"screenshot-run{0}-measure{1}-{2:yyyy-MM-dd_hh-mm-ss-tt}.png",
												programRunNum, measureNum, DateTime.Now);

				int width, height;
				//int left, top;

				width = pnlMain.Width;
				height = pnlMain.Height;
				//left = this.Left + pnlMain.Left;
				//top = this.Top + pnlMain.Top + 20;

				using (Bitmap bitmap = new Bitmap(width, height)) {
					pnlMain.DrawToBitmap(bitmap, new Rectangle(0, 0, bitmap.Width, bitmap.Height));
					//using (Graphics g = Graphics.FromImage(bitmap)) {
					//	g.CopyFromScreen(left, top, 0, 0, bitmap.Size);
					//}
					bitmap.Save(filepath, ImageFormat.Png);
				}
			}
		}
		#endregion

		#region Control Events
		private void trackbarSpeed_Scroll(object sender, EventArgs e) {
			// Look for subscribers.
			if (setProgramSpeedEvent != null) {
				setProgramSpeedEvent(this, new SetProgramSpeedEventArgs(trackbarSpeed.Value));
			}
		}

		private void barDetail_Scroll(object sender, EventArgs e) {
			DetailLevel = 32 * (100 - barDetail.Value);

			// Look for subscribers.
			if (setDetailLevelEvent != null) {
				setDetailLevelEvent(this, new SetDetailLevelEventArgs(barDetail.Value));
			}

			this.Refresh();
		}

		private void nudAnalogyNumber_ValueChanged(object sender, EventArgs e) {
			this.Refresh();
		}
		private void btnRestart_Click(object sender, EventArgs e) {
			// Look for subscribers.
			if (restartEvent != null) {
				restartEvent(this, new RestartEventArgs(txtInputMeasures.Text));
			}
		}

		private void btnPause_Click(object sender, EventArgs e) {
			// Look for subscribers.
			if (pauseEvent != null) {
				pauseEvent(this, new EventArgs());
			}
		}

		private void chkExpectations_CheckedChanged(object sender, EventArgs e) {
			this.Refresh();
		}


		private void chkChords_CheckedChanged(object sender, EventArgs e) {
			this.Refresh();
		}

		private void WorkspaceForm_Resize(object sender, EventArgs e) {
			pnlMain.Height = this.Height - pnlMain.Top;
            pnlMain.Width = this.Width - grpControls.Width;
		}
		#endregion

		private void WorkspaceForm_FormClosing(object sender, FormClosingEventArgs e) {
			if (closeEvent != null) {
				closeEvent(this, new EventArgs());
			}
			Settings.Default.workspaceSize = this.Size;
			Settings.Default.workspaceLoc = this.Location;

			Settings.Default.Save();
		}

		private void WorkspaceForm_Load(object sender, EventArgs e) {
			this.Size = Settings.Default.workspaceSize;
			this.Location = Settings.Default.workspaceLoc;
		}


		public void SetPauseButtonText(string txt) {
			if (this.btnPause.InvokeRequired) {
				this.btnPause.Invoke(new SetTextDelegate(SetPauseButtonText), txt);
			} else {
				btnPause.Text = txt;
			}
		}

		private void cboExamples_SelectedIndexChanged(object sender, EventArgs e) {
			txtInputMeasures.Text = melodyExamples[cboExamples.Text];
			curMelodyindex = cboExamples.SelectedIndex;
			// restart.

			// Look for subscribers.
			if (restartEvent != null) {
				restartEvent(this, new RestartEventArgs(txtInputMeasures.Text));
			}

		}

		public int MelodyIndex {
			get {
				return curMelodyindex;
			}
		}


	}
}
