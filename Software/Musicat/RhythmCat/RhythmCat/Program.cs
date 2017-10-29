using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.Threading;
using System.IO;
using System.Drawing;
using System.Windows.Forms;
using GroundTruth;
using MusicPrimitives;



namespace RhythmCat
{
	class Program
	{
        private const bool ESSEN_TESTING = true;


		private static Workspace workspace;
		private static Coderack coderack;
		private static Slipnet slipnet;

		private static int curMelodyIndex = 0;

		private static bool restarting;
		private static bool pausing;

		private static object restartLock = new object();
		private static string input = "";
		private static int programRunNum;

		private static int TIME_TO_SLEEP_BETWEEN_CODELETS = 5;
		private static int DISPLAY_DETAIL_LEVEL = 50;
		private static int NUM_CODELETS_UNTIL_DRAW = 5;

		// Command-line, No-GUI options
		private const int DEFAULT_OUTPUT_WIDTH = 600;
		private const int DEFAULT_OUTPUT_HEIGHT = 600;
		private static bool GUIenabled = true;
		private static bool inputFromCommandLine = false;
		private static string pngFilePath = null;
		private static int pngWidth = DEFAULT_OUTPUT_WIDTH;
		private static int pngHeight = DEFAULT_OUTPUT_HEIGHT;

		private static bool groundTruthTesting = false;
		private static string inputExampleName = null;
		private static string testFile = null;
		private static string resultFile = null;
		private static GroundTruthExample groundTruthExample;

		static void Main(string[] args) {

			// Parse command-line args
			foreach (string arg in args) {
				if (arg == "noGUI") {
					GUIenabled = false;
					Console.WriteLine("No GUI");
				} else if (arg.StartsWith("input=")) {
					input = arg.Substring(6, arg.Length - 6);
					inputFromCommandLine = true;
					Console.WriteLine("Input set to: " + input);
				} else if (arg.StartsWith("png=")) {
					pngFilePath = arg.Substring(4, arg.Length - 4);
					Console.WriteLine("Output .png: " + pngFilePath);
				} else if (arg.StartsWith("width=")) {
					pngWidth = int.Parse(arg.Substring(6, arg.Length - 6));
					Console.WriteLine("Output width: " + pngWidth.ToString());
				} else if (arg.StartsWith("height=")) {
					pngHeight = int.Parse(arg.Substring(7, arg.Length - 7));
					Console.WriteLine("Output height: " + pngHeight.ToString());
				} else if (arg.StartsWith("seed=")) {
					int seed = int.Parse(arg.Substring(5, arg.Length - 5));
					Utilities.InitRandom(seed);
					Console.WriteLine("Random seed: " + pngHeight.ToString());
				// Arguments for automatic testing and % correct reporting.
				} else if (arg.StartsWith("inputExampleName=")) {
					inputExampleName = arg.Substring(17, arg.Length - 17);
					Console.WriteLine("inputExampleName set to: " + inputExampleName);
				} else if (arg.StartsWith("testFile=")) {
					testFile = arg.Substring(9, arg.Length - 9);
					groundTruthTesting = true;
					Console.WriteLine("testFile set to: " + testFile);
				} else if (arg.StartsWith("resultFile=")) {
					resultFile = arg.Substring(11, arg.Length - 11);
					if (!File.Exists(resultFile)) {
						StreamWriter sw = File.CreateText(resultFile);
						sw.Close();
					}
						
					Console.WriteLine("resultFile set to: " + resultFile);
				} else
					throw new ArgumentException("Unknown command-line argument: " + arg);
			}
			if (!GUIenabled && pngFilePath == null && !groundTruthTesting) {
				throw new ArgumentException("png argument required to set filepath");
			}

			// Parse the file and set input if groundTruth testing
			if (groundTruthTesting) {
				Utilities.LogtoFile("GroundTruthTesting: " + DateTime.Now.ToString());
				int numRepetitions;
				List<GroundTruthExample> groundTruthExamples;
				Parser.Parse(testFile, out numRepetitions, out groundTruthExamples);
				foreach (GroundTruthExample e in groundTruthExamples) {
					if (e.name == inputExampleName) {
						groundTruthExample = e;
						break;
					}
				}
				if (groundTruthExample.name != inputExampleName)
					throw new Exception("Example name not found in ground truth file: " + inputFromCommandLine);

				input = groundTruthExample.input;
				Utilities.LogtoFile("Input: " + input);
			}

			if (!GUIenabled)
				restarting = false;
			else
				restarting = true;


			if (GUIenabled) {
				Console.WindowHeight = 40;
				Console.BufferHeight = 3000;
			} 

			try {
				// Verify screenshot path exists.
				System.IO.Directory.CreateDirectory(Constants.SCREENSHOT_PATH);

				while (true) {	//exectues once for each run of the program on an input
					Init();

					if (!GUIenabled)
						pausing = false;

					// Set the default input unless we reset with a different input.
					if (!GUIenabled || !restarting) {
						//
					} else {
						// Clear the restart flag, but leave the input alone.
						restarting = false;
					}

					// Parse the input measures.
					int upbeatOffset;
					Key key;
					List<Measure> parsedMeasures = Rhythm.ParseRhythmMeasures(workspace, input, out upbeatOffset, out key);
					workspace.Key = key;

					foreach (Measure m in parsedMeasures) {
						workspace.measuresInput.Add(m);
					}

					workspace.upbeatOffset = upbeatOffset;

					List<Measure> shiftedMeasures = Rhythm.ShiftRhythmMeasures(workspace, parsedMeasures, upbeatOffset);
					foreach (Measure m in shiftedMeasures) {
						workspace.measuresInputShifted.Add(m);
					}

					// If there is an upbeat, add the upbeat measure for drawing.
					Measure newDrawingMeasure = null;
					Measure drawingMeasureSource = null;
					int nextDrawingAttackPointIndex = 0;    // has to be outside of measure loop because drawing meausre have a different span.
					int nextDrawingAttackPoint = 0;
					if (upbeatOffset > 0) {
						workspace.measuresDrawing.Add(new Measure(workspace.measuresInput[0]));
						newDrawingMeasure = workspace.measuresDrawing[0];
						newDrawingMeasure.ClearNotes();
						drawingMeasureSource = workspace.measuresInput[0];
						nextDrawingAttackPoint = drawingMeasureSource.rhythm.AttackPointsIncludingRests[0] - upbeatOffset;
					}

					int numSixteenthsTotal = Constants.NUM_EXTRA_MEASURES_COMPUTATION * 16;
					foreach (Measure m in workspace.measuresInput)
						numSixteenthsTotal += m.MeasureDuration;

					workspace.MaxPossibleTime = Constants.NUM_CODELETS_PER_SIXTEENTH * numSixteenthsTotal - 1;

					if (groundTruthTesting) {
						//Utilities.LogtoFile("Ground truth testing: # measuresInput=" + workspace.measuresInput.Count.ToString());
						Utilities.LogtoFile("Ground truth testing: input = " + input);
					}


					#region Main Loop!

					// Run N codelets for each measure
                    int i;
					for (i = 0; i < workspace.measuresInput.Count + Constants.NUM_EXTRA_MEASURES_COMPUTATION; i++) {
						if (restarting)
							break;

						workspace.expectations.CleanExpectations();

						if (GUIenabled) {
							lock (restartLock) {
								if (restarting)
									break;
								workspace.Draw(coderack);
								TakeScreenshot(i + 1);
							}
						}


						int nextAttackPointShifted = 0;
						int nextAttackPointIndex = 0;
						Measure newShiftedMeasure = null;
						Measure shiftedMeasureSource = null;
						if (i < workspace.measuresInputShifted.Count)
							shiftedMeasureSource = workspace.measuresInputShifted[i];
						else {
							if (workspace.measuresInputShifted.Count > 0)
								shiftedMeasureSource = workspace.measuresInputShifted[workspace.measuresInputShifted.Count - 1];
						}
						int codeletsPerMeasure = 0;
						if (shiftedMeasureSource != null)
							codeletsPerMeasure = Constants.NUM_CODELETS_PER_SIXTEENTH * shiftedMeasureSource.MeasureDuration;

						// Run N codelets (60) per sixteenth note.
						for (int j = 0; j < codeletsPerMeasure && !restarting; j++) {

							while (pausing) {
								//if (groundTruthTesting)
								//	Utilities.LogtoFile("****************************** PAUSING in ground truth testing");
								Thread.Sleep(100);
                                //Application.DoEvents();
							}
							  
							 lock (restartLock) {
								if (restarting)
									break;

								if (GUIenabled) {
									workspace.frmWorkspace.SetCodeletNum(workspace.CurrentTime);
								}
								coderack.RunNextCodelet();
							}
							workspace.CurrentTime++;

							
							// Add measures and notes as time passes.
							
							// This got messey due to maintaining the drawing and internal shifted
							// versions for upbeat measures. Rewrite with less duplication would
							// be preferable, but it works for now. Note that the shifted version
							// has a different set of notes and attacks and durations, due to 
							// mandatory breaking-up of long notes crossing barlines.
							#region Add Measures and Notes
							// We will add notes if we reached a new attack point. But first create empty measures as needed.
							// Add a shifted measure if necessary.
							if (j == 0 && i < workspace.measuresInputShifted.Count) {
								workspace.measures.Add(new Measure(shiftedMeasureSource));
								newShiftedMeasure = workspace.measures[workspace.measures.Count - 1];
								newShiftedMeasure.ClearNotes();
							}
							  
							// Add a drawing measure if necessary.
							//if (j == ((workspace.measuresInput[1].MeasureDuration - upbeatOffset) % workspace.measuresInput[1].MeasureDuration) * Constants.NUM_CODELETS_PER_SIXTEENTH) {
							if (j == upbeatOffset * Constants.NUM_CODELETS_PER_SIXTEENTH) {
								drawingMeasureSource = null;
								if (upbeatOffset > 0) {
									if (i + 1 < workspace.measuresInput.Count) {
										drawingMeasureSource = workspace.measuresInput[i + 1];
									}
								} else if (i < workspace.measuresInput.Count)  {
									drawingMeasureSource = workspace.measuresInput[i];
								}
                                if (drawingMeasureSource != null) {
                                    workspace.measuresDrawing.Add(new Measure(drawingMeasureSource));
                                    newDrawingMeasure = workspace.measuresDrawing[workspace.measuresDrawing.Count - 1];
                                    newDrawingMeasure.ClearNotes();
                                    nextDrawingAttackPointIndex = 0;
                                    nextDrawingAttackPoint = drawingMeasureSource.rhythm.AttackPointsIncludingRests[0];
                                }
							}
							
							// Check for an attack point for the shifted measure.
							if (j == nextAttackPointShifted * Constants.NUM_CODELETS_PER_SIXTEENTH) {
								// If we aren't in the extra measures yet, add note to drawing and shifted measures.
								if (i < workspace.measuresInputShifted.Count) {
									// Add note.
									newShiftedMeasure.rhythm.AddNote(shiftedMeasureSource.rhythm.notes[nextAttackPointIndex]);
									
									// Increment attack point.
									nextAttackPointIndex++;
									if (nextAttackPointIndex < shiftedMeasureSource.rhythm.AttackPointsIncludingRests.Count)
										nextAttackPointShifted = shiftedMeasureSource.rhythm.AttackPointsIncludingRests[nextAttackPointIndex];
								}
							}

							// Check for an attack point for the drawing measure.
							if (j == ( ((nextDrawingAttackPoint + upbeatOffset) * Constants.NUM_CODELETS_PER_SIXTEENTH)
                                                                    % codeletsPerMeasure)) {
								// If we aren't in the extra measures yet, add note to drawing and shifted measures.
								if (i < workspace.measuresInputShifted.Count) {
									// Add note.
									newDrawingMeasure.rhythm.AddNote(drawingMeasureSource.rhythm.notes[nextDrawingAttackPointIndex]);

									// Increment attack point.
									nextDrawingAttackPointIndex++;
									if (nextDrawingAttackPointIndex < drawingMeasureSource.rhythm.AttackPointsIncludingRests.Count) {
										nextDrawingAttackPoint = drawingMeasureSource.rhythm.AttackPointsIncludingRests[nextDrawingAttackPointIndex];
										if (workspace.measuresDrawing.Count == 1)
											nextDrawingAttackPoint -= upbeatOffset; // correct the upbeat measure's attack points

									}
								}
							}


							#endregion

						   
							if (GUIenabled) {
								if (workspace.CurrentTime % 15 == 0) {
									Console.WriteLine("Time = " + workspace.CurrentTime.ToString());
								}

								lock (restartLock) {
									if (restarting)
										break;
									if (workspace.CurrentTime % NUM_CODELETS_UNTIL_DRAW == 0) {
										workspace.Draw(coderack);
									}
								}
							}

							// Update the slipnet every 15 codelets.
							if (workspace.CurrentTime % Constants.NUM_CODELETS_TILL_SLIPNET_UPDATE == 0) {
								// Generate list of concept instances from the workspace.
								List<ConceptInstance> conceptInstances = new List<ConceptInstance>();
								/*
								foreach (GroupElement ge in workspace.Layers[0].GroupElements) {
									// Grab all concept nodes activated above threshold in any perspect lists.
									foreach (Perspect p in ge.Perspects) {
										conceptInstances.Add(new ConceptInstance(p.ConceptNode, p.Strength));
									}
								}*/

								slipnet.UpdateActivations(conceptInstances);
							}

                            // Update the Temperature every 15 codelets.
                            if (workspace.CurrentTime % Constants.NUM_CODELETS_TILL_TEMPERATURE_UPDATE == 0) {
                                // Use the average happiness of structures in the recent window.
                                int max = workspace.MaxLocation;
                                int min = Math.Max(0, max - Constants.NUM_MEASURES_FOR_WORKSPACE_HAPPINESS);
                                workspace.Temperature = workspace.HappinessForRange(min, max);
                            }

                            // Update the coderack every 15 codelets.
                            if (workspace.CurrentTime % Constants.NUM_CODELETS_TILL_CODERACK_UPDATE == 0) {
                                // Add new codelets.
                                coderack.Populate();
                                coderack.Populate();
                            }


							// Clean out old codelets every 15 codelets.
							if (workspace.CurrentTime % Constants.NUM_CODELETS_TILL_CODERACK_CLEANUP == 0) {
								// Remove oldest codelets until the coderack is the maximum size.
								coderack.RemoveOld();
							}

							// Fade old links, relationships every 15 codelets.
							if (workspace.CurrentTime % Constants.NUM_CODELETS_TILL_LINK_FADE == 0) {
								// Remove oldest codelets until the coderack is the maximum size.
								workspace.WeakenOldLinks();
							}

							// Dump in more high-level codelets every 30 codelets.
							if (workspace.CurrentTime % Constants.NUM_CODELETS_TILL_NEW_HIGH_LEVEL == 0) {
								AddHighLevelCodelets(i);
							}

							if (GUIenabled) {
								Thread.Sleep(TIME_TO_SLEEP_BETWEEN_CODELETS);
							}
						}

						// Done with a measure.

						if (restarting) {
							//if (groundTruthTesting)
							//	Utilities.LogtoFile("****************************** RESTART in ground truth testing");
							break;
						}

						if (GUIenabled) {
							// Show coderack.
							//Console.WriteLine("Coderack:");
							//Console.WriteLine(codeRack.ToString());

							Console.WriteLine("Workspace:");
							Console.WriteLine(workspace.ToString());

							Console.WriteLine(slipnet.ToString());
						}
					} // Done with all measures!

#endregion

					// DONE with run!



					// Clear future expectations.
					workspace.expectations.RemoveAllExpectations();

					// Final draw of everything.
					workspace.Draw(coderack);

                    // Final screenshot w/o expectations.
                    TakeScreenshot(i + 1);

					// Finally generate the graphical output if we're in noGUI mode and not doing groundTruthTesting.
					if (!GUIenabled & !groundTruthTesting) {
						int width = DEFAULT_OUTPUT_WIDTH, height = DEFAULT_OUTPUT_HEIGHT;
						Bitmap bmp = new Bitmap(width, height);
						Graphics graphics = Graphics.FromImage(bmp);
						WorkspaceForm.DrawWorkspaceToContext(graphics, width, height, workspace, 0, false, false, bmp);

						// Now scale to desired size.
						Bitmap bmpScaled = new Bitmap(bmp, pngWidth, pngHeight);
						bmpScaled.Save(pngFilePath, System.Drawing.Imaging.ImageFormat.Png);
						return;
					}

					// Compute results and store to file if we're going GT testing.
					if (groundTruthTesting) {
						// Check groups.
                        int totalNumGroupsFound;
                        if (!ESSEN_TESTING) {
                            totalNumGroupsFound = workspace.groups.Count;
                        } else {
                            // Only count strong normal (non-meta) groups in Essen testing.
                            totalNumGroupsFound = 0;
                            foreach (Group g in workspace.groups)
                                if (g.Level == 1 && g.ComputeStrength() > 20)
                                    totalNumGroupsFound++;
                        }
                        int numMissingGroups = 0;
						foreach (GroundTruth.ExpectedGroup eg in groundTruthExample.groups) {
							// Try to find the expected group.
							bool found = false;
							foreach (Group g in workspace.groups) {
								if (g.MinLocation == eg.startMeasure && g.MaxLocation == eg.endMeasure) {
									found = true;
									break;
								}
							}
							if (!found)
								numMissingGroups++;
						}

						// Check analogies.
                        int totalNumAnalogiesFound = workspace.analogies.Count;
						int numMissingAnalogies = 0;
						foreach (GroundTruth.ExpectedAnalogy ea in groundTruthExample.analogies) {
							// Try to find the expected group.
							bool found = false;
							foreach (Analogy a in workspace.analogies) {
								if (a.LHS.MinLocation == ea.LHS.startMeasure &&
									a.LHS.MaxLocation == ea.LHS.endMeasure &&
									a.RHS.MinLocation == ea.RHS.startMeasure &&
									a.RHS.MaxLocation == ea.RHS.endMeasure) {

									found = true;
									break;
								}
							}
							if (!found)
								numMissingAnalogies++;
						}

						// Write to disk. Format:
						// # expected groups, # missing, #expected analogies, #missing, timestamp

						// Lock results file in case we run multiple musicats at once.
						bool retry = true;
						int retries = 10;
						while (retry) {
							retry = false;

							Utilities.LogtoFile("Avg ms: " + workspace.Coderack.AvgCodeletRunTimeMs.ToString() + 
								"\tavg age at run: " + workspace.Coderack.AverageCodeletAgeAtRun.ToString() +
								"\t% Killed: " + workspace.Coderack.PercentCodeletsKilled.ToString() + 
								"\ttimestep at end: " + workspace.CurrentTime.ToString() + 
								"\t#measures in workspaceShifted: " + workspace.measuresInputShifted.Count.ToString());

							try {
								FileStream fileStream = new FileStream(resultFile, FileMode.Append, FileAccess.Write, FileShare.None);
								using (TextWriter tw = new StreamWriter(fileStream)) {
									tw.WriteLine("{0},{1},{2},{3},{4},{5},{6}",
										groundTruthExample.groups.Count, numMissingGroups,
										groundTruthExample.analogies.Count, numMissingAnalogies,
										DateTime.Now.ToString(), totalNumGroupsFound, totalNumAnalogiesFound);
								}
							} catch (FileNotFoundException Ex) {
								// The file didn't exist
								MessageBox.Show("Didn't exist:" + Ex.ToString());
							} catch (AccessViolationException Ex) {
								// You don't have the permission to open this
								MessageBox.Show("Access violation: " + Ex.ToString());
							} catch (IOException Ex) {
								// IO exception -- try a few more times. 

								if (retries > 0) {
									retry = true;
									retries--;
								} else {
									MessageBox.Show("Max retries reached: IO exception: " + Ex.ToString());
								}
								Thread.Sleep(20);
							} catch (Exception Ex) {
								// Something happened! 
								MessageBox.Show("Unexpected exception: " + Ex.ToString());
							}
						}
						return;	// done with ground truth testing!
					}
					
					// Wait until reset event happens
					while (!restarting) {
						Thread.Sleep(100);
                        //Application.DoEvents();
					}
					curMelodyIndex = workspace.frmWorkspace.MelodyIndex;
					workspace.CloseForms();
					//Console.Read();
				}
			} catch (Exception e) {
				Utilities.LogtoFile(e.ToString());
#if DEBUG
					throw e;
#else
					MessageBox.Show(e.ToString());
#endif
				}
		}

		/// <summary>
		/// Adds high-level codelets.
		/// This function provides top-down control by finding which structures are missing, and adding codelets
		/// to search for the missing structures.
		/// </summary>
		/// <param name="currentMeasureIndex">The index of the current (newest) measure, useful for focusing our attention</param>
		private static void AddHighLevelCodelets(int currentMeasureIndex) {
			// Add old link-breaker codelets for old analogies. This removes links that are irrelevant, where those measures 
			// are already connected by anologies with good link/relationships.
			foreach (Analogy a in workspace.analogies) {
				if (a.Strength > 80 || a.MaxLocation < currentMeasureIndex - 4) {
					for (int j = 0; j < 5; j++) {
						OldMeasureLinkBreakerCodelet obc = new OldMeasureLinkBreakerCodelet(5, null, coderack, workspace, slipnet, a);
						coderack.AddCodelet(obc);
					}
				}
			}

			// Get happiness for each measure (taking into account a range before/after the measure.
			// Skip measures too far in past.
			for (int i = Math.Max(0, currentMeasureIndex - 8); i < workspace.measures.Count; i++) {
				int minMeasure;
				int maxMeasure;
				double happiness = workspace.GetHappinessStandardWindow(i, out minMeasure, out maxMeasure);
				double unhappiness = 100 - happiness;
				int urgency = (int)unhappiness;
				int linkUrgency = 0, groupUrgency = 0, analogyUrgency = 0;

				// Look for links at this measure. Do we need to search for more?
				double avgLinkStrength, maxLinkStrength;
				int numLinks = workspace.CountLinksToMeasure(i, out avgLinkStrength, out maxLinkStrength);

				if (numLinks < 1 || maxLinkStrength < 80 || avgLinkStrength < 50) {
					// Look for links.
					// TODO: how many to add?
					linkUrgency = (int)(100 - avgLinkStrength);
					for (int j = 0; j < 4; j++) {
						MeasureLinkerCodelet mlc = new MeasureLinkerCodelet(linkUrgency, null, coderack, workspace, slipnet, workspace.measures[i]);
						coderack.AddCodelet(mlc);
					}
				}

				// Look for Groups involving this measure. Do we need to search for more?.
				double avgGroupStrength, maxGroupStrength;
				int numGroups = workspace.CountGroupsInvolvingMeasure(i, out avgGroupStrength, out maxGroupStrength);

				if (numGroups < 1 || maxGroupStrength < 80 || avgGroupStrength < 50) {
					// Look for groups.
					// TODO: how many to add?
					for (int j = 0; j < 4; j++) {
						groupUrgency = Math.Max(5, (int)(100 - avgGroupStrength) - linkUrgency);
						MeasureSamenessGrouperCodelet msgc = new MeasureSamenessGrouperCodelet(groupUrgency, null, coderack, workspace, slipnet, workspace.PickMeasureLinkInRange(i, i));
						coderack.AddCodelet(msgc);

						ProximityGrouperCodelet pgc = new ProximityGrouperCodelet(groupUrgency, null, coderack, workspace, slipnet, workspace.PickGroupElementInRange(i, i));
						coderack.AddCodelet(pgc);

						MetaGrouperCodelet mgc = new MetaGrouperCodelet(groupUrgency, null, coderack, workspace, slipnet, workspace.PickGroupInRange(i, i));
						coderack.AddCodelet(mgc);
					}
				}

				// Improve existing group scores.
				if (numGroups > 0 && maxGroupStrength < 75) {
					// TODO: how many to add?
					for (int j = 0; j < numGroups; j++) {
						Codelet codelet = new GroupReasonAnalogyComponentCodelet((int)(100 - maxGroupStrength), null,
							coderack, workspace, slipnet, workspace.PickGroupInRange(i, i));
						coderack.AddCodelet(codelet);

						codelet = new GroupReasonComponentsSimilarCodelet((int)(100 - maxGroupStrength), null,
							coderack, workspace, slipnet, workspace.PickGroupInRange(i, i));
						coderack.AddCodelet(codelet);

						codelet = new GroupReasonNumberComponentsCodelet((int)(100 - maxGroupStrength), null,
							coderack, workspace, slipnet, workspace.PickGroupInRange(i, i));
						coderack.AddCodelet(codelet);

						codelet = new GroupReasonRhythmGapCodelet((int)(100 - maxGroupStrength), null,
							coderack, workspace, slipnet, workspace.PickGroupInRange(i, i));
						coderack.AddCodelet(codelet);

						codelet = new GroupPenaltySubcomponentLengthCodelet((int)(100 - maxGroupStrength), null,
							coderack, workspace, slipnet, workspace.PickGroupInRange(i, i));
						coderack.AddCodelet(codelet);
					}
				}

				// Look for Analogies involving this measure. Do we need to search for more?.
				double avgAnalogyStrength, maxAnalogyStrength;
				int numAnalogies = workspace.CountAnalogiesInvolvingMeasure(i, out avgAnalogyStrength, out maxAnalogyStrength);

				if (numAnalogies < 1 || maxAnalogyStrength < 80 || avgAnalogyStrength < 50) {
					// Look for analogies.
					// TODO: how many to add?
					for (int j = 0; j < 4; j++) {
						analogyUrgency = Math.Max(5, (int)(100 - avgAnalogyStrength) - groupUrgency - linkUrgency);
						CreateAnalogyCodelet cac = new CreateAnalogyCodelet(analogyUrgency, null, coderack, workspace, slipnet);
						coderack.AddCodelet(cac);
					}
				}


				// Add breakers and groupers. Add NUM_CODELETS_TILL_NEW_HIGH_LEVEL if we are super unhappy, less if happier.		
				// TODO: how many to add?
				for (int j = 0; j < unhappiness / 100 * Constants.NUM_CODELETS_TILL_NEW_HIGH_LEVEL / 20; j++) {
					GroupBreakerCodelet gbc = new GroupBreakerCodelet(urgency, null, coderack, workspace, slipnet, workspace.PickGroupInRange(minMeasure, maxMeasure));
					coderack.AddCodelet(gbc);

					/*MeasureSamenessGrouperCodelet msgc = new MeasureSamenessGrouperCodelet(urgency, null, coderack, workspace, slipnet, workspace.PickMeasureLinkInRange(minMeasure, maxMeasure));
					coderack.AddCodelet(msgc);

					ProximityGrouperCodelet pgc = new ProximityGrouperCodelet(urgency, null, coderack, workspace, slipnet, workspace.PickGroupElementInRange(minMeasure, maxMeasure));
					coderack.AddCodelet(pgc);

					MetaGrouperCodelet mgc = new MetaGrouperCodelet(urgency, null, coderack, workspace, slipnet, workspace.PickGroupInRange(minMeasure, maxMeasure));
					coderack.AddCodelet(mgc);*/

					//CreateAnalogyCodelet cac = new CreateAnalogyCodelet(urgency, null, coderack, workspace, slipnet);
					//coderack.AddCodelet(cac);
				}

				//Generate/manage Expectations

			}
		}

		private static void TakeScreenshot(int measureNum) {
			if (GUIenabled) {
				workspace.TakeScreenshot(measureNum);
			}
		}

		private static void frmWorkspace_setProgamSpeedEvent(object sender, WorkspaceForm.SetProgramSpeedEventArgs e) {
			TIME_TO_SLEEP_BETWEEN_CODELETS = e.delay;
			
			// Compute the number of codelets that can be run within one drawing cycle.
			double avgCodeletTimeMs = coderack.AvgCodeletRunTimeMs + TIME_TO_SLEEP_BETWEEN_CODELETS; // Include the artificial delay!!
			
			if (avgCodeletTimeMs == 0)
				return;
			double maxNumCodeletsPerDraw =  2 * workspace.AvgDrawTimeMs / avgCodeletTimeMs;

			// We can probably run 2000 or so codelets in one draw, so this is just an upper bound, which we get if we move the slider (e.delay) all the way to 0 (superfast).
			// if we move the slider to the slowest setting (e.delay = 50), we scale back a LOT from this upper bound.
			// Convert slider into range [0,1].
			double percentMaxCodeletsRunPerDraw = (50 - e.delay) / 50.0;
			int MAX_CODELETS_UNDRAWN = 500;	// let's not allow anything faster than this.
			int numUntilDrawTmp = Math.Min( (int)(percentMaxCodeletsRunPerDraw * maxNumCodeletsPerDraw), MAX_CODELETS_UNDRAWN);
			NUM_CODELETS_UNTIL_DRAW = Math.Max(2, numUntilDrawTmp);	// maxe sure we wait at least 2 codelets to draw.
		}

		private static void frmWorkspace_setDetailLevelEvent(object sender, WorkspaceForm.SetDetailLevelEventArgs e) {
			DISPLAY_DETAIL_LEVEL = e.detail;
		}

		private static void frmWorkspace_restartEvent(object sender, WorkspaceForm.RestartEventArgs e) {

			System.Windows.Forms.Application.DoEvents();

			lock (restartLock) {
				// Set the flag to start the restart process.
				restarting = true;
				pausing = true;
				workspace.frmWorkspace.SetPauseButtonText("Go");
				
				// Unsubscribe the form-close events that will stop the program.
				workspace.UnsubscribeFormEventHandlers();

				// Save the input rhythm from the textbox.
				input = e.inputMeasureText;

				
				// Close the forms.
				//workspace.CloseForms();
			}
		}

		private static void frmWorkspace_pauseEvent(object sender, EventArgs e) {
			pausing = !pausing;

			if (pausing)
				workspace.frmWorkspace.SetPauseButtonText("Unpause");
			else
				workspace.frmWorkspace.SetPauseButtonText("Pause");
		}

		private static void Init() {
			pausing = true;
			TextWriter tw;
			if (GUIenabled) {
				// Get and update the program run number.
				try {
					TextReader tr = new StreamReader(@"C:\RhythmcatScreenshots\config.txt");
					string numStr = tr.ReadToEnd().Trim();
					tr.Close();
					programRunNum = int.Parse(numStr) + 1;
				} catch {
					programRunNum = 1;
				}
				tw = new StreamWriter(@"C:\RhythmcatScreenshots\config.txt", false);
				tw.WriteLine(programRunNum);
				tw.Close();
			}

			workspace = new Workspace(programRunNum, GUIenabled, curMelodyIndex);
			
			coderack = new Coderack(workspace);
			slipnet = new Slipnet(coderack, workspace);

			workspace.Coderack = coderack;
			workspace.Slipnet = slipnet;
			coderack.Slipnet = slipnet; // todo must be in this order to get coderack and slipnet linked to each other.

			if (GUIenabled) {
				workspace.CreateTheForms();
			}

			// Reset the big objects.
			workspace.Reset();
			coderack.Reset();
			slipnet.Reset();

			if (GUIenabled) {
				workspace.frmWorkspace.setProgramSpeedEvent += new EventHandler<WorkspaceForm.SetProgramSpeedEventArgs>(frmWorkspace_setProgamSpeedEvent);
				workspace.frmWorkspace.setDetailLevelEvent += new EventHandler<WorkspaceForm.SetDetailLevelEventArgs>(frmWorkspace_setDetailLevelEvent);
				workspace.frmWorkspace.restartEvent += new EventHandler<WorkspaceForm.RestartEventArgs>(frmWorkspace_restartEvent);
				workspace.frmWorkspace.pauseEvent += new EventHandler<EventArgs>(frmWorkspace_pauseEvent);
				workspace.Close += new EventHandler<EventArgs>(workspace_Close);
			}

			List<Assembly> assemblies = new List<Assembly>();   // the list of assemblies containing codelets.

			assemblies.Add(Assembly.GetExecutingAssembly());    // Adds the musicat assembly.

			//assemblies.Add(Assembly.GetAssembly(typeof(MeasureLinkerCodelet)));	// Add the Codelets assembly.

			// Prepopulate the coderack with the codelets found in the source code tagged with the CodeletAttribute.
			coderack.Assemblies = assemblies;
			coderack.Populate();

			if (GUIenabled) {
				// Write a file containing the list of active codelets for this run.
				tw = new StreamWriter(string.Format(@"C:\RhythmcatScreenshots\codelets-run-{0}.txt", programRunNum), false);
				foreach (string name in coderack.GetActiveCodeletNames())
					tw.WriteLine(name);
				tw.Close();
			}

			// Setup the slipnet.
			slipnet.Initialize();

			if (GUIenabled) {
				Thread.Sleep(100);

				lock (restartLock) {
					if (!restarting)
						workspace.Draw(coderack);
				}
			}
		}

		static void workspace_Close(object sender, EventArgs e) {
			if (!restarting) {
				// Stop the program.
				System.Windows.Forms.Application.Exit();
			}
		}

	}
}
