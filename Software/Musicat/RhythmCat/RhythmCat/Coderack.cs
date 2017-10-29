using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Reflection;

namespace RhythmCat {

	public class CoderackEventArgs : EventArgs {
		public Coderack coderack;

		public CoderackEventArgs(Coderack coderack) {
			this.coderack = coderack;
		}
	}


	public class CoderackRunCodeletEventArgs : EventArgs {
		public Codelet codelet;

		public CoderackRunCodeletEventArgs(Codelet codelet) {
			this.codelet = codelet;
		}
	}


	public class Coderack : IEnumerable {
		private Workspace workspace;
		private Slipnet slipnet;

		public Slipnet Slipnet {
			set { slipnet = value; }
		}
		private List<Assembly> assemblies;

		public List<Assembly> Assemblies {
			set { assemblies = value; }
		}

		private List<Codelet> codelets;
		private List<Codelet> codelets_finished;
		private int _num_codelets_killed;

		private double totalCodeletRunTimeMsWindowed;		// total run time in a recent window
		private int totalNumCodeletsRunWindowed;		// total # run in recent window.  (for moving average stats)
		private double totalCodeletRunTimeMs;			// total run time all codelets.
		private int totalNumCodeletsRun;				// total # all codelets run.
		private int maxSize = 0;						// max size attained by coderack
		private int maxSizeTime = -1;

		public double AvgCodeletRunTimeMs {
			get {
				if (totalNumCodeletsRunWindowed == 0)
					return 0;
				else
					return totalCodeletRunTimeMsWindowed / totalNumCodeletsRunWindowed;
			}
		}
		private double _sum_codelet_age_at_run;

		public double AverageCodeletAgeAtRun {
			get {
				if (codelets_finished.Count == 0)
					return 0;
				return _sum_codelet_age_at_run / codelets_finished.Count;
			}

		}

		public double PercentCodeletsKilled {
			get {
				if (codelets_finished.Count + codelets.Count + _num_codelets_killed == 0)
					return 0;

				return 100.0 * _num_codelets_killed / (codelets_finished.Count + codelets.Count + _num_codelets_killed);
			}
		}
		private readonly object _codeletsLock = new object();

		public object CodeletsLock {
			get { return _codeletsLock; }
		}


		private List<string> disabled_codelet_names;

		public List<string> DisabledCodeletNames {
			get { return disabled_codelet_names; }
		}


		public List<string> CodeletNames {
			get {
				List<string> names = new List<string>();

				foreach (Assembly asm in assemblies) {
					foreach (Type t in asm.GetExportedTypes()) {
						foreach (Attribute a in t.GetCustomAttributes(false)) {
							CodeletAttribute ca = a as CodeletAttribute;
							if (ca != null) {
								if (ca.Active) {
									Codelet c = (Codelet)asm.CreateInstance(t.FullName, false, BindingFlags.CreateInstance, null,
											new object[] { ca.DefaultUrgency, null, this, workspace, slipnet },
											null, null);
									names.Add(c.Name);
								} else {
									throw new Exception("Deprecated: Codelet disabled in attribute: " + t.FullName);
								}
							}
						}
					}
				}

				return names;
			}
		}

		public List<string> EnabledCodeletNames {
			get {
				List<string> names = new List<string>();

				foreach (Assembly asm in assemblies) {
					foreach (Type t in asm.GetExportedTypes()) {
						foreach (Attribute a in t.GetCustomAttributes(false)) {
							CodeletAttribute ca = a as CodeletAttribute;
							if (ca != null) {
								if (ca.Active) {
									Codelet c = (Codelet)asm.CreateInstance(t.FullName, false, BindingFlags.CreateInstance, null,
											new object[] { ca.DefaultUrgency, null, this, workspace, slipnet },
											null, null);
									if (disabled_codelet_names.Contains(c.Name))
										break;
									else
										names.Add(c.Name);
								} else {
									throw new Exception("Deprecated: Codelet disabled in attribute: " + t.FullName);
								}
							}
						}
					}
				}

				return names;
			}
		}

		public event EventHandler<CoderackEventArgs> Update;
		public event EventHandler<CoderackRunCodeletEventArgs> RunCodeletEvent;

		public Coderack(Workspace workspace) {
			this.workspace = workspace;

			this.disabled_codelet_names = Utilities.DisabledCodelets();

			_sum_codelet_age_at_run = 0;

		}

		public void ClearTraceEventHandlers() {
			RunCodeletEvent = null;
		}

		public void Reset() {
			codelets = new List<Codelet>(Constants.MAX_CODERACK_SIZE + 50);
			codelets_finished = new List<Codelet>(50000);
			_sum_codelet_age_at_run = 0;
			_num_codelets_killed = 0;
		}

		public List<string> GetActiveCodeletNames() {
			List<string> names = new List<string>();

			foreach (Assembly asm in assemblies) {
				foreach (Type t in asm.GetExportedTypes()) {
					foreach (Attribute a in t.GetCustomAttributes(false)) {
						CodeletAttribute ca = a as CodeletAttribute;
						if (ca != null) {
							if (ca.Active) {
								Codelet c = (Codelet)asm.CreateInstance(t.FullName, false, BindingFlags.CreateInstance, null,
											new object[] { ca.DefaultUrgency, null, this, workspace, slipnet },
											null, null);
								names.Add(c.Name);
							}
						}
					}
				}
			}
			return names;
		}

		public void Populate() {
			//Reset();
			

			if (Constants.SHOW_CODERACK_URGENCIES_CONSOLE) {
				Log("Coderack size: " + codelets.Count.ToString());
				
				// Show % of each type.
				SortedDictionary<string, double> urgencies = new SortedDictionary<string, double>();
				double total = 0;
				foreach (Codelet c in codelets) {
					if (urgencies.ContainsKey(c.Name))
						urgencies[c.Name] += c.Urgency;
					else
						urgencies[c.Name] = c.Urgency;
					total += c.Urgency;
				}

				Log("\nCoderack state:");


				foreach (KeyValuePair<string, double> p in urgencies) {
					Console.WriteLine("{0:0.0}%:\t{1}", 100.0 * p.Value / total, p.Key);
				}
			}







			///


			// Initialize Coderack with the codelets we find via reflection.
			// TODO: Do this at compile-time, with PostSharp
			int numAdded = 0;
			foreach (Assembly asm in assemblies) {
				foreach (Type t in asm.GetExportedTypes()) {
					foreach (Attribute a in t.GetCustomAttributes(false)) {
						CodeletAttribute ca = a as CodeletAttribute;
						if (ca != null) {
							if (ca.Active) {
								// Add 1 copy of each codelet.
								for (int i = 0; i < 1; i++)
									lock (CodeletsLock) {
										Codelet c = (Codelet)asm.CreateInstance(t.FullName, false, BindingFlags.CreateInstance, null,
											new object[] { ca.DefaultUrgency, null, this, workspace, slipnet },
											null, null);
										c.PostTime = workspace.CurrentTime;
										codelets.Add(c);
										numAdded++;
									}
							} else {
								//throw new Exception("Deprecated: Codelet disabled in attribute: " + t.FullName);
							}
						}
					}
				}
			}
			Log("Number of codelets added in populate: " + numAdded.ToString());

			UpdateView();
		}

		/// <summary>
		/// Remove oldest codelets until the coderack is the maximum size.
		/// </summary>
		public void RemoveOld() {
			Log("Number of codelets before killing old: " + codelets.Count);
			lock (CodeletsLock) {
				while (codelets.Count > Constants.MAX_CODERACK_SIZE) {
					// Remove the oldest codelet.
					int max = codelets[0].Age;
					int argmax = 0;
					for (int i = 1; i < codelets.Count; i++) {
						Codelet c = codelets[i];
						if (c.Age > max) {
							max = c.Age;
							argmax = i;
						}
					}
					codelets.RemoveAt(argmax);
					_num_codelets_killed++;
				}
			}
			Log("Number of codelets after killing old: " + codelets.Count);
			Log("Max coderack size: " + maxSize.ToString() + " at time: " + totalNumCodeletsRun.ToString());

		}

		/// <summary>
		/// Picks a codelet based on urgency values, and runs it.
		/// Codelet is removed from rack when it starts running.
		/// TODO: could implement more efficiently if we persist the cumulative distribution
		/// values and update when codelets are removed.
		/// TODO: run in parallel instead of serial.
		/// </summary>
		public void RunNextCodelet() {
			// Remove any disabled codelet types.
			for (int i = 0; i < codelets.Count; i++) {  // codelets.Count must not be optimized away, or this deletion breaks.
				string name = codelets[i].Name;
				if (disabled_codelet_names.Contains(name)) {
					lock (CodeletsLock) {
						codelets.RemoveAt(i);
					}
					i--;
				}
			}

			double[] dist = new double[codelets.Count]; // stores the cumulative urgency distribution

			if (codelets.Count == 0) {
				Console.WriteLine("Out of Codelets!!!");
				return;
			}

			// Build cumulative prob. distribution.
			dist[0] = codelets[0].Urgency;
			for (int i = 1; i < codelets.Count; i++)
				dist[i] = dist[i - 1] + codelets[i].Urgency;

			double max = dist[codelets.Count - 1];

			// Choose a random-weighted codelet.
			double r = Utilities.rand.NextDouble() * max;	// generates a num from 0 to max

			for (int i = 0; i < codelets.Count; i++) {
				if (r < dist[i] + 0.00000001) {
					// Remove the winner from the to-run list.
					Codelet c = codelets[i];
					lock (CodeletsLock) {
						codelets.RemoveAt(i);
					}
					UpdateView();

					// Start timer.
					DateTime startTime = DateTime.Now;

					// Run this codelet.
					SendRunCodeletEvent(c);
					//Log("Running codelet " + c.ToString() + ". Age = " + c.Age + "Generation = " + c.Generation);

#if DEBUG
					string groupsBefore = workspace.GetGroupsString();
#endif

					c.Run();

					TimeSpan duration = DateTime.Now - startTime;
					double durMs =  duration.TotalMilliseconds;
					totalCodeletRunTimeMs += durMs;
					totalCodeletRunTimeMsWindowed += durMs;
					totalNumCodeletsRun++;
					totalNumCodeletsRunWindowed++;
					double avgTimeWindowed = totalCodeletRunTimeMsWindowed / totalNumCodeletsRunWindowed;
					if (totalNumCodeletsRunWindowed == 40) {	// rewindow every 30 codelets, keeping time of previous 10 to smooth out the average.
						totalCodeletRunTimeMsWindowed = 10 * avgTimeWindowed;
						totalNumCodeletsRunWindowed = 10;
						if (Constants.SHOW_CODELET_RUNTIME_CONSOLE)
							Console.WriteLine("Avg. codelet running time in ms in prev 30 codelets: {0}", avgTimeWindowed);
					}

					// Add to the completed list.
					codelets_finished.Add(c);
					_sum_codelet_age_at_run += c.Age;

					UpdateView();

					slipnet.UpdateView();
					//workspace.UpdateView();

#if DEBUG
					// Run a workspace consistency check.
					if (!workspace.verifyConsistent()) {
						Utilities.LogtoFile("\nFailed Consistency Check after codelet:");
						Utilities.LogtoFile(c.ToString());
						Utilities.LogtoFile("Groups Before:");
						Utilities.LogtoFile(groupsBefore);
						Utilities.LogtoFile("Groups After:");
						Utilities.LogtoFile(workspace.GetGroupsString());

						System.Diagnostics.Debugger.Break();
					}
#endif
					break;
				}
			}
		}

		public void AddCodelet(Codelet c) {
			// Set coderack post time.
			c.PostTime = workspace.CurrentTime;
			// Add to coderack.
			lock (CodeletsLock) {
				codelets.Add(c);
			}
			UpdateView();
			if (codelets.Count > maxSize) {
				maxSize = codelets.Count;
				maxSizeTime = totalNumCodeletsRun;
			}
		}

		public void AddCodelet(Codelet c, int numCopies) {
			for (int i = 0; i < numCopies; i++)
				AddCodelet((Codelet)c.Clone());
		}

		public int Count {
			get {
				return codelets.Count;
			}
		}

		public int CountCodeletsOfType(Type type) {
			int count = 0;
			lock (CodeletsLock) {
				foreach (Codelet c in codelets) {
					if (c.GetType() == type)
						count++;
				}
			}
			return count;
		}

		public double GetTotalUrgencyForCodeletsOfType(Type type) {
			double sum = 0;
			lock (CodeletsLock) {
				foreach (Codelet c in codelets) {
					if (c.GetType() == type)
						sum += c.Urgency;
				}
			}
			return sum;
		}

		public double GetTotalUrgency() {
			double sum = 0;
			lock (CodeletsLock) {
				foreach (Codelet c in codelets) {
					sum += c.Urgency;
				}
			}
			return sum;
		}

		public Codelet this[int index] {
			get {
				return codelets[index];
			}
		}


		#region IEnumerable Members

		public IEnumerator GetEnumerator() {
			return codelets.GetEnumerator();
		}

		#endregion

		private void Log(string s) {
			Console.WriteLine(s);
		}


		/// <summary>
		/// Raise an event to let viewers update their views.
		/// </summary>
		private void UpdateView() {
			// Verify we have subsubscribed event handlers.
			if (Update != null) {
				CoderackEventArgs ea = new CoderackEventArgs(this);
				Update(this, ea);
			}
		}

		// Modify the urgency of all codelets of the given type.
		public void ModifyCodeletUrgencies(Codelet c, double urgency) {
			lock (CodeletsLock) {
				foreach (Codelet c2 in codelets) {
					if (c.GetType() == c2.GetType()) {
						c2.Urgency = urgency;
					}
				}
			}
			UpdateView();
		}



		/// <summary>
		/// Raise an event to let viewers update their views.
		/// </summary>
		private void SendRunCodeletEvent(Codelet c) {
			// Verify we have subsubscribed event handlers.
			if (RunCodeletEvent != null) {
				CoderackRunCodeletEventArgs ea = new CoderackRunCodeletEventArgs(c);
				RunCodeletEvent(this, ea);
			}
		}

		public void UpdateDisabledCodeletList() {
			this.disabled_codelet_names = Utilities.DisabledCodelets();
		}

		public int CountTopDownCodelets() {
			int count = 0;
			lock (CodeletsLock) {
				foreach (Codelet c in codelets) {
					Type t = c.GetType();
					object[] attrs = t.GetCustomAttributes(typeof(TopDownCodeletAttribute), false);
					foreach (Attribute attr in attrs) {
						if (attr is TopDownCodeletAttribute) {
							count++;
							break;
						}
					}
				}
			}
			return count;
		}

		public double GetTotalTopDownUrgency() {
			double sum = 0;
			lock (CodeletsLock) {
				foreach (Codelet c in codelets) {
					Type t = c.GetType();
					object[] attrs = t.GetCustomAttributes(typeof(TopDownCodeletAttribute), false);
					foreach (Attribute attr in attrs) {
						if (attr is TopDownCodeletAttribute) {
							sum += c.Urgency;
							break;
						}
					}
				}
			}
			return sum;
		}


		public override string ToString() {
			StringBuilder sb = new StringBuilder();

			if (codelets != null) {
				foreach (Codelet c in codelets) {
					sb.AppendLine(c.ToString());
				}
			}
			return sb.ToString();
		}
	}
}
