using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace GroundTruth {
	public struct GroundTruthExample {
		public string name;
		public string input;
		public List<ExpectedGroup> groups;
		public List<ExpectedAnalogy> analogies;
		public List<ExpectedSequence> sequences;
	}

	public struct ExpectedGroup {
		public int startMeasure;  //0-based
		public int endMeasure;	 //0-based

		public ExpectedGroup(string groupString) {
			// Parse the string.
			string[] tokens = groupString.Split(new char[] { '-' });
			if (tokens.Length != 2)
				throw new ArgumentException("Can't read group string " + groupString);
			if (!(int.TryParse(tokens[0], out startMeasure)))
				throw new ArgumentException("Can't parse group start meaure " + tokens[0].ToString() + " in " + groupString);
			if (!(int.TryParse(tokens[1], out endMeasure)))
				throw new ArgumentException("Can't parse group end meaure " + tokens[0].ToString() + " in " + groupString);
			// Adjust to 0-based indexing.
			startMeasure--;
			endMeasure--;
		}
	}

	public struct ExpectedAnalogy {
		public ExpectedGroup LHS;
		public ExpectedGroup RHS;

		public ExpectedAnalogy(string analogyString) {
			// Parse the string.
			string[] tokens = analogyString.Split(new char[] { '=' });
			if (tokens.Length != 2)
				throw new ArgumentException("Can't read analogy string " + analogyString);

			string left = tokens[0];
			string right = tokens[1];
			LHS = new ExpectedGroup(left);
			RHS = new ExpectedGroup(right);
		}
	}


	public struct ExpectedSequence {
		public int startMeasure;  //0-based
		public int endMeasure;	 //0-based

		public ExpectedSequence(string sequenceString) {
			// Parse the string.
			string[] tokens = sequenceString.Split(new char[] { '-' });
			if (tokens.Length != 2)
				throw new ArgumentException("Can't read sequence string " + sequenceString);
			if (!(int.TryParse(tokens[0], out startMeasure)))
				throw new ArgumentException("Can't parse sequence start meaure " + tokens[0].ToString() + " in " + sequenceString);
			if (!(int.TryParse(tokens[1], out endMeasure)))
				throw new ArgumentException("Can't parse sequence end meaure " + tokens[0].ToString() + " in " + sequenceString);
			// Adjust to 0-based indexing.
			startMeasure--;
			endMeasure--;
		}
	}

	public static class Parser {

		private enum ParserState {
			None,
			Repetitions,
			Name,
			Input,
			OutputGroups,
			OutputAnalogies,
			OutputSequences
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="groundTruthFilePath"></param>
		/// <returns>True on success, false otherwise</returns>
		public static bool Parse(string groundTruthFilePath, out int numRepetitions, out List<GroundTruthExample> groundTruthExamples) {
			groundTruthExamples = new List<GroundTruthExample>();
			numRepetitions = 1;
			ParserState parserState = ParserState.None;

			try {
				TextReader tr = File.OpenText(groundTruthFilePath);
				string line;

				// Start a new example.
				GroundTruthExample example = new GroundTruthExample();
                example.groups = new List<ExpectedGroup>();
                example.analogies = new List<ExpectedAnalogy>();
                example.sequences = new List<ExpectedSequence>();
											

				while ((line = tr.ReadLine()) != null) {
					// Look for a blank line, denoting a new example.
					if (line.Trim() == "") {
						parserState = ParserState.None;
						continue;
					}

					// Look for a section heading command.
					if ( line[0] == '[') {
						ParserState prevParserState = parserState;
						string state = line.Substring(1, line.Length - 2);
						switch (state) {
							case "name":
								parserState = ParserState.Name;
								break;
							case "input":
								parserState = ParserState.Input;
								break;
							case "output-groups":
								parserState = ParserState.OutputGroups;
								example.groups = new List<ExpectedGroup>();
								break;
							case "output-analogies":
								parserState = ParserState.OutputAnalogies;
								example.analogies = new List<ExpectedAnalogy>();
								break;
							case "output-sequences":
								parserState = ParserState.OutputSequences;
								example.sequences = new List<ExpectedSequence>();
								break;
							case "repetitions":
								parserState = ParserState.Repetitions;
								break;
							default:
								throw new ArgumentException("Parse error: Unknown command: " + state);
						}
						if (prevParserState == ParserState.None) {
							if (example.input != null)
								groundTruthExamples.Add(example);
							example = new GroundTruthExample();
                            example.groups = new List<ExpectedGroup>();
                            example.analogies = new List<ExpectedAnalogy>();
                            example.sequences = new List<ExpectedSequence>();
						}

						continue;
					}


					// Try to parse the line; now we're inside a section for the current example.
					switch (parserState) {
						case ParserState.Repetitions:
							if (!int.TryParse(line, out numRepetitions))
								throw new ArgumentException("Could not parse repetitions=" + line);
							break;
						case ParserState.Name:
							example.name = line.Trim();
							break;
						case ParserState.Input:
							example.input = line.Trim();
							break;
						case ParserState.OutputGroups:
							example.groups.Add(new ExpectedGroup(line.Trim()));
							break;
						case ParserState.OutputAnalogies:
							example.analogies.Add(new ExpectedAnalogy(line.Trim()));
							break;
						case ParserState.OutputSequences:
							example.sequences.Add(new ExpectedSequence(line.Trim()));
							break;
					}

				} // Read all lines of file.

				// Add the final example.
				groundTruthExamples.Add(example);

			} catch {
				return false;
			}

			return true;
		}

	}
}
