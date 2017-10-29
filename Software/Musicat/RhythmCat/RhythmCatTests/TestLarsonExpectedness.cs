using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MusicPrimitives;
using RhythmCat;

namespace RhythmCatTests {
	[TestClass]
	public class TestLarsonExpectedness {
		[TestMethod]
		public void TestExpectationVector() {

			List<Note> notes = new List<Note>();
			notes.Add(new Note(1, false, false, new MidiInfo(60, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(62, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(64, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(65, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(67, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(69, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(71, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(72, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(72, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(71, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(69, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(68, Accidental.Flat)));
			notes.Add(new Note(1, false, false, new MidiInfo(67, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(65, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(64, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(62, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(61, Accidental.Flat)));
			notes.Add(new Note(1, false, false, new MidiInfo(60, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(59, Accidental.Natural)));
			notes.Add(new Note(1, false, false, new MidiInfo(60, Accidental.Natural)));

			Workspace w = new Workspace(-1, false, -1);

			Rhythm rhythm = new Rhythm(notes);
			Measure m = new Measure(w, rhythm);

			Group g = new Group(w);
			g.AddGroupElement(m);

			Key cmajor = new Key(0, KeyMode.Major);
			List<float> larson = g.GetNoteExpectednessLarson(cmajor, Alphabet.GetScaleAlphabet(cmajor));

			for (int i = 0; i < notes.Count; i++) {
				// Test for local min/max.
				if (i > 0 && i < notes.Count - 1) {
					if (larson[i - 1] < larson[i] && larson[i + 1] < larson[i])
						Console.Write("MAX: ");
					if (larson[i - 1] > larson[i] && larson[i + 1] > larson[i])
						Console.Write("MIN: ");
				}
				Console.WriteLine("{0}: {1:0.###}", notes[i].ToString(), larson[i]);
			}


		/*	List<ScaleDegree> degrees = new List<ScaleDegree>();
			degrees.Add(new ScaleDegree(1, Alteration.None));
			degrees.Add(new ScaleDegree(4, Alteration.None));

			List<Alphabet> result = Alphabet.GetConsistentAlphabetsForScaleDegrees(degrees, KeyMode.Major);

			Console.WriteLine("Major");
			foreach (Alphabet a in result) {
				Console.WriteLine(a.ToString());
			}

			result = Alphabet.GetConsistentAlphabetsForScaleDegrees(degrees, KeyMode.Minor);

			Console.WriteLine("Minor");
			foreach (Alphabet a in result) {
				Console.WriteLine(a.ToString());
			}
		*/

		}
	}
}
