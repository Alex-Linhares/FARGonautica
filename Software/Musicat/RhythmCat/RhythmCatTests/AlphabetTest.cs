using System;
using System.Text;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MusicPrimitives;
using RhythmCat;

namespace RhythmCatTests {
	[TestClass]
	public class AlphabetTest {
		[TestMethod]
		public void TestGetAlphabets() {
			List<ScaleDegree> degrees = new List<ScaleDegree>();
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
		
		}

		/// <summary>
		///A test for GetMajorScaleAlphabetWithTriadOnDegree
		///</summary>
		[TestMethod()]
		public void GetMajorScaleAlphabetWithTriadOnDegreeTest() {
			Key key = new Key(); 
			int degree = 5; 
			
			Alphabet a;
			a = Alphabet.GetMajorScaleAlphabetWithTriadOnDegree(key, degree);
			Assert.AreEqual(1, a[0].Number);
			Assert.AreEqual(false, a[0].IsStable);

			Assert.AreEqual(2, a[1].Number);
			Assert.AreEqual(true, a[1].IsStable);

			Assert.AreEqual(3, a[2].Number);
			Assert.AreEqual(false, a[2].IsStable);

			Assert.AreEqual(4, a[3].Number);
			Assert.AreEqual(false, a[3].IsStable);

			Assert.AreEqual(5, a[4].Number);
			Assert.AreEqual(true, a[4].IsStable);

			Assert.AreEqual(6, a[5].Number);
			Assert.AreEqual(false, a[5].IsStable);

			Assert.AreEqual(7, a[6].Number);
			Assert.AreEqual(true, a[6].IsStable);
		}

		/// <summary>
		///A test for GetAlphabetFromScaleWithTriadOnDegree
		///</summary>
		[TestMethod()]
		public void GetAlphabetFromScaleWithTriadOnDegreeTest() {
			Key key = new Key(0, KeyMode.Minor);	// C minor
			Alphabet scale = Alphabet.GetScaleAlphabet(key);
			ScaleDegree sd = new ScaleDegree(3, Alteration.Lowered);
			
			Alphabet a = Alphabet.GetAlphabetFromScaleWithTriadOnDegree(key, scale, sd);

			Assert.AreEqual(1, a[0].Number);
			Assert.AreEqual(Alteration.None, a[0].Alteration);
			Assert.AreEqual(false, a[0].IsStable);

			Assert.AreEqual(2, a[1].Number);
			Assert.AreEqual(Alteration.None, a[1].Alteration);
			Assert.AreEqual(false, a[1].IsStable);
			
			Assert.AreEqual(3, a[2].Number);
			Assert.AreEqual(Alteration.Lowered, a[2].Alteration);
			Assert.AreEqual(true, a[2].IsStable);

			Assert.AreEqual(4, a[3].Number);
			Assert.AreEqual(Alteration.None, a[3].Alteration);
			Assert.AreEqual(false, a[3].IsStable);

			Assert.AreEqual(5, a[4].Number);
			Assert.AreEqual(Alteration.None, a[4].Alteration);
			Assert.AreEqual(true, a[4].IsStable);

			Assert.AreEqual(6, a[5].Number);
			Assert.AreEqual(Alteration.Lowered, a[5].Alteration);
			Assert.AreEqual(false, a[5].IsStable);

			Assert.AreEqual(7, a[6].Number);
			Assert.AreEqual(Alteration.Lowered, a[6].Alteration);
			Assert.AreEqual(true, a[6].IsStable);

			key = new Key(-1, KeyMode.Minor);	// F minor
			scale = Alphabet.GetScaleAlphabet(key);
			sd = new ScaleDegree(2, Alteration.None);

			a = Alphabet.GetAlphabetFromScaleWithTriadOnDegree(key, scale, sd);



			Assert.AreEqual("F", key.GetScaleDegreePitch(a[0], 4).ToString());
			Assert.AreEqual(1, a[0].Number);
			Assert.AreEqual(Alteration.None, a[0].Alteration);
			Assert.AreEqual(false, a[0].IsStable);

			Assert.AreEqual("G", key.GetScaleDegreePitch(a[1], 4).ToString());
			Assert.AreEqual(2, a[1].Number);
			Assert.AreEqual(Alteration.None, a[1].Alteration);
			Assert.AreEqual(true, a[1].IsStable);

			Assert.AreEqual("Ab", key.GetScaleDegreePitch(a[2], 4).ToString());
			Assert.AreEqual(3, a[2].Number);
			Assert.AreEqual(Alteration.Lowered, a[2].Alteration);
			Assert.AreEqual(false, a[2].IsStable);

			Assert.AreEqual("Bb",key.GetScaleDegreePitch(a[3],4).ToString());
			Assert.AreEqual(4, a[3].Number);
			Assert.AreEqual(Alteration.None, a[3].Alteration);
			Assert.AreEqual(true, a[3].IsStable);

			Assert.AreEqual("C", key.GetScaleDegreePitch(a[4], 4).ToString());
			Assert.AreEqual(5, a[4].Number);
			Assert.AreEqual(Alteration.None, a[4].Alteration);
			Assert.AreEqual(false, a[4].IsStable);

			Assert.AreEqual("Db", key.GetScaleDegreePitch(a[5], 4).ToString());
			Assert.AreEqual(6, a[5].Number);
			Assert.AreEqual(Alteration.Lowered, a[5].Alteration);
			Assert.AreEqual(true, a[5].IsStable);

			Assert.AreEqual("Eb", key.GetScaleDegreePitch(a[6], 4).ToString());
			Assert.AreEqual(7, a[6].Number);
			Assert.AreEqual(Alteration.Lowered, a[6].Alteration);
			Assert.AreEqual(false, a[6].IsStable);
		}
	}
}
