using RhythmCat;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using MusicPrimitives;

namespace RhythmCatTests
{
    
    
    /// <summary>
    ///This is a test class for GroupTest and is intended
    ///to contain all GroupTest Unit Tests
    ///</summary>
	[TestClass()]
	public class GroupTest {


		private TestContext testContextInstance;

		/// <summary>
		///Gets or sets the test context which provides
		///information about and functionality for the current test run.
		///</summary>
		public TestContext TestContext {
			get {
				return testContextInstance;
			}
			set {
				testContextInstance = value;
			}
		}

		#region Additional test attributes
		// 
		//You can use the following additional attributes as you write your tests:
		//
		//Use ClassInitialize to run code before running the first test in the class
		//[ClassInitialize()]
		//public static void MyClassInitialize(TestContext testContext)
		//{
		//}
		//
		//Use ClassCleanup to run code after all tests in a class have run
		//[ClassCleanup()]
		//public static void MyClassCleanup()
		//{
		//}
		//
		//Use TestInitialize to run code before running each test
		//[TestInitialize()]
		//public void MyTestInitialize()
		//{
		//}
		//
		//Use TestCleanup to run code after each test has run
		//[TestCleanup()]
		//public void MyTestCleanup()
		//{
		//}
		//
		#endregion

		/// <summary>
		///A test for GetAlphabetsWithLikelihoods
		///</summary>
		[TestMethod()]
		public void GetAlphabetsWithLikelihoodsTest1() {
			Workspace workspace = new Workspace(-1, false, -1); // TODO: Initialize to an appropriate value

			int upbeatOffset;
			Key key;

			List<Measure> measures = Rhythm.ParseRhythmMeasures(workspace, "HG QA QB | QC ED EC QB QG", out upbeatOffset, out key);
			workspace.Key = key;

			
			Group target = new Group(workspace, measures[0], measures[1]);

			List<Tuple<Alphabet, float>> actual;
			
			actual = target.GetAlphabetsWithLikelihoods();

			Alphabet argmax = FindBestAlphabet(actual);

			Assert.AreEqual("C Major:G triad", argmax.Name);
			Assert.AreEqual(5, argmax.RootScaleDegree.Number);
			Assert.AreEqual(Alteration.None, argmax.RootScaleDegree.Alteration);
			Assert.AreEqual(7, argmax.RootPitchClass);
		}

		/// <summary>
		///A test for GetAlphabetsWithLikelihoods
		///</summary>
		[TestMethod()]
		public void GetAlphabetsWithLikelihoodsTest2() {
			Workspace workspace = new Workspace(-1, false, -1); // TODO: Initialize to an appropriate value

			int upbeatOffset;
			Key key;

			List<Measure> measures = Rhythm.ParseRhythmMeasures(workspace, "HG QF# QB | QC ED EC QB QG", out upbeatOffset, out key);
			workspace.Key = key;


			Group target = new Group(workspace, measures[0], measures[1]);

			List<Tuple<Alphabet, float>> actual;

			actual = target.GetAlphabetsWithLikelihoods();

			Alphabet argmax = FindBestAlphabet(actual);

			Assert.AreEqual("C Major:G triad", argmax.Name);
			Assert.AreEqual(5, argmax.RootScaleDegree.Number);
			Assert.AreEqual(Alteration.None, argmax.RootScaleDegree.Alteration); 
			Assert.AreEqual(7, argmax.RootPitchClass);
		}

		private static Alphabet FindBestAlphabet(List<Tuple<Alphabet, float>> actual) {
			// Pick out max.
			float max = -1;
			Alphabet argmax = null;
			foreach (Tuple<Alphabet, float> x in actual) {
				if (x.Item2 > max) {
					max = x.Item2;
					argmax = x.Item1;
				}
			}
			return argmax;
		}
	}
}
