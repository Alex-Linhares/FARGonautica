using RhythmCat;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace RhythmCatTests
{
	
	
	/// <summary>
	///This is a test class for MeasureTest and is intended
	///to contain all MeasureTest Unit Tests
	///</summary>
	[TestClass()]
	public class MeasureTest {


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
		///A test for GetAttackPointLevels
		///</summary>
		[TestMethod()]
		public void GetAttackPointLevelsTest44() {
			int octave = 4;
			bool tie;
			Workspace w = new Workspace(-1, false, -1);
			Measure target = new Measure(w, new Rhythm("QC QC QC QC", false, out tie, ref octave));
			int[] expected = {4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0}; 
			int[] actual;
			actual = target.GetAttackPointLevels();
			CollectionAssert.AreEqual(expected, actual);
		}

		/// <summary>
		///A test for GetAttackPointLevels
		///</summary>
		[TestMethod()]
		public void GetAttackPointLevelsTest34() {
			int octave = 4;
			bool tie;
			Workspace w = new Workspace(-1, false, -1);

			Measure target = new Measure(w, new Rhythm("QC QC QC", false, out tie, ref octave));
			int[] expected = { 4, 0, 1, 0, 2, 0, 1, 0, 2, 0, 1, 0};
			int[] actual;
			actual = target.GetAttackPointLevels();
			CollectionAssert.AreEqual(expected, actual);
		}
	}
}
