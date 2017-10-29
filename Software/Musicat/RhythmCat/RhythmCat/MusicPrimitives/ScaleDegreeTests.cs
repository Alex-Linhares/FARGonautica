using System;
using NUnit.Framework;
using MusicatCore;

namespace MusicPrimitives.Tests
{
	/// <summary>
	/// Summary description for ScaleDegreeTests.
	/// </summary>
	[TestFixture]
	public class ScaleDegreeTests
	{
		public ScaleDegreeTests()
		{
			//
			// TODO: Add constructor logic here
			//
		}

		[Test]
		public void ScaleDegreeConstructor(){
			ScaleDegree d = new ScaleDegree(4, Alteration.Raised);

			Assert.AreEqual(4, d.Number);
			Assert.AreEqual(Alteration.Raised, d.Alteration);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void ConstructorNumberException(){
			ScaleDegree d = new ScaleDegree(8, Alteration.None);
		}

		[Test]
		public void NumberSetGet(){
			ScaleDegree d = new ScaleDegree();
			Assert.AreEqual(1, d.Number);

			d.Number = 7;
			Assert.AreEqual(7, d.Number);
			d.Number = 1;
			Assert.AreEqual(1, d.Number);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void NumberException1(){
			ScaleDegree d = new ScaleDegree();

			d.Number = 0;
		}
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void NumberException2(){
			ScaleDegree d = new ScaleDegree();

			d.Number = 8;
		}

		[Test]
		public void AccidentalGetSet() {
			ScaleDegree d = new ScaleDegree();
			Assert.AreEqual(Alteration.None, d.Alteration);

			d.Alteration = Alteration.Augmented;
			Assert.AreEqual(Alteration.Augmented, d.Alteration);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void AlterationException1() {
			ScaleDegree d = new ScaleDegree(1, (Alteration)5);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void AlterationException2() {
			ScaleDegree d = new ScaleDegree(1, (Alteration)(-1));
		}
	}
}
