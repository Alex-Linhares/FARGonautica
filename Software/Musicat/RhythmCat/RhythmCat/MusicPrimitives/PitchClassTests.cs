using System;
using NUnit.Framework;
using MusicatCore;

namespace MusicPrimitives.Tests
{
	/// <summary>
	/// Summary description for PitchClassTests.
	/// </summary>
	[TestFixture]
	public class PitchClassTests
	{
		public PitchClassTests()
		{
			//
			// TODO: Add constructor logic here
			//
		}

		[Test]
		public void Success() 
		{
		}

		[Test]
		public void C() 
		{
			PitchClass pc = new PitchClass(0, Accidental.Natural);

			Assert.AreEqual('C', pc.Name);
			Assert.AreEqual("C", pc.ToString());
		}

		[Test]
		public void Eb() 
		{
			PitchClass pc = new PitchClass(2, Accidental.Flat);

			Assert.AreEqual('E', pc.Name);
			Assert.AreEqual("Eb", pc.ToString());
		}
		
		[Test]
		public void Bx() 
		{
			PitchClass pc = new PitchClass(6, Accidental.DoubleSharp);

			Assert.AreEqual('B', pc.Name);
			Assert.AreEqual("Bx", pc.ToString());
		}

		[Test]
		public void NameConstructor() 
		{
			PitchClass pc = new PitchClass('A', Accidental.Flat);

			Assert.AreEqual('A', pc.Name);
			Assert.AreEqual("Ab", pc.ToString());
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void ConstructorRange() 
		{
			PitchClass pc = new PitchClass(7, Accidental.Sharp);
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void NameConstructorRange() 
		{
			PitchClass pc = new PitchClass('H', Accidental.Flat);
		}
	}
}
