using System;
using NUnit.Framework;

namespace MusicCore.Tests
{
	/// <summary>
	/// Summary description for KeyTests.
	/// </summary>
	[TestFixture]
	public class KeyTests
	{
		public KeyTests()
		{
			//
			// TODO: Add constructor logic here
			//
		}
		[Test]
		public void Success() {
		}

		[Test]
		public void GetScaleDegreeTonicPitchTest() {
			Key key = new Key();	// C major.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(), 4);	//Tonic in the 4th octave.
			Assert.AreEqual(35, p.Number);	//Middle C.
			Assert.AreEqual(4, p.Octave);

			key.Fifths = -1;		// F major.
			p = key.GetScaleDegreePitch(new ScaleDegree(), 4);	//Tonic in the 4th octave.
			Assert.AreEqual(38, p.Number);	//Middle F.
			Assert.AreEqual(4, p.Octave);

			key.Fifths = -2;		// Bb.
			p = key.GetScaleDegreePitch(new ScaleDegree(), 4);	//Tonic in the 4th octave.
			Assert.AreEqual(41, p.Number);	//Middle Bb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);

			key.Fifths = -6;		// Gb.
			p = key.GetScaleDegreePitch(new ScaleDegree(), 4);	//Tonic in the 4th octave.
			Assert.AreEqual(39, p.Number);	//Middle Gb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);
		
			key.Fifths = -7;		// Cb.
			p = key.GetScaleDegreePitch(new ScaleDegree(), 4);	//Tonic in the 4th octave.
			Assert.AreEqual(35, p.Number);	//Middle Cb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);
		
			key.Fifths = 5;		// B.
			p = key.GetScaleDegreePitch(new ScaleDegree(), 5);	//Tonic in the 5th octave.
			Assert.AreEqual(48, p.Number);	//5th octave B.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);
			
			key.Fifths = 7;		// C#.
			p = key.GetScaleDegreePitch(new ScaleDegree(), 3);	//Tonic in the 3rd octave.
			Assert.AreEqual(28, p.Number);	//3rd octave C#.
			Assert.AreEqual(3, p.Octave);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);
		}

		[Test]
		public void GetScaleDegreePitchTest() 
		{
			Key key = new Key();	// C major.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(7, Alteration.None), 4);	//7th degree in the 4th octave.
			Assert.AreEqual(41, p.Number);	//Middle B.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);

			key.Fifths = 1;		// G major.
			p = key.GetScaleDegreePitch(new ScaleDegree(7, Alteration.None), 4);	//7th degree in the 4th octave.
			Assert.AreEqual(38, p.Number);	//Middle F#.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);

			key.Fifths = -1;	// F major.
			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Lowered), 4);	//Lowered 3rd degree in the 4th octave.
			Assert.AreEqual(40, p.Number);	//Middle Ab.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);

			key.Fifths = 7;	// C# Minor.
			key.Mode = KeyMode.Minor;
			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Lowered), 4);	//Lowered 3rd degree in the 4th octave.
			Assert.AreEqual(37, p.Number);	//Middle E.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);

			key.Fifths = 7;	// C# Minor.
			key.Mode = KeyMode.Minor;	 // Note: this should not affect this test.
			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.None), 4);	//Lowered 3rd degree in the 4th octave.
			Assert.AreEqual(37, p.Number);	//Middle E#.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);
	}

		[Test]
		public void GetScaleDegreePitchTestB() 
		{
			Key key = new Key(5, KeyMode.Major);	// B major.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Lowered), 4);	//Lowered 3rd degree in the 4th octave.
			Assert.AreEqual(36, p.Number);	//Middle D.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Diminished), 4);	//Diminished 3rd degree in the 4th octave.
			Assert.AreEqual(36, p.Number);	//Middle Db.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Raised), 4);	//Raised 3rd degree in the 4th octave.
			Assert.AreEqual(36, p.Number);	//Middle Dx.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.DoubleSharp, p.Accidental);
		
			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Augmented), 4);	//Augmented 3rd degree in the 4th octave.
			Assert.AreEqual(37, p.Number);	//Middle E#.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);
		}

		[Test]
		public void GetScaleDegreePitchTestCb() 
		{
			Key key = new Key(-7, KeyMode.Major);	// Cb major.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(1, Alteration.Raised), 4);	//Raised tonic degree in the 4th octave.
			Assert.AreEqual(35, p.Number);	//Middle C.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);		


			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.Lowered), 4);	//Lowered second degree in the 4th octave.
			Assert.AreEqual(36, p.Number);	//Middle Dbb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.DoubleFlat, p.Accidental);		
			
			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.None), 4);	//Normal 2nd degree in the 4th octave.
			Assert.AreEqual(36, p.Number);	//Middle Db.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);		

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.None), 4);	//Normal 3rd degree in the 4th octave.
			Assert.AreEqual(37, p.Number);	//Middle Eb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(4, Alteration.None), 4);	//Normal 4th degree in the 4th octave.
			Assert.AreEqual(38, p.Number);	//Middle Fb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	
		}

		[Test]
		public void GetScaleDegreePitchTestGb() 
		{
			Key key = new Key(-6, KeyMode.Major);	// Gb major.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(1, Alteration.Raised), 4);	//Raised tonic degree in the 4th octave.
			Assert.AreEqual(39, p.Number);	//Middle G.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);		


			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.Lowered), 4);	//Lowered second degree in the 4th octave.
			Assert.AreEqual(40, p.Number);	//Middle Abb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.DoubleFlat, p.Accidental);		
			
			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.None), 4);	//Normal 2nd degree in the 4th octave.
			Assert.AreEqual(40, p.Number);	//Middle Ab.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);		

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.None), 4);	//Normal 3rd degree in the 4th octave.
			Assert.AreEqual(41, p.Number);	//Middle Bb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(4, Alteration.None), 5);	//Normal 4th degree in the 5th octave.
			Assert.AreEqual(42, p.Number);	//Middle Cb.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(5, Alteration.None), 5);	//Normal 5th degree in the 5th octave.
			Assert.AreEqual(43, p.Number);	//Middle Db.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(6, Alteration.None), 5);	//Normal 6th degree in the 5th octave.
			Assert.AreEqual(44, p.Number);	//Middle Eb.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	
			
			p = key.GetScaleDegreePitch(new ScaleDegree(7, Alteration.None), 5);	//Normal 7th degree in the 5th octave.
			Assert.AreEqual(45, p.Number);	//Middle F.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);	
		}

		[Test]
		public void GetScaleDegreePitchTestEbMinor() 
		{
			Key key = new Key(-3, KeyMode.Minor);	// Eb Minor.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(1, Alteration.Raised), 4);	//Raised tonic degree in the 4th octave.
			Assert.AreEqual(37, p.Number);	//Middle E.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);		


			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.Lowered), 4);	//Lowered second degree in the 4th octave.
			Assert.AreEqual(38, p.Number);	//Middle Fb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);		
			
			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.None), 4);	//Normal 2nd degree in the 4th octave.
			Assert.AreEqual(38, p.Number);	//Middle F.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);		

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.Lowered), 4);	//Lowered 3rd degree in the 4th octave.
			Assert.AreEqual(39, p.Number);	//Middle Gb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.None), 4);	//Normal 3rd degree in the 4th octave.
			Assert.AreEqual(39, p.Number);	//Middle G.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(4, Alteration.None), 4);	//Normal 4th degree in the 4th octave.
			Assert.AreEqual(40, p.Number);	//Middle Ab.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(5, Alteration.None), 4);	//Normal 5th degree in the 4th octave.
			Assert.AreEqual(41, p.Number);	//Middle Bb.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(6, Alteration.None), 5);	//Normal 6th degree in the 5th octave.
			Assert.AreEqual(42, p.Number);	//C 5.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);	
			
			p = key.GetScaleDegreePitch(new ScaleDegree(6, Alteration.Lowered), 5);	//Lowered 6th degree in the 5th octave.
			Assert.AreEqual(42, p.Number);	//Cb 5.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	
			
			p = key.GetScaleDegreePitch(new ScaleDegree(7, Alteration.None), 5);	//Normal 7th degree in the 5th octave.
			Assert.AreEqual(43, p.Number);	//D 5.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);	

			p = key.GetScaleDegreePitch(new ScaleDegree(7, Alteration.Lowered), 5);	//Lowered 7th degree in the 5th octave.
			Assert.AreEqual(43, p.Number);	//Db 5.
			Assert.AreEqual(5, p.Octave);
			Assert.AreEqual(Accidental.Flat, p.Accidental);	

		}

		[Test]
		public void GetScaleDegreePitchTestFSharp() 
		{
			Key key = new Key(6, KeyMode.Major);	// F# major.

			Pitch p = key.GetScaleDegreePitch(new ScaleDegree(1, Alteration.Raised), 4);	//Raised tonic degree in the 4th octave.
			Assert.AreEqual(38, p.Number);	//Middle Fx.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.DoubleSharp, p.Accidental);		


			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.Lowered), 4);	//Lowered second degree in the 4th octave.
			Assert.AreEqual(39, p.Number);	//Middle G.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);		
			
			p = key.GetScaleDegreePitch(new ScaleDegree(2, Alteration.None), 4);	//Normal 2nd degree in the 4th octave.
			Assert.AreEqual(39, p.Number);	//Middle G#.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);		

			p = key.GetScaleDegreePitch(new ScaleDegree(3, Alteration.None), 4);	//Normal 3rd degree in the 4th octave.
			Assert.AreEqual(40, p.Number);	//Middle A#.
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);	

			// Critical test
			p = key.GetScaleDegreePitch(new ScaleDegree(4, Alteration.None), 4);	//Normal 4th degree in the 4th octave.
			Assert.AreEqual(41, p.Number);	//Middle B (this is critical to make sure we don't write "A#" instead.)
			Assert.AreEqual(4, p.Octave);
			Assert.AreEqual(Accidental.Natural, p.Accidental);	
		}


	[Test]
		public void GetSetFifthsTest() {
			Key key = new Key();
			Assert.AreEqual(0, key.Fifths);

			key.Fifths = 2;
			Assert.AreEqual(2, key.Fifths);

			key.Fifths = 0;
			Assert.AreEqual(0, key.Fifths);
			
			key.Fifths = -3;
			Assert.AreEqual(-3, key.Fifths);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void FifthsException1Test1(){
			Key key = new Key();
			key.Fifths = 8;
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void FifthsExceptionTest2(){
			Key key = new Key();
			key.Fifths = -8;
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void FifthsExceptionTest3(){
			Key key = new Key(9, KeyMode.Major);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void FifthsExceptionTest4(){
			Key key = new Key(-9, KeyMode.Major);
		}
		
		[Test]
		public void ConstructorTest(){
			Key key = new Key(-7, KeyMode.Minor);
			Assert.AreEqual(-7, key.Fifths);
			Assert.AreEqual(KeyMode.Minor, key.Mode);
		}

		[Test]
		public void ModeTest(){
			Key key = new Key();

			Assert.AreEqual(KeyMode.Major, key.Mode);
		
			key.Mode = KeyMode.Minor;
			Assert.AreEqual(KeyMode.Minor, key.Mode);
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void ModeExceptionTest1(){
			Key key = new Key();	
			key.Mode = (KeyMode)2;
		}

		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void ModeExceptionTest2(){
			Key key = new Key();	
			key.Mode = (KeyMode)(-1);
		}
		
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		[Test]
		public void ModeExceptionTest3(){
			Key key = new Key(0, (KeyMode)2);	
		}

		[Test]
		public void ToStringTest(){
			// Test "Ab Major", "E Minor", or "F# Major"
			Key key = new Key();

			key.Fifths = -4;
			key.Mode = KeyMode.Major;
			Assert.AreEqual("Ab Major", key.ToString());

			key.Fifths = 4;
			key.Mode = KeyMode.Minor;
			Assert.AreEqual("E Minor", key.ToString());

			key.Fifths = 6;
			key.Mode = KeyMode.Major;
			Assert.AreEqual("F# Major", key.ToString());

		}

	}
}
