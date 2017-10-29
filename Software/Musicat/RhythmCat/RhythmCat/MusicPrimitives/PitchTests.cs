using System;
using MusicPrimitives;
using NUnit.Framework;
using MusicatCore;

namespace MusicPrimitives.Tests {
	/// <summary>
	/// Summary description for PitchTests.
	/// </summary>
	[TestFixture]
	public class PitchTests {
		public PitchTests() {
		}

		[SetUp]
		public void Init() {

		}

		[TearDown]
		public void Destroy() {

		}

		[Test]
		public void Success() {
		}

		[Test]
		public void NumberGetSet() {
			Pitch p = new Pitch();
			Assert.AreEqual(0, p.Number);

			p.Number = 74;
			Assert.AreEqual(74, p.Number);

			Pitch p2 = new Pitch(94, Accidental.DoubleSharp);
			Assert.AreEqual(94, p2.Number);

			p2.Number = 3;
			Assert.AreEqual(3, p2.Number);
		}

		[Test]
		public void NameGet() {
			Pitch p = new Pitch();

			p.Number = 35;
			Assert.AreEqual('C', p.Name, "Number35 to Name");

			p.Number = 42;
			Assert.AreEqual('C', p.Name, "Number42 to Name");

			p.Number = 39;
			Assert.AreEqual('G', p.Name, "Number39 to Name");
			
			p.Number = 40;
			Assert.AreEqual('A', p.Name, "Number40 to Name");

			Pitch p2 = new Pitch(35, Accidental.Natural);
			Assert.AreEqual('C', p2.Name, "ConstructorNumber35 to Name");

			p2 = new Pitch(33, Accidental.Natural);
			Assert.AreEqual('A', p2.Name, "ConstructorNumber33 to Name");

			Pitch p3 = new Pitch('C', 5, Accidental.Natural);
			Assert.AreEqual('C', p3.Name, "Constructor name, octave to Name");
			Assert.AreEqual(5, p3.Octave, "Constructor name, octave to Octave");
			Assert.AreEqual(42, p3.Number, "Constructor name, octave to Number");

			p3 = new Pitch('C', 4, Accidental.Natural);
			Assert.AreEqual('C', p3.Name, "Constructor name, octave to Name");
			Assert.AreEqual(4, p3.Octave, "Constructor name, octave to Octave");
			Assert.AreEqual(35, p3.Number, "Constructor name, octave to Number");

			p3 = new Pitch('B', 5, Accidental.Natural);
			Assert.AreEqual('B', p3.Name, "Constructor name, octave to Name");
			Assert.AreEqual(5, p3.Octave, "Constructor name, octave to Octave");
			Assert.AreEqual(48, p3.Number, "Constructor name, octave to Number");
		}

		[Test]
		public void NameSet() {
			Pitch p = new Pitch('C', 4, Accidental.Natural);
			p.Name = 'C';
			Assert.AreEqual('C', p.Name);
			Assert.AreEqual(4, p.Octave);
			p.Name = 'D';
			Assert.AreEqual('D', p.Name);
			Assert.AreEqual(4, p.Octave);
			p.Name = 'A';
			Assert.AreEqual('A', p.Name);
			Assert.AreEqual(4, p.Octave);
			p.Name = 'B';
			Assert.AreEqual('B', p.Name);
			Assert.AreEqual(4, p.Octave);
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void InvalidPitchNameException() {
			Pitch p = new Pitch('a', 4, Accidental.Natural);
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void InvalidNameSetException() {
			Pitch p = new Pitch();
			p.Name = 'H';
			}

		[Test]
		public void OctaveGet() {
			Pitch p = new Pitch();

			p.Number = 35;
			Assert.AreEqual(4, p.Octave, "Number to Octave");
		
			p.Number = 41;
			Assert.AreEqual(4, p.Octave, "Number to Octave");

			p.Number = 42;
			Assert.AreEqual(5, p.Octave, "Number to Octave");

			Pitch p2 = new Pitch(35, Accidental.Natural);
			Assert.AreEqual(4, p2.Octave);
		}

		[Test]
		public void OctaveSet() {
			Pitch p = new Pitch('G', 3, Accidental.Natural);
			p.Octave = 6;

			Assert.AreEqual(6, p.Octave);
			Assert.AreEqual(53, p.Number);

			p.Octave = 2;
			Assert.AreEqual(2, p.Octave);
			Assert.AreEqual(25, p.Number);
		}
		
		[Test]
		public void Accidentals() {
			Pitch p = new Pitch();
		
			Assert.AreEqual(Accidental.Natural, p.Accidental);

			p.Accidental = Accidental.DoubleSharp;
			Assert.AreEqual(Accidental.DoubleSharp, p.Accidental);

			p.Accidental = Accidental.DoubleFlat;
			Assert.AreEqual(Accidental.DoubleFlat, p.Accidental);

			Pitch p2 = new Pitch(35, Accidental.Flat);
			Assert.AreEqual(Accidental.Flat, p2.Accidental);

			Pitch p3 = new Pitch('D', 3, Accidental.Sharp);
			Assert.AreEqual(Accidental.Sharp, p3.Accidental);
		}

		
		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void InvalidAccidentalException1() {
			Pitch p = new Pitch();
			p.Accidental = (Accidental)7;
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void InvalidAccidentalException2() {
			Pitch p = new Pitch(45, (Accidental)(-1));
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void InvalidAccidentalException3() {
			Pitch p = new Pitch('C', 5, (Accidental)5);
		}

		[Test]
		public void MidiNumberGet() {
			Pitch p = new Pitch(39, Accidental.Natural);
			Assert.AreEqual(67, p.MidiNumber);

			p.Accidental = Accidental.Sharp;
			Assert.AreEqual(68, p.MidiNumber);

			p.Accidental = Accidental.DoubleSharp;
			Assert.AreEqual(69, p.MidiNumber);

			p.Accidental = Accidental.Flat;
			Assert.AreEqual(66, p.MidiNumber);
		
			p.Accidental = Accidental.DoubleFlat;
			Assert.AreEqual(65, p.MidiNumber);

			Pitch pC = new Pitch('C', 1, Accidental.Natural);
			Assert.AreEqual(24, pC.MidiNumber);
			Pitch pD = new Pitch('D', 2, Accidental.Natural);
			Assert.AreEqual(38, pD.MidiNumber);
			Pitch pE = new Pitch('E', 3, Accidental.Natural);
			Assert.AreEqual(52, pE.MidiNumber);
			Pitch pF = new Pitch('F', 4, Accidental.Natural);
			Assert.AreEqual(65, pF.MidiNumber);
			Pitch pG = new Pitch('G', 5, Accidental.Natural);
			Assert.AreEqual(79, pG.MidiNumber);
			Pitch pA = new Pitch('A', 6, Accidental.Natural);
			Assert.AreEqual(93, pA.MidiNumber);
			Pitch pB = new Pitch('B', 7, Accidental.Natural);
			Assert.AreEqual(107, pB.MidiNumber);

		}
			
		[Test]
		public void MidiNumberSet() {
			Pitch p = new Pitch(60,true);
			Assert.AreEqual(60, p.MidiNumber);
			Assert.AreEqual('C', p.Name);
			Assert.AreEqual(Accidental.Natural, p.Accidental);
			p = new Pitch(61, true);
			Assert.AreEqual(61, p.MidiNumber);
			Assert.AreEqual('C', p.Name);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);
			p = new Pitch(61, false);
			Assert.AreEqual(61, p.MidiNumber);
			Assert.AreEqual('D', p.Name);
			Assert.AreEqual(Accidental.Flat, p.Accidental);
			p = new Pitch(59, false);
			Assert.AreEqual(59, p.MidiNumber);
			Assert.AreEqual('B', p.Name);
			Assert.AreEqual(Accidental.Natural, p.Accidental);
			p = new Pitch(56, false);
			Assert.AreEqual(56, p.MidiNumber);
			Assert.AreEqual('A', p.Name);
			Assert.AreEqual(Accidental.Flat, p.Accidental);
			p = new Pitch(56, true);
			Assert.AreEqual(56, p.MidiNumber);
			Assert.AreEqual('G', p.Name);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);
		}

		[Test]
		public void MidiNumberKeySet() {
			Key k = new Key();
			k.Fifths = 1;
			k.Mode = KeyMode.Major;
			Pitch p = new Pitch(60,k);
			Assert.AreEqual(60, p.MidiNumber);
			Assert.AreEqual('C', p.Name);
			Assert.AreEqual(Accidental.Natural, p.Accidental);
			p = new Pitch(61, k);
			Assert.AreEqual(61, p.MidiNumber);
			Assert.AreEqual('C', p.Name);
			Assert.AreEqual(Accidental.Sharp, p.Accidental);
			k.Fifths = -1;
			p = new Pitch(61, k);
			Assert.AreEqual(61, p.MidiNumber);
			Assert.AreEqual('D', p.Name);
			Assert.AreEqual(Accidental.Flat, p.Accidental);


		}
	}
}
