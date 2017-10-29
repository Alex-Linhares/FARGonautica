using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MusicPrimitives;

namespace RhythmCat {
	public class NoteWithAttackPoint : Note, ICloneable {
		public int AttackPoint {
			get;
			set;

		}

		public NoteWithAttackPoint(Note n, int attackPoint) : base(n.duration, n.tiedBefore, n.tiedAfter, n.midiInfo) {
			AttackPoint = attackPoint;
		}

		object ICloneable.Clone() {
			NoteWithAttackPoint n = (NoteWithAttackPoint)base.Clone();
			n.AttackPoint = AttackPoint;
			return n;
		}
	}

	public class Note : ICloneable {
		public bool isRest {
			get;
			protected set;
		}

		public bool tiedBefore {
			get;
			protected set;
		}

		public bool tiedAfter {
			get;
			protected set;
		}

		public int duration {
			get;
			protected set;
		}

		public int duartionIncludingTiesAfter {
			get;
			set;
		}

		public MidiInfo midiInfo {
			get;
			protected set;
		}

		public int Midi {
			get {
				if (midiInfo == null)
					return -1;
				else
					return midiInfo.midi;
			}
		}

		public ScaleDegree GetScaleDegree(Key k) {
			if (midiInfo == null)
				return null;
			return midiInfo.GetScaleDegree(k);
		}

		public int GetOctave() {
			if (midiInfo == null)
				return -1;
			return midiInfo.GetOctave();
		}
		

		public Note(int duration, bool tiedBefore, bool tiedAfter, MidiInfo pitch) {
			this.isRest = false;
			this.tiedBefore = tiedBefore;
			this.tiedAfter = tiedAfter;
			this.duration = duration;
			this.duartionIncludingTiesAfter = duration;
			this.midiInfo = pitch;
		}

		protected Note(int duration, bool tiedBefore, bool tiedAfter) {
			this.isRest = false;
			this.tiedBefore = tiedBefore;
			this.tiedAfter = tiedAfter;
			this.duration = duration;
			this.duartionIncludingTiesAfter = duration;
			this.midiInfo = null;
		}


		public virtual object Clone() {
			Note n;
			if (midiInfo == null)
				n = new Note(this.duration, this.tiedBefore, this.tiedAfter, null);
			else
				n = new Note(this.duration, this.tiedBefore, this.tiedAfter, (MidiInfo)midiInfo.Clone());
			return n;
		}

		public bool isLastInMeasure { get; set; }

		public override string ToString() {
			return this.midiInfo.ToString();
		}
	}

	public class Rest : Note {
		public Rest(int duration, bool tiedBefore, bool tiedAfter)
			: base(duration, tiedBefore, tiedAfter) {
				isRest = true;
		}

		public override object Clone() {
			Rest r = new Rest(this.duration, this.tiedBefore, this.tiedAfter);
			return r;
		}
	}

	public class UnpitchedNote : Note {
		public UnpitchedNote(int duration, bool tiedBefore, bool tiedAfter)
			: base(duration, tiedBefore, tiedAfter) {

		}

		public override object Clone() {
			UnpitchedNote n = new UnpitchedNote(this.duration, this.tiedBefore, this.tiedAfter);
			return n;
		}
	}

	
}
