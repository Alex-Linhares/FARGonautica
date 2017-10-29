using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace RhythmCat
{
	static public class Constants
	{
		public const string LOGFILE_PATH = @"C:\RhythmcatTesting\log.txt";
		public const string MELODY_PATH = @"C:\Users\epnichols\Documents\Visual Studio 2010\Projects\RhythmCat\RhythmCat\melodies.txt";

		// Coderack.
		public const int MAX_CODERACK_SIZE = 50;

		// Barlines.
		public const int BARLINE_MAX_HEIGHT = 5;

		// Main program coderack loop.

		/// <summary>
		/// Sets the effective tempo
		/// </summary>
		public const int NUM_CODELETS_PER_SIXTEENTH = 60;		//62.5
		public const int NUM_EXTRA_MEASURES_COMPUTATION = 4;

		public const int NUM_CODELETS_TILL_SLIPNET_UPDATE = 15;
		public const int NUM_CODELETS_TILL_TEMPERATURE_UPDATE = 15;
		public const int NUM_CODELETS_TILL_CODERACK_UPDATE = 15;
		public const int NUM_CODELETS_TILL_CODERACK_CLEANUP = 15;
		public const int NUM_CODELETS_TILL_NEW_HIGH_LEVEL = 30;
		public const int NUM_CODELETS_TILL_LINK_FADE = NUM_CODELETS_PER_SIXTEENTH * 16;	
		public const float LINKS_PERCENT_FADE = 0.5f;	// from 0 to 1.

		// Time and memory
		public const int NUM_MEASURES_FOR_WORKSPACE_HAPPINESS = 8;  // used to compute workspace happiness
		public const int NUM_MEASURES_OF_PERFECT_MEMORY = 4; //4	// for links, etc.
		public const int MIN_ANALOGY_LENGTH_LTM = 4;		// min total analogy length to get put into LTM for recussitation
		public const int MIN_ANALOGY_STRENGTH_LTM = 60;		// min total analogy strength to get put into LTM for recussitation
		
		/// <summary>
		/// When a group's final element has been around for this many measures, don't ever modify the group.
		/// </summary>
		public const int NUM_MEASURES_UNTIL_GROUPS_UNBREAKABLE = 4;	// for groups, analogies.
		public const int NUM_MEASURES_UNTIL_ANALOGIES_OLD = 4;	// for analogy scorings.


		// Melody contour
		public const int INVERT_CONTOUR_COST = 1;	// cost of a "reverse" operation in edit distance

		// Musical Forces.
		public const float LARSON_WEIGHT_G = 0.088f;
		public const float LARSON_WEIGHT_M = 0.432f;
		public const float LARSON_WEIGHT_I = 0.187f;
		public const float MAX_LARSON_EXPECTEDNESS = 0.7f;

		public const float ALPHABET_SCORING_DURATION_WEIGHT = 0.7f;
		public const float ALPHABET_SCORING_LEVEL_WEIGHT = 0.25f;
		public const float ALPHABET_SCORING_FINAL_NOTE_WEIGHT = 0.05f;

		// Analogy.
		public const bool MAKE_SINGLE_MEASURE_ANALOGIES = true;

		// Analogy scoring weights;
		public const float WEIGHT_RHYTHM = 0.7f;
		public const float WEIGHT_MELODY = 0.3f;

		public const float ANALOGY_SCORE_WEIGHT_RELATIONSHIPS = 0.35f;              //.375
		public const float ANALOGY_SCORE_WEIGHT_COMPLETENESS = 0.35f;		        //.375
		public const float ANALOGY_SCORE_WEIGHT_SIZE = 0.20f;                       //.25
		public const float ANALOGY_SCORE_WEIGHT_AGE = 0.10f;                        //0

		public const float DESTROY_ANALOGIES_BELOW_THRESHOLD = 70.0f;		// analogies above this strength will be saved from destruction
		
		// Group reason weights.
		public const double WEIGHT_REASON_ANALOGY = 0.60; //.6
		public const double WEIGHT_REASON_IDENTICAL_COMPONENTS = 0.30;
		public const double WEIGHT_REASON_SIMILAR_COMPONENTS = 0.20;
		public const double WEIGHT_REASON_END_BEFORE_GAP = 0.15;
		public const double WEIGHT_REASON_END_BEFORE_LEAP = 0.1;
		public const double WEIGHT_REASON_END_DOMINANT = 0.10;
		public const double WEIGHT_REASON_END_TONIC = 2.0;
		public const double WEIGHT_REASON_END_MUSICAL_FORCES_CLOSURE = 2.0;

		public const double WEIGHT_REASON_SLOWING_END = .4;		// gets multiplied by level multiplier
		public const double MAX_WEIGHT_REASON_SLOWING_END_LEVEL_MULTIPLIER = 2.0;

		public const double WEIGHT_REASON_EXPECTED = 0.15;
		public const double WEIGHT_REASON_NUMBER_SUBELEMENTS = 0.30;
		public const double WEIGHT_REASON_SEQUENCE = 1.95;
		public const double WEIGHT_REASON_START_AFTER_GAP = 0.15;
		public const double WEIGHT_REASON_START_AFTER_LEAP = 0.1;
		public const double WEIGHT_REASON_BOUNDARY_POSITION = 0.15;

		public const double WEIGHT_REASON_END_POSITION = 0.1;
		public const double MAX_WEIGHT_REASON_END_POSITION_LEVEL_MULTIPLIER = 2.0;
		
		public const double WEIGHT_REASON_START_POSITION = 0.1;


		// Group penalty weights.
		public const double WEIGHT_PENALTY_REASON_SUBCOMPONENT_LENGTH = 4.0;//2.6;//3.40;		//0.4
		public const double WEIGHT_PENALTY_REASON_HIERARCHY_CROSSING = 0.6;// 6.6; //6.60;		//0.6

		// Other scoring factors.
		public const double WEIGHT_SCREAMING_MEASURE = 20;//10

		public const int HAPPINESS_WINDOW_SIZE = 1;

		public const double PENALTY_PER_LEVEL_HIERARCHY_CROSSING = 30;

		public const float ANALOGY_EXPECTATION_STRENGTH_MULTIPLIER = 0.5f;
		public const float GROUP_EXPECTATION_STRENGTH_MULTIPLIER = 0.5f;
		
		// Mist GUI debugging output.
		public const int NUM_ATTENTION_HISTORY_DOTS = 20;
		public const string SCREENSHOT_PATH = @"C:\RhythmcatScreenshots\";
		public const bool SHOW_CODERACK_URGENCIES_CONSOLE = true;
		public const bool SHOW_CODELET_RUNTIME_CONSOLE = false;


		
	}
}
