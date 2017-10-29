using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace RhythmCat {
	public class ColorList {

		public static Color COLOR_PINK = Color.FromArgb(229, 163, 180);
		public static Color COLOR_APRICOT = Color.FromArgb(237, 200, 154);
		public static Color COLOR_LEMON = Color.FromArgb(242, 240, 143);
		public static Color COLOR_CHARTREUSE = Color.FromArgb(224, 230, 122);
		public static Color COLOR_MINT = Color.FromArgb(187, 221, 174);
		public static Color COLOR_AQUA = Color.FromArgb(167, 216, 189);
		public static Color COLOR_AZURE = Color.FromArgb(161, 218, 225);
		public static Color COLOR_MAUVE = Color.FromArgb(197, 160, 201);

		public static Color COLOR_RED = Color.FromArgb(207, 32, 39);
		public static Color COLOR_ORANGE = Color.FromArgb(219, 121, 40);
		public static Color COLOR_YELLOW = Color.FromArgb(236, 218, 66);
		public static Color COLOR_LIME = Color.FromArgb(165, 195, 59);
		public static Color COLOR_GREEN = Color.FromArgb(119, 194, 88);
		public static Color COLOR_TURQUOISE = Color.FromArgb(78, 147, 137);
		public static Color COLOR_BLUE = Color.FromArgb(72, 110, 182);
		public static Color COLOR_PURPLE = Color.FromArgb(120, 54, 149);

		private static Color[] pastel_reversed = new Color[] {COLOR_MINT, COLOR_AZURE, 
			COLOR_APRICOT, COLOR_MAUVE, COLOR_CHARTREUSE, COLOR_AQUA, COLOR_LEMON, COLOR_PINK};

		private static Color[] vivid_reversed = new Color[] {COLOR_RED, COLOR_GREEN, COLOR_BLUE, 
			COLOR_ORANGE, COLOR_TURQUOISE, COLOR_LIME, COLOR_PURPLE, COLOR_YELLOW};

		public static Color[] pastel, vivid;

		static ColorList() {
			pastel = new Color[pastel_reversed.Length];
			for (int i = 0; i < pastel_reversed.Length; i++)
				pastel[pastel_reversed.Length - i - 1] = pastel_reversed[i];
			
			vivid = new Color[vivid_reversed.Length];
			for (int i = 0; i < vivid_reversed.Length; i++)
				vivid[vivid_reversed.Length - i - 1] = vivid_reversed[i];
		}
	}

	
}
