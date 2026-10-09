"""Tk colour specs → ``#RRGGBB``, as Perl/Tk resolves them on the X server (Xvfb).

X11 names differ from SVG/Qt ones (X11 ``gray`` is #BEBEBE, ``green`` #00FF00, ``maroon``
#B03060, ``purple`` #A020F0), so names are resolved here, never by ``QColor(name)``.
Checked against Tk's ``$mw->rgb`` in tests/golden/gui_smoke.json (case ``colours``).
"""

import re

_X11 = {
    "aliceblue": "#F0F8FF", "antiquewhite": "#FAEBD7", "aquamarine": "#7FFFD4",
    "azure": "#F0FFFF", "beige": "#F5F5DC", "bisque": "#FFE4C4", "black": "#000000",
    "blanchedalmond": "#FFEBCD", "blue": "#0000FF", "blueviolet": "#8A2BE2",
    "brown": "#A52A2A", "burlywood": "#DEB887", "cadetblue": "#5F9EA0",
    "chartreuse": "#7FFF00", "chocolate": "#D2691E", "coral": "#FF7F50",
    "cornflowerblue": "#6495ED", "cornsilk": "#FFF8DC", "cyan": "#00FFFF",
    "darkblue": "#00008B", "darkcyan": "#008B8B", "darkgoldenrod": "#B8860B",
    "darkgray": "#A9A9A9", "darkgreen": "#006400", "darkkhaki": "#BDB76B",
    "darkmagenta": "#8B008B", "darkolivegreen": "#556B2F", "darkorange": "#FF8C00",
    "darkorchid": "#9932CC", "darkred": "#8B0000", "darksalmon": "#E9967A",
    "darkseagreen": "#8FBC8F", "darkslateblue": "#483D8B", "darkslategray": "#2F4F4F",
    "darkturquoise": "#00CED1", "darkviolet": "#9400D3", "deeppink": "#FF1493",
    "deepskyblue": "#00BFFF", "dimgray": "#696969", "dodgerblue": "#1E90FF",
    "firebrick": "#B22222", "floralwhite": "#FFFAF0", "forestgreen": "#228B22",
    "gainsboro": "#DCDCDC", "ghostwhite": "#F8F8FF", "gold": "#FFD700",
    "goldenrod": "#DAA520", "gray": "#BEBEBE", "green": "#00FF00",
    "greenyellow": "#ADFF2F", "honeydew": "#F0FFF0", "hotpink": "#FF69B4",
    "indianred": "#CD5C5C", "ivory": "#FFFFF0", "khaki": "#F0E68C",
    "lavender": "#E6E6FA", "lavenderblush": "#FFF0F5", "lawngreen": "#7CFC00",
    "lemonchiffon": "#FFFACD", "lightblue": "#ADD8E6", "lightcoral": "#F08080",
    "lightcyan": "#E0FFFF", "lightgoldenrod": "#EEDD82", "lightgoldenrodyellow": "#FAFAD2",
    "lightgray": "#D3D3D3", "lightgreen": "#90EE90", "lightpink": "#FFB6C1",
    "lightsalmon": "#FFA07A", "lightseagreen": "#20B2AA", "lightskyblue": "#87CEFA",
    "lightslateblue": "#8470FF", "lightslategray": "#778899", "lightsteelblue": "#B0C4DE",
    "lightyellow": "#FFFFE0", "limegreen": "#32CD32", "linen": "#FAF0E6",
    "magenta": "#FF00FF", "maroon": "#B03060", "mediumaquamarine": "#66CDAA",
    "mediumblue": "#0000CD", "mediumorchid": "#BA55D3", "mediumpurple": "#9370DB",
    "mediumseagreen": "#3CB371", "mediumslateblue": "#7B68EE",
    "mediumspringgreen": "#00FA9A", "mediumturquoise": "#48D1CC",
    "mediumvioletred": "#C71585", "midnightblue": "#191970", "mintcream": "#F5FFFA",
    "mistyrose": "#FFE4E1", "moccasin": "#FFE4B5", "navajowhite": "#FFDEAD",
    "navy": "#000080", "navyblue": "#000080", "oldlace": "#FDF5E6",
    "olivedrab": "#6B8E23", "orange": "#FFA500", "orangered": "#FF4500",
    "orchid": "#DA70D6", "palegoldenrod": "#EEE8AA", "palegreen": "#98FB98",
    "paleturquoise": "#AFEEEE", "palevioletred": "#DB7093", "papayawhip": "#FFEFD5",
    "peachpuff": "#FFDAB9", "peru": "#CD853F", "pink": "#FFC0CB", "plum": "#DDA0DD",
    "powderblue": "#B0E0E6", "purple": "#A020F0", "red": "#FF0000",
    "rosybrown": "#BC8F8F", "royalblue": "#4169E1", "saddlebrown": "#8B4513",
    "salmon": "#FA8072", "sandybrown": "#F4A460", "seagreen": "#2E8B57",
    "seashell": "#FFF5EE", "sienna": "#A0522D", "skyblue": "#87CEEB",
    "slateblue": "#6A5ACD", "slategray": "#708090", "snow": "#FFFAFA",
    "springgreen": "#00FF7F", "steelblue": "#4682B4", "tan": "#D2B48C",
    "thistle": "#D8BFD8", "tomato": "#FF6347", "turquoise": "#40E0D0",
    "violet": "#EE82EE", "violetred": "#D02090", "wheat": "#F5DEB3",
    "white": "#FFFFFF", "whitesmoke": "#F5F5F5", "yellow": "#FFFF00",
    "yellowgreen": "#9ACD32",
}

_GRAY_N = re.compile(r"gr[ae]y(\d{1,3})")


def x11_names():
    """Every base colour name this module knows (lower case, no spaces)."""
    return sorted(_X11)


def to_hex(colour):
    """``#RRGGBB`` for a Tk colour spec, or None for an unset colour (None or '').

    Names ignore case and spaces and accept ``grey`` for ``gray`` (as X does). ``#RGB`` means
    ``#R0G0B0`` and longer forms keep each channel's top byte, as the 24-bit visual realises them.
    Raises KeyError for a name it doesn't know.
    """
    if colour is None or colour == "":
        return None
    c = str(colour)
    if c.startswith("#"):
        digits = c[1:]
        if len(digits) not in (3, 6, 9, 12) or not re.fullmatch(r"[0-9A-Fa-f]+", digits):
            raise KeyError(colour)
        n = len(digits) // 3
        chans = [digits[i * n:(i + 1) * n] for i in range(3)]
        return "#" + "".join((ch + "0")[:2] if n == 1 else ch[:2] for ch in chans).upper()
    name = c.replace(" ", "").lower().replace("grey", "gray")
    m = _GRAY_N.fullmatch(name)
    if m and int(m.group(1)) <= 100:
        v = int(int(m.group(1)) * 2.55 + 0.5)
        return "#%02X%02X%02X" % (v, v, v)
    return _X11[name]
