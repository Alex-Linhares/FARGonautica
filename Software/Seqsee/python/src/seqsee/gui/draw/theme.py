"""Colours and styles: lib/SColor.pm (HSV2RGB, HSV2Color) and lib/Themes/Std2.pm (Style::*).

Each ``Style.X(...)`` returns a fresh dict of Tk item options without the leading '-', in
Perl's order, ready to splat into a draw op: ``ops.text(x, y, text=t, **Style.Element(0))``.
``None`` stands for Perl's undef (an unset option). Perl memoizes these pure functions;
only ``hsv2color`` is cached here, since the styles return mutable dicts.
"""

import functools

from seqsee.util import perl_num, perl_str, perl_true

_D2H = "0123456789ABCDEF"

FONT_10 = "-adobe-helvetica-bold-r-normal--10-140-100-100-p-105-iso8859-4"
FONT_14 = "-adobe-helvetica-bold-r-normal--14-140-100-100-p-105-iso8859-4"
FONT_20 = "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4"
FONT_28 = "-adobe-helvetica-bold-r-normal--28-140-100-100-p-105-iso8859-4"


def hsv2rgb(h, s, v):
    """SColor::HSV2RGB: h in degrees, s in 0..100; r, g, b on v's scale (0..100)."""
    if s <= 0:
        return (v, v, v)
    h /= 60  # now 0-5
    s /= 100
    f = h - int(h)
    h = int(h)
    p = v * (1 - s)
    q = v * (1 - s * f)
    t = v * (1 - s * (1 - f))
    if h == 0:
        return (v, t, p)
    if h == 1:
        return (q, v, p)
    if h == 2:
        return (p, v, t)
    if h == 3:
        return (p, q, v)
    if h == 4:
        return (t, p, v)
    if h == 5:
        return (v, p, q)
    # PERL-QUIRK: for h >= 360 no branch matches and the sub returns the last condition's
    # value, the false ''.
    return ("",)


def _hex_digit(n):
    # PERL-QUIRK: $D2H{n} is undef (joined as '') outside 0..15; int(-0.x) is key "0".
    return _D2H[n] if 0 <= n <= 15 else ""


@functools.lru_cache(maxsize=None)
def hsv2color(h, s, v):
    """SColor::HSV2Color: '#RRGGBB' with each channel int(x * 2.56).

    PERL-QUIRK: a channel outside 0..255 (v >= 100 or v < 0) loses its high digit, giving
    short strings such as '#000' for v = 100.
    """
    out = "#"
    for x in hsv2rgb(h, s, v):
        n = int(perl_num(x) * 2.56)
        out += _hex_digit(int(n / 16)) + _hex_digit(n % 16)
    return out


HSV = hsv2color


def _clamped_v(attention):
    v = 400 * attention
    if v < 0:
        v = 0
    if v > 99:
        v = 99
    return v


class Style:
    """Themes::Std2: one static method per ``Style::*`` function."""

    @staticmethod
    def Element(hilit):
        if hilit == 1:
            fill = "#00FF00"
        elif hilit == 2:
            fill = "#0000FF"
        else:
            fill = HSV(160, 20, 20)
        return {"fill": fill, "anchor": "center",
                "font": FONT_28 if perl_true(hilit) else FONT_20}

    @staticmethod
    def Starred():
        return {"fill": HSV(240, 50, 50), "anchor": "center", "font": FONT_20}

    @staticmethod
    def Relation(strength, hilit):
        return {"width": 5 if perl_true(hilit) else 3,
                "fill": "#00FF00" if perl_true(hilit) else "#777777",
                "smooth": 1, "arrow": "last"}

    @staticmethod
    def Group(meto, strength, is_largest):
        s, v = 40, 90 - 0.4 * strength
        if perl_true(meto):
            fill = HSV(240 - 20 * is_largest, s, v)
        else:
            fill = HSV(160 - 20 * is_largest, s, v)
        return {"width": 0, "fill": fill}

    @staticmethod
    def Group2(meto, category_name, is_largest):
        name = perl_str(category_name)
        if perl_true(meto):
            fill = HSV(240 - 20 * is_largest, 40, 60)
        elif name in ("ascending", "descending"):
            fill = HSV(180, 60, 80)
        elif name == "sameness":
            fill = HSV(240, 40, 60)
        else:
            fill = HSV(120, 40, 60 + 20 * is_largest)
        if perl_true(is_largest) or perl_true(category_name):
            stipple = None
        else:
            stipple = "gray75"
        return {"width": 0, "fill": fill, "stipple": stipple}

    @staticmethod
    def GroupBorder(hilit):
        if hilit == 1:
            outline = "#000000"
        elif hilit == 2:
            outline = "#0000FF"
        else:
            outline = HSV(240, 70, 70)
        return {"width": 2 + 2 * hilit, "dash": "---" if hilit == 1 else None,
                "outline": outline}

    @staticmethod
    def ElementAttention(attention):
        return {"fill": HSV(300, 40, _clamped_v(attention)), "font": FONT_20}

    @staticmethod
    def GroupAttention(attention):
        return {"fill": HSV(160, 40, _clamped_v(attention))}

    @staticmethod
    def GroupBorderAttention():
        return {"outline": HSV(180, 40, 5)}

    @staticmethod
    def RelationAttention(attention):
        return {"width": 4, "fill": HSV(190, 40, _clamped_v(attention)),
                "smooth": 1, "arrow": "last"}

    @staticmethod
    def NetActivation(raw_significance):
        return {"fill": HSV(240, 30, 90 - 0.88 * raw_significance)}

    @staticmethod
    def ThoughtBox(hit_intensity, is_current):
        hit_intensity = perl_num(hit_intensity)   # undef (a thought never hit) is 0
        if hit_intensity > 2000:
            hit_intensity = 2000
        s, v = 40, 90 - 0.02 * hit_intensity
        return {"width": 3 if perl_true(is_current) else 1,
                "fill": HSV(120, s, v) if perl_true(is_current) else HSV(100, s, v)}

    @staticmethod
    def ThoughtComponent(presence_level, component_importance):
        return {"fill": HSV(250, 90, 80), "font": FONT_10}

    @staticmethod
    def ThoughtHead():
        return {"font": FONT_14}


def style(name, *args):
    """``Style::<name>(@args)``; an unknown name fails like Style::AUTOLOAD."""
    fn = getattr(Style, name, None)
    if fn is None or name.startswith("_"):
        raise AttributeError(
            f"Unknown method Style::{name} called. Have you defined this style?")
    return fn(*args)
