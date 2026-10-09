"""Colours and theme (loop0002 item 002): seqsee/gui/draw/theme.py against lib/SColor.pm
(HSV2RGB, HSV2Color) and lib/Themes/Std2.pm (every Style::* function), golden: gui_theme.

Every call is an exact match, including Perl's out-of-range quirks (v >= 100 or v < 0 give
short colour strings such as '#000'; h >= 360 gives '#00').
"""

import pytest

import golden
from seqsee.gui.draw import ops, theme
from seqsee.util import perl_str

CASES = {c["name"]: c for c in golden.load("gui_theme")}
STYLE_NAMES = sorted({c["fn"] for c in CASES["styles"]["calls"]})


def test_hsv2rgb_matches_perl():
    bad = []
    for call in CASES["hsv"]["calls"]:
        # Perl stringifies -0.0 as "0"; the value is the same.
        got = [perl_str(x) for x in theme.hsv2rgb(*call["args"])]
        got = ["0" if s == "-0" else s for s in got]
        if got != call["rgb"]:
            bad.append((call["args"], got, call["rgb"]))
    assert not bad, bad[:10]


def test_hsv2color_matches_perl():
    bad = []
    for call in CASES["hsv"]["calls"]:
        got = theme.hsv2color(*call["args"])
        if got != call["color"]:
            bad.append((call["args"], got, call["color"]))
    assert not bad, bad[:10]
    assert len(CASES["hsv"]["calls"]) > 4000


def test_quirky_colours():
    assert theme.hsv2color(0, 0, 100) == "#000"  # 256 -> D2H{16} is undef
    assert theme.hsv2color(360, 50, 50) == "#00"  # h/60 == 6 matches no branch
    assert theme.hsv2color(160, 20, 20) == "#28332F"


def _pairs(style):
    return [["-" + k, v] for k, v in style.items()]


@pytest.mark.parametrize("name", STYLE_NAMES)
def test_style_matches_perl(name):
    fn = getattr(theme.Style, name)
    calls = [c for c in CASES["styles"]["calls"] if c["fn"] == name]
    assert calls
    for call in calls:
        assert _pairs(fn(*call["args"])) == call["result"], call


@pytest.mark.perl_source
def test_every_perl_style_is_ported():
    perl = set()
    src = (golden.GOLDEN_DIR.parents[2] / "lib" / "Themes" / "Std2.pm").read_text()
    for line in src.splitlines():
        if line.lstrip().startswith("sub Style::"):
            perl.add(line.split("Style::")[1].split("{")[0].strip())
    assert perl == set(STYLE_NAMES)
    assert all(callable(getattr(theme.Style, n)) for n in perl)


def test_wrong_arity_and_unknown_style_raise():
    assert CASES["arity"]["dies"] == [1, 1, 1, 1, 1]
    with pytest.raises(TypeError):
        theme.Style.Element()
    with pytest.raises(TypeError):
        theme.Style.Starred(1)
    with pytest.raises(TypeError):
        theme.Style.Relation(1)
    with pytest.raises(TypeError):
        theme.Style.Group(1, 2)
    with pytest.raises(AttributeError, match="Have you defined this style"):
        theme.style("NoSuchStyle")
    assert theme.style("GroupBorder", 1) == theme.Style.GroupBorder(1)


def test_styles_feed_ops():
    """A Style result can be splatted into an op, as Perl splats it into createX."""
    t = ops.text(10, 20, text="7", **theme.Style.Element(1))
    assert t.fill == "#00FF00" and t.font.endswith("--28-140-100-100-p-105-iso8859-4")
    r = ops.rectangle(0, 0, 5, 5, **theme.Style.Group2(0, None, 0))
    assert r.stipple == "gray75" and r.width == 0
    b = ops.rectangle(0, 0, 5, 5, **theme.Style.GroupBorder(0))
    assert b.dash is None and b.width == 2
    assert ops.line(0, 0, 1, 1, **theme.Style.Relation(50, 1)).arrow == "last"


def test_results_are_fresh():
    a = theme.Style.Element(0)
    a["fill"] = "red"
    assert theme.Style.Element(0)["fill"] == "#28332F"
