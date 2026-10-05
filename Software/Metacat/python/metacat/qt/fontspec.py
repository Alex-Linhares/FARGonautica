"""Tk font words: what Tk 8.6 reads from a font description (no Qt).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026), for loop0003 item 03.

A font word is what ``swl.tcl_word`` makes of an SWL font, ``(face size
style...)``, or the same as a Tcl list string.  Tk (tkFont.c's
ParseFontNameObj) also reads the option form ``-family f -size n -weight w
-slant s -underline b -overstrike b`` and named fonts; the panels send only
``TkDefaultFont`` (a text item's default).  A negative size is in pixels; a
positive one in points at the GUIs' fixed Tk scaling of 96 dpi, so
``(int)(points * 96 / 72 + 0.5)`` pixels (TkFontGetPixels); 0 or no size is
Tk's default.  The styles after the size may be single words or lists; a
later weight or slant overrides an earlier one, and the words are
case-sensitive.  Errors carry Tk's messages.
"""
from __future__ import annotations

from typing import NamedTuple

from metacat.qt.displaylist import TclError, split_list

# TkDefaultFont on X11 (tk/library/ttk/fonts.tcl: -size -12, sans-serif), with
# helvetica for the face, as Metacat's sans-serif face falls back to it
DEFAULT_FAMILY = "helvetica"
DEFAULT_PIXELS = 12
NAMED = {"TkDefaultFont", "TkTextFont", "TkHeadingFont", "TkCaptionFont",
         "TkTooltipFont", "TkIconFont", "TkMenuFont", "TkSmallCaptionFont"}

_STYLES = {"normal": ("bold", False), "bold": ("bold", True),
           "roman": ("italic", False), "italic": ("italic", True),
           "underline": ("underline", True), "overstrike": ("overstrike", True)}
_TRUE = ("1", "true", "yes", "on")
_FALSE = ("0", "false", "no", "off")


class FontSpec(NamedTuple):
    family: str
    size: int           # as given: negative = pixels, positive = points, 0 = default
    pixels: int
    bold: bool
    italic: bool
    underline: bool
    overstrike: bool


def _integer(word):
    try:
        return int(str(word), 10)
    except ValueError:
        raise TclError('expected integer but got "%s"' % word) from None


def _boolean(word):
    w = str(word).lower()
    if w in _TRUE:
        return True
    if w in _FALSE:
        return False
    raise TclError('expected boolean value but got "%s"' % word)


def pixels(size):
    """port: TkFontGetPixels at 96 dpi (0 = Tk's default size)"""
    if size == 0:
        return DEFAULT_PIXELS
    if size < 0:
        return -size
    return int(size * 96 / 72 + 0.5)


def _spec(family, size, attrs):
    return FontSpec(family, size, pixels(size), attrs["bold"], attrs["italic"],
                    attrs["underline"], attrs["overstrike"])


def _options(words):
    """port: ConfigAttributesObj, the -option form"""
    family, size = DEFAULT_FAMILY, 0
    attrs = dict(bold=False, italic=False, underline=False, overstrike=False)
    for k in range(0, len(words), 2):
        option = words[k]
        if k + 1 == len(words):
            raise TclError('value for "%s" option missing' % option)
        value = words[k + 1]
        if option == "-family":
            family = value
        elif option == "-size":
            size = _integer(value)
        elif option == "-weight":
            if value not in ("normal", "bold"):
                raise TclError('bad -weight value "%s": must be normal, or bold' % value)
            attrs["bold"] = value == "bold"
        elif option == "-slant":
            if value not in ("roman", "italic"):
                raise TclError('bad -slant value "%s": must be roman, or italic' % value)
            attrs["italic"] = value == "italic"
        elif option in ("-underline", "-overstrike"):
            attrs[option[1:]] = _boolean(value)
        else:
            raise TclError('bad option "%s": must be -family, -size, -weight, -slant, '
                           '-underline, or -overstrike' % option)
    return _spec(family, size, attrs)


def parse_font(font):
    """port: ParseFontNameObj: the FontSpec of a Tk font word"""
    words = split_list(font)
    if not words or (len(words) == 1 and words[0] in NAMED):
        return FontSpec(DEFAULT_FAMILY, -DEFAULT_PIXELS, DEFAULT_PIXELS,
                        False, False, False, False)
    if words[0].startswith("-") and words[0][1:2] != "*":
        return _options(words)
    attrs = dict(bold=False, italic=False, underline=False, overstrike=False)
    size = _integer(words[1]) if len(words) > 1 else 0
    for group in words[2:]:
        for word in split_list(group):
            if word not in _STYLES:
                raise TclError('unknown font style "%s"' % word)
            key, value = _STYLES[word]
            attrs[key] = value
    return _spec(words[0], size, attrs)
