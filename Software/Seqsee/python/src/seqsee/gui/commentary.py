"""The commentary's text and questions (lib/Tk/SCommentary.pm, lib/UI/Graphical.pm's message),
in pure Python: no Qt here (the dock is ``seqsee.gui.qt.commentary``).

Tk::SCommentary is a read-only, word-wrapped Text (GUI_sparse.conf's [SCommentary]: 10 lines
of 60 characters, Lucida Bright bold 17 px) with the [SCommentary_tags] tags, and a column of
four disabled buttons (15 characters wide) above a "Start Debug" button.

- MessageRequiringNoResponse(@msg) is ``$Text->insert('end', @msg)``: Tk's insert takes
  ``chars, tagList, chars, tagList, ...`` (``Log.insert``, ``insert_runs``).
- MessageRequiringAResponse([choices], @msg) inserts @msg, puts the choices on the first
  buttons (the others stay disabled), sets AttentionNeeded and waits; the buttons, or the keys
  1-4 (ignored past the active buttons, ``key_choice``), answer. Then it inserts
  ``'  ', [], $response, ['user_response'], "\\n"`` (``response_parts``) and returns the
  chosen string. MessageRequiringBooleanResponse asks ``['yes', 'no']`` and returns 1 for yes,
  else 0 (``boolean_value``).
- UI/Graphical.pm's ``main::message($msg, $no_break)``: with $no_break, a message without a
  response (a string gets a "\\n"; an array ref is inserted as is, without one); without, a
  question with the single choice 'continue' (``message_request``).
- "Start Debug": ``$Global::debugMAX = 1 - $Global::debugMAX`` and
  ``main::message("debugMAX=$Global::debugMAX", 1)`` (``debug_message``).
"""
import dataclasses

from seqsee import util

# GUI_sparse.conf [SCommentary]
TEXT_FONT = "-lucida-lucida bright-bold-r-normal--17-120-100-100-p-0-iso8859-15"
TEXT_HEIGHT = 10                # lines
TEXT_WIDTH = 60                 # characters
TEXT_WRAP = "word"

# GUI_sparse.conf [SCommentary_tags]
TAG_CONFIG = {
    "user_response": "-foreground #FF0000",
    "debug": "-foreground #0000FF -font -lucida--bold-r-normal--10-120-100-100-p-0-iso8859-15",
    "codelet_family": "-foreground #0000FF",
    "green": "-foreground #00FF00",
}
# Not in Perl: the port logs model errors in the commentary too (Perl shows a tkdie dialog).
ERROR_TAG = "error"
EXTRA_TAG_CONFIG = {ERROR_TAG: "-foreground #B00000"}

BUTTON_COUNT = 4
BUTTON_WIDTH = 15               # characters
DEBUG_BUTTON_TEXT = "Start Debug"
BOOLEAN_CHOICES = ("yes", "no")
CONTINUE_CHOICES = ("continue",)


@dataclasses.dataclass(frozen=True)
class TagStyle:
    foreground: str = None
    font: str = None


def parse_tag_config(value):
    """SGUI::tags_to_aref: ``split /\\s+/`` into option/value pairs."""
    words = value.split()
    opts = dict(zip(words[0::2], words[1::2]))
    return TagStyle(foreground=opts.get("-foreground"), font=opts.get("-font"))


TAGS = {name: parse_tag_config(v) for name, v in {**TAG_CONFIG, **EXTRA_TAG_CONFIG}.items()}
# Tk tag priority, lowest first (the order tags_to_aref's hash walk configured them; from the
# oracle): where tags overlap, the later tag's options win.
TAG_PRIORITY = ("codelet_family", "debug", "green", "user_response", ERROR_TAG)


def style_of(tags):
    """The options a run with ``tags`` shows: each option from its highest-priority tag."""
    style = TagStyle()
    for name in sorted((t for t in tags if t in TAGS), key=TAG_PRIORITY.index):
        t = TAGS[name]
        style = TagStyle(foreground=t.foreground or style.foreground, font=t.font or style.font)
    return style


def tag_list(tags):
    """A Tk tagList: a list of names, or a string read as a Tcl list ('' is no tag)."""
    if tags is None:
        return ()
    if isinstance(tags, str):
        return tuple(tags.split())
    return tuple(util.perl_str(t) for t in tags)


def insert_runs(*args):
    """``$Text->insert('end', chars, tagList, chars, tagList, ...)`` as (text, tags) runs; a
    trailing ``chars`` has no tags. Empty texts are dropped."""
    runs = []
    for i in range(0, len(args), 2):
        text = util.perl_str(args[i])
        tags = tag_list(args[i + 1]) if i + 1 < len(args) else ()
        if text:
            runs.append((text, tags))
    return runs


def response_parts(response):
    """What MessageRequiringAResponse inserts after the answer."""
    return ("  ", [], response, ["user_response"], "\n")


def boolean_value(response):
    """MessageRequiringBooleanResponse: ``($res eq 'yes') ? 1 : 0``."""
    return 1 if response == "yes" else 0


def key_choice(key, active_count):
    """The <KeyPress-1..4> bindings: the button index, or None past the active buttons."""
    index = key - 1
    if 0 <= index < BUTTON_COUNT and index < active_count:
        return index
    return None


def message_request(msg, no_break=None):
    """UI/Graphical.pm's ``main::message($msg, $no_break)``: ``(parts, choices)``; choices
    is None for a message without response, else ``('continue',)``."""
    is_list = isinstance(msg, (list, tuple))
    if util.perl_true(no_break):
        return (tuple(msg) if is_list else (f"{util.perl_str(msg)}\n",)), None
    return (tuple(msg) if is_list else (msg,)), CONTINUE_CHOICES


def debug_message(debug_max):
    """The "Start Debug" button's message (after the toggle)."""
    return message_request(f"debugMAX={util.perl_str(debug_max)}", 1)[0]


def parts_text(parts):
    """The plain text of Tk insert arguments."""
    return "".join(text for text, _ in insert_runs(*parts))


class Log:
    """The Text's content as runs of (text, sorted tags); adjacent runs with the same tags
    merge (as ``$Text->dump`` reads them back)."""

    def __init__(self):
        self._runs = []

    def insert(self, *args):
        """``$Text->insert('end', @args)``; returns the runs inserted."""
        new = insert_runs(*args)
        for text, tags in new:
            tags = tuple(sorted(set(tags)))
            if self._runs and self._runs[-1][1] == tags:
                self._runs[-1] = (self._runs[-1][0] + text, tags)
            else:
                self._runs.append((text, tags))
        return new

    def clear(self):
        self._runs = []

    def runs(self):
        return [(text, list(tags)) for text, tags in self._runs]

    def text(self):
        return "".join(text for text, _ in self._runs)
