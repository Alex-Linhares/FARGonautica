"""commentary-graphics.ss: the Commentary window.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from commentary-graphics.ss,
with racket/gui/commentary-graphics.rktl as a worked translation.

new-comment-window's closure is `CommentWindow`, which delegates what it does
not answer to its scrollable text window (general-graphics.ss's
make-scrollable-text-window, gui/general_graphics.py).  It keeps both versions
of every paragraph, the Eliza one and the plain one, and draws the one
%eliza-mode% chooses; headless.py's HeadlessCommentWindow does the same without
a text window.  The two fonts, top-level definitions of the original, are made
by `load()`, once fonts.ss's faces are chosen.  This module does not import
tkinter.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import chez, setup
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import exists_p, first, second

String = chez.String
_HALF = Fraction(1, 2)

# made by load(): (make-mfont sans-serif 12 '(bold italic)), (make-mfont serif 12 '(normal))
p_comment_window_font = False
p_comment_window_reminder_font = False


def make_comment_window(*optional_args):
    """commentary-graphics.ss: make-comment-window"""
    from metacat.gui import constants as K
    width = K.p_default_comment_window_width if len(optional_args) == 0 else first(optional_args)
    height = (K.p_default_comment_window_height if len(optional_args) < 2
              else second(optional_args))
    window = new_comment_window(width, height)
    tell(window, "initialize")
    return window


def new_comment_window(x_pixels, y_pixels):
    """commentary-graphics.ss: new-comment-window"""
    from metacat.gui import constants as K, general_graphics as gg
    text_window = gg.make_scrollable_text_window(
        x_pixels, y_pixels, K.p_virtual_comment_window_length,
        K.p_comment_window_background_color)
    tell(text_window, "set-icon-label", K.p_comment_window_icon_label)
    if exists_p(K.p_comment_window_icon_image):
        tell(text_window, "set-icon-image", K.p_comment_window_icon_image)
    if exists_p(K.p_comment_window_title):
        tell(text_window, "set-window-title", K.p_comment_window_title)
    return CommentWindow(text_window)


class CommentWindow(SchemeObject):
    """commentary-graphics.ss: new-comment-window's closure"""

    def __init__(this, text_window):
        this.text_window = text_window
        this.reminder_y = chez.mul(_HALF, chez.add(tell(text_window, "get-y-max"),
                                                          tell(text_window, "get-visible-y-min")))
        this.reminder_message = [
            "let-sgl", [["font", p_comment_window_reminder_font],
                        ["text-justification", "center"]],
            ["text", [_HALF, this.reminder_y], String("(Move scroll bar to bottom)")]]
        this.eliza_paragraphs = []
        this.non_eliza_paragraphs = []

    @message("object-type")
    def object_type(this, self):
        return "comment-window"

    @message("new-problem")
    def new_problem(this, self, initial_sym, modified_sym, target_sym, answer_sym):
        f = chez.format_
        if setup.p_justify_mode is not False:
            lines1 = [f('Let\'s see... "~a" changes to "~a", and', initial_sym, modified_sym),
                      f(' "~a" changes to "~a".  Hmm...', target_sym, answer_sym)]
            lines2 = [f('Beginning justify run:  "~a" changes to "~a", and',
                        initial_sym, modified_sym),
                      f(' "~a" changes to "~a"...', target_sym, answer_sym)]
        else:
            lines1 = [f('Okay, if "~a" changes to "~a", what', initial_sym, modified_sym),
                      f(' does "~a" change to?  Hmm...', target_sym)]
            lines2 = [f('Beginning run:  If "~a" changes to "~a", what',
                        initial_sym, modified_sym),
                      f(' does "~a" change to?', target_sym)]
        tell(self, "add-comment", lines1, lines2)
        return "done"

    @message("add-comment")
    def add_comment(this, self, lines1, lines2):
        paragraph1 = String("".join(lines1))
        paragraph2 = String("".join(lines2))
        this.eliza_paragraphs = [1, paragraph1] + this.eliza_paragraphs
        this.non_eliza_paragraphs = [1, paragraph2] + this.non_eliza_paragraphs
        return tell(this.text_window, "draw-paragraph",
                    paragraph1 if setup.p_eliza_mode is not False else paragraph2)

    @message("switch-modes")
    def switch_modes(this, self):
        tell(this.text_window, "set-paragraphs",
             this.eliza_paragraphs if setup.p_eliza_mode is not False
             else this.non_eliza_paragraphs)
        return tell(this.text_window, "redraw")

    @message("clear")
    def clear(this, self):
        this.eliza_paragraphs = []
        this.non_eliza_paragraphs = []
        return tell(this.text_window, "clear")

    @message("initialize")
    def initialize(this, self):
        tell(self, "clear")
        tell(this.text_window, "new-font", p_comment_window_font)
        tell(this.text_window, "centering-off")
        return tell(this.text_window, "draw", this.reminder_message)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.text_window)


def load():
    """port: commentary-graphics.ss's two top-level fonts, made once fonts.ss's
    faces are chosen (fonts.load())."""
    global p_comment_window_font, p_comment_window_reminder_font
    from metacat.gui import fonts
    p_comment_window_font = fonts.make_mfont(fonts.sans_serif, 12, ["bold", "italic"])
    p_comment_window_reminder_font = fonts.make_mfont(fonts.serif, 12, ["normal"])
