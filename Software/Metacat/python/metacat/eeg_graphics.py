"""eeg-graphics.ss: the EEG object (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from eeg-graphics.ss, with
racket/engine/eeg-graphics.rktl as a worked translation.

The model's part of eeg-graphics.ss, as the Racket port split it
(docs/porting-notes.md, item 14): %EEG-table%, %EEG-buffer-size%, make-EEG and
*EEG*.  workspace.ss's initialize tells *EEG* 'initialize on every run, and
run.ss's update-everything feeds it ('record-current-values) when
%workspace-graphics% is on.  The EEG window (%max-EEG-window-cycles%, its font,
make-EEG-window) is metacat/gui's.  The EEG only reads the Workspace, the
temperature and its own buffer: it never draws a random number and never
imports tkinter.  *workspace* and *temperature* are read at call time.
"""
from __future__ import annotations

from metacat import chez, setup, workspace
from metacat.chez import String
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import average, base_object, first, maximum, minimum, round_to_100ths

import metacat as _metacat

# The %EEG-table% specifies a set of values to track, which of these values
# to plot in the EEG window, and how to label the information.  Each table
# entry defines a particular value that the EEG object will compute and
# record at regular intervals during a run.  If desired, these values may be
# computed from other values recorded by the EEG (using the EEG's
# 'get-current-value or 'get-average-value methods).
#
# Each entry in the table is of the form:
#
#  (index-number label-string color-name initial-value plot? thunk)
#
# Values are referred to by their index number.  All values listed in the
# table will be recorded, but only those with plot? set to #t will be
# plotted.  All plotted values must be in the range [0..100]
#
# For example, the table below specifies three values to track: the current
# Workspace activity, the average of the last 10 values recorded for table
# entry #0 (Workspace activity), and the current temperature.  The average
# Workspace activity will be plotted in yellow and the temperature will be
# plotted in red.  Current Workspace activity is recorded so that the average
# Workspace activity can be computed, but is not plotted itself.  In general,
# plotting average values instead of instantaneous values usually results in
# a smoother curve that is less sensitive to momentary fluctuations in value.

p_EEG_table = [
    [0, String("Workspace Activity"), String("white"), 100, False,
     # current Workspace activity value
     lambda: tell(workspace.g_workspace, "get-activity")],
    [1, String("Average Workspace Activity"), String("yellow"), 100, True,
     # average of last 10 Workspace activity values
     lambda: tell(_metacat.eeg_graphics.g_EEG, "get-average-value", 0, 10)],
    [2, String("Temperature"), String("red"), 100, True,
     # current temperature
     lambda: setup.g_temperature],
]

# ---------------------------------------------------------------------------

p_EEG_buffer_size = 40


class EEG(SchemeObject):
    """eeg-graphics.ss: make-EEG (the closure)"""
    __slots__ = ("circular_array", "initial_values", "value_procs", "get_values", "next")

    def __init__(this):
        this.circular_array = False
        this.initial_values = False
        this.value_procs = False
        this.get_values = False
        this.next = False

    @message("object-type")
    def object_type(this, self):
        return "EEG"

    @message("print")
    def print_(this, self):
        for i in range(len(p_EEG_table)):
            chez.printf("[~a]:  ", i)
            for j in range(p_EEG_buffer_size):
                chez.printf("~a ", round_to_100ths(this.circular_array[j][i]))
            chez.newline()
        return chez.printf("next = ~a~n", this.next)

    @message("get-current-value")
    def get_current_value(this, self, value_index):
        return tell(self, "get-current-values")[value_index]

    @message("get-current-values")
    def get_current_values(this, self):
        return this.circular_array[chez.modulo(chez.sub1(this.next), p_EEG_buffer_size)]

    @message("record-current-values")
    def record_current_values(this, self):
        this.circular_array[this.next] = this.get_values()
        this.next = chez.modulo(chez.add1(this.next), p_EEG_buffer_size)
        return "done"

    @message("get-average-value")
    def get_average_value(this, self, value_index, *args):
        if len(args) == 0:
            return average(tell(self, "get-previous-values", value_index))
        return average(tell(self, "get-previous-values", value_index, first(args)))

    @message("get-max-variation")
    def get_max_variation(this, self, value_index, *args):
        if len(args) == 0:
            previous_values = tell(self, "get-previous-values", value_index)
        else:
            previous_values = tell(self, "get-previous-values", value_index, first(args))
        return chez.abs_(chez.sub(maximum(previous_values), minimum(previous_values)))

    @message("get-previous-values")
    def get_previous_values(this, self, value_index, *args):
        spread_size = (p_EEG_buffer_size if len(args) == 0
                       else chez.min_(first(args), p_EEG_buffer_size))
        previous_values = []
        for i in range(1, spread_size + 1):
            previous_values = ([this.circular_array[chez.modulo(chez.sub(this.next, i),
                                                                p_EEG_buffer_size)][value_index]]
                               + previous_values)
        return previous_values

    @message("initialize")
    def initialize(this, self):
        this.initial_values = [entry[3] for entry in p_EEG_table]    # (map 4th ...)
        this.value_procs = [entry[5] for entry in p_EEG_table]       # (map 6th ...)
        # chez: map's order of application (the thunks only read)
        this.get_values = lambda: chez.map_(lambda f: f(), this.value_procs)
        # make-vector: every slot holds the same list, as in the original
        this.circular_array = chez.make_vector(p_EEG_buffer_size, this.initial_values)
        this.next = 0
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_EEG():
    """eeg-graphics.ss: make-EEG"""
    return EEG()


g_EEG = make_EEG()
