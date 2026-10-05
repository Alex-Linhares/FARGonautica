"""Globals that the model reads but whose values come from the graphics files.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) after racket/engine/view-globals.rktl.

Colours and fonts (constants.ss's graphics part, general-graphics.ss,
workspace-graphics.ss, coderack-graphics.ss) and gui.ss's speed settings.  In
the original they are top-level definitions of those files, or created by set!
when the windows are made.  Here, as in the Racket port, they are engine
globals, #f until the views (item 14) are attached; the views set them through
engine.set_global.  A headless run never draws, so it never reads them.  The
engine never imports tkinter.
"""
from __future__ import annotations

# constants.ss: common colour names
c_white = c_black = c_grey = c_red = c_green = c_blue = c_yellow = c_pink = c_orange = False

# constants.ss: colours the model files keep or draw with
p_vertical_slippage_color = False
p_dim_vertical_slippage_color = False
p_coattail_inducing_slippage_color = False
p_dim_coattail_inducing_slippage_color = False
p_top_bridge_color = False
p_vertical_bridge_color = False
p_bottom_bridge_color = False
p_bridge_label_background_color = False
p_faded_bridge_label_background_color = False
p_top_rule_color = False
p_bottom_rule_color = False
p_snag_color = False
p_theme_supporting_concept_mapping_color = False
p_faded_workspace_structure_color = False
p_workspace_event_structure_color = False
p_clamp_event_concept_pattern_color = False
p_concept_activation_event_concept_pattern_color = False
p_concept_mapping_event_concept_pattern_color = False
p_group_event_concept_pattern_color = False
p_top_rule_event_concept_pattern_color = False
p_bottom_rule_event_concept_pattern_color = False
p_snag_event_concept_pattern_color = False
p_coderack_background_color = False
p_current_codelet_color = False
p_extremely_low_urgency_color = False
p_very_low_urgency_color = False
p_low_urgency_color = False
p_medium_urgency_color = False
p_high_urgency_color = False
p_very_high_urgency_color = False
p_extremely_high_urgency_color = False

# general-graphics.ss: the default foreground colour, which init-mcat (run.ss)
# and the Temporal Trace (trace.ss) set *fg-color* back to
p_default_fg_color = False
g_fg_color = False

# workspace-graphics.ss: fonts created by set! when the Workspace window is made
# (select-workspace-fonts), never defined; groups.ss, bridge-graphics.ss and
# rule-graphics.ss read the first four
p_group_letter_category_font = False
p_relevant_group_length_font = False
p_bridge_label_font = False
p_rule_font = False
p_workspace_title_font = False
p_codelet_count_font = False
p_letter_font = False
p_irrelevant_group_length_font = False
p_relevant_concept_mapping_font = False
p_irrelevant_concept_mapping_font = False
p_concept_mapping_list_superscript_font = False


def restore_current_state():
    """workspace-graphics.ss: restore-current-state.  run.ss's go calls it when
    *display-mode?* is on, which only the Workspace window's handlers turn on;
    the views replace it."""
    raise RuntimeError("restore-current-state: no views are loaded")


# coderack-graphics.ss: a font created by set! when the Coderack window is made
# (select-coderack-fonts), never defined; coderack.ss's codelet types draw their
# counts with it
p_coderack_codelet_count_font = False

# gui.ss: the speed settings, read by the windows (flashes, pauses) and set by
# the control panel's speed slider
p_num_of_flashes = False
p_flash_pause = False
p_snag_pause = False
p_codelet_highlight_pause = False
p_text_scroll_pause = False
