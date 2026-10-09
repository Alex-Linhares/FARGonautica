"""Which golden case each Perl screenshot (python/docs/gui/perl/NAME.png) was saved from.

Every drawing oracle saves its PNG from one case, chosen by a condition in the oracle (e.g.
``$PNG{$recipe} && $x == 0 && $w == 780`` in gui_workspace.pl). ``SCREENS`` repeats those
conditions, so the Qt renderer can draw the same items and save them under
python/docs/gui/screens/NAME.png for a side-by-side look.
"""
import golden


def _rect_780(case):
    x, _, w, _ = case["rect"]
    return x == 0 and w == 780


def _first_page(case):
    return _rect_780(case) and case.get("page", 0) == 0 and not case.get("redraw")


def _view(recipe, view):
    return ("gui_views", lambda c: c["recipe"] == recipe and c["view"] == view
            and c["size"][0] == 780 and not c["attention"] and not c["pages"])


def _recipe(golden_name, recipe, cond=_rect_780):
    return (golden_name, lambda c: c.get("recipe") == recipe and cond(c))


SCREENS = {
    "smoke": ("gui_smoke", lambda c: c["name"] == "styled"),
    "workspace_hilit_debug": _recipe("gui_workspace", "hilit_debug"),
    "workspace_twenty": _recipe("gui_workspace", "twenty_elements"),
    "workspace_bar_lines": _recipe("gui_workspace", "bar_lines"),
    "workspace_groups_relations": _recipe("gui_workspace", "groups_relations"),
    "workspace_nested_groups": _recipe("gui_workspace", "nested_groups"),
    "workspace_metonyms": _recipe("gui_workspace", "metonyms"),
    "workspace_overlapping_relations": _recipe("gui_workspace", "overlapping_relations"),
    "workspace_large": _recipe("gui_workspace", "large"),
    "attention_elements": _recipe("gui_workspace_attention", "attention_elements"),
    "attention_groups": _recipe("gui_workspace_attention", "attention_groups"),
    "attention_relations": _recipe("gui_workspace_attention", "attention_relations"),
    "slipnet_small": _recipe("gui_slipnet", "slipnet_small"),
    "slipnet_full": _recipe("gui_slipnet", "slipnet_full"),
    "slipnet_long_text": _recipe("gui_slipnet", "slipnet_long_text"),
    "slipnet_dir": _recipe("gui_slipnet", "slipnet_dir"),
    "coderack_small": _recipe("gui_coderack", "coderack_small", _first_page),
    "coderack_many": _recipe("gui_coderack", "coderack_many", _first_page),
    "coderack_zero_urgency": _recipe("gui_coderack", "coderack_zero_urgency", _first_page),
    "stream_small": _recipe("gui_stream", "stream_small"),
    "stream_full": _recipe("gui_stream", "stream_full"),
    "stream_real": _recipe("gui_stream", "stream_real"),
    "relations_pane": _recipe("gui_relations", "relations_pane"),
    "relations_many": _recipe("gui_relations", "relations_many"),
    "relations_groups_relations": _recipe("gui_relations", "groups_relations"),
    "list_groups": _recipe("gui_list_groups", "groups_list", _first_page),
    "list_groups_many": _recipe("gui_list_groups", "groups_many", _first_page),
    "list_groups_many_page3": ("gui_list_groups", lambda c: c["recipe"] == "groups_many"
                               and c["rect"][2] == 400 and c["page"] == 2
                               and not c.get("redraw")),
    "list_categories": _recipe("gui_list_categories", "groups_list", _first_page),
    "list_categories_many": _recipe("gui_list_categories", "categories_many", _first_page),
    "list_stream_small": _recipe("gui_list_stream", "stream_small", _first_page),
    "list_stream_many": _recipe("gui_list_stream", "stream_list_many", _first_page),
    "list_stream_real": _recipe("gui_list_stream", "stream_real", _first_page),
    "list_rules_fake": ("gui_list_rules", lambda c: c["fake"] and _first_page(c)),
    "views_0_attention_groups": _view("attention_groups", 0),
    "views_0_groups_relations": _view("groups_relations", 0),
    "views_2_attention_groups": _view("attention_groups", 2),
    "views_2_groups_relations": _view("groups_relations", 2),
    "views_3_slipnet_small": _view("slipnet_small", 3),
    "views_4_categories_many": _view("categories_many", 4),
    "views_5_coderack_small": _view("coderack_small", 5),
    "views_6_groups_relations": _view("groups_relations", 6),
    "views_9_stream_small": _view("stream_small", 9),
    "views_10_stream_list_many": _view("stream_list_many", 10),
    # Item 022: all 11 views of the end of a solved run (the Qt run: screens/e2e_run_<v>.png).
    **{f"views_{v}_solution": _view("solution", v) for v in range(11)},
}

# Screenshots of the whole Perl widget (menu strip + canvas; oracle/gui_window.pl), not of a
# golden case: the Qt side is a grab of the main window (test_gui_mainwindow.py), showing the
# recipe in view 0.
WINDOW_SCREENS = {"window_groups_relations": "groups_relations"}

# Screenshots of Perl widgets outside the canvas: the button frame (oracle/gui_controls.pl;
# the Qt side is the toolbar, test_gui_controls.py) and the commentary
# (oracle/gui_commentary.pl; the Qt side is the dock, test_gui_commentary.py), and the
# sequence-entry and more-terms windows (oracle/gui_seqentry.pl; test_gui_seqentry.py).
CONTROL_SCREENS = {"controls_buttons": "gui_controls", "commentary": "gui_commentary",
                   "seqentry": "gui_seqentry", "more_terms": "gui_seqentry",
                   "list_popup_groups": "gui_list_interaction", "entry_window": "gui_entry"}

_LOADED = {}


def _cases(name):
    if name not in _LOADED:
        _LOADED[name] = golden.load(name)
    return _LOADED[name]


def canvas_size(case):
    """The Perl canvas's size: smoke cases record it, the per-view oracles make the canvas
    ``x + w`` by ``y + h``, gui_views records ``size``."""
    if "width" in case:
        return case["width"], case["height"]
    if "size" in case:
        return tuple(case["size"])
    x, y, w, h = case["rect"]
    return x + w, y + h


def screen_case(name):
    """The (first) golden case the Perl screenshot NAME was saved from."""
    golden_name, cond = SCREENS[name]
    for case in _cases(golden_name):
        if cond(case):
            return case
    raise LookupError(name)
