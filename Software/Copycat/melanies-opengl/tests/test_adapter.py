"""The adapter builds, from a recorded trace, the objects Hager's client reads."""
import os

from melanies_opengl import lisprun
from melanies_opengl.trace import CODELET_CLASSES, Group, Letter, compact_rule

HERE = os.path.dirname(os.path.abspath(__file__))
FIXTURE = os.path.join(HERE, "fixtures", "ijk-seed1.jsonl")

HAGER_CODELET_NAMES = {
    'BondBottomUpScout', 'BondBuilder', 'BondStrengthTester', 'BondTopDownCategoryScout',
    'BondTopDownDirectionScout', 'Breaker', 'CorrespondenceBottomUpScout',
    'CorrespondenceBuilder', 'CorrespondenceImportantObjectScout',
    'CorrespondenceStrengthTester', 'DescriptionBottomUpScout', 'DescriptionBuilder',
    'DescriptionStrengthTester', 'DescriptionTopDownScout', 'GroupBuilder',
    'GroupStrengthTester', 'GroupTopDownCategoryScout', 'GroupTopDownDirectionScout',
    'GroupWholeStringScout', 'ReplacementFinder', 'RuleBuilder', 'RuleScout',
    'RuleStrengthTester', 'RuleTranslator'}


def test_init_line_builds_the_strings_and_the_slipnet():
    run = lisprun.replay_run(FIXTURE)
    ws = run.workspace
    assert ws.initial_string.name == "abc"
    assert ws.modified_string.name == "abd"
    assert ws.target_string.name == "ijk"
    assert ws.answer_string is None
    letters = ws.initial_string.get_letters()
    assert [l.name for l in letters] == ["a", "b", "c"]
    assert [l.left_string_position for l in letters] == [0, 1, 2]
    assert len(run.slipnet.slipnodes) == 59
    names = [n.short_name for n in run.slipnet.slipnodes]
    assert names[:3] == ["a", "b", "c"] and "rmost" in names and "letcat" in names
    assert run.timestep == 15 and run.coderack.time == 0
    assert ws.temperature == 100


def test_the_whole_replay_reaches_the_answer():
    run = lisprun.replay_run(FIXTURE).run_to_end()
    ws = run.workspace
    assert run.answer == "ijl"
    assert [l.name for l in ws.answer_string.get_letters()] == ["i", "j", "l"]
    assert ws.rule.to_string() == "Replace letter-category of rmost letter by successor"
    assert ws.rule.full.startswith('Replace letter with string-position-category "rmost"')
    assert ws.translated_rule.to_string().startswith("Replace ")
    assert run.coderack.time == 389
    assert ws.temperature == 13
    # the changed letter is marked, as Hager's view draws it in red
    assert [l.is_changed for l in ws.initial_string.get_letters()] == [False, False, True]


def test_structures_have_the_shape_the_client_draws():
    run = lisprun.replay_run(FIXTURE).run_to_end()
    ws = run.workspace
    bonds = ws.initial_string.get_bonds() + ws.target_string.get_bonds()
    assert bonds, "the final state has built bonds"
    for b in bonds:
        assert b.bond_category.name in ("successor", "predecessor", "sameness")
        assert b.bond_category.short_name in ("succ", "pred", "same")
        assert b.direction_category is None or b.direction_category.name in ("left", "right")
        assert isinstance(b.left_object, (Letter, Group))
        assert b.left_object.left_string_position <= b.right_object.left_string_position
        assert b.string in (ws.initial_string, ws.target_string)
    groups = ws.initial_string.get_groups() + ws.target_string.get_groups()
    assert groups
    for g in groups:
        assert g.type_name == "group"
        assert g.group_category.name in ("succesor group", "predecessor group", "sameness group")
        assert g.objects and all(o.type_name in ("letter", "group") for o in g.objects)
    corrs = ws.correspondences()
    assert corrs
    for c in corrs:
        assert c.object1.string is ws.initial_string
        assert c.object2.string is ws.target_string
        assert c.concept_mappings
        cm = c.concept_mappings[0]
        assert isinstance(cm.is_distinguishing(), bool)
        assert cm.descriptor1.short_name and cm.descriptor2.short_name


def test_structure_objects_keep_their_identity_between_lines():
    run = lisprun.replay_run(FIXTURE)
    seen = {}
    while run.step():
        for b in run.workspace.initial_string.get_bonds():
            key = (b.left_object.left_string_position, b.right_object.left_string_position,
                   b.bond_category.name, b.direction_category and b.direction_category.name)
            assert seen.setdefault(key, id(b)) == id(b)


def test_coderack_counts_use_hagers_class_names():
    run = lisprun.replay_run(FIXTURE)
    run.step(); run.step()
    names = {c.__class__.__name__ for c in run.coderack.codelets()}
    assert names and names <= HAGER_CODELET_NAMES
    assert run.coderack.last_chosen.__class__.__name__ in HAGER_CODELET_NAMES
    assert set(CODELET_CLASSES.values()) == HAGER_CODELET_NAMES


def test_slipnet_activations_follow_the_trace():
    run = lisprun.replay_run(FIXTURE).run_to_end()
    by = run.slipnet.by_name
    assert by["rmost"].activation == 100 and by["rmost"].is_active()
    assert by["middle"].activation == 0 and not by["middle"].is_active()
    assert all(isinstance(n.clamp, bool) for n in run.slipnet.slipnodes)


def test_compact_rule():
    assert compact_rule('Replace letter with string-position-category "rmost" by letter '
                        'with letter-category "D"') == \
        'Replace letter-category of rmost letter by "D"'
    assert compact_rule("Don't replace anything") == "Don't replace anything"
    assert compact_rule("something new") == "something new"
