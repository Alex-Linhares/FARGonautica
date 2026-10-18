"""Loop0004 item 02: the run inputs and the appendix presets
(`seekwhence/gui/models/presets.py`), Qt-free (G-009).

The three appendix presets carry Meredith's console lines, commands included, and her
answers [M86 pp. 164-179] on the golden seeds, verbose or terse as the printout is; a
seed can be given; `custom_inputs` takes inputs directly.
"""

from seekwhence import bench
from seekwhence.config import Config
from seekwhence.gui.models import presets


def test_the_appendix_presets_are_meredith_s_inputs():
    exp = bench.load_expectations()
    assert presets.appendix_ids() == ("run1_3737", "run2_373373", "run3_121231234")
    for run_id, seed in (("run1_3737", 54), ("run2_373373", 54), ("run3_121231234", 30)):
        spec = bench.run_spec(exp, run_id)
        inputs = presets.appendix_inputs(run_id)
        assert inputs.preset == run_id and inputs.seed == seed
        assert list(inputs.lines) == bench.inputs_of(spec)
        assert list(inputs.answers) == bench.answers_of(spec)
        assert inputs.config == Config(verbose=spec["prints_task_lines"])
        assert not inputs.interactive and inputs.title.startswith("run ")
    assert presets.appendix_inputs("run2_373373").config.verbose is False   # [M86 p. 167]


def test_a_seed_and_interactive_can_be_given():
    inputs = presets.appendix_inputs("run1_3737", seed=7, interactive=True)
    assert inputs.seed == 7 and inputs.interactive
    assert inputs.lines == (3, 7, 3, "show-hypothesis", 7) and inputs.answers == ("ok",)


def test_custom_inputs():
    inputs = presets.custom_inputs(Config(), 3, [1, 8, 5, 5], ["no"])
    assert inputs == presets.RunInputs("custom", Config(), 3, (1, 8, 5, 5), ("no",), False,
                                       "1 8 5 5")
    assert presets.custom_inputs(Config(), 1, None).lines is None
