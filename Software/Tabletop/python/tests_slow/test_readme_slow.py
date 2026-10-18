"""The slow tests of `tests/test_readme.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_readme import (
    PYTHON_DIR,
    TARGET,
    block_prints_what_is_shown,
    front_blocks,
    front_outputs,
    json,
    pytest,
    readme_outputs,
    readme_run_blocks,
    run_block,
    shown_pairs,
)

pytestmark = pytest.mark.slow


def test_the_front_page_runs_the_quick_start_the_cli_and_the_benches(front_outputs):
    assert len(front_outputs) >= 4
    for block, proc in front_outputs:
        assert proc.returncode == 0, f"{block}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
        assert proc.stderr == "", (block, proc.stderr)
    joined = "\n".join(b for b, _ in front_outputs)
    assert "python3 -m tabletop surround-05 --seed 3 --quiet" in joined
    assert "python3 -m tabletop.bench families" in joined
    assert "python3 -m tabletop.bench ch2" in joined


def test_the_readme_blocks_run_from_the_python_folder(readme_outputs, tmp_path):
    assert len(readme_outputs) == 2, "the command line block and the quick bench block"
    for block, proc in readme_outputs:
        assert proc.returncode == 0, f"{block}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
        assert proc.stderr == "", (block, proc.stderr)
    cli, bench = readme_outputs
    assert "Eliza touches " in cli[1].stdout and "answer " in cli[1].stdout
    assert "Surround 5" in bench[1].stdout and "Blockage 6" in bench[1].stdout
    written = PYTHON_DIR / "run.jsonl"                 # the block's --json run.jsonl
    assert written.is_file()
    first = json.loads(written.read_text().splitlines()[0])
    assert first["event"] == "RunStarted" and first["seed"] == 3
    written.unlink()


def test_the_front_page_shows_what_its_blocks_print(front_outputs):
    printed = {block: proc.stdout for block, proc in front_outputs}
    for block, shown in shown_pairs():
        block_prints_what_is_shown(block, shown, printed[block])
