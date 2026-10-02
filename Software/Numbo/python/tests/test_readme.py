"""Loop0002 item 13: python/README.md can be followed from a fresh shell.

As lisp/tests/readme-test.sh does for lisp/src/README.md: every ```sh block of
python/README.md is run as written, by bash, from Software/Numbo/, in an
environment with only PATH, HOME and LANG (no PYTHONPATH, no virtualenv
activation, nothing installed).  Every block must exit 0, the first block's
output must end with the text the README shows after "The output ends with:"
(trailing blanks ignored),
and some block must print a valid solution.
"""

import os
import re
import subprocess

import pytest

from full_runs import PYTHON_DIR, REPO_DIR

README = PYTHON_DIR / "README.md"


def sh_blocks():
    return re.findall(r"^```sh\n(.*?)^```$", README.read_text(), re.M | re.S)


def shown_output():
    text = README.read_text()
    after = text.split("The output ends with:", 1)[1]
    return re.search(r"^```\n(.*?)^```$", after, re.M | re.S).group(1)


def blocks_with_shown_output():
    """(sh block, shown output) for each ```sh block followed by a plain ```
    block with only prose ending in a colon, or nothing, in between."""
    fences = re.findall(r"^```(\w*)\n(.*?)^```$\n(.*?)(?=^```|\Z)", README.read_text(),
                        re.M | re.S)
    pairs = []
    for (lang, body, between), (next_lang, next_body, _) in zip(fences, fences[1:]):
        if lang == "sh" and next_lang == "" and (not between.strip()
                                                 or between.strip().endswith(":")):
            pairs.append((body, next_body))
    return pairs


def fresh_env():
    return {k: os.environ[k] for k in ("PATH", "HOME", "LANG") if k in os.environ}


@pytest.fixture(scope="module")
def outputs():
    results = []
    for block in sh_blocks():
        proc = subprocess.run(["bash", "-c", block], cwd=REPO_DIR, env=fresh_env(),
                              capture_output=True, text=True, timeout=600)
        results.append((block, proc))
    return results


def test_readme_has_blocks():
    assert len(sh_blocks()) >= 3


def test_every_block_runs(outputs):
    for block, proc in outputs:
        assert proc.returncode == 0, f"{block}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"


def test_first_block_output_is_shown(outputs):
    """Trailing blanks ignored ("applied " has one), as readme-test.sh does."""
    expected = [line.rstrip() for line in shown_output().splitlines()]
    got = [line.rstrip() for line in outputs[0][1].stdout.splitlines()]
    assert len(expected) >= 5
    assert got[-len(expected):] == expected


def test_shown_outputs(outputs):
    """Every output the README shows after a block is what the block prints
    (its end, trailing blanks ignored)."""
    printed = {block: proc.stdout + proc.stderr for block, proc in outputs}
    pairs = blocks_with_shown_output()
    assert len(pairs) >= 3
    for block, shown in pairs:
        expected = [line.rstrip() for line in shown.splitlines()]
        got = [line.rstrip() for line in printed[block].splitlines()]
        assert got[-len(expected):] == expected, block


def test_a_block_solves_a_puzzle(outputs):
    assert any("\ncheck: valid: " in proc.stdout for _, proc in outputs)


# -- loop0003 item 12: the GUI and architecture sections ----------------------------

def section(title):
    """The text of the `## TITLE` section, up to the next `## ` heading."""
    match = re.search(rf"^## {re.escape(title)}\n(.*?)(?=^## |\Z)", README.read_text(),
                      re.M | re.S)
    assert match, f"no '## {title}' section"
    return match.group(1)


def test_the_gui_section_runs_the_smoke_command_and_the_cli():
    blocks = re.findall(r"^```sh\n(.*?)^```$", section("The GUI"), re.M | re.S)
    assert any("python3 -m numbo.gui --smoke" in b for b in blocks)
    assert any("python3 -m numbo " in b for b in sh_blocks())
    pairs = [body for body, _ in blocks_with_shown_output()]
    assert any("numbo.gui --smoke" in body for body in pairs), "its output is shown"


def test_the_screenshots_are_committed_small_pngs():
    images = re.findall(r"!\[[^\]]+\]\(([^)]+)\)", section("The GUI"))
    assert 1 <= len(images) <= 2, images
    for image in images:
        path = (PYTHON_DIR / image).resolve()
        assert path.parent == PYTHON_DIR / "docs", image
        data = path.read_bytes()
        assert data.startswith(b"\x89PNG\r\n\x1a\n"), image
        assert len(data) < 200 * 1024, (image, len(data))
        tracked = subprocess.run(["git", "check-ignore", "-q", str(path)], cwd=REPO_DIR)
        assert tracked.returncode == 1, f"{image} is gitignored"


def test_the_architecture_section():
    text = section("Architecture")
    assert re.search(r"^```text\n.*Subject.*^```$", text, re.M | re.S), "a diagram"
    for words in ("OracleTraceWriter", "SessionRecorder", "TreeModel", "RunController",
                  "worker thread", "queued", "acknowledge", "paint_all", "Qt-free"):
        assert words in text, words
