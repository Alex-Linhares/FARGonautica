"""The folder's links and its standalone check (ANSWERS.md 6; TASK.md "Standalone"):
`tools/check_links.py` passes on the folder as it is read in both places (inside FARGonautica
and on its own), catches every kind of broken link on a synthetic folder, and, when a
FARGonautica checkout is found, every GitHub link to FARGonautica's files names a file that
exists there; `tools/standalone_check.sh` is sound bash and covers the steps that
`docs/standalone.md` records; the README's install leaves nothing git would see.

No PDF is needed; the test that needs a FARGonautica checkout skips without one.
"""

import importlib.util
import re
import subprocess
import sys
from pathlib import Path

import pytest

from sources import PYTHON_DIR, TARGET, checker

TOOL = PYTHON_DIR / "tools" / "check_links.py"
SCRIPT = PYTHON_DIR / "tools" / "standalone_check.sh"
DOC = PYTHON_DIR / "docs" / "standalone.md"
FRONT = TARGET / "README.md"
README = PYTHON_DIR / "README.md"


def load_tool():
    spec = importlib.util.spec_from_file_location("tabletop_check_links", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


links = load_tool()
CHECKOUT = checker.fargonautica()


# -- the folder's own links ---------------------------------------------------------------

def test_every_link_of_the_folder_is_good_offline():
    report = links.check_folder(TARGET, CHECKOUT)
    assert not report.failures, "\n".join(report.failures)
    assert len(report.links) >= 60
    assert report.count("relative") >= 30 and report.count("anchor") >= 20
    assert report.count("github") >= 4, "the sources and Seek-Whence are linked on GitHub"
    assert report.count("outside") >= 1, "the standalone repository's own URL"


def test_the_summary_says_which_place_it_is_in():
    with_checkout = links.summary(links.check_folder(TARGET, TARGET))
    without = links.summary(links.check_folder(TARGET, None))
    assert "inside the FARGonautica checkout" in with_checkout
    assert "no FARGonautica checkout" in without
    assert re.search(r"^\d+ links in \d+ Markdown files under ", without)


@pytest.mark.skipif(CHECKOUT is None,
                    reason="no FARGonautica checkout found: set FARGONAUTICA")
def test_every_github_link_names_a_file_of_the_fargonautica_checkout():
    report = links.check_folder(TARGET, CHECKOUT)
    github = [l.target for l in report.links if l.kind == "github"]
    assert len(github) >= 4
    for target in github:
        rel = links.unquote(links.GITHUB_BLOB.match(target).group(1))
        assert (CHECKOUT / rel).exists(), rel
        assert " " not in target and ".." not in target


def test_the_command_line_passes_on_the_folder_and_fails_on_a_wrong_checkout(tmp_path):
    ok = subprocess.run([sys.executable, str(TOOL), str(TARGET)],
                        capture_output=True, text=True)
    assert ok.returncode == 0, ok.stdout + ok.stderr
    assert "every link checked is good." in ok.stdout
    bad = subprocess.run([sys.executable, str(TOOL), str(TARGET), "--fargonautica",
                          str(tmp_path)], capture_output=True, text=True)
    assert bad.returncode == 1
    fails = [l for l in bad.stdout.splitlines() if l.startswith("FAIL ")]
    assert len(fails) >= 4 and all("in the FARGonautica checkout" in l for l in fails)
    assert any("copycat.md" in l for l in fails)
    missing = subprocess.run([sys.executable, str(TOOL), str(tmp_path / "none")],
                             capture_output=True, text=True)
    assert missing.returncode == 2


# -- each kind of broken link ------------------------------------------------------------

def synthetic_folder(tmp_path: Path) -> tuple[Path, Path]:
    root = tmp_path / "folder"
    (root / "docs").mkdir(parents=True)
    (root / "README.md").write_text(
        "# Real heading\n\n"
        "[ok](#real-heading) [bad](#nope) [doc](docs/b.md) [there](docs/b.md#there)\n"
        "[gone](docs/b.md#missing) [nothing](docs/c.md) [out](../out.md) [root](/etc)\n"
        "[pdf](https://github.com/fargonauts/FARGonautica/blob/master/Literature/x%20y.pdf)\n"
        "[absent](https://github.com/fargonauts/FARGonautica/blob/master/missing.md)\n"
        "[mail](mailto:someone@example.org) [site](https://example.org/)\n"
        "`[code](nowhere.md)`\n\n```\n[fenced](nowhere-else.md)\n```\n",
        encoding="utf-8")
    (root / "docs" / "b.md").write_text("# There\n\ntext\n", encoding="utf-8")
    checkout = tmp_path / "checkout"
    (checkout / "Literature").mkdir(parents=True)
    (checkout / "Literature" / "x y.pdf").write_bytes(b"%PDF-")
    return root, checkout


def test_each_kind_of_broken_link_is_caught_and_the_good_ones_pass(tmp_path):
    root, checkout = synthetic_folder(tmp_path)
    report = links.check_folder(root, checkout)
    assert [l.kind for l in report.links].count("mailto") == 1
    assert report.count("outside") == 1 and report.count("github") == 2
    reasons = {f.split(": ", 1)[1] for f in report.failures}
    assert reasons == {
        "#nope (no such heading)",
        "docs/b.md#missing (no such heading in b.md)",
        "docs/c.md (points at nothing)",
        "../out.md (leaves the folder)",
        "/etc (leaves the folder)",
        f"https://github.com/fargonauts/FARGonautica/blob/master/missing.md (no missing.md "
        f"in the FARGonautica checkout {checkout})",
    }
    assert not any("nowhere" in l.target for l in report.links), "code and fences are skipped"
    assert all(f.startswith("FAIL README.md: ") for f in report.failures)


def test_without_a_checkout_the_github_paths_are_only_counted(tmp_path):
    root, _ = synthetic_folder(tmp_path)
    report = links.check_folder(root, None)
    assert report.count("github") == 2
    assert not any("FARGonautica checkout" in f for f in report.failures)


def test_online_checks_fetch_each_url_once_and_retry_a_server_error(tmp_path):
    root, checkout = synthetic_folder(tmp_path)
    report = links.check_folder(root, checkout)
    calls = []
    answers = {"https://example.org/": [503, 200],
               "https://github.com/fargonauts/FARGonautica/blob/master/missing.md": [404]}

    def fetch(url, timeout):
        calls.append(url)
        queue = answers.get(url, [200])
        return queue.pop(0) if len(queue) > 1 else queue[0]

    links.check_online(report, 5.0, fetch=fetch)
    assert set(report.online) == set(report.urls) and len(report.urls) == 3
    assert calls.count("https://example.org/") == 2, "retried once on 503"
    assert report.online["https://example.org/"] == 200
    assert report.online_failures() == [
        "ONLINE https://github.com/fargonauts/FARGonautica/blob/master/missing.md: 404"]
    assert "online: 2 of 3 URLs answered 200." in links.summary(report)


def test_fetch_status_names_an_error_instead_of_raising():
    status = links.fetch_status("https://localhost.invalid/", 2.0)
    assert isinstance(status, str) and status.endswith("Error")


# -- the procedure: the script and the document ------------------------------------------

def test_the_standalone_script_is_sound_bash_and_covers_the_procedure():
    assert subprocess.run(["bash", "-n", str(SCRIPT)]).returncode == 0
    text = SCRIPT.read_text(encoding="utf-8")
    for needed in ("git subtree split --prefix=Software/Tabletop", "-u FARGONAUTICA",
                   "-u FCCA_PDF", 'FARGONAUTICA="$REPO"', "python3 -m venv",
                   "pip install", "tabletop", "run-tests.sh", "check_links.py", "--online",
                   "--offline-install", "mktemp -d /tmp/"):
        assert needed in text, needed
    assert "push" not in text.replace("pushes anything", ""), "the script never pushes"
    usage = subprocess.run(["bash", str(SCRIPT), "--help"], capture_output=True, text=True)
    assert usage.returncode == 0 and "git subtree split" in usage.stdout


def test_the_standalone_doc_records_the_procedure_and_its_results():
    text = DOC.read_text(encoding="utf-8")
    for needed in ("git subtree split --prefix=Software/Tabletop", "FARGONAUTICA", "FCCA_PDF",
                   "pip install", "tabletop --list", "check_links.py", "standalone_check.sh",
                   "2026-10-13", "passed", "skipped", "fargonauts/Tabletop", "404"):
        assert needed in text, needed
    assert "/home/" not in text and "Zotero" not in text


def test_both_readmes_point_at_the_standalone_doc_and_the_tools():
    front = FRONT.read_text(encoding="utf-8")
    readme = README.read_text(encoding="utf-8")
    assert "python/docs/standalone.md" in front and "standalone_check.sh" in front
    assert "docs/standalone.md" in readme and "check_links.py" in readme
    assert "--no-build-isolation" in front, "the offline install is explained"


def test_gitignore_covers_what_an_install_and_the_tests_leave():
    ignored = (PYTHON_DIR / ".gitignore").read_text(encoding="utf-8").split()
    for entry in (".venv/", "*.egg-info/", "__pycache__/", ".pytest_cache/", "build/",
                  "dist/", "bench-results/"):
        assert entry in ignored, entry
