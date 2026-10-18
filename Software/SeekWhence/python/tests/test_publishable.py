"""The folder can be published: no file under `Software/SeekWhence/` refers to unpublished
material kept outside it (by title, short title, code package or folder).

Every tracked file is read (every file, when the folder is not a git checkout); binary files
are skipped. The forbidden phrases are matched case-insensitively and across line breaks and
comment markers. They are assembled here from pieces, so that this file, which must name
them to look for them, does not contain them and needs no exemption.
"""

import re
import subprocess
from pathlib import Path

FOLDER = Path(__file__).resolve().parents[2]   # Software/SeekWhence (or a standalone clone)

_GAP = r"[\W_]+"
FORBIDDEN = [
    re.compile(_GAP.join(["how", "to", "co" + "de"]), re.I),
    re.compile(_GAP.join(["clean" + "ly", "and", "sim" + "ply"]), re.I),
    re.compile("farg" + "book", re.I),
    re.compile(r"\b" + "bo" + "ok" + "/", re.I),
]

SKIPPED_DIRS = {".git", "__pycache__", ".pytest_cache", ".venv", "venv"}


def files() -> list[Path]:
    """The tracked files of the folder, or all of them outside a git checkout."""
    try:
        out = subprocess.run(["git", "ls-files", "-z", "."], cwd=FOLDER, capture_output=True,
                             check=True).stdout
        tracked = [FOLDER / name for name in out.decode().split("\0") if name]
        if tracked:
            return [p for p in tracked if p.is_file()]
    except (OSError, subprocess.CalledProcessError):
        pass
    return [p for p in FOLDER.rglob("*")
            if p.is_file() and not SKIPPED_DIRS.intersection(p.relative_to(FOLDER).parts)]


def test_the_folder_has_files():
    names = {p.name for p in files()}
    assert "README.md" in names and "test_publishable.py" in names


def test_no_file_refers_to_unpublished_material():
    hits = []
    for path in files():
        try:
            text = path.read_text(encoding="utf-8")
        except (UnicodeDecodeError, OSError):
            continue
        for pattern in FORBIDDEN:
            for m in pattern.finditer(text):
                line = text.count("\n", 0, m.start()) + 1
                hits.append(f"{path.relative_to(FOLDER)}:{line}: {m.group()!r}")
    assert not hits, "references to unpublished material:\n" + "\n".join(hits)
