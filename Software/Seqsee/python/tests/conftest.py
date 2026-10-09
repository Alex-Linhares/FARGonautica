import importlib.util
import os
import pathlib

# Qt tests run headless: never open a real window (must be set before Qt is imported).
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest

from seqsee import s, util

HAVE_PYSIDE6 = importlib.util.find_spec("PySide6") is not None
# The Perl Seqsee tree (lib/) next to python/. A copy of the port without it (e.g. in
# FARGonautica) skips the tests that read the Perl source.
HAVE_PERL_SOURCE = (pathlib.Path(__file__).resolve().parents[2] / "lib").is_dir()


def pytest_addoption(parser):
    parser.addoption("--write-screens", action="store_true", default=False,
                     help="save the Qt screenshots under python/docs/gui/screens/")


def pytest_collection_modifyitems(config, items):
    """Tests marked ``gui`` need PySide6; skip them (like importorskip) when it is missing.
    Tests marked ``perl_source`` read the Perl lib/; skip them when it is not there."""
    no_qt = pytest.mark.skip(reason="PySide6 not installed")
    no_perl = pytest.mark.skip(reason="the Perl Seqsee source (../lib) is not next to python/")
    for item in items:
        if "gui" in item.keywords and not HAVE_PYSIDE6:
            item.add_marker(no_qt)
        if "perl_source" in item.keywords and not HAVE_PERL_SOURCE:
            item.add_marker(no_perl)


def _reset_all():
    s.reset_all()


@pytest.fixture(autouse=True)
def _reset_global_state():
    """Give every test fresh $Global::… state, an empty LTM and an empty
    Mapping::Numeric create memo, an empty SRule create memo, an empty
    SLTM::Platonic create memo, an empty Memory::LTM, an empty workspace, an empty coderack,
    an empty SThought create memo, fresh AllMX and Seqsee.pm module state and the default UI
    answer callbacks with empty RulesAskedSoFar/SolutionConfirmation state, and no cached
    script validation spec (``s.reset_all``)."""
    _reset_all()
    yield
    _reset_all()


@pytest.fixture
def seeded():
    """Seed the central RNG (seqsee.util, Perl's drand48) so stochastic code is reproducible."""
    util.srand(12345)
    yield
