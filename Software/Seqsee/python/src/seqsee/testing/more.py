"""The part of Test::More (Test::Builder) the harness uses: ``ok($test, $name)``.

Each call appends ``(ok, name)`` to ``details`` (Test::Builder's details) and returns the
ok value (1 or 0), so a pytest test can ``assert`` it or inspect ``details``. Nothing is
printed. ``reset()`` empties ``details``.
"""
from seqsee.util import perl_true

details = []


def ok(test, name=None):
    """Perl: Test::More::ok($test, $name)."""
    result = 1 if perl_true(test) else 0
    details.append((result, name))
    return result


def reset():
    details.clear()
