"""docs/python-translation-plan.md lists every `# chez:` and `# 1.2:` site of the
package (item 17); the list must be the code's.  After changing a site, run
python3 python/tests/quirk_sites.py --write.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).
"""

import quirk_sites


def test_the_plan_lists_every_site():
    assert quirk_sites.plan_section(quirk_sites.PLAN.read_text()) == quirk_sites.render()


def test_docstrings_are_not_sites():
    # groups.py's module docstring mentions `# chez:`; only comments count
    assert all(text != "`.  The engine never" for _, _, _, text in quirk_sites.sites())
    assert len(quirk_sites.sites()) > 200
