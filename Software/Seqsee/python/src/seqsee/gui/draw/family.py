"""SGUI::Coderack::family_to_name (lib/SGUI/Coderack.pm): the display name of a runnable.

Perl reads the package variable ``${ $family . '::NAME' }``. Only the SThought classes define
``our $NAME``; no codelet family does, so for the ``Seqsee::SCF::<family>`` strings that
``$Global::CurrentRunnableString`` holds in a real run the name is undef (an empty label).
"""

# Every `our $NAME` in lib/ (tests/test_gui_workspace.py checks this against the Perl source
# and the NAME attributes of the ported SThought classes).
FAMILY_NAMES = {
    "SThought::Seqsee::Anchored": "Focusing on a Group",
    "SThought::Seqsee::Element": "Focusing on a Single Element",
    "SThought::SRelation": "Focusing on an Analogy",
    "SThought::SCat": "Focusing on a Category",
}


def family_to_name(family):
    """The ``$NAME`` of package ``family``, or None (Perl's undef)."""
    return FAMILY_NAMES.get(family or "")
