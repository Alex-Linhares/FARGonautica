"""Port of lib/S.pm: the category/metonym singletons (``$S::NUMBER`` etc.) and its ``use``
list (``load``). The constant packages (DIR, POS_MODE, …) are in constants.py.

The singletons are created in S.pm's order. ASCENDING … EVEN are the real categories
(items 010–012). Each is a ``new`` object, so ``Ascending()`` gives a different one
(oracle-confirmed). DOUBLE (item 015) is the SMetonymType S.pm builds with ``new``, so it
is not in the create memo. Read them at call time (``S.NUMBER``) so later replacement takes
effect.

PERL-QUIRK: ``$S::AD_HOC = $SCat::ad_hoc::AD_HOC`` names a package that no longer exists,
so AD_HOC is undef (None).

``use S`` loads almost every module. Here modules import what they need, and many imports
are lazy to break cycles, so a codelet family or multimethod variant can be missing until
its module is first imported. ``load()`` imports everything in S.pm's order, plus
Sanity.pm (``use``d by Seqsee.pm). Call it before a run (item 047).
"""
import importlib

from seqsee.categories.ascending import Ascending
from seqsee.categories.descending import Descending
from seqsee.categories.even import Even
from seqsee.categories.mountain import Mountain
from seqsee.categories.number import Number
from seqsee.categories.odd import Odd
from seqsee.categories.prime import Prime
from seqsee.categories.sameness import Sameness
from seqsee.smetonym_type import SMetonymType

ASCENDING = Ascending()
DESCENDING = Descending()
MOUNTAIN = Mountain()
SAMENESS = Sameness()
AD_HOC = None
NUMBER = Number()
PRIME = Prime()
ODD = Odd()
EVEN = Even()

DOUBLE = SMetonymType({"category": SAMENESS, "name": "each", "info_loss": {"length": 2}})

# S.pm's `use` list, in order, as Python modules. The *::Load modules are expanded into
# the modules they `use`.
USES = (
    "seqsee.global_",                      # Global
    "seqsee.shistory",                     # SHistory
    "seqsee.errors",                       # SErr
    "seqsee.user_interaction",             # UserInteraction
    "seqsee.schoose",                      # SChoose
    "seqsee.sbindings",                    # SBindings
    "seqsee.spos",                         # SPos
    "seqsee.sfasc",                        # SFasc
    "seqsee.scodelet",                     # SCodelet
    "seqsee.saction",                      # SAction
    "seqsee.scoderack",                    # SCoderack
    "seqsee.smetonym",                     # SMetonym
    "seqsee.smetonym_type",                # SMetonymType
    "seqsee.categories.mapping_based",     # SCategory::Load
    "seqsee.categories.interlaced",
    "seqsee.categories.alternating",
    "seqsee.categories.ascending",
    "seqsee.categories.descending",
    "seqsee.categories.mountain",
    "seqsee.categories.sameness",
    "seqsee.categories.number",
    "seqsee.categories.prime",
    "seqsee.categories.odd",
    "seqsee.categories.even",
    "seqsee.objects.object",               # Seqsee::Object
    "seqsee.objects.anchored",             # Seqsee::Anchored
    "seqsee.objects.element",              # Seqsee::Element
    "seqsee.sint",                         # SInt
    "seqsee.sworkspace",                   # SWorkspace
    "seqsee.objects.result_of_can_be_seen_as",
    "seqsee.mapping",                      # Mapping
    "seqsee.mapping.numeric",
    "seqsee.mapping.structural",
    "seqsee.mapping.position",
    "seqsee.mapping.meto_type",
    "seqsee.mapping.dir",
    "seqsee.srelation",                    # SRelation, SRelation::Structural
    "seqsee.sthought",                     # SThought
    "seqsee.sstream2",                     # SStream2
    "seqsee.codelets.all_mx",              # Seqsee::SCF_MX::Load
    "seqsee.codelets.all_mx2",
    "seqsee.codelets.ui",
    "seqsee.codelets.general",
    "seqsee.codelets.large_gp",
    "seqsee.sthought.sobject",             # SThought::Load
    "seqsee.sthought.relations",
    "seqsee.sthought.scat",
    "seqsee.scripts.describe_solution",    # Seqsee::Scripts::Load
    "seqsee.slink_activation",             # SLinkActivation
    "seqsee.snode_activation",             # SNodeActivation
    "seqsee.sltm",                         # SLTM
    "seqsee.srule",                        # SRule
    "seqsee.srule_app",                    # SRuleApp
    "seqsee.position_structure",           # PositionStructure
    "seqsee.sanity",                       # Sanity (used by Seqsee.pm)
)


def load():
    """Perl: ``use S; use Sanity;``: import every module in ``USES`` (idempotent) and
    return them, in order. Afterwards every codelet family, script and multimethod variant
    is registered."""
    from seqsee.codelets import family
    mods = tuple(importlib.import_module(name) for name in USES)
    family.load_families()
    return mods


def reset_all():
    """Reset every module-level singleton to its state in a fresh process (no Perl
    counterpart: Seqsee.pl handles one run per process). Used by the test fixtures and by
    the CLI before each run."""
    from seqsee import (global_, scoderack, scripts, seqsee_main, sltm, sltm_platonic, srule,
                        sthought, sworkspace, user_interaction, util)
    from seqsee.codelets import all_mx
    from seqsee.mapping import meto_type, numeric, structural
    from seqsee.memory import ltm
    global_.reset()
    sltm.reset()
    numeric.reset()
    meto_type.reset()
    structural.reset()
    srule.reset()
    sltm_platonic.reset()
    ltm.reset()
    util.reset_each_iterators()
    sworkspace.reset()
    scoderack.reset()
    sthought.reset()
    all_mx.reset()
    user_interaction.reset()
    scripts.reset()
    seqsee_main.reset()
    from seqsee.testing import harness
    harness.reset()
