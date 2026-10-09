"""Port of lib/Seqsee/SCF.pm: ``ContinueWith``, which codelet bodies use to hand a thought
to the main stream."""
from seqsee import global_ as Global
from seqsee.errors import Confess
from seqsee.multimethods import perl_isa


def continue_with(*args):
    """Perl: ContinueWith($thought). Adds the thought to ``$Global::MainStream``."""
    if len(args) != 1:
        raise Confess("ContinueWith takes a single argument!")
    thought = args[0]
    if not perl_isa(thought, "SThought"):
        raise Confess("ContinueWith takes a thought as argument")
    if Global.Feature.get("CodeletTree"):
        # Perl prints the stringified ref ("SThought::X=HASH(0x…)").
        Global.CodeletTreeLogHandle.write(f"\t{thought}\n")
    Global.MainStream.add_thought(thought)
