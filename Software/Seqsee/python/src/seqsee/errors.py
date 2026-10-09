"""Seqsee exceptions: SErr.pm plus every other ``SErr::...`` class.

Perl declares these with ``Exception::Class``:

* SErr.pm: SErr, SErr::LTM_LoadFailure, SErr::MetonymNotAppicable,
  SErr::FinishedTest, SErr::FinishedTestBlemished, SErr::NotClairvoyant,
  SErr::CouldNotCreateExtendedGroup, SErr::AskUser
* UserInteraction.pm: SErr::ElementsBeyondKnownSought
* Seqsee/Scripts.pm: SErr::ScriptReturn, SErr::CallSubscript

Python names drop the ``SErr::`` prefix (``SErr::FinishedTest`` ->
``FinishedTest``), except the generic ``SErr`` itself. ``BY_PERL_NAME`` maps
Perl names to classes.

Exception::Class does not infer a hierarchy from ``::`` in names, and SErr.pm
gives no ``isa``, so every class inherits from Exception::Class::Base
(``ExceptionClassBase`` here) directly. Catching ``SErr`` does NOT catch
``FinishedTest``. The oracle confirms this.

``Confess`` stands in for a plain Perl ``die``/``confess`` with a string.

Carp/Seqsee.pm only changes how Carp formats backtraces and arguments (debug
output). Python tracebacks replace it, so it is not ported.
"""


class ExceptionClassBase(Exception):
    """Port of Exception::Class::Base: message plus declared fields.

    ``Cls("msg")`` sets the message. ``Cls(message=..., error=..., field=...)``
    sets the message (``error`` is an alias) and fields. An unknown keyword
    raises an ExceptionClassBase, as Perl does. Unset fields read as None.
    """

    perl_name = "Exception::Class::Base"
    fields = ()
    description = "Generic exception"

    def __init__(self, *args, **params):
        if len(args) == 1 and not params:
            params = {"message": args[0]}
        elif args:
            raise TypeError("positional args: only a single message is allowed")
        self.message = params.get("message") or params.get("error") or ""
        self.show_trace = params.get("show_trace", False)
        for f in self.fields:
            setattr(self, f, None)
        for key, value in params.items():
            if key in ("error", "message", "show_trace"):
                continue
            if key not in self.fields:
                raise ExceptionClassBase(
                    error=f"unknown field {key} passed to constructor for class "
                    f"{self.perl_name}"
                )
            setattr(self, key, value)
        super().__init__(self.message)

    @property
    def error(self):
        """Perl: error (alias of message)."""
        return self.message

    def full_message(self):
        return self.message

    def as_string(self):
        """Perl: as_string / stringification: the message, or
        ``[description]`` if the message is empty."""
        return self.full_message() or f"[{self.description}]"

    def __str__(self):
        return self.as_string()

    @classmethod
    def throw(cls, *args, **params):
        """Perl: ``Cls->throw(...)`` = raise ``Cls(...)``."""
        raise cls(*args, **params)

    def rethrow(self):
        """Perl: ``$e->rethrow``."""
        raise self


def caught(err, cls=None):
    """Perl: ``Exception::Class->caught([class])``, with ``$@`` passed as *err*.

    With no class, returns *err*. Otherwise returns *err* if it is an instance
    of *cls*, else None.
    """
    if cls is None:
        return err
    return err if isinstance(err, cls) else None


class Confess(Exception):
    """A plain Perl ``die``/``confess`` with a string message."""


def _declare(perl_name, fields=()):
    name = perl_name.split("::")[-1]
    return type(name, (ExceptionClassBase,), {
        "perl_name": perl_name,
        "fields": tuple(fields),
        "__doc__": f"Perl: {perl_name}.",
        "__module__": __name__,
    })


# SErr.pm
SErr = _declare("SErr")
LTM_LoadFailure = _declare("SErr::LTM_LoadFailure", ["what"])
MetonymNotAppicable = _declare("SErr::MetonymNotAppicable")
FinishedTest = _declare("SErr::FinishedTest", ["got_it"])
FinishedTestBlemished = _declare("SErr::FinishedTestBlemished")
NotClairvoyant = _declare("SErr::NotClairvoyant")
CouldNotCreateExtendedGroup = _declare("SErr::CouldNotCreateExtendedGroup")


class AskUser(ExceptionClassBase):
    """Perl: SErr::AskUser (SErr.pm). WorthAsking/Ask are defined in
    SWorkspace.pm; the port keeps them in sworkspace.py."""

    perl_name = "SErr::AskUser"
    fields = ("already_matched", "next_elements", "object", "from_position",
              "direction")

    def worth_asking(self, trust_level):
        """Perl: SErr::AskUser::WorthAsking (SWorkspace.pm)."""
        from seqsee import sworkspace
        return sworkspace.ask_user_worth_asking(self, trust_level)

    def ask(self, msg):
        """Perl: SErr::AskUser::Ask (SWorkspace.pm)."""
        from seqsee import sworkspace
        return sworkspace.ask_user_ask(self, msg)


class ElementsBeyondKnownSought(ExceptionClassBase):
    """Perl: SErr::ElementsBeyondKnownSought (UserInteraction.pm).

    Ask, AskBasedOn*, DoInsertBookKeeping and *Penetration need the workspace
    and the UI; they are in user_interaction.py.
    """

    perl_name = "SErr::ElementsBeyondKnownSought"
    fields = ("next_elements",)

    def actual_question(self):
        """Perl: ActualQuestion. Dies on an empty list
        (``### require: @items`` under Smart::Comments)."""
        items = [str(i) for i in self.next_elements]
        if not items:
            raise Confess("require: @items")
        if len(items) == 1:
            # PERL-QUIRK: doubled question mark.
            return f"Is the next term {items[0]}??"
        count = len(items)
        ret = f"Are the next {count} terms " + ", ".join(items[:-1])
        if count >= 3:
            ret += ","
        return ret + f" and {items[-1]}?"

    def worth_asking(self, *args):
        """Perl: WorthAsking. Always confesses."""
        raise Confess("Should never be called. Caller does the dirty work.")

    def ask(self, question_prefix=None, question_suffix=None, debug_msg=None):
        """Perl: Ask (user_interaction.ask)."""
        from seqsee import user_interaction
        return user_interaction.ask(self, question_prefix, question_suffix, debug_msg)

    def ask_based_on_relation(self, relation, msg_prefix):
        """Perl: AskBasedOnRelation (user_interaction.ask_based_on_relation)."""
        from seqsee import user_interaction
        return user_interaction.ask_based_on_relation(self, relation, msg_prefix)

    def ask_based_on_rule_app(self, ruleapp, msg_prefix):
        """Perl: AskBasedOnRuleApp (user_interaction.ask_based_on_rule_app)."""
        from seqsee import user_interaction
        return user_interaction.ask_based_on_rule_app(self, ruleapp, msg_prefix)

    def ask_based_on_group(self, group, msg_prefix):
        """Perl: AskBasedOnGroup (user_interaction.ask_based_on_group)."""
        from seqsee import user_interaction
        return user_interaction.ask_based_on_group(self, group, msg_prefix)

    def do_insert_book_keeping(self):
        """Perl: DoInsertBookKeeping (user_interaction.do_insert_book_keeping)."""
        from seqsee import user_interaction
        return user_interaction.do_insert_book_keeping(self)

    def rule_app_penetration(self, ruleapp_left_edge):
        """Perl: RuleAppPenetration (user_interaction.rule_app_penetration)."""
        from seqsee import user_interaction
        return user_interaction.rule_app_penetration(self, ruleapp_left_edge)

    def relation_penetration(self, *args):
        """Perl: RelationPenetration (user_interaction.relation_penetration)."""
        from seqsee import user_interaction
        return user_interaction.relation_penetration(self, *args)


# Seqsee/Scripts.pm
ScriptReturn = _declare("SErr::ScriptReturn")
CallSubscript = _declare("SErr::CallSubscript", ["name", "arguments"])

# PERL-QUIRK: Seqsee.pm throws SErr::Fatal, but it is never declared, so in Perl
# the throw itself dies ("Can't locate object method "throw" via package
# "SErr::Fatal""). Either way it is an uncaught fatal error, so the port
# declares it. Kept out of BY_PERL_NAME because Perl has no such class.
Fatal = _declare("SErr::Fatal")

BY_PERL_NAME = {
    c.perl_name: c
    for c in (SErr, LTM_LoadFailure, MetonymNotAppicable, FinishedTest,
              FinishedTestBlemished, NotClairvoyant,
              CouldNotCreateExtendedGroup, AskUser, ElementsBeyondKnownSought,
              ScriptReturn, CallSubscript)
}
