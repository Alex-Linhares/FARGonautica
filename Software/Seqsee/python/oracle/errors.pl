# Oracle for the SErr exception classes (SErr.pm, UserInteraction.pm,
# Seqsee/Scripts.pm) and their Exception::Class::Base behaviour.
# Output: tests/golden/errors.json
use strict;
use Oracle;
use S;
use Seqsee::Scripts;

sub b { $_[0] ? 1 : 0 }

my %fields = (
  'SErr'                            => [],
  'SErr::LTM_LoadFailure'           => ['what'],
  'SErr::MetonymNotAppicable'       => [],
  'SErr::FinishedTest'              => ['got_it'],
  'SErr::FinishedTestBlemished'     => [],
  'SErr::NotClairvoyant'            => [],
  'SErr::CouldNotCreateExtendedGroup' => [],
  'SErr::AskUser' =>
    [qw(already_matched next_elements object from_position direction)],
  'SErr::ElementsBeyondKnownSought' => ['next_elements'],
  'SErr::ScriptReturn'              => [],
  'SErr::CallSubscript'             => [qw(name arguments)],
);
my @classes = sort keys %fields;

for my $c (@classes) {
  no strict 'refs';
  my $e = $c->new();
  record(
    op => 'class', class => $c,
    isa => [@{"${c}::ISA"}],
    fields => [sort $c->Fields],
    description => $e->description,
    message => $e->message,
    as_string => $e->as_string,
    stringified => "$e",
    isa_serr => b($e->isa('SErr')),
    new_with_message => $c->new("hello")->message,
    new_with_error => $c->new(error => "err")->message,
    new_with_message_key => $c->new(message => "m")->message,
    throw_dies => dies(sub { $c->throw("x") }),
    caught_as_self => b(eval { $c->throw("x"); 1 } || Exception::Class->caught($c)),
    caught_as_serr => b(eval { $c->throw("x"); 1 } || Exception::Class->caught('SErr')),
    unknown_field_dies => dies(sub { $c->new(bogus => 1) }),
    unknown_field_error => (eval { $c->new(bogus => 1); 1 } ? undef : "$@" =~ s/ at .*//sr),
  );
}

# Field values round-trip; unset fields read back undef.
record(op => 'fields', class => 'SErr::LTM_LoadFailure',
       what => SErr::LTM_LoadFailure->new(what => "bad file")->what,
       unset => SErr::LTM_LoadFailure->new()->what);
record(op => 'fields', class => 'SErr::FinishedTest',
       got_it => SErr::FinishedTest->new(got_it => 1)->got_it,
       message => SErr::FinishedTest->new(got_it => 1)->message);
{
  my $e = SErr::AskUser->new(already_matched => [1, 2], next_elements => [3],
                             from_position => 4, message => "q");
  record(op => 'fields', class => 'SErr::AskUser',
         already_matched => $e->already_matched, next_elements => $e->next_elements,
         from_position => $e->from_position, object => $e->object,
         direction => $e->direction, message => $e->message);
}
{
  my $e = SErr::CallSubscript->new(name => "foo", arguments => {a => 1});
  record(op => 'fields', class => 'SErr::CallSubscript',
         name => $e->name, arguments => $e->arguments);
}

# Messages used at throw sites.
for my $msg ("OutOfRange [obj=x]index=5, size=3, ", "Not of category", "") {
  eval { SErr->throw($msg) };
  my $e = $@;
  record(op => 'throw', class => 'SErr', arg => $msg,
         message => $e->message, as_string => $e->as_string, error => $e->error);
}

# rethrow keeps the same object (message intact).
{
  eval { eval { SErr::FinishedTest->throw(got_it => 0) }; $@->rethrow };
  record(op => 'rethrow', class => 'SErr::FinishedTest',
         caught => b(Exception::Class->caught('SErr::FinishedTest')),
         got_it => $@->got_it);
}

# Exception::Class->caught with a plain die string.
{
  eval { die "plain\n" };
  record(op => 'caught_plain', caught_class => b(Exception::Class->caught('SErr')),
         caught_any => Exception::Class->caught());
}

# SErr::Fatal (thrown in Seqsee.pm) is never declared.
{
  eval { SErr::Fatal->throw("boom") };
  my $err = "$@";
  record(op => 'undeclared', class => 'SErr::Fatal', dies => ($err ? 1 : 0),
         error => $err =~ s/ at .*//sr);
}

# SErr::ElementsBeyondKnownSought::ActualQuestion
for my $items ([7], [7, 8], [7, 8, 9], [1, 2, 3, 4], [-1, 10]) {
  my $e = SErr::ElementsBeyondKnownSought->new(next_elements => $items);
  record(op => 'actual_question', next_elements => $items,
         question => $e->ActualQuestion);
}
{
  my $e = SErr::ElementsBeyondKnownSought->new(next_elements => []);
  my $q = eval { $e->ActualQuestion };
  record(op => 'actual_question', next_elements => [], question => $q,
         dies => (defined $q ? 0 : 1));
}
record(op => 'worth_asking', class => 'SErr::ElementsBeyondKnownSought',
       dies => dies(sub { SErr::ElementsBeyondKnownSought->new->WorthAsking }));

emit();
