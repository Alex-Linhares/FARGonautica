# Oracle for SLTM/Platonic.pm, Memory/*.pm, LTMStorable.pm (and LTMStorable/Independent.pm).
# Output: tests/golden/memory_plumbing.json
use strict;
no warnings;    # undef strings in the edge cases are intended
use Oracle;
use S;

sub err { my ($e) = @_; $e =~ s/ at \S+ line \d+\.?\n.*//s; $e =~ s/ at constructor .*//s; $e =~ s/ at accessor .*//s; $e }

# ---- SLTM::Platonic::structure_from_string ----
my @STRINGS = (
  "5", "0", "12abc", "a1", "1,2", "-3", "[1,2]", "[1, 2]", " [ [1,2], [3] ] ", "[]", "[[]]", "[[], []]",
  "[1,a,2]", "[1,a2]", "[1,-2]", "[1.5,2]", "[0]", "[,1,,2,]", "[x]", "[1,[2,[3,4]],5]", "[01,1e3]",
  "[1][2]", "[1", "]", "", "x", "[[1,2]", "[1]]", "[1]x", "x[1]", "\t[1,\n2]",
);
for my $s ( @STRINGS, undef ) {
  my $r = eval { SLTM::Platonic::structure_from_string($s) };
  record( op => 'structure_from_string', string => $s, result => $r, error => ( $@ ? err($@) : undef ) );
}

# ---- SLTM::Platonic objects ----
for my $s ( "5", "[1, 2]", "[1,2]", "[[1,2],[3]]", "[]", "a1" ) {
  my $p = SLTM::Platonic->create($s);
  record(
    op               => 'create',
    string           => $s,
    as_text          => $p->as_text,
    serialize        => $p->serialize,
    structure        => $p->get_structure,
    structure_string => $p->get_structure_string,
    memoized         => ( $p == SLTM::Platonic->create($s) ) ? 1 : 0,
    deserialize_same => ( $p == SLTM::Platonic->deserialize( $p->serialize ) ) ? 1 : 0,
    pure_same        => ( $p == $p->get_pure ) ? 1 : 0,
    deps             => [ $p->get_memory_dependencies ],
    ref              => ref($p),
  );
}
{
  my $p = SLTM::Platonic->create(7);
  record( op => 'create_number', as_text => $p->as_text, structure => $p->get_structure,
    structure_string => $p->get_structure_string, same_as_string => ( $p == SLTM::Platonic->create("7") ) ? 1 : 0 );
  my $q = SLTM::Platonic->create("[1, 2]");
  my $r = SLTM::Platonic->create("[1,2]");
  record( op => 'whitespace_keys_differ', same => ( $q == $r ) ? 1 : 0 );
  for my $bad ( undef, "", "[1", "]" ) {
    eval { SLTM::Platonic->create($bad) };
    record( op => 'create_dies', string => $bad, error => err($@) );
  }
  my $old = $q->set_structure_string("zz");
  record( op => 'set_structure_string', old => $old, as_text => $q->as_text, serialize => $q->serialize,
    still_memoized => ( $q == SLTM::Platonic->create("[1, 2]") ) ? 1 : 0,
    zz_dies => dies( sub { SLTM::Platonic->create("zz") } ) );
  my $old2 = $q->set_structure( [9] );
  record( op => 'set_structure', old => $old2, structure => $q->get_structure );
  for my $args ( {}, { structure => 1 }, { structure_string => "1" }, { structure => 1, structure_string => "1" },
    { foo => 1 } ) {
    my $n = eval { SLTM::Platonic->new($args) };
    record( op => 'new', args => $args, error => ( $@ ? err($@) : undef ),
      as_text => ( $n ? $n->as_text : undef ),
      same_as_create => ( $n && $n == SLTM::Platonic->create("1") ) ? 1 : 0 );
  }
  eval { SLTM::Platonic->new( 1, 2 ) };
  record( op => 'new_nonhash', error => err($@) );
}

# ---- Callers: SInt and Seqsee::Object get_pure ----
for my $mag ( 3, 0, -2 ) {
  my $pure = SInt->new($mag)->get_pure;
  record( op => 'sint_pure', mag => $mag, as_text => $pure->as_text,
    same => ( $pure == SLTM::Platonic->create($mag) ) ? 1 : 0 );
}
{
  my $e = Seqsee::Element->create( 4, 0 );
  my $g = Seqsee::Object->create( 1, [ 2, 3 ] );
  for my $o ( $e, $g ) {
    my $pure = $o->get_pure;
    record( op => 'object_pure', structure_string => $o->get_structure_string, as_text => $pure->as_text,
      structure => $pure->get_structure,
      same => ( $pure == SLTM::Platonic->create( $o->get_structure_string ) ) ? 1 : 0 );
  }
}

# ---- LTMStorable: SpikeBy / InsertISALink delegate to SLTM ----
{
  my @calls;
  no strict 'refs';
  local *SLTM::SpikeBy = sub { push @calls, [ 'SpikeBy', $_[0], map { $_->as_text } @_[ 1 .. $#_ ] ]; return 'spiked' };
  local *SLTM::InsertISALink = sub { push @calls, [ 'InsertISALink', map { $_->as_text } @_ ]; return 'linked' };
  my $r1 = $S::ASCENDING->SpikeBy(5);
  my $r2 = $S::ASCENDING->InsertISALink($S::DESCENDING);
  my $r3 = $S::ODD->SpikeBy();
  record( op => 'ltmstorable', calls => \@calls, returns => [ $r1, $r2, $r3 ],
    does => [ map { $_->does('LTMStorable') ? 1 : 0 } $S::ASCENDING, $S::ODD ] );
  record( op => 'platonic_not_ltmstorable', dies => dies( sub { SLTM::Platonic->create(5)->SpikeBy(1) } ) );
}

# ---- LTMStorable::Independent ----
{
  my $c = $S::ASCENDING;
  record( op => 'independent', is_pure => $c->is_pure, pure_same => ( $c->get_pure == $c ) ? 1 : 0,
    deps => [ $c->get_memory_dependencies ], serialize => $c->serialize,
    deserialize_ref => ref( SCategory::Ascending->deserialize( $c->serialize ) ) );
}

# ---- Memory::* ----
# PERL-QUIRK: Memory::LTM calls confess without importing Carp, so it doesn't compile.
# (Tried in a BEGIN block, before the workaround below imports Carp at compile time.)
our ( $LTM_OK, $LTM_ERR );
BEGIN {
  $LTM_OK = eval { require Memory::LTM; 1 } ? 1 : 0;
  ($LTM_ERR) = ( $@ =~ /^(.*?) at / );
  delete $INC{'Memory/LTM.pm'};
}
record( op => 'memory_ltm_load', loaded => $LTM_OK, error => $LTM_ERR );
# Harness workaround: import Carp into Memory::LTM first, then the module compiles.
{ package Memory::LTM; use Carp; }
require Memory::Storable;
require Memory::Node;
require Memory::LTM;

package T::Store;
use Moose;
with 'Memory::Storable';
has name => ( is => 'ro' );
has deps => ( is => 'rw', default => sub { [] } );
our @LOG;
sub GetMemoryDependencies { my $s = shift; push @LOG, $s->name; @{ $s->deps } }
sub Serialize   { $_[0]->name }
sub Deserialize { }

package T::Ins;
use Moose;
with 'Memory::Insertible';
has target => ( is => 'ro' );
sub GetNormalizedForMemory { $_[0]->target }

package T::Plain;
use Moose;

package main;
sub name_of { my ($x) = @_; ref($x) ? $x->name : $x }

{
  my $a = T::Store->new( name => 'a' );
  my $b = T::Store->new( name => 'b', deps => [$a] );
  my $nb = Memory::LTM->InsertItem($b);
  record( op => 'insert', item => 'b', node_ref => ref($nb), core => $nb->core->name, log => [@T::Store::LOG] );
  my $nb2 = Memory::LTM->InsertItem($b);
  record( op => 'insert_again', same => ( $nb == $nb2 ) ? 1 : 0, log => [@T::Store::LOG] );
  my $na = Memory::LTM->InsertItem( T::Ins->new( target => $a ) );
  record( op => 'insert_normalized', core => $na->core->name, log => [@T::Store::LOG] );
  my $x = T::Store->new( name => 'x' );
  my $nx = Memory::LTM->InsertItem( T::Ins->new( target => $x ) );
  my $nx2 = Memory::LTM->InsertItem($x);
  record( op => 'insert_via_normalized', core => $nx->core->name, same => ( $nx == $nx2 ) ? 1 : 0,
    log => [@T::Store::LOG] );
  # A diamond: d depends on b and c, both depend on a (already present).
  my $c = T::Store->new( name => 'c', deps => [$a] );
  my $d = T::Store->new( name => 'd', deps => [ $b, $c ] );
  @T::Store::LOG = ();
  Memory::LTM->InsertItem($d);
  record( op => 'insert_diamond', log => [@T::Store::LOG] );

  record( op => 'storable_normalized', same => ( $a->GetNormalizedForMemory == $a ) ? 1 : 0,
    does => T::Store->does('Memory::Insertible') ? 1 : 0 );

  for my $case ( [ spike => sub { Memory::LTM->SpikeBy( 5, $a ) } ],
    [ weaken => sub { Memory::LTM->WeakenBy( 5, $a ) } ],
    [ spike_none => sub { Memory::LTM->SpikeBy(5) } ] ) {
    @T::Store::LOG = ();
    my $ok = eval { $case->[1]->(); 1 };
    record( op => 'spike_weaken', case => $case->[0], ok => $ok ? 1 : 0, error => ( $ok ? undef : err($@) ) );
  }
  @T::Store::LOG = ();
  my $fresh = T::Store->new( name => 'fresh' );
  eval { Memory::LTM->SpikeBy( 5, $fresh ) };
  my $nf = Memory::LTM->InsertItem($fresh);
  record( op => 'spike_inserts_first', error => err($@), log => [@T::Store::LOG], core => $nf->core->name );

  # Errors. A die during installation leaves the item marked as "currently installing".
  @T::Store::LOG = ();
  for my $case ( [ normalized_string => sub { Memory::LTM->InsertItem( T::Ins->new( target => "str" ) ) } ],
    [ string => sub { Memory::LTM->InsertItem("str") } ],
    [ blessed => sub { Memory::LTM->InsertItem( bless {}, 'Foo' ) } ],
    [ undef => sub { Memory::LTM->InsertItem(undef) } ],
    [ plain_moose => sub { Memory::LTM->InsertItem( T::Plain->new ) } ] ) {
    eval { $case->[1]->() };
    record( op => 'insert_dies', case => $case->[0], error => err($@) );
  }
  my $p = T::Store->new( name => 'p' );
  my $q = T::Store->new( name => 'q', deps => [$p] );
  $p->deps( [$q] );
  @T::Store::LOG = ();
  eval { Memory::LTM->InsertItem($p) };
  record( op => 'loop', error => err($@), log => [@T::Store::LOG] );
  @T::Store::LOG = ();
  eval { Memory::LTM->InsertItem($p) };
  record( op => 'loop_again', error => err($@), log => [@T::Store::LOG] );
  eval { Memory::LTM->InsertItem($q) };
  record( op => 'loop_q', error => err($@), log => [@T::Store::LOG] );
  my $self_dep = T::Store->new( name => 'self' );
  $self_dep->deps( [$self_dep] );
  eval { Memory::LTM->InsertItem($self_dep) };
  record( op => 'self_loop', error => err($@) );
}

{
  my $a = T::Store->new( name => 'na' );
  my $b = T::Store->new( name => 'nb' );
  my $n = Memory::Node->new( core => $a );
  record( op => 'node_new', core => $n->core->name );
  my $ret = $n->core($b);
  record( op => 'node_set', core => $n->core->name, ret => name_of($ret) );
  my $n2 = Memory::Node->new( { core => $a } );
  record( op => 'node_new_hashref', core => $n2->core->name );
  for my $case ( [ missing => sub { Memory::Node->new() } ],
    [ blessed => sub { Memory::Node->new( core => bless {}, 'Foo' ) } ],
    [ number => sub { Memory::Node->new( core => 1 ) } ],
    [ string => sub { Memory::Node->new( core => "x" ) } ],
    [ undef => sub { Memory::Node->new( core => undef ) } ],
    [ insertible => sub { Memory::Node->new( core => T::Ins->new( target => $a ) ) } ],
    [ set_number => sub { $n->core(1) } ],
    [ set_undef => sub { $n->core(undef) } ] ) {
    eval { $case->[1]->() };
    record( op => 'node_dies', case => $case->[0], error => err($@) );
  }
}

emit();
