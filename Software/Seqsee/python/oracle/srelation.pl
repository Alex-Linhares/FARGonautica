# Oracle for SRelation.pm and SRelation/Structural.pm (item 024).
# Output: tests/golden/srelation.json
#
# Ends are real Seqsee::Elements and Seqsee::Anchored groups; types are real mappings.
# SWorkspace->AddRelation/RemoveRelation (item 034) and
# SLTM::GetRealActivationsForOneConcept (item 029) are replaced by recorders below.
# SWorkspace->are_there_holes_here (used by BUILD) is the real one.
use strict;
use Oracle;
use S;

our @LOG;
our %ACT;         # mapping as_text => activation
our $ADD = 1;     # what SWorkspace->AddRelation returns
our $ADD_DIES;    # SWorkspace->AddRelation dies with this, if set

package FakeMapping;    # a Mapping whose FlippedVersion is scripted
our @ISA = ('Mapping');
sub new { my ( $p, %h ) = @_; bless {%h}, $p }
sub FlippedVersion { $_[0]->{flip} }
sub as_text        { 'fake ' . $_[0]->{name} }
sub get_category   { $_[0]->{cat} }
sub get_name       { $_[0]->{name} }

package main;

{
  no warnings 'redefine';
  *SLTM::GetRealActivationsForOneConcept = sub {
    my ($c) = @_;
    push @LOG, [ 'activation', $c->as_text ];
    return $ACT{ $c->as_text };
  };
  *SWorkspace::AddRelation = sub {
    my ( $p, $r ) = @_;
    push @LOG, [ 'ws_add', $p, $r->as_text ];
    die $ADD_DIES if $ADD_DIES;
    return $ADD;
  };
  *SWorkspace::RemoveRelation = sub {
    my ( $p, $r ) = @_;
    push @LOG, [ 'ws_remove', $p, $r->as_text ];
    return 'REMOVED';
  };
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    my $m = $e->can('message') ? $e->message : "$e";
    $m =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
    return { class => ref($e), message => $m };
  }
  my $s = "$e";
  $s =~ s/ at \S+ line \d+.*//s;
  $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
  return { class => 'DIE', message => $s };
}

sub E { Seqsee::Element->create(@_) }
sub G { Seqsee::Anchored->create(@_) }
sub log_take { my @l = @LOG; @LOG = (); return \@l }

# The ends, by name. The Python test builds the same ones.
my %OBJ;
$OBJ{e0} = E( 5, 0 );
$OBJ{e1} = E( 6, 1 );
$OBJ{e2} = E( 7, 2 );
$OBJ{e3} = E( 8, 3 );
$OBJ{e5} = E( 9, 5 );
$OBJ{e1b} = E( 6, 1 );
$OBJ{g01} = G( $OBJ{e0}, $OBJ{e1} );
$OBJ{g23} = G( $OBJ{e2}, $OBJ{e3} );
$OBJ{obj} = Seqsee::Object->create( 1, 2 );    # a group that isn't anchored

my %TYPE = (
  succ     => Mapping::Numeric->create( 'succ', $S::NUMBER ),
  pred     => Mapping::Numeric->create( 'pred', $S::NUMBER ),
  same     => Mapping::Numeric->create( 'same', $S::NUMBER ),
  foo      => Mapping::Numeric->create( 'foo',  $S::NUMBER ),
  evensucc => Mapping::Numeric->create( 'succ', $S::EVEN ),
  dir      => $DIR::RIGHT,
  fake     => FakeMapping->new( name => 'x', cat => $S::NUMBER ),
  fakeflip => FakeMapping->new(
    name => 'y', cat => $S::PRIME, flip => FakeMapping->new( name => 'yflip', cat => $S::PRIME )
  ),
);
$TYPE{struct} = Mapping::Structural->create(
  { category => $S::ASCENDING, meto_mode => $METO_MODE::NONE, direction_reln => Mapping::Dir->create('Same'),
    changed_bindings => { start => Mapping::Numeric->create( 'succ', $S::NUMBER ) }, slippages => {} } );

sub V {    # a named value: object, type, or plain scalar
  my ($v) = @_;
  return $v unless defined $v;
  return $OBJ{$v} if exists $OBJ{$v};
  return $TYPE{$v} if exists $TYPE{$v};
  return [] if $v eq 'ARRAY';
  return {} if $v eq 'HASH';
  return SHistory->new() if $v eq 'SHistory';
  return $v;
}

sub describe {
  my ($r) = @_;
  return {
    ref       => ref($r),
    as_text   => $r->as_text,
    strength  => $r->get_strength,
    holeyness => $r->get_holeyness,
    dir_reln  => $r->get_direction_reln,
    history   => [ @{ $r->get_history } ],
    extent    => [ $r->get_extent ],
    span      => $r->get_span,
    contig    => $r->are_ends_contiguous,
    direction => $r->get_direction->{text},
    pure_is_type => ( $r->get_pure == $r->get_type ? 1 : 0 ),
  };
}

# --- Moose new ------------------------------------------------------------------------------
my @argsets = (
  [],
  [ first => 'e0' ],
  [ first => 'e0', second => 'e1' ],
  [ first => 'e0', second => 'e1', type => 'succ' ],
  [ first => 'e0', second => 'e2', type => 'succ' ],
  [ first => 'e2', second => 'e0', type => 'pred' ],
  [ first => 'e0', second => 'e5', type => 'succ' ],
  [ first => 'e1', second => 'e1b', type => 'same' ],
  [ first => 'e1', second => 'e1', type => 'same' ],
  [ first => 'g01', second => 'g23', type => 'struct' ],
  [ first => 'g01', second => 'e3', type => 'fake' ],
  [ first => 'e0', second => 'g01', type => 'fake' ],
  [ first => 'e0', second => 'e1', type => 'dir' ],
  [ first => 'e0', second => 'e1', type => 5 ],
  [ first => 'e0', second => 'e1', type => undef ],
  [ first => 5, second => 'e1', type => 'succ' ],
  [ first => 'e0', second => 'x', type => 'succ' ],
  [ first => 'e0', second => 'ARRAY', type => 'succ' ],
  [ first => 'e0', second => 'e1', type => 'succ', strength => 40 ],
  [ first => 'e0', second => 'e1', type => 'succ', strength => 'abc' ],
  [ first => 'e0', second => 'e1', type => 'succ', holeyness => 5 ],
  [ first => 'e0', second => 'e1', type => 'succ', holeyness => 1 ],
  [ first => 'e0', second => 'e1', type => 'succ', direction_reln => 'q' ],
  [ first => 'e0', second => 'e1', type => 'succ', history_object => 5 ],
  [ first => 'e0', second => 'e1', type => 'succ', history_object => 'SHistory' ],
  [ first => 'e0', second => 'e1', type => 'succ', unchanged_bindings => 'HASH' ],
  [ first => 'obj', second => 'e1', type => 'succ' ],
  [ first => 'e0', second => 'obj', type => 'succ' ],
  [ type => 5 ],
  [ second => 5, type => 5 ],
  [ first => 5, type => 5 ],
  [ first => 'e0', second => 'e1', type => 'succ', unchanged_bindings => 5 ],
  [ first => 'e0', second => 'e1', type => 'succ', unchanged_bindings => 'HASH' ],
  [ first => 'e0', second => 'e1', type => 'succ', unchanged_bindings => 'ARRAY' ],
);
for my $class (qw(SRelation SRelation::Structural)) {
  for my $as (@argsets) {
    my %h = @$as;
    $h{$_} = V( $h{$_} ) for keys %h;
    my $r = eval { $class->new( \%h ) };
    my $e = err($@);
    my %c = ( kind => 'new', class => $class, args => $as, err => $e );
    if ($r) {
      $c{obj} = describe($r);
      if ( $class eq 'SRelation::Structural' ) {
        $c{unchanged} = [ sort keys %{ $r->get_unchanged_bindings } ];
        $c{no_unchanged} = $r->no_unchanged_bindings ? 1 : 0;
      }
    }
    record(%c);
  }
}
# A list instead of a hash ref.
{
  my $r = eval { SRelation->new( first => $OBJ{e0}, second => $OBJ{e1}, type => $TYPE{succ} ) };
  record( kind => 'new_list', err => err($@), as_text => ( $r ? $r->as_text : undef ) );
}

# --- accessors on a direction matrix ---------------------------------------------------------
for my $pair ( [qw(e0 e1)], [qw(e1 e0)], [qw(e0 e2)], [qw(e2 e0)], [qw(e1 e1b)],
  [qw(g01 g23)], [qw(g23 g01)], [qw(g01 e2)], [qw(e0 g01)], [qw(e0 e5)], [qw(e5 e0)] )
{
  my $r = SRelation->new( { first => V( $pair->[0] ), second => V( $pair->[1] ), type => $TYPE{succ} } );
  my @ends = $r->get_ends;
  record(
    kind  => 'accessors',
    pair  => $pair,
    obj   => describe($r),
    ends_ok => ( $ends[0] == V( $pair->[0] ) && $ends[1] == V( $pair->[1] ) ? 1 : 0 ),
    nends => scalar(@ends),
  );
}

# --- writers --------------------------------------------------------------------------------
{
  my @w = (
    [ set_strength => 77 ], [ set_strength => 'z' ], [ set_holeyness => 1 ], [ set_holeyness => 7 ],
    [ set_holeyness => undef ],
    [ set_direction_reln => 'w' ], [ set_type => 'pred' ], [ set_type => 5 ], [ set_type => 'dir' ],
    [ set_first => 'e2' ], [ set_first => 5 ], [ set_second => 'g23' ], [ set_second => undef ],
    [ history_object => 5 ], [ history_object => 'SHistory' ],
  );
  for my $w (@w) {
    my $r = SRelation->new( { first => $OBJ{e0}, second => $OBJ{e1}, type => $TYPE{succ} } );
    my ( $m, $v ) = @$w;
    eval { $r->$m( V($v) ) };
    my $e = err($@);
    record( kind => 'writer', writer => $w, err => $e, obj => describe($r) );
  }
  my $r = SRelation::Structural->new( { first => $OBJ{e0}, second => $OBJ{e1}, type => $TYPE{struct} } );
  for my $v ( { a => 1 }, 5, undef, {} ) {
    eval { $r->set_unchanged_bindings($v) };
    record(
      kind => 'writer_unchanged', value => $v, err => err($@),
      unchanged => [ sort keys %{ $r->get_unchanged_bindings // {} } ],
      no_unchanged => ( eval { $r->no_unchanged_bindings } ? 1 : 0 ),
    );
  }
}

# --- history delegations --------------------------------------------------------------------
{
  my $r = SRelation->new( { first => $OBJ{e0}, second => $OBJ{e1}, type => $TYPE{succ} } );
  $r->AddHistory('first note');
  $Global::Steps_Finished = 7;
  $r->AddHistory('second note');
  record(
    kind      => 'history',
    history   => [ @{ $r->get_history } ],
    search    => [ $r->search_history(qr/note/) ],
    age       => $r->GetAge,
    unchanged => [ map { $r->UnchangedSince($_) ? 1 : 0 } 0, 3, 7, 9 ],
    as_text   => $r->history_as_text,
    hobj_ref  => ref( $r->history_object ),
  );
  eval { $r->set_history( [] ) };
  record( kind => 'set_history', err => err($@) );
  $Global::Steps_Finished = 0;
}

# --- UpdateStrength -------------------------------------------------------------------------
for my $act ( 0, 1, 2.5, 3, 5, 6, undef, -1, '' ) {
  for my $pair ( [qw(e0 e1)], [qw(e0 e2)] ) {
    local $ACT{succ} = $act;
    my $r = SRelation->new( { first => V( $pair->[0] ), second => V( $pair->[1] ), type => $TYPE{succ} } );
    my $ret = $r->UpdateStrength;
    record( kind => 'update_strength', act => $act, pair => $pair, strength => $r->get_strength,
      ret => $ret, log => log_take() );
  }
}

# --- as_text --------------------------------------------------------------------------------
for my $t (qw(succ pred same foo evensucc struct fake)) {
  my $r = SRelation->new( { first => $OBJ{e0}, second => $OBJ{g23}, type => $TYPE{$t} } );
  record( kind => 'as_text', type => $t, as_text => $r->as_text );
}

# --- SuggestCategory ------------------------------------------------------------------------
for my $t (qw(succ pred same foo evensucc struct fake fakeflip)) {
  my $r = SRelation->new( { first => $OBJ{e0}, second => $OBJ{e1}, type => $TYPE{$t} } );
  my $sc = $r->SuggestCategory;
  my @sl = $r->SuggestCategory;
  my %c = ( kind => 'suggest', type => $t, defined => ( defined $sc ? 1 : 0 ), nlist => scalar(@sl) );
  if ( ref $sc ) {
    $c{ref}  = ref($sc);
    $c{name} = $sc->get_name;
    $c{same_as} = ( $sc == $S::SAMENESS ? 'SAMENESS' : $sc == $S::ASCENDING ? 'ASCENDING'
        : $sc == $S::DESCENDING ? 'DESCENDING'
        : $sc == SCategory::MappingBased->Create( $TYPE{$t} ) ? 'MB' : 'OTHER' );
  }
  elsif ( defined $sc ) {
    $c{value} = $sc;
  }
  $c{list} = [ map { ref($_) ? ref($_) : $_ } @sl ];
  record(%c);
  my @e = $r->SuggestCategoryForEnds;
  record( kind => 'suggest_ends', type => $t, n => scalar(@e),
    scalar => ( defined( scalar $r->SuggestCategoryForEnds ) ? 1 : 0 ) );
}

# --- FlippedVersion -------------------------------------------------------------------------
for my $case ( [qw(e0 e1 succ)], [qw(e0 e2 pred)], [qw(e1 e1b same)], [qw(g01 g23 fakeflip)],
  [qw(e0 e1 fake)], [qw(e0 e1 foo)], [qw(e0 e5 succ)], [qw(g01 g23 struct)] )
{
  my ( $a, $b, $t ) = @$case;
  my $r = SRelation->new( { first => V($a), second => V($b), type => $TYPE{$t} } );
  my $f = eval { $r->FlippedVersion };
  my $e = err($@);
  my @fl = eval { $r->FlippedVersion };
  my %c = ( kind => 'flip', case => $case, err => $e, defined => ( defined $f ? 1 : 0 ),
    nlist => scalar(@fl) );
  if ($f) {
    $c{obj} = describe($f);
    $c{ends_swapped} = ( $f->get_first == V($b) && $f->get_second == V($a) ? 1 : 0 );
    $c{ref} = ref($f);
  }
  record(%c);
}

# --- insert / uninsert ----------------------------------------------------------------------
sub ends_state {
  my (@o) = @_;
  return [ map {
      my $x = $_;
      { rels  => scalar( $x->all_relations ),
        hist  => [ grep { /reln/ } @{ $x->get_history } ] }
    } @o ];
}

{
  local $ACT{succ} = 2;
  local $ACT{pred} = 1;
  my $a = E( 1, 10 );
  my $b = E( 2, 11 );
  my $c = E( 3, 13 );

  my $r1 = SRelation->new( { first => $a, second => $b, type => $TYPE{succ} } );
  log_take();
  my $ret = $r1->insert;
  record( kind => 'insert', step => 'first', ret => $ret, log => log_take(), strength => $r1->get_strength,
    exists => ( $a->get_relation($b) == $r1 && $b->get_relation($a) == $r1 ? 1 : 0 ),
    ends => ends_state( $a, $b ) );

  # Replacing it: the old relation is uninserted first.
  my $r2 = SRelation->new( { first => $a, second => $b, type => $TYPE{pred} } );
  log_take();
  $r2->insert;
  record( kind => 'insert', step => 'replace', log => log_take(), strength => $r2->get_strength,
    exists => ( $a->get_relation($b) == $r2 && $b->get_relation($a) == $r2 ? 1 : 0 ),
    ends => ends_state( $a, $b ) );

  # Reverse direction: b->a, replacing r2 (get_relation is symmetric).
  my $r3 = SRelation->new( { first => $b, second => $a, type => $TYPE{succ} } );
  log_take();
  $r3->insert;
  record( kind => 'insert', step => 'reverse', log => log_take(), strength => $r3->get_strength,
    exists => ( $a->get_relation($b) == $r3 ? 1 : 0 ), ends => ends_state( $a, $b ) );

  # Workspace refuses: ends untouched, strength still updated.
  {
    local $ADD = undef;
    my $r4 = SRelation->new( { first => $b, second => $c, type => $TYPE{succ} } );
    log_take();
    my $ret4 = $r4->insert;
    record( kind => 'insert', step => 'refused', ret => $ret4, log => log_take(),
      strength => $r4->get_strength, holey => $r4->get_holeyness,
      exists => ( $b->get_relation($c) ? 1 : 0 ), ends => ends_state( $b, $c ) );
  }
  {
    local $ADD = 0;
    my $r4 = SRelation->new( { first => $b, second => $c, type => $TYPE{succ} } );
    log_take();
    $r4->insert;
    record( kind => 'insert', step => 'refused0', log => log_take(),
      exists => ( $b->get_relation($c) ? 1 : 0 ) );
  }
  # Workspace dies: confess.
  {
    local $ADD_DIES = "boom\n";
    my $r5 = SRelation->new( { first => $b, second => $c, type => $TYPE{succ} } );
    log_take();
    eval { $r5->insert };
    my $e = err($@);
    record( kind => 'insert', step => 'dies', err => $e, log => log_take(),
      exists => ( $b->get_relation($c) ? 1 : 0 ) );
  }
  {
    local $ADD_DIES = SErr->new("ws err");
    my $r5 = SRelation->new( { first => $b, second => $c, type => $TYPE{succ} } );
    log_take();
    eval { $r5->insert };
    my $e = err($@);
    record( kind => 'insert', step => 'dies_obj', err => $e, log => log_take() );
  }

  # uninsert
  my $u = $r3->uninsert;
  record( kind => 'uninsert', log => log_take(), ret => $u, exists => ( $a->get_relation($b) ? 1 : 0 ),
    ends => ends_state( $a, $b ) );
  # uninsert again: RemoveRelation on the ends -- no relation stored any more.
  eval { $r3->uninsert };
  record( kind => 'uninsert', step => 'again', err => err($@), log => log_take(),
    ends => ends_state( $a, $b ) );

  # A relation whose existing duplicate is in the ends' hash but is a different object:
  # inserting a second while a FakeReln is there.
  my $r6 = SRelation->new( { first => $a, second => $b, type => $TYPE{succ} } );
  $r6->insert;
  log_take();
  my $r7 = SRelation->new( { first => $a, second => $c, type => $TYPE{succ} } );
  $r7->insert;
  record( kind => 'insert', step => 'second_pair', log => log_take(),
    rels_a => scalar( $a->all_relations ), ends => ends_state( $a, $b, $c ) );
  $a->RemoveAllRelations;
  record( kind => 'remove_all', log => log_take(), ends => ends_state( $a, $b, $c ) );
}

# --- Object.pm integration: apply_reln_scheme and recalculate_relations ----------------------
{
  local $ACT{succ} = 3;
  local $ACT{same} = 1;
  my @e = ( E( 4, 20 ), E( 5, 21 ), E( 6, 22 ) );
  my $g = G(@e);
  log_take();
  $g->apply_reln_scheme($RELN_SCHEME::CHAIN);
  my $l = log_take();
  my @pairs = map {
    my $r = $e[$_]->get_relation( $e[ $_ + 1 ] );
    $r ? { as_text => $r->as_text, strength => $r->get_strength, ref => ref($r) } : undef
  } 0, 1;
  record( kind => 'chain', log => $l, pairs => \@pairs );

  # Change the middle element's mag so recalculate finds different mappings.
  $e[1]->mag(4);
  $e[0]->recalculate_relations;
  $l = log_take();
  my $r = $e[0]->get_relation( $e[1] );
  record( kind => 'recalc', log => $l, rel => ( $r ? $r->as_text : undef ),
    rel2 => ( $e[1]->get_relation( $e[2] ) ? $e[1]->get_relation( $e[2] )->as_text : undef ) );
}

emit();
