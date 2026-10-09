# Oracle for Seqsee/Element.pm and Seqsee/Anchored.pm (item 023).
# Output: tests/golden/element_anchored.json
#
# SWorkspace (__FindGroupsConflictingWith, GetSuperGroups, __DeleteGroup, remove_gp,
# __UpdateGroup, $ElementCount) and SLTM::GetRealActivationsForConcepts are replaced
# by recorders below; they belong to later items (031-034, 029).
use strict;
use Oracle;
use S;

our @LOG;
our %ACT;          # category name => activation
our $CONFLICTS;    # what __FindGroupsConflictingWith returns
our @SUPER;        # what GetSuperGroups returns
our $FIND_DIES;    # __FindGroupsConflictingWith dies with this, if set

package FakeConflicts;
sub new { my ( $p, %h ) = @_; bless {%h}, $p }
sub Resolve {
  my ( $self, $opts ) = @_;
  push @main::LOG, [ 'Resolve', main::bounds( $opts->{IgnoreConflictWith} ) ];
  return $self->{ok};
}

package FakeRuleApp;
sub new { my ( $p, %h ) = @_; bless {%h}, $p }
sub get_rule { push @main::LOG, ['get_rule']; $_[0]->{rule} }
sub FindExtension {
  my ( $self, $opts ) = @_;
  push @main::LOG,
    [ 'FindExtension', "$opts->{direction_to_extend_in}{text}", $opts->{skip_this_many_elements} ];
  return 'EXT';
}

package TestGroup;    # an Anchored whose set_underlying_ruleapp is scripted
use Moose;
extends 'Seqsee::Anchored';
has mode => ( is => 'rw', default => 'ok' );
sub set_underlying_ruleapp {
  my ( $self, $rule ) = @_;
  push @main::LOG, [ 'set_underlying_ruleapp', $rule ];
  my $mode = $self->mode;
  die "rule failed\n" if $mode eq 'die';
  $self->set_underlying_reln(undef) if $mode eq 'lose';
  return;
}

package SubAnchored;
use Moose;
extends 'Seqsee::Anchored';

package main;

{
  no warnings 'redefine';
  *SLTM::GetRealActivationsForConcepts = sub {
    my ($cats) = @_;
    push @LOG, [ 'activations', [ sort map { $_->get_name } @$cats ] ];
    return [ map { $ACT{ $_->get_name } } @$cats ];
  };
  *SWorkspace::__FindGroupsConflictingWith = sub {
    my ($gp) = @_;
    die $FIND_DIES if $FIND_DIES;
    push @LOG, [ 'conflicts', bounds($gp), ref($gp) ];
    return $CONFLICTS;
  };
  *SWorkspace::GetSuperGroups = sub {
    my ( $p, $gp ) = @_;
    push @LOG, [ 'GetSuperGroups', $p, bounds($gp) ];
    return @SUPER;
  };
  *SWorkspace::__DeleteGroup = sub { push @LOG, [ 'DeleteGroup', bounds( $_[0] ) ]; };
  *SWorkspace::remove_gp     = sub { push @LOG, [ 'remove_gp', $_[0], bounds( $_[1] ) ]; };
  *SWorkspace::__UpdateGroup = sub { push @LOG, [ 'UpdateGroup', bounds( $_[0] ) ]; 'UPDATED' };
}

sub bounds {
  my ($o) = @_;
  return 'UNDEF' unless defined $o;
  return "$o" unless ref $o;
  return join( ',', $o->get_edges );
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    return { class => ref($e), message => $e->can('message') ? $e->message : "$e" };
  }
  my $s = "$e";
  $s =~ s/ at \S+ line \d+.*//s;
  $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
  return { class => 'DIE', message => $s };
}

sub cats { [ sort map { $_->get_name } @{ $_[0]->get_categories } ] }

sub describe {
  my ($o) = @_;
  return { ref => 0, value => $o } unless ref $o;
  return { ref => ref($o) } unless $o->isa('Seqsee::Object');
  my %d = (
    ref       => ref($o),
    edges     => [ $o->get_edges ],
    structure => $o->get_structure_string,
    strength  => $o->get_strength,
    group_p   => $o->get_group_p,
    cats      => cats($o),
    history   => [ @{ $o->get_history } ],
    nitems    => scalar( @{ $o->get_parts_ref } ),
  );
  $d{self_item} = ( $o->get_parts_ref->[0] // 0 ) == $o ? 1 : 0 if @{ $o->get_parts_ref };
  return \%d;
}

sub hist { [ @{ $_[0]->get_history } ] }

sub E { Seqsee::Element->create(@_) }

# --- Moose new ------------------------------------------------------------------------------
my @argsets = (
  [], [ group_p => 0 ], [ group_p => 0, left_edge => 1 ],
  [ group_p => 0, left_edge => 1, right_edge => 1 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 5 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 1.5 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => '5' ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => '-3' ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => '+3' ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => ' 3' ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 'x' ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => undef ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 5.0 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => [] ],
  [ group_p => 0, left_edge => undef, right_edge => 'a', mag => 5 ],
  [ group_p => 0, left_edge => 1, mag => 'x' ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 2, is_locked_against_deletion => 1 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 2, is_locked_against_deletion => 2 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 2, is_locked_against_deletion => 2, items => 4 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, mag => 2, metonym_activeness => 3 ],
  [ categories => 5 ],
  [ group_p => 0, categories => 5 ],
  [ group_p => 0, history_obj => 5 ],
  [ history_obj => 5 ],
  [ items => 5 ],
  [ group_p => 0, items => 5 ],
  [ group_p => 0, is_locked_against_deletion => 5 ],
  [ is_locked_against_deletion => 5 ],
  [ group_p => 0, left_edge => 1, right_edge => 1, reln_other_end => 5 ],
  [ group_p => 0, left_edge => 1, metonym_activeness => 5 ],
  [ group_p => 0, left_edge => 1, mag => 2, metonym_activeness => 5 ],
  [ group_p => 0, left_edge => 1, mag => 2, reln_other_end => 5 ],
  [ group_p => 0, left_edge => 1, mag => 2, right_edge => 3, items => [ 1, 2 ] ],
);
for my $class (qw(Seqsee::Element Seqsee::Anchored)) {
  for my $i ( 0 .. $#argsets ) {
    my @a = @{ $argsets[$i] };
    my $o = eval { $class->new(@a) };
    my $e = err($@);
    my %c = ( case => 'new', class => $class, index => $i );
    if ($e) { $c{error} = $e }
    else {
      $c{edges}  = [ $o->get_edges ];
      $c{cats}   = cats($o);
      $c{hist}   = hist($o);
      $c{nitems} = scalar( @{ $o->get_parts_ref } );
      $c{locked} = $o->get_is_locked_against_deletion;
      $c{mag}    = $o->get_mag if $o->can('get_mag');
    }
    record(%c);
  }
}

# --- Element->create and the Element methods ----------------------------------------------
for my $args ( [ 5, 2 ], [ 0, 0 ], [ -3, -1 ], [ '7', 3 ], [ 12, 4 ] ) {
  my $e = E(@$args);
  record(
    case       => 'element_create',
    args       => $args,
    describe   => describe($e),
    as_text    => $e->as_text,
    structure  => $e->get_structure,
    flattened  => $e->get_flattened,
    span       => $e->get_span,
    bounds     => $e->get_bounds_string,
    annotated  => $e->GetAnnotatedStructureString,
    flush_left => $e->IsFlushLeft,
  );
}
for my $bad ( 1.5, 'x' ) {
  record( case => 'element_create_bad', mag => $bad, error => err( eval { E( $bad, 0 ); 1 } ? '' : $@ ) );
}
for my $feature ( [ 'Primes', [ 7, 8, 9, 1, 2 ] ], [ 'Parity', [ 7, 8, 0, -3 ] ], [ 'Both', [ 2, 3, 4, 9 ] ] ) {
  my ( $f, $mags ) = @$feature;
  local %Global::Feature = $f eq 'Both' ? ( Primes => 1, Parity => 1 ) : ( $f => 1 );
  for my $m (@$mags) {
    my $e = E( $m, 0 );
    record( case => 'element_features', feature => $f, mag => $m, cats => cats($e), hist => hist($e) );
  }
}
{
  my $e = E( 4, 1 );
  for my $p ( 1, -1, 2, 3 ) {
    my $r = eval { $e->get_at_position( SPos->new($p) ) };
    record(
      case     => 'get_at_position',
      position => $p,
      is_self  => ( defined $r and $r == $e ) ? 1 : 0,
      error    => err($@)
    );
  }
  my @r = $e->UpdateStrength;
  record( case => 'element_update_strength', returned => scalar(@r), strength => $e->get_strength );
  $e->set_strength(33);
  $e->UpdateStrength;
  record( case => 'element_update_strength_keeps', strength => $e->get_strength );

  my $m = $e->mag;
  $e->mag(9);
  my $bad = err( eval { $e->mag(2.5); 1 } ? '' : $@ );
  record( case => 'mag_accessor', before => $m, after => $e->get_mag, bad => $bad, as_text => $e->as_text );
}
{
  # CheckSquintability: describe_as NUMBER first (re-adding it if removed).
  my $e = E( 4, 0 );
  $e->remove_category($S::NUMBER);
  my $cats_before = cats($e);
  my $r = $e->CheckSquintability( E( 4, 1 ) );
  record( case => 'element_squint', cats_before => $cats_before, result => $r, cats_after => cats($e),
    hist => hist($e) );
}

# --- Anchored accessors ---------------------------------------------------------------------
{
  my $a = Seqsee::Anchored->new( group_p => 1, left_edge => 2, right_edge => 4, items => [ E( 1, 2 ), E( 2, 3 ), E( 3, 4 ) ] );
  my @log;
  push @log, [ 'edges', [ $a->get_edges ] ];
  push @log, [ 'bounds', $a->get_bounds_string ];
  push @log, [ 'span', $a->get_span ];
  push @log, [ 'as_text', $a->as_text ];
  $a->set_underlying_reln(1);
  push @log, [ 'as_text_u', $a->as_text ];
  $a->set_underlying_reln(0);
  push @log, [ 'as_text_0', $a->as_text ];
  my $ret = $a->set_edges( 5, 9 );
  push @log, [ 'set_edges_returns_self', ( $ret == $a ) ? 1 : 0 ];
  push @log, [ 'edges', [ $a->get_edges ] ];
  push @log, [ 'span', $a->get_span ];
  $a->recalculate_edges;
  push @log, [ 'recalculated', [ $a->get_edges ] ];
  push @log, [ 'locked_default', $a->get_is_locked_against_deletion ];
  $a->set_is_locked_against_deletion(1);
  push @log, [ 'locked', $a->get_is_locked_against_deletion ];
  push @log, [ 'locked_bad', err( eval { $a->set_is_locked_against_deletion(5); 1 } ? '' : $@ ) ];
  push @log, [ 'left_any', eval { $a->set_left_edge('abc'); $a->get_left_edge } ];
  record( case => 'anchored_accessors', log => \@log );
}
{
  my $empty = Seqsee::Anchored->new( group_p => 1, left_edge => 0, right_edge => 1 );
  record( case => 'anchored_as_text_empty', as_text => $empty->as_text );
}
for my $edges ( [ 0, 0 ], [ 3, 5 ], [ 1, 2 ] ) {
  my $a = Seqsee::Anchored->new( group_p => 1, left_edge => $edges->[0], right_edge => $edges->[1] );
  my %r;
  for my $d (qw(LEFT RIGHT UNKNOWN NEITHER)) {
    no strict 'refs';
    my $dir = &{"DIR::$d"}();
    my $v   = eval { $a->get_next_pos_in_dir($dir) };
    $r{$d} = $@ ? err($@) : $v;
  }
  $r{UNDEF} = do { my $v = eval { $a->get_next_pos_in_dir(undef) }; $@ ? err($@) : $v };
  my @flush;
  for my $count ( 0, 1, 2, 6 ) {
    local $SWorkspace::ElementCount = $count;
    push @flush, [ $count, $a->IsFlushRight, $a->IsFlushLeft ];
  }
  record( case => 'next_pos_and_flush', edges => $edges, next => \%r, flush => \@flush );
}
{
  my @iv = ( [ 0, 0 ], [ 0, 3 ], [ 1, 2 ], [ 2, 5 ], [ 3, 3 ], [ 4, 6 ], [ 7, 8 ] );
  my @objs = map { Seqsee::Anchored->new( group_p => 1, left_edge => $_->[0], right_edge => $_->[1] ) } @iv;
  my @m;
  for my $i ( 0 .. $#objs ) {
    for my $j ( 0 .. $#objs ) {
      my $s = $objs[$i]->spans( $objs[$j] );
      my $o = $objs[$i]->overlaps( $objs[$j] );
      push @m, [ $iv[$i], $iv[$j], ( $s ? 1 : 0 ), "$s", ( $o ? 1 : 0 ), "$o" ];
    }
  }
  record( case => 'spans_overlaps', matrix => \@m );
}

# --- Anchored->create ---------------------------------------------------------------------
%ACT = ( number => 0.1, ascending => 0.5 );
sub mk_inputs {
  my $e0 = E( 1, 0 );
  my $e1 = E( 2, 1 );
  my $e2 = E( 3, 2 );
  my $e3 = E( 4, 3 );
  my $g12 = Seqsee::Anchored->new( group_p => 1, left_edge => 1, right_edge => 2, items => [ $e1, $e2 ] );
  my $obj = Seqsee::Object->create( 1, 2 );
  return (
    empty       => [],
    one_elt     => [$e0],
    one_group   => [$g12],
    two         => [ $e0, $e1 ],
    three       => [ $e0, $e1, $e2 ],
    hole        => [ $e0, $e2 ],
    reversed    => [ $e1, $e0 ],
    same        => [ $e0, $e0 ],
    group_elt   => [ $g12, $e3 ],
    elt_group   => [ $e0, $g12 ],
    overlap     => [ $e1, $g12 ],
    unanchored  => [$obj],
    unanch_2nd  => [ $e0, $obj ],
    hole_then_unanch => [ $e0, $e2, $obj ],
    number      => [5],
  );
}
{
  my %in    = mk_inputs();
  for my $name ( sort keys %in ) {
    local @LOG;
    my @items = @{ $in{$name} };
    my $valid = eval { Seqsee::Anchored::_CheckValidity(@items) };
    my $verr  = err($@);
    @LOG = ();
    my $r = eval { Seqsee::Anchored->create(@items) };
    my $cerr = err($@);
    record(
      case         => 'create',
      name         => $name,
      valid        => $valid,
      valid_error  => $verr,
      error        => $cerr,
      result       => defined($r) ? describe($r) : undef,
      same_as_item => ( defined($r) && @items == 1 && ref $items[0] && $r == $items[0] ) ? 1 : 0,
      log          => [@LOG],
    );
  }
  my %in2 = mk_inputs();
  my $r   = SubAnchored->create( @{ $in2{two} } );
  record( case => 'create_subclass', ref => ref($r), describe => describe($r) );
  my $r1 = SubAnchored->create( @{ $in2{one_elt} } );
  record( case => 'create_subclass_one', ref => ref($r1) );
}

# --- Extend / SafeExtend ------------------------------------------------------------------
sub ext_fixture {
  my @e = map { E( $_ + 1, $_ ) } 0 .. 4;
  my $g = Seqsee::Anchored->new( group_p => 1, left_edge => 1, right_edge => 2, items => [ $e[1], $e[2] ] );
  $g->describe_as($S::ASCENDING);
  return ( $g, @e );
}
my @ext_cases = (
  # name, insert index, at_end, conflicts, super, seed
  [ 'end',              3, 1, undef, 0, 1 ],
  [ 'start',            0, 0, undef, 0, 1 ],
  [ 'hole_end',         4, 1, undef, 0, 1 ],
  [ 'wrong_side',       0, 1, undef, 0, 1 ],
  [ 'conflict_ok',      3, 1, 1,     0, 1 ],
  [ 'conflict_fail',    3, 1, 0,     0, 1 ],
  [ 'conflict_undef',   3, 1, 'u',   0, 1 ],
  [ 'super_seed1',      3, 1, undef, 2, 1 ],
  [ 'super_seed2',      3, 1, undef, 2, 2 ],
  [ 'super_seed3',      3, 1, undef, 2, 3 ],
  [ 'super_seed4',      3, 1, undef, 2, 4 ],
  [ 'at_end_string0',   3, '0', undef, 0, 1 ],
  [ 'at_end_empty',     0, '', undef, 0, 1 ],
);
for my $c (@ext_cases) {
  my ( $name, $idx, $at_end, $conf, $nsuper, $seed ) = @$c;
  for my $method (qw(Extend SafeExtend)) {
    my ( $g, @e ) = ext_fixture();
    local @LOG;
    local $CONFLICTS =
      !defined($conf) ? undef : $conf eq 'u' ? FakeConflicts->new( ok => undef ) : FakeConflicts->new( ok => $conf );
    local @SUPER = map { Seqsee::Anchored->new( group_p => 1, left_edge => 0, right_edge => 2 + $_ ) } 1 .. $nsuper;
    srand($seed);
    my @r = eval { $g->$method( $e[$idx], $at_end ) };
    my $error = err($@);
    record(
      case     => 'extend',
      method   => $method,
      name     => $name,
      seed     => $seed,
      returned => [@r],
      error    => $error,
      group    => describe($g),
      log      => [@LOG],
      next_rand => rand(),
    );
  }
}
for my $method (qw(Extend SafeExtend)) {
  my ( $g, @e ) = ext_fixture();
  record( case => 'extend_arity', method => $method, n => 1, error => err( eval { $g->$method( $e[3] ); 1 } ? '' : $@ ) );
  record( case => 'extend_arity', method => $method, n => 3,
    error => err( eval { $g->$method( $e[3], 1, 2 ); 1 } ? '' : $@ ) );
}
for my $die ( "boom\n", 'obj' ) {
  my ( $g, @e ) = ext_fixture();
  local @LOG;
  local $FIND_DIES = $die eq 'obj' ? SErr->new('serr boom') : $die;
  my @r = eval { $g->SafeExtend( $e[3], 1 ) };
  record( case => 'safe_extend_rethrow', die => $die, returned => [@r], error => err($@) );
}

# --- Update -------------------------------------------------------------------------------
for my $mode (qw(none ok die lose)) {
  my @e = map { E( $_ + 1, $_ ) } 0 .. 3;
  my $g = TestGroup->new( group_p => 1, left_edge => 7, right_edge => 9, items => [ @e[ 0 .. 2 ] ] );
  $g->describe_as($S::ASCENDING);
  if ( $mode ne 'none' ) {
    $g->set_underlying_reln( FakeRuleApp->new( rule => 'RULE' ) );
    $g->mode($mode);
  }
  local @LOG;
  my @r = eval { $g->Update };
  record(
    case     => 'update',
    mode     => $mode,
    returned => [@r],
    error    => err($@),
    group    => describe($g),
    has_reln => $g->get_underlying_reln ? 1 : 0,
    log      => [@LOG],
  );
}
{
  # Update when the categories no longer fit: recalculate_categories confesses.
  my @e = map { E( 5, $_ ) } 0 .. 2;
  my $g = TestGroup->new( group_p => 1, left_edge => 0, right_edge => 2, items => [@e] );
  $g->add_category( $S::ASCENDING, SBindings->create( {}, {}, $g ) );
  local @LOG;
  my @r = eval { $g->Update };
  my $error = err($@);
  $error->{message} =~ s/!!! .*/!!!/s if $error;
  record( case => 'update_lost_cats', returned => [@r], error => $error, log => [@LOG] );
}

# --- FindExtension ------------------------------------------------------------------------
{
  my $g = Seqsee::Anchored->new( group_p => 1, left_edge => 0, right_edge => 1 );
  local @LOG;
  my @r = $g->FindExtension( $DIR::RIGHT, 0 );
  record( case => 'find_extension', with_reln => 0, returned => [@r], log => [@LOG] );
  $g->set_underlying_reln( FakeRuleApp->new );
  @r = $g->FindExtension( $DIR::LEFT, 2 );
  record( case => 'find_extension', with_reln => 1, returned => [@r], log => [@LOG] );
  record( case => 'find_extension_arity', error => err( eval { $g->FindExtension($DIR::LEFT); 1 } ? '' : $@ ) );
}

emit();
