# Oracle for SLTM.pm, part I: nodes, links, activations, spreading, SpikeAndChoose, DecayAll,
# the Get*/Set*/Choose* helpers and GetTopConcepts.
# Output: tests/golden/sltm_core.json
use strict;
no warnings;    # undef strings in the edge cases are intended
use Oracle;
use S;

sub err { my ($e) = @_; $e =~ s/ at \S+ line \d+\.?\n.*//s; $e =~ s/ at \(eval \d+\) line \d+\.?\n.*//s; $e }

our @DEBUG;
{ no strict 'refs'; *{"main::debug_message"} = sub { push @DEBUG, [@_] }; }

sub P { SLTM::Platonic->create( $_[0] ) }

# Observable state of the LTM: nodes, activations and links (links sorted by from/type/to).
sub state {
  my @acts = map { [@$_] } @SLTM::ACTIVATIONS;
  my @links;
  for my $from ( 1 .. $#SLTM::OUT_LINKS ) {
    my $lr = $SLTM::OUT_LINKS[$from];
    for my $type ( 0 .. $#$lr ) {
      my $h = $lr->[$type] or next;
      for my $to ( sort { $a <=> $b } keys %$h ) {
        push @links, [ $from, $type, $to + 0, [ @{ $h->{$to} } ] ];
      }
    }
  }
  return {
    node_count  => $SLTM::NodeCount,
    nodes       => [ map { $SLTM::MEMORY[$_]->as_text } 1 .. $#SLTM::MEMORY ],
    activations => \@acts,
    links       => \@links,
    link_count  => scalar(@SLTM::LINKS),
    out_links_shape => [ map { ref($_) ? scalar(@$_) : $_ } @SLTM::OUT_LINKS ],
  };
}

# ---- constants and Clear ----
record( op => 'constants', LTM_FOLLOWS => SLTM::LTM_FOLLOWS, LTM_IS => SLTM::LTM_IS,
  LTM_CAN_BE_SEEN_AS => SLTM::LTM_CAN_BE_SEEN_AS, LTM_TYPE_COUNT => SLTM::LTM_TYPE_COUNT,
  link_type_2_str => \%SLTM::LinkType2Str );
SLTM::Clear();
record( op => 'clear', state => state() );

# ---- InsertNode / GetMemoryIndex push activations and out-link lists ----
SLTM::Clear();
my @idx = map { SLTM::GetMemoryIndex( P($_) ) } 1 .. 4;
record( op => 'insert_nodes', indices => \@idx, again => SLTM::GetMemoryIndex( P(2) ), state => state() );

# ---- links ----
{
  SLTM::Clear();
  my $l1 = SLTM::InsertISALink( P(1), P(2) );
  my $l2 = SLTM::InsertISALink( P(1), P(2) );
  my $l3 = SLTM::InsertFollowsLink( P(1), P(3) );
  my $l4 = SLTM::InsertISALink( P(3), P(1) );
  my $l5 = SLTM::__InsertLinkUnlessPresent( 2, 3, 1, 3 );
  my $l6 = SLTM::__InsertLinkUnlessPresent( 2, 3, 2, 3 );
  record( op => 'insert_links', same => ( $l1 == $l2 ) ? 1 : 0, new_link => [@$l1], follows => [@$l3],
    modifier_link => [@$l5], same_modifier_ignored => ( $l5 == $l6 ) ? 1 : 0,
    distinct => ( $l1 != $l3 && $l3 != $l4 ) ? 1 : 0, state => state() );
}

# ---- StrengthenLink ----
{
  SLTM::Clear();
  SLTM::InsertISALink( P(1), P(2) );
  my @r;
  for my $amt ( 1, 10, 0, undef, 50, 200, -3 ) {
    push @r, [ $amt, SLTM::StrengthenLinkGivenNodes( P(1), P(2), SLTM::LTM_IS, $amt ) ];
  }
  push @r, [ 'index', SLTM::StrengthenLinkGivenIndex( 1, 2, 2, 7 ) ];
  my $e1 = err( eval { SLTM::StrengthenLinkGivenIndex( 1, 2, 1, 7 ); 1 } ? '' : $@ );
  my $e2 = err( eval { SLTM::StrengthenLinkGivenNodes( P(2), P(1), 2, 7 ); 1 } ? '' : $@ );
  record( op => 'strengthen', results => \@r, missing_type_error => $e1, missing_link_error => $e2,
    state => state() );
}

# ---- SpikeBy / WeakenBy ----
{
  SLTM::Clear();
  my @r;
  push @r, [ 'spike', 10, SLTM::SpikeBy( 10, P(1), P(2) ) ];
  push @r, [ 'spike', 0, SLTM::SpikeBy( 0, P(1) ) ];
  push @r, [ 'spike', undef, SLTM::SpikeBy( undef, P(2) ) ];
  push @r, [ 'spike_strings_skipped', 5, SLTM::SpikeBy( 5, "abc", P(3), undef, 7 ) ];
  push @r, [ 'spike_twice', 20, SLTM::SpikeBy( 20, P(3), P(3) ) ];
  push @r, [ 'spike_big', 400, SLTM::SpikeBy( 400, P(4) ) ];
  push @r, [ 'weaken', 3, SLTM::WeakenBy( 3, P(1), P(3) ) ];
  push @r, [ 'weaken', 0, SLTM::WeakenBy( 0, P(2) ) ];
  push @r, [ 'weaken_big', 1000, SLTM::WeakenBy( 1000, P(4) ) ];
  my $e1 = err( eval { SLTM::SpikeBy(5); 1 } ? '' : $@ );
  my $e2 = err( eval { SLTM::SpikeBy( 5, "x", undef ); 1 } ? '' : $@ );
  my $e3 = err( eval { SLTM::WeakenBy(5); 1 } ? '' : $@ );
  record( op => 'spike_weaken', results => \@r, no_concepts_error => $e1, only_strings_error => $e2,
    weaken_none_error => $e3, state => state() );
}

# ---- SpikeAndChoose (seeded) ----
{
  SLTM::Clear();
  srand(42);
  my @r;
  push @r, [ 'empty', [ SLTM::SpikeAndChoose(10) ] ];
  my $e = err( eval { SLTM::SpikeAndChoose( 10, P(1), undef ); 1 } ? '' : $@ );
  for my $k ( 1 .. 40 ) {
    my @c = map { P($_) } grep { ( $k + $_ ) % 3 } 1 .. 5;
    my $amt = ( $k * 7 ) % 30;
    my @got = SLTM::SpikeAndChoose( $amt, @c );
    push @r, [ $k, $amt, [ map { $_->as_text } @c ], [ map { defined $_ ? $_->as_text : undef } @got ] ];
  }
  push @r, [ 'after', rand() ];
  record( op => 'spike_and_choose', results => \@r, undef_error => $e, state => state() );
}
{
  # Activations below 0.02 count as 0: choose_if_non_zero returns undef without a draw.
  SLTM::Clear();
  SLTM::GetMemoryIndex( P($_) ) for 1 .. 2;
  $SLTM::ACTIVATIONS[$_][0] = -150 for 1 .. 2;
  srand(5);
  my @got = SLTM::SpikeAndChoose( 1, P(1), P(2) );
  record( op => 'spike_and_choose_low', got => [ map { defined $_ ? $_->as_text : undef } @got ],
    count => scalar(@got), next_rand => rand(), state => state() );
  $SLTM::ACTIVATIONS[1][0] = -300;
  srand(5);
  my @got2 = SLTM::SpikeAndChoose( 1, P(1), P(2) );
  record( op => 'spike_and_choose_undef_activation',
    got => [ map { defined $_ ? $_->as_text : undef } @got2 ], next_rand => rand(), state => state() );
}

# ---- SpreadActivationFrom ----
sub build_graph {
  SLTM::Clear();
  SLTM::GetMemoryIndex( P($_) ) for 1 .. 8;
  my %links = @_;
  for my $spec ( @{ $links{links} } ) {
    my ( $from, $to, $mod, $type, $sig, $stab ) = @$spec;
    my $l = SLTM::__InsertLinkUnlessPresent( $from, $to, $mod, $type );
    $l->[1] = $sig  if defined $sig;
    $l->[2] = $stab if defined $stab;
  }
  for my $a ( @{ $links{acts} || [] } ) {
    my ( $i, $raw ) = @$a;
    $SLTM::ACTIVATIONS[$i][0] = $raw;
    $SLTM::ACTIVATIONS[$i][2] = $SLinkActivation::PRECALCULATED[$raw];
  }
}

my @GRAPHS = (
  [ 'no_links', { links => [] }, 1 ],
  [ 'one_link', { links => [ [ 1, 2, 0, 2 ] ] }, 1 ],
  [ 'type1_only', { links => [ [ 1, 2, 0, 1 ] ] }, 1 ],
  [ 'type3_only', { links => [ [ 1, 3, 0, 3 ] ] }, 1 ],
  [ 'fan_out', { links => [ [ 1, 2, 0, 1 ], [ 1, 3, 0, 2 ], [ 1, 4, 0, 3 ] ], acts => [ [ 1, 60 ] ] }, 1 ],
  [ 'strong_fan_out', { links => [ [ 1, 2, 0, 1, 30 ], [ 1, 3, 0, 2, 3 ], [ 1, 4, 0, 3, 50, 0.01 ] ],
      acts => [ [ 1, 80 ] ] }, 1 ],
  [ 'distance_two', { links => [ [ 1, 2, 0, 2, 40 ], [ 2, 3, 0, 2, 40 ], [ 2, 4, 0, 1 ], [ 3, 5, 0, 2, 40 ],
      [ 2, 1, 0, 3, 40 ] ], acts => [ [ 1, 70 ] ] }, 1 ],
  [ 'distance_two_shared', { links => [ [ 1, 2, 0, 2, 40 ], [ 1, 3, 0, 2, 40 ], [ 2, 6, 0, 2, 20 ],
      [ 3, 6, 0, 1, 20 ], [ 2, 3, 0, 1, 20 ], [ 3, 7, 0, 3 ] ], acts => [ [ 1, 50 ] ] }, 1 ],
  [ 'weak_middle', { links => [ [ 1, 2, 0, 2 ], [ 2, 3, 0, 2, 90 ] ], acts => [ [ 1, 99 ] ] }, 1 ],
  [ 'self_link', { links => [ [ 1, 1, 0, 2, 40 ], [ 1, 2, 0, 2, 40 ] ], acts => [ [ 1, 60 ] ] }, 1 ],
  [ 'modifier', { links => [ [ 1, 2, 5, 2, 40 ], [ 1, 3, 6, 2, 40 ], [ 2, 4, 5, 1, 40 ] ],
      acts => [ [ 1, 70 ], [ 5, 90 ], [ 6, 2 ] ] }, 1 ],
  [ 'saturate_target', { links => [ [ 1, 2, 0, 2, 99, 0.001 ] ], acts => [ [ 1, 99 ], [ 2, 50 ] ] }, 1 ],
  [ 'root_is_target_only', { links => [ [ 2, 1, 0, 2, 40 ] ], acts => [ [ 1, 60 ] ] }, 1 ],
  [ 'other_root', { links => [ [ 1, 2, 0, 2, 40 ], [ 4, 1, 0, 2, 40 ], [ 4, 5, 0, 1, 20 ] ],
      acts => [ [ 4, 75 ] ] }, 4 ],
);
for my $g (@GRAPHS) {
  my ( $name, $spec, $root ) = @$g;
  build_graph(%$spec);
  my $before = state();
  @DEBUG = ();
  my $e = eval { SLTM::SpreadActivationFrom($root); 1 } ? undef : err($@);
  record( op => 'spread', name => $name, root => $root, links => $spec->{links}, acts => $spec->{acts},
    before => $before, after => state(), debug => [ sort { $a->[0] cmp $b->[0] } @DEBUG ], error => $e );
}
{
  SLTM::Clear();
  my $e = err( eval { SLTM::SpreadActivationFrom(3); 1 } ? '' : $@ );
  record( op => 'spread_missing_root', error => $e );
}

# ---- DecayAll ----
{
  build_graph( links => [ [ 1, 2, 0, 2, 3 ], [ 2, 3, 0, 1 ], [ 3, 4, 0, 3, 1.5, 0.5 ] ],
    acts => [ [ 1, 60 ], [ 2, 3 ], [ 3, 30 ] ] );
  $SLTM::ACTIVATIONS[0][0] = 40;
  my @states;
  for ( 1 .. 6 ) { SLTM::DecayAll(); push @states, state(); }
  SLTM::Clear();
  my $r = [ SLTM::DecayAll() ];
  record( op => 'decay_all', states => \@states, empty_return => $r, empty_state => state() );
}

# ---- Get*Activations*, Set*ForIndex, GetTopConcepts ----
{
  build_graph( links => [], acts => [ [ 1, 60 ], [ 2, 3 ], [ 3, 30 ] ] );
  record( op => 'getters',
    raw_for_indices      => SLTM::GetRawActivationsForIndices( [ 1, 2, 3, 3, 0 ] ),
    real_for_indices     => SLTM::GetRealActivationsForIndices( [ 3, 1 ] ),
    real_for_concepts    => SLTM::GetRealActivationsForConcepts( [ P(1), P(3) ] ),
    real_for_one         => SLTM::GetRealActivationsForOneConcept( P(2) ),
    real_for_new_concept => SLTM::GetRealActivationsForOneConcept( P(99) ),
    empty                => SLTM::GetRawActivationsForIndices( [] ),
    top                  => [ map { [ $_->[0]->as_text, $_->[1], $_->[2] ] } SLTM::GetTopConcepts(3) ],
    state                => state() );
  my $raw_oob = SLTM::GetRawActivationsForIndices( [20] );
  record( op => 'getter_out_of_range', result => $raw_oob, activations_len => scalar(@SLTM::ACTIVATIONS),
    node_count => $SLTM::NodeCount );
}
{
  build_graph( links => [] );
  SLTM::SetSignificanceAndStabilityForIndex( 1, 23, 0.5 );
  SLTM::SetSignificanceAndStabilityForIndex( 2, -7, 0.25 );
  SLTM::SetDepthReciprocalForIndex( 3, 0.125 );
  SLTM::SetDepthReciprocalForIndex( 4, "0.5" );
  SLTM::SetRawActivationForIndex( 5, 77 );
  my @r = ( SLTM::SetRawActivationForIndex( 6, 12 ), SLTM::SetDepthReciprocalForIndex( 7, 0.3 ),
    SLTM::SetSignificanceAndStabilityForIndex( 8, 12, 0.1 ) );
  record( op => 'setters', returns => \@r, state => state() );
  SLTM::SpikeBy( 10, P(3), P(4), P(5) );
  record( op => 'setters_then_spike', state => state() );
  SLTM::Clear();
  record( op => 'top_empty', top => [ SLTM::GetTopConcepts(5) ] );
}

# ---- Choose* (seeded) ----
{
  build_graph( links => [], acts => [ [ 1, 60 ], [ 2, 3 ], [ 3, 30 ], [ 4, 95 ] ] );
  srand(7);
  my @r;
  for ( 1 .. 25 ) {
    push @r, [
      SLTM::ChooseIndexGivenIndex( [ 1, 2, 3, 4 ] ),
      SLTM::ChooseConceptGivenIndex( [ 2, 3 ] )->as_text,
      SLTM::ChooseIndexGivenConcept( [ P(1), P(4) ] ),
      SLTM::ChooseConceptGivenConcept( [ P(3), P(2), P(1) ] )->as_text,
    ];
  }
  my @empty = ( SLTM::ChooseIndexGivenIndex( [] ), SLTM::ChooseConceptGivenConcept( [] ) );
  my $unknown = SLTM::ChooseConceptGivenConcept( [ P(50), P(51) ] );
  record( op => 'choose', results => \@r, empty => \@empty, unknown => $unknown->as_text,
    unknown_inserted => $SLTM::NodeCount, next_rand => rand() );
}

# ---- A seeded random walk over the operations ----
{
  SLTM::Clear();
  srand(2024);
  my @log;
  my @concepts = map { P($_) } 1 .. 7;
  for my $step ( 1 .. 300 ) {
    my $op = int( rand(7) );
    # (Not $a/$b: lexicals with those names would break the sort below.)
    my $c1 = $concepts[ int( rand(7) ) ];
    my $c2 = $concepts[ int( rand(7) ) ];
    my $amt = int( rand(40) );
    my $res;
    if    ( $op == 0 ) { SLTM::InsertISALink( $c1, $c2 ); $res = 'isa' }
    elsif ( $op == 1 ) { SLTM::InsertFollowsLink( $c1, $c2 ); $res = 'follows' }
    elsif ( $op == 2 ) { $res = SLTM::SpikeBy( $amt, $c1, $c2 ) }
    elsif ( $op == 3 ) { my $c = SLTM::SpikeAndChoose( $amt, $c1, $c2 ); $res = defined $c ? $c->as_text : undef }
    elsif ( $op == 4 ) { SLTM::DecayAll(); $res = 'decay' }
    elsif ( $op == 5 ) {
      my $i = SLTM::GetMemoryIndex($c1);
      my $t = ( $amt % 2 ) + 1;
      if ( $SLTM::OUT_LINKS[$i][$t] and %{ $SLTM::OUT_LINKS[$i][$t] } ) {
        my ($to) = sort { $a <=> $b } keys %{ $SLTM::OUT_LINKS[$i][$t] };
        $res = SLTM::StrengthenLinkGivenIndex( $i, $to, $t, $amt * 5 );
      }
      else { $res = 'nolink' }
    }
    else { SLTM::SpreadActivationFrom( SLTM::GetMemoryIndex($c1) ); $res = 'spread' }
    push @log, [ $step, $op, $res ];
  }
  record( op => 'random_walk', log => \@log, state => state() );
}

emit();
