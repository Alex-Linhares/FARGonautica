# Oracle for SNodeActivation.pm and SLinkActivation.pm. Output: tests/golden/activations.json
use strict;
no warnings;    # undef arithmetic in the edge cases is intended
use Oracle;

# SNodeActivation confesses at load time unless SLinkActivation is loaded first.
{
  my $ok = eval { require SNodeActivation; 1 };
  my ($msg) = ( $@ =~ /^(.*?) at / );
  record( op => 'load_order', loaded => $ok ? 1 : 0, error => $msg );
  delete $INC{'SNodeActivation.pm'};
}
require SLinkActivation;
require SNodeActivation;

sub state { [ @{ $_[0] } ] }
sub err { my ($e) = @_; $e =~ s/ at \(eval \d+\) line \d+\.\n//; $e =~ s/ at \S+ line \d+\.?\n.*//s; $e }

# Constants and the lookup table.
record( op => 'precalculated', values => [@SLinkActivation::PRECALCULATED] );
record(
  op                => 'link_constants',
  RAW_ACTIVATION    => SLinkActivation::RAW_ACTIVATION(),
  RAW_SIGNIFICANCE  => SLinkActivation::RAW_SIGNIFICANCE(),
  STABILITY_RECIPROCAL => SLinkActivation::STABILITY_RECIPROCAL(),
  REAL_ACTIVATION   => SLinkActivation::REAL_ACTIVATION(),
  MODIFIER_NODE_INDEX => SLinkActivation::MODIFIER_NODE_INDEX(),
  Initial_Raw_Activation => $SLinkActivation::Initial_Raw_Activation,
  Initial_Raw_Significance => $SLinkActivation::Initial_Raw_Significance,
  Initial_Stability => $SLinkActivation::Initial_Stability,
  Initial_Stability_Reciprocal => $SLinkActivation::Initial_Stability_Reciprocal,
);
record(
  op                       => 'node_constants',
  RAW_ACTIVATION           => SNodeActivation::RAW_ACTIVATION(),
  DEPTH_RECIPROCAL         => SNodeActivation::DEPTH_RECIPROCAL(),
  REAL_ACTIVATION          => SNodeActivation::REAL_ACTIVATION(),
  Initial_Raw_Activation   => SNodeActivation::Initial_Raw_Activation(),
  Initial_Depth            => SNodeActivation::Initial_Depth(),
  Initial_Depth_Reciprocal => SNodeActivation::Initial_Depth_Reciprocal(),
);

# ---- SNodeActivation ----
for my $arg ( [], [undef], [0], [''], ['0'], [0.5], [0.1], ['0.25'], [1], [-0.5] ) {
  my $n = SNodeActivation->new(@$arg);
  record( op => 'node_new', args => $arg, state => state($n) );
}

my @SPIKES = ( undef, 0, 1, 2.7, 5, 10, 50, 97, 100, 500, 0.5, -1, -5, -300, '3' );
for my $spike (@SPIKES) {
  for my $dr ( 0.2, 0.5, 1 ) {
    my $n     = SNodeActivation->new($dr);
    my @steps = ();
    for ( 1 .. 6 ) {
      my $ret = SNodeActivation::SpikeSeveral( $spike, $n );
      push @steps, { ret => $ret, state => state($n) };
    }
    record( op => 'node_spike', spike => $spike, depth_reciprocal => $dr, steps => \@steps );

    my $w = SNodeActivation->new($dr);
    SNodeActivation::SpikeSeveral( 60, $w );
    my @wsteps = ();
    for ( 1 .. 6 ) {
      my $ret = SNodeActivation::WeakenSeveral( $spike, $w );
      push @wsteps, { ret => $ret, state => state($w) };
    }
    record( op => 'node_weaken', spike => $spike, depth_reciprocal => $dr, steps => \@wsteps );
  }
}

for my $times ( undef, 0, 1, 3, 0.5, 2.5, '2', -10, -100, -1000 ) {
  for my $dr ( 0.2, 0.5, 1 ) {
    my $n = SNodeActivation->new($dr);
    SNodeActivation::SpikeSeveral( 40, $n );
    my @steps = ();
    for ( 1 .. 6 ) {
      SNodeActivation::DecayManyTimes( $times, $n );
      push @steps, state($n);
    }
    record( op => 'node_decay', times => $times, depth_reciprocal => $dr, steps => \@steps );
  }
}

# Several at once (including the same activation twice); the return is the last one's real activation.
{
  my @n = map { SNodeActivation->new($_) } ( 0.2, 0.5, 1 );
  my $ret = SNodeActivation::SpikeSeveral( 7, @n, $n[0] );
  record( op => 'node_spike_several', ret => $ret, states => [ map { state($_) } @n ] );
  $ret = SNodeActivation::WeakenSeveral( 3, $n[2], @n );
  record( op => 'node_weaken_several', ret => $ret, states => [ map { state($_) } @n ] );
  SNodeActivation::DecayManyTimes( 2, @n, $n[1] );
  record( op => 'node_decay_several', states => [ map { state($_) } @n ] );
  SNodeActivation::DecayManyTimes(2);
  record( op => 'node_decay_none', ok => 1 );
}
for my $f (qw(SpikeSeveral WeakenSeveral)) {
  no strict 'refs';
  my $code = \&{"SNodeActivation::$f"};
  eval { $code->(3) };
  record( op => 'node_empty', func => $f, error => err($@) );
}

# ---- SLinkActivation ----
for my $arg ( [], [undef], [0], [3], ['7'] ) {
  my $l = SLinkActivation->new(@$arg);
  record(
    op     => 'link_new',
    args   => $arg,
    state  => state($l),
    raw    => $l->GetRawActivation,
    sig    => $l->GetRawSignificance,
    stab_r => $l->GetStabilityReciprocal
  );
}

# Decay: 40 steps from new, and from a spiked link (significance decays).
for my $pre ( 0, 30, 95, 250, 600 ) {
  my $l = SLinkActivation->new;
  SLinkActivation::Spike( $l, $pre ) if $pre;
  my @steps = ();
  for ( 1 .. 40 ) {
    my $ret = SLinkActivation::Decay($l);
    push @steps, { ret => $ret, state => state($l) };
  }
  record( op => 'link_decay', pre => $pre, steps => \@steps );
}

for my $spike ( undef, 0, 1, 2.5, 10, 50, 94, 95, 100, 200, 0.5, -3, -500, '4' ) {
  my $l     = SLinkActivation->new;
  my @steps = ();
  for ( 1 .. 8 ) {
    my $ret = SLinkActivation::Spike( $l, $spike );
    push @steps, { ret => $ret, state => state($l) };
  }
  record( op => 'link_spike', spike => $spike, steps => \@steps );
}

# Long run of big spikes: significance saturates and stability grows.
{
  my $l     = SLinkActivation->new;
  my @steps = ();
  for ( 1 .. 120 ) {
    my $ret = SLinkActivation::Spike( $l, 100 );
    push @steps, { ret => $ret, state => state($l) };
  }
  record( op => 'link_saturate', steps => \@steps );
}

# DecayMany: 1-based, the first $cnt entries; undef slots are skipped.
for my $cnt ( 0, 1, 2, 3, 5, 2.5 ) {
  my @arr = ( 'zero', map { SLinkActivation->new } 1 .. 3 );
  SLinkActivation::Spike( $arr[$_], 10 * $_ ) for 1 .. 3;
  SLinkActivation::DecayMany( \@arr, $cnt ) for 1 .. 3;
  record(
    op     => 'link_decay_many',
    cnt    => $cnt,
    first  => $arr[0],
    length => scalar(@arr),
    states => [ map { state( $arr[$_] ) } 1 .. 3 ]
  );
}
{
  my @arr = ( undef, SLinkActivation->new, undef, SLinkActivation->new );
  SLinkActivation::DecayMany( \@arr, 4 );
  record(
    op     => 'link_decay_many_holes',
    length => scalar(@arr),
    defs   => [ map { defined $_ ? 1 : 0 } @arr ],
    states => [ state( $arr[1] ), state( $arr[3] ) ]
  );
}
for my $args ( [], [ [] ], [ [], 1, 2 ] ) {
  eval { SLinkActivation::DecayMany(@$args) };
  record( op => 'link_decay_many_dies', nargs => scalar(@$args), error => err($@) );
}

# AmountToSpread
{
  local @SLTM::ACTIVATIONS = ( SNodeActivation->new, SNodeActivation->new, SNodeActivation->new(0.5) );
  SNodeActivation::SpikeSeveral( 30, $SLTM::ACTIVATIONS[2] );
  SNodeActivation::SpikeSeveral( 9, $SLTM::ACTIVATIONS[1] );
  for my $mod ( undef, 0, '', 1, 2, '2', -1 ) {
    for my $pre ( 0, 50, 200 ) {
      my $l = SLinkActivation->new($mod);
      SLinkActivation::Spike( $l, $pre ) if $pre;
      for my $amt ( 0, 1, 10, 37.5, 100, -20 ) {
        record(
          op       => 'amount_to_spread',
          modifier => $mod,
          pre      => $pre,
          amount   => $amt,
          ret      => $l->AmountToSpread($amt)
        );
      }
    }
  }
  for my $mod ( 3, 9 ) {
    my $l = SLinkActivation->new($mod);
    my $d = dies( sub { $l->AmountToSpread(10) } );
    my ($prefix) = ( $@ =~ /^(AmountToSpread: <SLinkActivation=ARRAY)/ );
    record( op => 'amount_to_spread_dies', modifier => $mod, dies => $d, prefix => $prefix );
  }
}

# A seeded random walk mixing every operation (the draws match drand48 in Python).
{
  srand(42);
  my @nodes = map { SNodeActivation->new } 1 .. 3;
  my @links = ( undef, map { SLinkActivation->new } 1 .. 3 );
  my @steps;
  for ( 1 .. 400 ) {
    my $op  = int( rand(5) );
    my $who = int( rand(3) );
    my $amt = int( rand(40) ) - 5;
    my $ret;
    if    ( $op == 0 ) { $ret = SNodeActivation::SpikeSeveral( $amt, $nodes[$who] ) }
    elsif ( $op == 1 ) { $ret = SNodeActivation::WeakenSeveral( $amt, $nodes[$who] ) }
    elsif ( $op == 2 ) { SNodeActivation::DecayManyTimes( $who, @nodes ) }
    elsif ( $op == 3 ) { $ret = SLinkActivation::Spike( $links[ $who + 1 ], $amt ) }
    else               { SLinkActivation::DecayMany( \@links, $who + 1 ) }
    push @steps,
    {
      op    => $op,
      who   => $who,
      amt   => $amt,
      ret   => $ret,
      nodes => [ map { state($_) } @nodes ],
      links => [ map { state( $links[$_] ) } 1 .. 3 ]
    };
  }
  record( op => 'random_walk', seed => 42, steps => \@steps );
}

emit();
