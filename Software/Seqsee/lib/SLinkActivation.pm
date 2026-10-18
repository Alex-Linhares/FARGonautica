package SLinkActivation;
use 5.10.0;
use strict;
use warnings;
use Carp;

use constant RAW_ACTIVATION       => 0;    # Index.
use constant RAW_SIGNIFICANCE     => 1;    # Index.
use constant STABILITY_RECIPROCAL => 2;    # Index.
use constant REAL_ACTIVATION      => 3;    # Index.
use constant MODIFIER_NODE_INDEX  => 4;    # Index. *only* used for links.

my $RAW_ACTIVATION       = RAW_ACTIVATION();
my $RAW_SIGNIFICANCE     = RAW_SIGNIFICANCE();
my $STABILITY_RECIPROCAL = STABILITY_RECIPROCAL();
my $REAL_ACTIVATION      = REAL_ACTIVATION();
my $MODIFIER_NODE_INDEX  = MODIFIER_NODE_INDEX();

sub GetRawActivation       { return $_[0]->[RAW_ACTIVATION]; }
sub GetRawSignificance     { return $_[0]->[RAW_SIGNIFICANCE]; }
sub GetStabilityReciprocal { return $_[0]->[STABILITY_RECIPROCAL]; }

our @PRECALCULATED;
for ( 0 .. 200 ) {
  $PRECALCULATED[$_] =
  0.4815 + 0.342 * atan2( 12 * ( $_ / 100 - 0.5 ), 1 );    # change!
}

our $Initial_Raw_Activation       = 5;
our $Initial_Raw_Significance     = 1;
our $Initial_Stability            = 50;
our $Initial_Stability_Reciprocal = 1 / $Initial_Stability;
my $Initial_Real_Activation =
$PRECALCULATED[ $Initial_Raw_Activation + $Initial_Raw_Significance ]
// confess "Initial_Real_Activation not defined!";

sub new {
  my ( $package, $modifier_index ) = @_;
  bless [
    $Initial_Raw_Activation,       $Initial_Raw_Significance,
    $Initial_Stability_Reciprocal, $Initial_Real_Activation,
    $modifier_index,
  ], $package;
}

our $DECAY_CODE = q{
     $_->[0]-- if $_->[0] > 1;
     if ($_->[0] <= 1) {
        $_->[0] = 5;
        $_->[1] -= $_->[2] if $_->[1] > 1;
     }
     $_->[3] = $PRECALCULATED[$_->[0] + $_->[1]];
};

our $SPIKE_CODE = q{
    $spike ||=1;
    $_->[0]+= $spike;
    if ($_->[0] > 99) {
      $_->[1] += 2;
      $_->[0] = 90;
      if ($_->[1] > 99) {
        $_->[1] = 90;
        my $stab = 1 / $_->[2];
        $_->[2] = 1 / ($stab + 3);
      }
    }
    $_->[3] = $PRECALCULATED[$_->[0] + $_->[1]];
};

*Decay = eval qq{sub {\$_ = \$_[0]; $DECAY_CODE }};

*DecayMany = eval qq{
sub {
    \@_ == 2 or confess "DecayMany needs 2 args";
    my ( \$arr_ref, \$cnt ) = \@_;
    for my \$i ( 1 .. \$cnt ) {
        \$_ = \$arr_ref->[ \$i ];
        $DECAY_CODE;
    }
    }
};

*Spike = eval qq{
sub {
    my \$spike;
    ( \$_, \$spike ) = \@_;
    $SPIKE_CODE;
    return \$_->[3];
}
};

sub AmountToSpread {
  my ( $self, $original_amount ) = @_;
  my $amt1 =
  $original_amount * $self->[RAW_SIGNIFICANCE] / $self->[STABILITY_RECIPROCAL];
  my $modifier = $self->[MODIFIER_NODE_INDEX];
  if ($modifier) {
    my $modifier_activation =
    $SLTM::ACTIVATIONS[$modifier][2];    # SNodeActivation::REAL_ACTIVATION()
    $modifier_activation // confess
    "AmountToSpread: <$self>, <$modifier> <$SLTM::ACTIVATIONS[$modifier]> <$modifier_activation>";
    $amt1 *= ( $modifier_activation * 3 );
  }
  $amt1 /= 100;
  $amt1 += 1;
  return $amt1;
}

1;
