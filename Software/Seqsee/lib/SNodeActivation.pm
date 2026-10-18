# SActivation will now be for links only...
package SNodeActivation;
use 5.10.0;
use strict;
use warnings;
use Carp;

use constant RAW_ACTIVATION   => 0;
use constant DEPTH_RECIPROCAL => 1;
use constant REAL_ACTIVATION  => 2;

my @PRECALCULATED = @SLinkActivation::PRECALCULATED;
confess "Load order issues" unless @PRECALCULATED;

use constant Initial_Raw_Activation   => 2;
use constant Initial_Depth            => 5;
use constant Initial_Depth_Reciprocal => 1 / Initial_Depth;
my $Initial_Real_Activation = $PRECALCULATED[Initial_Raw_Activation]
// confess "Initial_Real_Activation not defined!";

sub new {
  my ( $package, $depth_reciprocal ) = @_;
  bless [
    Initial_Raw_Activation, $depth_reciprocal || Initial_Depth_Reciprocal,
    $Initial_Real_Activation
  ], $package;
}

my $DECAY_CODE = q{
$times ||= 1;
$_->[0] -= $_->[1] * $times;
$_->[0] = 2 if $_->[0] < 2;
$_->[2] = $PRECALCULATED[$_->[0]];
};

my $SPIKE_CODE = q{
$spike ||= 1;
$_->[0] += $_->[1] * $spike;
if ($_->[0] > 98) { $_->[0] = 90; $_->[1] = 1 / (1 + 1 / $_->[1]);}
$_->[2] = $PRECALCULATED[$_->[0]];
};

my $WEAKEN_CODE = q{
$spike ||= 1;
$_->[0] -= $_->[1] * $spike;
$_->[0] = 2 if $_->[0] < 2;
$_->[2] = $PRECALCULATED[$_->[0]];
};

*DecayManyTimes = eval qq{
sub {
    my \$times = shift;
    for ( \@_ ) {
        $DECAY_CODE;
    }
}
};

*SpikeSeveral = eval qq{
sub {
    my \$spike = shift;
    for ( \@_ ) {
        $SPIKE_CODE;
    }
    return \$_[-1][2];
}
};

*WeakenSeveral = eval qq{
sub {
    my \$spike = shift;
    for ( \@_ ) {
        $WEAKEN_CODE;
    }
    return \$_[-1][2];
}
};
