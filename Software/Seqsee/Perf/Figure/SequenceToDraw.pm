package Perf::Figure::SequenceToDraw;

## STANDARD MODULES THAT I INCLUDE EVERYWHERE
use strict;
use warnings;

use List::Util qw{min max sum first};
use Time::HiRes;
use Getopt::Long;
use Storable;

use File::Slurp;
use Smart::Comments;
use IO::Prompt;
use Class::Std;
use Class::Multimethods;

use Carp;
## END OF STANDARD INCLUDES


my %Sequence_With_Markup_of : ATTR(:name<sequence_with_markup>);
my %Distractors_of : ATTR(:name<distractors>);
my %Label_of : ATTR(:name<label>);

sub BUILD {
    my ( $self, $id, $opts_ref ) = @_;
    my $string = $opts_ref->{string}
      // confess "Missing required argument 'string'";
    my $config = $opts_ref->{config}
      // confess "Missing required argument 'config'";
    my $possible_label = $opts_ref->{possible_label} // "label";

    $string =~ s# ^ ([^\|]*) \| ([^\|]*) .*#$1\|$2#x;
    $Sequence_With_Markup_of{$id} = $string;
    $Label_of{$id} = $config->{label} || $possible_label;
    
    if (my $d = $config->{distractor}) {
        $Distractors_of{$id} = (ref $d) ? $d : [$d];
    }
    $Distractors_of{$id} //= [];
}

1;

