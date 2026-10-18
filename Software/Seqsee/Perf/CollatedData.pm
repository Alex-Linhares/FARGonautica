package Perf::CollatedData;

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

use Statistics::Basic qw{:all};

# Array-ref, each a Seqsee::ResultOfTestRun
my %Data_for : ATTR(:name<data>);

my %total_count_of : ATTR(:name<total_count>);

my %successful_count_of : ATTR(:name<successful_count>);
my %successful_times_of :ATTR(:name<successful_times>);
my %vector_of_successful_of : ATTR(:name<vector_of_successful>);
my %avg_time_to_success_of : ATTR(:name<avg_time_to_success>);
my %sdv_time_to_success_of : ATTR(:name<sdv_time_to_success>);
my %success_percentage_of : ATTR(:name<success_percentage>);
my %max_time_to_success_of : ATTR(:name<max_time_to_success>);
my %min_time_to_success_of : ATTR(:name<min_time_to_success>);
my %quartile_1_of : ATTR(:name<quartile_1>);
my %quartile_3_of : ATTR(:name<quartile_3>);
my %median_of : ATTR(:name<median>);

sub BUILD {
    my ( $self, $id, $opts_ref ) = @_;
    $Data_for{$id} = $opts_ref->{data};
    my @results = @{ $Data_for{$id} };

    unless (@results) {
        $total_count_of{$id}          = 0;
        $successful_count_of{$id}     = 0;
        $avg_time_to_success_of{$id}  = 0;
        $sdv_time_to_success_of{$id}  = 0;
        $min_time_to_success_of{$id}  = 0;
        $max_time_to_success_of{$id}  = 0;
        $success_percentage_of{$id}   = 0;
        $quartile_3_of{$id}           = 0;
        $quartile_1_of{$id}           = 0;
        $median_of{$id}               = 0;
        $vector_of_successful_of{$id} = vector();
        $successful_times_of{$id} = [];
        return;
    }

    @results = map { Storable::thaw($_) } @results;
    for (@results) {
        ## Attempt Result: $_->_DUMP()
    }

    $total_count_of{$id} = scalar(@results);
    my @successful =
      grep { $_->get_status()->IsSuccess() } @results;
    my @successful_times = sort { $a <=> $b }
      map { $_->get_steps() } @successful;

    ## Seccess Rate: scalar(@successful), scalar(@results)

    $min_time_to_success_of{$id} = $successful_times[0] // 0;
    $max_time_to_success_of{$id} = $successful_times[-1] // 0;
    my $success_count = $successful_count_of{$id} = scalar(@successful);

    my $q1_index     = ( $success_count - 1 ) / 4;
    my $median_index = 2 * $q1_index;
    my $q3_index     = 3 * $q1_index;

    ( $quartile_1_of{$id}, $median_of{$id}, $quartile_3_of{$id} ) =
      map { idx_to_value( \@successful_times, $_ ) }
      ( $q1_index, $median_index, $q3_index );

    my $vector = $vector_of_successful_of{$id} = vector( \@successful_times );

    $successful_times_of{$id} = \@successful_times;
    $avg_time_to_success_of{$id} = 0 + mean($vector);
    $sdv_time_to_success_of{$id} = 0 + stddev($vector);
    $success_percentage_of{$id}  = 100 * scalar(@successful) / scalar(@results);

}

sub DisplayStatus {
    my ($self)  = @_;
    my $id      = ident $self;
    my @results = @{ $Data_for{$id} };
    my %count;
    for my $res (@results) {
        $count{ $res->get_status()->get_status_string() }++;
    }
    print "\n";
    while ( my ( $k, $v ) = each %count ) {
        print "$k\t=>$v\n";
    }

    print $success_percentage_of{$id}, '%',
      "averaging $avg_time_to_success_of{$id} steps", "\n";
}

sub idx_to_value {
    my ($aref, $idx) = @_;
    my ($int, $frac) = (int($idx), $idx - int($idx));
    return $aref->[$int] unless $frac;
    return (1 - $frac) * $aref->[$int] + $frac * $aref->[$int + 1];
}

## Method ######################
#  Name             : TimeToSuccessAtPercentile
#  Returns          : value at possibly fractional index
#  Params via href  : No
#  Parameters       : fraction
#  Purpose          : fraction=0.5 returns the median, fraction=0.25 first q, etc

sub TimeToSuccessAtPercentile {
    my ($self, $fraction) = @_;
    my $id = ident $self;
    my $successful_times_ref = $successful_times_of{$id};
    my $max_index = $successful_count_of{$id} - 1;
    return idx_to_value($successful_times_ref, $max_index * $fraction);
}


1;

