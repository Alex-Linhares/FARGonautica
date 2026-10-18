package Perf::Figure::SequenceToChart;
use 5.10.0;
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

my %Label_of : ATTR(:name<label>);
my %Data_Indexed_By_Cluster_for : ATTR(:name<data_indexed_by_cluster>);
my %Max_Avg_Steps_of : ATTR(:name<max_avg_steps>);
my %Max_Max_Steps_of : ATTR(:name<max_max_steps>);
my %Sequence_of : ATTR(:name<sequence>);

sub BUILD {
    my ( $self, $id, $opts_ref ) = @_;
    my $string = $opts_ref->{string}
      // confess "Missing required argument 'string'";
    my $label = $opts_ref->{label}
      // confess "Missing required argument 'label'";
    my $is_ltm_self_config = $opts_ref->{is_ltm_self_config}
      // confess "Missing required argument 'is_ltm_self_config'";
    my $all_read_data = $opts_ref->{all_read_data}
      // confess "Missing required argument 'all_read_data'";
    my $clusters = $opts_ref->{clusters}
      // confess "Missing required argument 'clusters'";

    my $sequence = $Sequence_of{$id} =
      Perf::TestSequence->new( { string => $string } );
    $Label_of{$id} = $label;
    if ($is_ltm_self_config) {
        my @data_sets = $all_read_data->GetDataForSequenceAndCluster(
            {
                sequence => $sequence,
                cluster  => $clusters->[0]
            }
        );
        my @collated_data_bins;
        for ( 0 .. 9 ) {
            $collated_data_bins[$_] = [];
        }

        for my $data_set (@data_sets) {
            my @results = @{ $data_set->get_results };
            for ( 0 .. 9 ) {
                push @{ $collated_data_bins[$_] }, $results[$_];
            }
        }

        my %data_indexed_by_cluster;
        my $max_max_steps = 0;
        my $max_avg_steps = 0;
        for ( 0 .. 9 ) {
            my $data = $data_indexed_by_cluster{ 'cluster_' . $_ } =
              Perf::CollatedData->new( { data => $collated_data_bins[$_] } );
            my $avg_steps = $data->get_avg_time_to_success();
            $max_avg_steps = $avg_steps if $avg_steps > $max_avg_steps;

            my $max_steps = $data->get_max_time_to_success();
            $max_max_steps = $max_steps if $max_steps > $max_max_steps;
        }

        $Data_Indexed_By_Cluster_for{$id} = \%data_indexed_by_cluster;
        $Max_Max_Steps_of{$id}            = $max_max_steps;
        $Max_Avg_Steps_of{$id}            = $max_avg_steps;
    }
    else {
        my %data_indexed_by_cluster;
        my $max_avg_steps = 0;
        my $max_max_steps = 0;
        for my $cluster ( @{$clusters} ) {
            my @data_sets = $all_read_data->GetDataForSequenceAndCluster(
                { sequence => $sequence, cluster => $cluster } );
            my @collated_data = map { @{ $_->get_results } } @data_sets;
            my $is_human = $cluster->is_human;

            my $data = $data_indexed_by_cluster{$cluster} =
              Perf::CollatedData->new( { data => \@collated_data } );
            my $avg_steps = $data->get_avg_time_to_success();
            $avg_steps *= $Perf::AllCollectedData::CODELETS_PER_SECOND
              if $is_human;
            $max_avg_steps = $avg_steps if $avg_steps > $max_avg_steps;

            my $max_steps = $data->get_max_time_to_success();
            $max_steps *= $Perf::AllCollectedData::CODELETS_PER_SECOND
              if $is_human;
            $max_max_steps = $max_steps if $max_steps > $max_max_steps;
        }
        $Data_Indexed_By_Cluster_for{$id} = \%data_indexed_by_cluster;
        $Max_Max_Steps_of{$id}            = $max_max_steps;
        $Max_Avg_Steps_of{$id}            = $max_avg_steps;
    }
}

1;
