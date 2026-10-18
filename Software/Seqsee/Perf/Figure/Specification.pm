package Perf::Figure::Specification;
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
use Config::Std;

use Carp;
## END OF STANDARD INCLUDES

# Enough information to draw the chart fully.
my %Title_of : ATTR(:name<title>);
my %Figure_Type_of : ATTR(:name<figure_type>);

my %Cluster_Count_of : ATTR(:name<cluster_count>);
my %Sequences_to_Draw_Count_of : ATTR(:name<sequences_to_draw_count>);
my %Sequences_to_Chart_Count_of : ATTR(:name<sequences_to_chart_count>);

my %Clusters_of : ATTR(:name<clusters>);
my %Sequences_to_Draw_of : ATTR(:name<sequences_to_draw>);
my %Sequences_to_Chart_of : ATTR(:name<sequences_to_chart>);

my %Draw_Chart_of : ATTR(:name<draw_chart>);
my %Draw_Sequence_of : ATTR(:name<draw_seq>);
my %Has_Human_Data_of : ATTR(:name<has_human_data>);
my %Split_Chart_of : ATTR(:name<split_chart>);

my %Custom_Legend_of : ATTR(:name<custom_legend>);

sub new_from_specfile {
    my ( $package, $opts_ref ) = @_;
    my $all_read_data = $opts_ref->{all_read_data}
      // confess "Missing required argument 'all_read_data'";
    my $specfile = $opts_ref->{specfile}
      // confess "Missing required argument 'specfile'";

    read_config $specfile, my %Config;
    my $type = $Config{''}{Type} or confess "Missing type";
    my $title = $Config{''}{Title} //= "Title";

    my $draw_charts = $Config{''}{NoChart}    ? 0 : 1;
    my $draw_seq    = $Config{''}{NoSeq}      ? 0 : 1;
    my $split_chart = $Config{''}{SplitChart} ? 1 : 0;

    my $custom_legend = $Config{''}{custom_legend} // '';
    if ( $custom_legend and not( ref $custom_legend ) ) {
        $custom_legend = [$custom_legend];
    }

    my $has_human_data = 0;

    ## Cluster Count
    my $Cluster_Count =
      ( $type eq 'LTM_SELF_CONTEXT' ) ? 10 : $Config{''}{Cluster_Count};
    $Cluster_Count // confess "Need Cluster_Count";

    my @Clusters = map {
        my $config = $Config{ 'Cluster_' . $_ } || {};
        my $cluster = Perf::Figure::Cluster->new(
            { config => $config, figure_type => $type } );
        $has_human_data = 1 if $cluster->is_human();
        $cluster->set_label($_ - 1) if $type eq 'LTM_SELF_CONTEXT';
        ## human: $has_human_data
        $cluster
    } ( 1 .. $Cluster_Count );

    ## 'Sequences' are always display sequences.
    my @sequence_strings = @{ _ForceToBeARef( $Config{Sequences}{seq} ) };
    my @display_sequences;

    my $tentative_label = 'a';
    for my $i ( 1 .. scalar(@sequence_strings) ) {
        my $config_for_sequence = $Config{ 'Sequence_' . $i } || {};
        push @display_sequences,
          Perf::Figure::SequenceToDraw->new(
            {
                string         => $sequence_strings[ $i - 1 ],
                config         => $config_for_sequence,
                possible_label => $tentative_label,
            }
          );
        $tentative_label++;
    }

    $display_sequences[0]->set_label('Sequence') if $type eq 'LTM_SELF_CONTEXT';

    if ( $type eq 'LTM_WITH_CONTEXT' ) {
        for ( 1 .. $Cluster_Count ) {
            my $config = $Config{ 'Cluster_' . $_ } || {};
            if ( exists $config->{context} ) {
                push @display_sequences,
                  Perf::Figure::SequenceToDraw->new(
                    {
                        string => $config->{context},
                        config => $Config{ 'Cluster_Config_' . $_ } || {},
                        possible_label => 'context',
                    }
                  );
            }
            elsif ( $config->{source} eq 'LTM' ) {
                confess
                  "source=LTM but no context not sensible for the chart type";
            }
        }
    }

    # Sequences to chart;
    my @Sequences_to_Chart;
    if ( $type eq 'LTM_SELF_CONTEXT' ) {
        my $label = '';
        $Sequences_to_Chart[0] = Perf::Figure::SequenceToChart->new(
            {
                label              => $label,
                is_ltm_self_config => 1,
                clusters           => [ $Clusters[0] ],
                string             => $sequence_strings[0],
                all_read_data      => $all_read_data,
            }
        );
    }
    elsif ( $type eq 'LTM_WITH_CONTEXT' ) {
        $Sequences_to_Chart[0] = Perf::Figure::SequenceToChart->new(
            {
                string             => $sequence_strings[0],
                label              => 'Target Sequence',
                clusters           => \@Clusters,
                all_read_data      => $all_read_data,
                is_ltm_self_config => 0,
            }
        );
    }
    else {
        my $counter = 0;
        for my $sequence_string (@sequence_strings) {
            my $label = $display_sequences[$counter]->get_label();
            push @Sequences_to_Chart,
              Perf::Figure::SequenceToChart->new(
                {
                    string             => $sequence_string,
                    label              => $label,
                    clusters           => \@Clusters,
                    all_read_data      => $all_read_data,
                    is_ltm_self_config => 0,
                }
              );
            $counter++;
        }
    }

    return $package->new(
        {
            title                    => $title,
            figure_type              => $type,
            cluster_count            => $Cluster_Count,
            sequences_to_draw_count  => scalar(@display_sequences),
            sequences_to_chart_count => scalar(@Sequences_to_Chart),
            clusters                 => \@Clusters,
            sequences_to_draw        => \@display_sequences,
            sequences_to_chart       => \@Sequences_to_Chart,
            draw_chart               => $draw_charts,
            draw_seq                 => $draw_seq,
            has_human_data           => $has_human_data,
            split_chart              => $split_chart,
            custom_legend            => $custom_legend

        }
    );
}

sub _ForceToBeARef {
    my ($arg) = @_;
    return ( ref($arg) eq 'ARRAY' ) ? $arg : [$arg];
}

1;
