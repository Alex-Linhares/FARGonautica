package Perf::AllCollectedData;
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

our $CODELETS_PER_SECOND = 300;

# Each value if an ArrayRef, consisting of Seqsee::ResultOfTestRuns objects.
my %Seqsee_Data_of : ATTR(:name<seqsee_data>);
my %Human_Data_of : ATTR(:name<human_data>);
my %LTM_Data_of : ATTR(:name<ltm_data>);

sub BUILD {
    my ( $self, $id, $opts_ref ) = @_;
    $Seqsee_Data_of{$id} = _ReadDataFromDirectory('Perf/data/Seqsee');
    $Human_Data_of{$id}  = _ReadDataFromDirectory('Perf/data/Human');
    $LTM_Data_of{$id}    = _ReadDataFromDirectory('Perf/data/LTM');
}

sub _ReadDataFromDirectory {
    my ($directory) = @_;
    my @ret;

    $| = 1;
    for my $filename (<$directory/*>) {
        print '.';
        my $text       = read_file($filename);
        my $result_set = Storable::thaw($text);

        $result_set->set_version(
            Perf::Version->new( { string => $result_set->get_version } ) );
        $result_set->set_features(
            Perf::FeatureSet->new( { string => $result_set->get_features } ) );

        my $sequence = $result_set->get_terms;
        $sequence = Perf::TestSequence->new( { string => $sequence } );
        $result_set->set_terms($sequence);

        if ( $result_set->get_is_ltm_result() ) {
            if ( my $context_sequence = $result_set->get_context() ) {
                $result_set->set_context(
                    Perf::TestSequence->new( { string => $context_sequence } )
                );
            }
        }

        push @ret, $result_set;
    }

    return \@ret;
}

sub _GetDataForSequence {
    my ( $self, $opts_ref ) = @_;
    my %opts = %$opts_ref;
    my ( $source, $sequence, $context, $min_version, $max_version,
        $exact_feature_set )
      = @opts{
        qw{source sequence context min_version max_version exact_feature_set }};
    my $id = ident $self;

    $sequence->isa("Perf::TestSequence")
      or confess "Expected \$sequence to be of type Perf::TestSequence."
      . "Instead, it is of type "
      . ref($sequence);

    not( defined $context )
      or $context->isa("Perf::TestSequence")
      or confess
      "Expected \$context to be undefined or of type Perf::TestSequence."
      . "Instead, it is of type "
      . ref($context);

    not( defined $min_version )
      or $min_version->isa("Perf::Version")
      or confess
      "Expected $min_version to be undefined or of type Perf::Version."
      . "Instead, it is of type "
      . ref($min_version);

    not( defined $max_version )
      or $max_version->isa("Perf::Version")
      or confess
      "Expected $max_version to be undefined or of type Perf::Version."
      . "Instead, it is of type "
      . ref($max_version);

    not( defined $exact_feature_set )
      or $exact_feature_set->isa("Perf::FeatureSet")
      or confess
      "Expected $exact_feature_set to be undefined or of type Perf::FeatureSet."
      . "Instead, it is of type "
      . ref($exact_feature_set);

    my $array_ref;
    if    ( $source eq 'Seqsee' ) { $array_ref = $Seqsee_Data_of{$id} }
    elsif ( $source eq 'Human' )  { $array_ref = $Human_Data_of{$id} }
    elsif ( $source eq 'LTM' )    { $array_ref = $LTM_Data_of{$id} }
    else                          { confess "source $source unknown" }

    # say "Getting data for: " , $sequence->_DUMP();

    my @ret;
    if ( $source eq 'LTM' ) {
        for my $result_set ( @{$array_ref} ) {
            next
              unless $sequence->IsCompatibleWith( $result_set->get_terms );
            if ( defined $context ) {
                next
                  unless $context->IsCompatibleWith( $result_set->get_context );
            }
            ## ResultSet: $result_set->_DUMP()
            push @ret, $result_set;
        }
    }
    else {
        for my $result_set (@$array_ref) {
            next
              unless $sequence->IsCompatibleWith( $result_set->get_terms );
            push @ret, $result_set;
        }
    }

    if ( $source eq 'Human' ) {
        #  Constraints make no sense for human data.
        return @ret;
    }

    if ( defined $min_version ) {
        @ret = grep { $min_version <= $_->get_version } @ret;
    }

    if ( defined $max_version ) {
        @ret = grep { $max_version >= $_->get_version } @ret;
    }

    if ( defined $exact_feature_set ) {
        @ret = grep { $exact_feature_set eq $_->get_features } @ret;
    }

    return @ret;
}

sub GetDataForSequenceAndCluster {
    my ( $self, $opts_ref ) = @_;
    my $id       = ident $self;
    my $sequence = $opts_ref->{sequence}
      // confess "Missing required argument 'sequence'";
    my $cluster = $opts_ref->{cluster}
      // confess "Missing required argument 'cluster'";

    $sequence->isa("Perf::TestSequence")
      or confess "Expected \$sequence to be of type Perf::TestSequence."
      . "Instead, it is of type "
      . ref($sequence);

    $cluster->isa("Perf::Figure::Cluster")
      or confess "Expected \$cluster to be of type Perf::Figure::Cluster."
      . "Instead, it is of type "
      . ref($cluster);

    my $source = $cluster->get_source();
    return $self->_GetDataForSequence(
        {
            source   => $source,
            sequence => $sequence,
            $cluster->get_constraints()
        }
    );
}

1;
