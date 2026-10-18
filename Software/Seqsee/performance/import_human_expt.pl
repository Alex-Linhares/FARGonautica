use 5.10.0;
use strict;
use Statistics::Basic qw{:all};

use lib 'lib';
use Test::Seqsee;
use Global;
use List::Util qw{min max sum};
use Time::HiRes qw{time};
use Getopt::Long;
use Storable;
use File::Slurp;
use Smart::Comments;
use IO::Prompt;
use Storable;

use lib 'HumanExpt';
use Experiment::Encounter;
use Experiment::Participant;
use Experiment::Sequence;
use Experiment::SequenceSet;

use Statistics::Descriptive;

use constant ANSWERS_FILE      => 'HumanExpt/InputList3.answers';
use constant RESULTS_DIRECTORY => 'HumanExpt/RealData';

read_config ANSWERS_FILE() => my %ExperimentConfig;
my @SequenceSets = SetupSequences( \%ExperimentConfig );

# Format of @Participants
# Each entry is for a single Experiment::Participant.
my @Participants = ReadResults(RESULTS_DIRECTORY);

#print "STATISTICS:\n";
#for my $stat (qw{AverageTimeToUnderstand AverageTimeToUnderstandWhenCorrect PercentCorrect}) {
#    my @set = map { $_->$stat() } @Participants;
#    my ( $min, $max, $sum ) = ( min(@set), max(@set), sum(@set) );
#    my $avg = $sum / scalar(@set);
#    print "$stat:\n\t$min/$avg/$max\n";
#    print "\t", join( ", ", ( sort { $a <=> $b } @set )[ 0 .. 10 ] ),   "\n";
#    print "\t", join( ", ", ( sort { $a <=> $b } @set )[ -10 .. -1 ] ), "\n";
#}

for my $participant (@Participants) {
    if (   $participant->AverageTimeToUnderstand() < 4
        or $participant->PercentCorrect() < 40 )
    {
        print "MArked participant as outlier\n";
        $participant->MarkAllEncountersAsOutliers();
    }
}

for my $ss (@SequenceSets) {
    print "$ss\n";
    for my $seq ( @{ $ss->get_sequences() } ) {
        print "\t", $seq->as_text(), "\n";

#        print "\t\t%Correct: ", $seq->PercentCorrectForInlierParticipants(), "\n";
        my $stats = Statistics::Descriptive::Full->new();
        $stats->add_data(
            $seq->UnderstandingTimeWhenCorrectForInlierParticipants() );

#        print "\t\tUnderstanding Times when correct: ",
#            join( ', ', $seq->UnderstandingTimeWhenCorrectForInlierParticipants ), "\n";
#        for (qw{mean variance standard_deviation}) {
#            print "\t\t$_:\t", $stats->$_(), "\n";
#        }
        my $mean               = $stats->mean();
        my $standard_deviation = $stats->standard_deviation();
        $seq->SetAsOutliers(
            'get_time_to_understand',
            $mean - 2 * $standard_deviation,
            $mean + 2 * $standard_deviation
        );
    }
}

WriteOutAllSequences();

sub GetAllSequences {
    my %Sequences;
    for my $ss (@SequenceSets) {
        my @seq = @{ $ss->get_sequences() };
        @Sequences{@seq} = @seq;
    }
    return values %Sequences;
}

sub WriteOutAllSequences {
    my @sequences = GetAllSequences();
    my $counter   = 'a';
    for my $seq (@sequences) {
        my $filename = "performance/human_data/$counter";
        my $RTR      = SequenceToResultsOfTestRuns($seq) or next;
        open my $OUT, '>', "$filename";
        print {$OUT} Storable::freeze($RTR);
        close $OUT;
        $counter++;
    }
}

# We need to write one ResultsOfTestRuns object per file.

sub SequenceToResultsOfTestRuns {
    my ($ExptSeq) = @_;
    my @encounters = $ExptSeq->GetInlierEncounters();
    my @results;    # Each is a Seqsee::ResultOfTestRun, frozen.

    my $terms = join(' ', @{$ExptSeq->get_presented_terms()});
    my @possible_extensions = @{$ExptSeq->get_acceptable_extensions()};
    return unless @possible_extensions == 1;
    $terms .= '|'. join(' ', @{$possible_extensions[0]});

    @results = map { EncounterToRTR_Frozen($_) } @encounters;

    return ResultsOfTestRuns->new(
        {
            results  => \@results,
            terms    => $terms,
            version  => 'human',
            features => 'human',
            times    => [],
            rate     => [],
        }
    );
}

sub EncounterToRTR_Frozen {
    my ($encounter) = @_;

    # Status: Can be Success or NotEvenExtended
    my $status_string =
      $encounter->get_is_extension_correct() ? 'Successful' : 'NotEvenExtended';
    my $status = TestOutputStatus->new( { status_string => $status_string } );

    return Storable::freeze(Seqsee::ResultOfTestRun->new(
        {
            status => $status,
            steps  => $encounter->get_time_to_understand(),
            error  => ''
        }
    ));
}

sub ReadResults {
    my ($directory) = @_;
    my @results;
    for my $file (<$directory/*>) { ## Reasing Results ===>              Done[%]
        read_config $file => my %data_for_file;

        #print $file, "\n\t";
        my $participant = Experiment::Participant->new();
        while ( my ( $section, $content ) = each %data_for_file ) {
            next if ( not( $section =~ /^ extend \s* ([\d\s-]+) $/x ) );
            my $presented_terms_string = $1;
            $presented_terms_string =~ s#\s*$##;
            my @presented_terms = split( /\s+/, $presented_terms_string );
            my $sequence = Experiment::Sequence->FetchGivenPresentedTerms(
                \@presented_terms );
            my @next_terms = grep { $_ } @{ $content->{next_terms_entered} };

            my $encounter = new Experiment::Encounter(
                {
                    participant            => $participant,
                    sequence               => $sequence,
                    presented_terms_string => $presented_terms_string,
                    presented_terms =>
                      [ split( /\s+/, $presented_terms_string ) ],
                    extension_by_user  => \@next_terms,
                    time_to_understand => $content->{time_to_understand},
                    typing_times       => $content->{typing_times},
                }
            );
            $encounter->set_is_extension_correct(
                $encounter->IsExtensionCorrect() );
            $participant->add_extension_encounter($encounter);
            $sequence->add_encounter($encounter);
        }

        # print "\t", $participant->PercentCorrect(), "\n";
        push @results, $participant;
    }
    return @results;
}

sub SetupSequences {
    my ($expt_config_ref) = @_;
    my @Ret;
    while ( my ( $k, $v ) = each %$expt_config_ref )
    {    ### Reading Config ==>  Done[%]
        next unless $k =~ /^\s*Extend/;
        my $type = $v->{Type};
        my @sequences =
          map { m/^\s*(\S.*?)\s*\|/ or die "No | in seq $_"; $1 }
          @{ $v->{seq} };
        my @sequence_objects;
        for my $seq (@sequences) {
            my $key = "Sequence $seq";
            die "Did not find key >>$key<<"
              unless exists( $expt_config_ref->{$key} );
            my $extensions = $expt_config_ref->{$key}{ValidExtension};
            if ( !$extensions ) {
                $extensions = [];
            }
            elsif ( !( ref $extensions ) ) {
                $extensions = [$extensions];
            }
            my @terms = split( /\s+/, $seq );
            my @valid_extensions =
              map { [ split( /\s+/, $_ ) ] } map { trim($_) } @{$extensions};
            push @sequence_objects,
              Experiment::Sequence->FetchOrCreate(
                {
                    presented_terms       => \@terms,
                    acceptable_extensions => \@valid_extensions,
                }
              );
        }
        push @Ret,
          Experiment::SequenceSet->new( { sequences => \@sequence_objects } );
    }
    return @Ret;
}

sub trim {
    $_[0] =~ s#^\s*##;
    $_[0] =~ s#\s*$##;
    $_[0];
}

