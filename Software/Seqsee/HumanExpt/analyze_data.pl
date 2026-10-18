use strict;
use Smart::Comments;
use List::Util;
use Carp;

use constant ANSWERS_FILE      => 'InputList3.answers';
use constant RESULTS_DIRECTORY => 'RealData';

use Config::Std;
read_config ANSWERS_FILE() => my %ExperimentConfig;

my %track_these_times
    = map { $_ => 1 } qw{time_to_understand time_to_understand_plus_first_entry};
my $display_what = $ARGV[0] || 'time_to_understand';

unless ( $track_these_times{$display_what} ) {
    die "Not tracking $display_what!\n\nOnly tracking: ", join( ', ', keys %track_these_times ),
        "\n";
}

my %Results;

# Format:
# Key: Seqeuence (say, 'Extend 1 2 3 4')
# Value: Hash: Key:    time_to_understand
#              Value:  Array of Understanding Times

%Results = ReadResults(RESULTS_DIRECTORY);
## Results: %Results

DisplayResults( \%Results );

sub ReadResults {
    my ($directory) = @_;
    my %results;
    for my $file (<$directory/*>) {
        read_config $file => my %results_for_file;
        while ( my ( $section, $content ) = each %results_for_file ) {
            next if ( not( $section =~ /^ variation \s* [\d\s-]+ $/x ) );
            my $extension = join( ', ', @{ $content->{next_terms_entered} } );
            for ( keys %track_these_times ) {
                $results{$section}{$extension}{$_} ||= [];
            }
            push(
                @{ $results{$section}{$extension}{time_to_understand} },
                $content->{time_to_understand}
            );
            push(
                @{ $results{$section}{$extension}{time_to_understand_plus_first_entry} },
                $content->{time_to_understand} + $content->{typing_times}[0]
            );
        }
    }
    return %results;
}

sub DisplayResults {
    my ($results_ref) = @_;
    while ( my ( $set, $values ) = each %ExperimentConfig ) {
        my $is_extend;
        next if $set eq '';

        my $type = $values->{Type};
        next if $set =~ /variation/i;

        my $expected_difficulty = $values->{ExpectedDifficulty} || '';
        $expected_difficulty =~ s#\s+##g;
        $expected_difficulty = [map {[split(/,/, $_)]} split(/;/, $expected_difficulty)];

        print "NEXT SET: [$type]\n";
        my @sequences_in_set = @{ $values->{seq} } or die "No sequences for set $set!";
        my @difficulty_seen_measure;
        for my $sequence_and_ext (@sequences_in_set) {
            $sequence_and_ext =~ m/^\s* (.*?) \s* \| \s* (.*?) \s* $/x
                or confess "Extension missing for $sequence_and_ext";
            my ( $sequence, $ext ) = ( $1, $2 );
            my @ext = split( /\s+/, $ext );
            print "   SEQ: $sequence... ($ext)\n";
            if ( exists $results_ref->{"variation $sequence"} ) {
                my @total_times;
                my @correct_total_times;
                while ( my ( $extension, $times ) = each %{ $results_ref->{"variation $sequence"} } ) {
                    my @times = @{ $times->{$display_what} };
                    my $is_correct = IsExtensionCorrect( $extension, \@ext );
                    my $prefix
                        = ( $is_correct == -1 ) ? '???' : ( $is_correct == 1 ) ? '***' : 'xxx';

                    print "\t $extension\n";
                    print "\t\t", join( ", ", map { sprintf( "%5.3f", $_ ) } @times ), "\n";
                    #print "\t\t AVERAGE: ", List::Util::sum(@times) / scalar(@times), "\n";
                    push @total_times, @times;
                    push( @correct_total_times, @times ) if $is_correct == 1;
                }
                #print "\t Average: ", List::Util::sum(@total_times) / scalar(@total_times), "\n";
                my $average_time_when_correct = 100000; # will be fixed, if number available.
                my $fraction_correct= 0;
                if (@correct_total_times) {
                    $average_time_when_correct = List::Util::sum(@correct_total_times) / scalar(@correct_total_times);
                    print "\t Avg. When Correct: ", $average_time_when_correct , "\n";
                        
                }
                if ( scalar(@ext) and scalar(@total_times) ) {
                    $fraction_correct = scalar(@correct_total_times) / scalar(@total_times);
                    print "\t % Correct Answers: ",
                        sprintf( "%5.3f",
                        100 * $fraction_correct),
                        "\n";
                }
                push(@difficulty_seen_measure, [$average_time_when_correct, $fraction_correct]);
            }
            else {
                print "\t\t--\n";
            }
        }
        if ($expected_difficulty) {
            DisplayDifficultyAnalysis($expected_difficulty, \@sequences_in_set, \@difficulty_seen_measure);
        }
    }
}

sub DisplayDifficultyAnalysis {
    my ( $expected_difficulty, $sequences_ref, $difficulty_ref ) = @_;
    my @groups = @$expected_difficulty;
    my $group_count = scalar(@groups);
    return if $group_count <= 1;
    my $all_well = 1;
    for my $easier_index (0..$group_count-2) {
        my $easier_group_sequences = $groups[$easier_index];
        for my $harder_index ($easier_index+1..$group_count-1) {
            my $harder_group_sequences = $groups[$harder_index];
            for my $easier (@$easier_group_sequences) {
                my $easier_difficulty = $difficulty_ref->[$easier-1];
                for my $harder (@$harder_group_sequences) {
                    my $harder_difficulty = $difficulty_ref->[$harder-1];
                    if ($easier_difficulty->[0] > $harder_difficulty->[0]) {
                        print "!!! UNEXPECTED: Easier sequence took longer\n";
                        print "\t\tExpected Easier: $easier_difficulty->[0]:", $sequences_ref->[$easier-1], "\n";
                        print "\t\tExpected Harder: $harder_difficulty->[0]:", $sequences_ref->[$harder-1], "\n";
                        $all_well = 0;
                    }
                    if ($easier_difficulty->[1] < $harder_difficulty->[1]) {
                        print "!!! UNEXPECTED: Easier sequence correct less often\n";
                        print "\t\tExpected Easier: $easier_difficulty->[1]:", $sequences_ref->[$easier-1], "\n";
                        print "\t\tExpected Harder: $harder_difficulty->[1]:", $sequences_ref->[$harder-1], "\n";
                        $all_well = 0;
                    }
                }
            }
        }
    }
    if ($all_well) {
        print "~~~ Relative difficulty as expected.\n\n\n";
    }

}


sub IsExtensionCorrect {
    my ( $string, $correct ) = @_;
    my @given_extension = grep { $_ =~ /\d/ } split( /, /, $string );
    my @correct_extension = @$correct;
    return -1 unless @correct_extension;
    for ( 0 .. List::Util::min( scalar(@given_extension), scalar(@correct_extension) ) - 1 ) {
        return 0 unless $given_extension[$_] == $correct_extension[$_];
    }
    return 1;
}

sub CompareDifficulty {
    my ( $for_seq1, $for_seq2 ) = @_;
     
}
