use strict;
use Config::Std;
use Carp;
use List::Util qw(sum min max);
use Smart::Comments;

use Experiment::Encounter;
use Experiment::Participant;
use Experiment::Sequence;
use Experiment::SequenceSet;
use Win32::Excel::Util;

use OLE;
use Win32::OLE::Const 'Microsoft.Excel';     # wd  constants
use Win32::OLE::Const 'Microsoft Office';    # mso constants

use Statistics::Descriptive;

use constant ANSWERS_FILE      => 'InputList3.answers';
use constant RESULTS_DIRECTORY => 'RealData';
use constant OUTPUT_FILE       => 'Results2.xsl';

use constant FirstSequenceOffset        => 15;
use constant RowsPerSequence            => 6;
use constant RowOffsetForPercentCorrect => 1;
use constant ColumnForPercentCorrect    => 4;
use constant ColumnForAvgTime           => 8;
use constant RowOffsetForAvgTime        => 1;
use constant TableStartRow              => 13;
use constant TableStartCol              => 2;
$| = 1;

read_config ANSWERS_FILE() => my %ExperimentConfig;
my @SequenceSets = SetupSequences( \%ExperimentConfig );

# Format of @Participants
# Each entry is for a single Experiment::Participant.
my @Participants = ReadResults(RESULTS_DIRECTORY);
print "STATISTICS:\n";
for my $stat (qw{AverageTimeToUnderstand AverageTimeToUnderstandWhenCorrect PercentCorrect}) {
    my @set = map { $_->$stat() } @Participants;
    my ( $min, $max, $sum ) = ( min(@set), max(@set), sum(@set) );
    my $avg = $sum / scalar(@set);
    print "$stat:\n\t$min/$avg/$max\n";
    print "\t", join( ", ", ( sort { $a <=> $b } @set )[ 0 .. 10 ] ),   "\n";
    print "\t", join( ", ", ( sort { $a <=> $b } @set )[ -10 .. -1 ] ), "\n";
}

for my $participant (@Participants) {
    if ( $participant->AverageTimeToUnderstand() < 4 or $participant->PercentCorrect() < 40 ) {
        print "MArked participant as outlier\n";
        $participant->MarkAllEncountersAsOutliers();
    }
}

for my $ss (@SequenceSets) {
    print "$ss\n";
    for my $seq ( @{ $ss->get_sequences() } ) {
        print "\t", $seq->as_text(), "\n";
        print "\t\t%Correct: ", $seq->PercentCorrectForInlierParticipants(), "\n";
        my $stats = Statistics::Descriptive::Full->new();
        $stats->add_data( $seq->UnderstandingTimeWhenCorrectForInlierParticipants() );
        print "\t\tUnderstanding Times when correct: ",
            join( ', ', $seq->UnderstandingTimeWhenCorrectForInlierParticipants ), "\n";
        for (qw{mean variance standard_deviation}) {
            print "\t\t$_:\t", $stats->$_(), "\n";
        }
        my $mean               = $stats->mean();
        my $standard_deviation = $stats->standard_deviation();
        $seq->SetAsOutliers(
            'get_time_to_understand',
            $mean - 2 * $standard_deviation,
            $mean + 2 * $standard_deviation
        );
    }
}

## ACTUAL EXCEL STUFF
my $EXCEL = CreateObject OLE 'Excel.Application' or die $!;
$EXCEL->{Visible} = 1;
my $WORKBOOK = $EXCEL->Workbooks->Add;
for my $ss (@SequenceSets) {
    CreateSheetForSequenceSet( $ss, $WORKBOOK );
}

sub CreateSheetForSequenceSet {
    my ( $ss, $WORKBOOK ) = @_;
    $WORKBOOK->Sheets->Add;
    my $SHEET = $WORKBOOK->ActiveSheet;

    my @sequences      = @{ $ss->get_sequences() };
    my $sequence_count = scalar(@sequences);
    my $row            = FirstSequenceOffset + 2 * $sequence_count;
    my @data_ranges;
    for my $seq (@sequences) {
        my $data_range = InsertSequenceData( $SHEET, $seq, $row );
        push @data_ranges, $data_range;
        $row += RowsPerSequence;
    }

    CreateTTestTable( $SHEET, $sequence_count, \@data_ranges, \@sequences );
    CreateChart( $SHEET, $sequence_count );
    ListSequences( $SHEET, @sequences );
}

sub ListSequences {
    my ( $SHEET, @sequences ) = @_;
    my $count = 0;
    my $row   = 13;
    for my $seq (@sequences) {
        $count++;
        $row++;
        enter_value_in_cell( $SHEET, 2, $row, "Sequence $count" );
        enter_value_in_cell( $SHEET, 4, $row, $seq->as_text );
        my $Font = $SHEET->Range( cell_to_range_string( 4, $row ) )->{Font};
        $Font->{Size}       = 15;
        $Font->{Bold}       = 1;
        $Font->{ColorIndex} = 3;
    }
}

sub CreateChart {
    my ( $SHEET, $sequence_count ) = @_;
    my $chart_object = $SHEET->ChartObjects->Add( 50, 20, 250, 150 );
    my $chart = $chart_object->{Chart};
    $chart->SetSourceData(
        $SHEET->Range(
            cell_to_range_string( TableStartCol + $sequence_count + 3,
                TableStartRow + $sequence_count + 2 )
                . ':'
                . cell_to_range_string(
                TableStartCol + $sequence_count + 3,
                TableStartRow + 2 * $sequence_count + 1
                )
        )
    );
    $chart->SeriesCollection()->Item(1)->{Name} = "Avg Time To Understand";

    my $chart_object2 = $SHEET->ChartObjects->Add( 350, 20, 250, 150 );
    my $chart2 = $chart_object2->{Chart};
    $chart2->SetSourceData(
        $SHEET->Range(
            cell_to_range_string( TableStartCol + $sequence_count + 2,
                TableStartRow + $sequence_count + 2 )
                . ':'
                . cell_to_range_string(
                TableStartCol + $sequence_count + 2,
                TableStartRow + 2 * $sequence_count + 1
                )
        )
    );
    $chart2->SeriesCollection()->Item(1)->{Name} = "% Correct";
}

sub CreateTTestTable {
    my ( $SHEET, $sequence_count, $data_ranges, $sequences ) = @_;
    my @data_ranges     = @$data_ranges;
    my $table_start_row = TableStartRow + $sequence_count;
    my $table_start_col = TableStartCol;

    for ( 1 .. $sequence_count ) {
        enter_value_in_cell( $SHEET, $table_start_col, $table_start_row + $_, "Seq $_" );

        #enter_value_in_cell( $SHEET, $table_start_col + $_, $table_start_row, "Seq $_" );

        # Copy % correct and average time
        # % Correct
        enter_formula_in_cell(
            $SHEET,
            $table_start_col + 2 + $sequence_count,
            $table_start_row + $_,
            '='
                . cell_to_range_string(
                ColumnForPercentCorrect,
                FirstSequenceOffset + 2 * $sequence_count + ( $_ - 1 ) * RowsPerSequence
                    + RowOffsetForPercentCorrect,
                ),
        );

        # Avg Time
        enter_formula_in_cell(
            $SHEET,
            $table_start_col + 3 + $sequence_count,
            $table_start_row + $_,
            '='
                . cell_to_range_string(
                ColumnForAvgTime,
                FirstSequenceOffset + 2 * $sequence_count + ( $_ - 1 ) * RowsPerSequence
                    + RowOffsetForAvgTime,
                ),
        );

        enter_value_in_cell(
            $SHEET,
            $table_start_col + 4 + $sequence_count,
            $table_start_row + $_,
            $sequences->[ $_ - 1 ]->as_text(),
        );
    }
    for my $i ( 1 .. $sequence_count ) {
        for my $j ( $i + 1 .. $sequence_count ) {
            my $dr1 = $data_ranges[ $i - 1 ] or next;
            my $dr2 = $data_ranges[ $j - 1 ] or next;
            enter_formula_in_cell(
                $SHEET,
                $table_start_col + $i,
                $table_start_row + $j,
                "=TTEST($dr1, $dr2, 2, 3)"
            );
            my $range = $SHEET->Range(
                cell_to_range_string( $table_start_col + $i, $table_start_row + $j ) );
            my $format_condition = $range->FormatConditions()->Add(
                1,    # Cell value
                8,    # <=
                0.1
            );
            $format_condition->{Font}{Bold} = 1;
            $format_condition->{Font}{ColorIndex} = 3;
            $format_condition->{Interior}{ColorIndex} = 4;
        }
    }
    my $data_range = cell_to_range_string( $table_start_col, $table_start_row + 1 ) . ':'
        . cell_to_range_string( $table_start_col + $sequence_count + 4,
        $table_start_row + $sequence_count );
    my $table = $SHEET->{ListObjects}->Add( 1, $SHEET->Range($data_range),, 2 );
    my $table_columns = $table->ListColumns();
    for ( 1 .. $sequence_count ) {
        $table_columns->Item( $_ + 1 )->{Name} = "Seq $_";
    }

    $table_columns->Item(1)->{Name}                     = "";
    $table_columns->Item( $sequence_count + 2 )->{Name} = "";
    $table_columns->Item( $sequence_count + 3 )->{Name} = "% Correct";
    $table_columns->Item( $sequence_count + 4 )->{Name} = "Avg Time";
    $table_columns->Item( $sequence_count + 5 )->{Name} = "Sequence";
}

sub InsertSequenceData {    #Returns range-string of Inlier Timing data
    my ( $SHEET, $seq, $row ) = @_;
    enter_value_in_cell( $SHEET, 1, $row, $seq->as_text );

    enter_value_in_cell( $SHEET, 2, $row + RowOffsetForPercentCorrect, "% Correct: " );
    enter_value_in_cell(
        $SHEET, ColumnForPercentCorrect,
        $row + RowOffsetForPercentCorrect,
        $seq->PercentCorrectForInliers
    );

    enter_value_in_cell( $SHEET, 2, $row + 2, "Time to understand when correct: " );
    enter_value_in_cell( $SHEET, 3, $row + 3, "Outliers: " );
    my $inlier_stat         = Statistics::Descriptive::Full->new;
    my $inlier_correct_stat = Statistics::Descriptive::Full->new;
    $seq->set_inlier_stat($inlier_stat);
    $seq->set_inlier_correct_stat($inlier_correct_stat);
    my $col         = 4;
    my $outlier_col = 4;
    for my $encounter ( $seq->GetEncountersForInlierParticipants ) {
        my $is_outlier = $encounter->get_is_outlier;
        my $time       = $encounter->get_time_to_understand;
        $inlier_stat->add_data($time);
        if ($is_outlier) {
            enter_value_in_cell( $SHEET, $outlier_col, $row + 3, $time );
            $outlier_col++;
        }
        else {
            enter_value_in_cell( $SHEET, $col, $row + 2, $time );
            $inlier_correct_stat->add_data($time);
            $col++;
        }
    }

    my @typing_time_profile = $seq->CreateTypingTimeProfile();
    my @valid_extensions = @{$seq->get_acceptable_extensions()};
    my @first_valid_extension = @valid_extensions? @{$valid_extensions[0]} : ();
    enter_value_in_cell($SHEET, 2, $row+4, "More terms");
    enter_value_in_cell($SHEET, 2, $row+5, "Typing time");
    for (0..9) {
        enter_value_in_cell( $SHEET, 4 + $_, $row + 4, $first_valid_extension[$_]);
        enter_value_in_cell( $SHEET, 4 + $_, $row + 5, $typing_time_profile[$_]);
    }
    $SHEET->Range(cell_to_range_string(2, $row).':'.cell_to_range_string(2,$row+5))->{Font}{Bold} = 1;
    if ( $col > 4 ) {
        enter_value_in_cell( $SHEET, 6, $row + RowOffsetForAvgTime, "Avg Time: " );
        enter_value_in_cell(
            $SHEET, ColumnForAvgTime,
            $row + RowOffsetForAvgTime,
            $seq->get_inlier_correct_stat()->mean()
        );

        return cell_to_range_string( 4, $row + 2 ) . ':'
            . cell_to_range_string( $col - 1, $row + 2 );
    }
    else {
        die "Fishy: No correct extension that isn't an outlier?" . $seq->as_text
            if $seq->HasAnyCorrectExtension;
        return '';
    }
}

sub ReadResults {
    my ($directory) = @_;
    my @results;
    for my $file (<$directory/*>) {    ## Reasing Results ===>              Done[%]
        read_config $file => my %data_for_file;

        #print $file, "\n\t";
        my $participant = Experiment::Participant->new();
        while ( my ( $section, $content ) = each %data_for_file ) {
            next if ( not( $section =~ /^ extend \s* ([\d\s-]+) $/x ) );
            my $presented_terms_string = $1;
            $presented_terms_string =~ s#\s*$##;
            my @presented_terms = split( /\s+/, $presented_terms_string );
            my $sequence = Experiment::Sequence->FetchGivenPresentedTerms( \@presented_terms );
            my @next_terms = grep {$_} @{ $content->{next_terms_entered} };

            my $encounter = new Experiment::Encounter(
                {   participant            => $participant,
                    sequence               => $sequence,
                    presented_terms_string => $presented_terms_string,
                    presented_terms        => [ split( /\s+/, $presented_terms_string ) ],
                    extension_by_user      => \@next_terms,
                    time_to_understand     => $content->{time_to_understand},
                    typing_times           => $content->{typing_times},
                }
            );
            $encounter->set_is_extension_correct( $encounter->IsExtensionCorrect() );
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
    while ( my ( $k, $v ) = each %$expt_config_ref ) {    ### Reading Config ==>  Done[%]
        next unless $k =~ /^\s*Extend/;
        my $type = $v->{Type};
        my @sequences = map { m/^\s*(\S.*?)\s*\|/ or die "No | in seq $_"; $1 } @{ $v->{seq} };
        my @sequence_objects;
        for my $seq (@sequences) {
            my $key = "Sequence $seq";
            die "Did not find key >>$key<<" unless exists( $expt_config_ref->{$key} );
            my $extensions = $expt_config_ref->{$key}{ValidExtension};
            if ( !$extensions ) {
                $extensions = [];
            }
            elsif ( !( ref $extensions ) ) {
                $extensions = [$extensions];
            }
            my @terms = split( /\s+/, $seq );
            my @valid_extensions = map { [ split( /\s+/, $_ ) ] } map { trim($_) } @{$extensions};
            push @sequence_objects,
                Experiment::Sequence->FetchOrCreate(
                {   presented_terms       => \@terms,
                    acceptable_extensions => \@valid_extensions,
                }
                );
        }
        push @Ret, Experiment::SequenceSet->new( { sequences => \@sequence_objects } );
    }
    return @Ret;
}

sub trim {
    $_[0] =~ s#^\s*##;
    $_[0] =~ s#\s*$##;
    $_[0];
}

