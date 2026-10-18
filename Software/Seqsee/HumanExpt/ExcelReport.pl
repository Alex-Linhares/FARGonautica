use strict;
use Config::Std;
use Carp;
use List::Util qw(sum);
use Smart::Comments;

use OLE;
use Win32::OLE::Const 'Microsoft.Excel';     # wd  constants
use Win32::OLE::Const 'Microsoft Office';    # mso constants

use constant ANSWERS_FILE      => 'InputList3.answers';
use constant RESULTS_DIRECTORY => 'RealData';
use constant OUTPUT_FILE       => 'Results.xsl';

read_config ANSWERS_FILE() => my %ExperimentConfig;

use constant {
    COLUMN_FOR_SET                => 'A',
    COLUMN_FOR_SEQUENCE           => 'B',
    COLUMN_FOR_COUNT              => 'C',
    COLUMN_FOR_PERCENT_CORRECT    => 'D',
    COLUMN_FOR_UNDERSTANDING_TIME => 'E',
};
my $used_up_columns = 5;

my @Results = ReadResults(RESULTS_DIRECTORY);

my $excel = CreateObject OLE 'Excel.Application' or die $!;
$excel->{Visible} = 1;
my $workbook = $excel->Workbooks->Add;
my $sheet    = $workbook->ActiveSheet();

DisplayResults();

sub DisplayResults {

    my $row_counter = 5;

    # Fix column widths!
    my $rightmost_subject_column_integer = $used_up_columns + scalar(@Results);
    my $leftmost_subject_column = integer_to_column($used_up_columns + 1);
    my $rightmost_subject_column         = integer_to_column($rightmost_subject_column_integer);
    ### rightmost_subject_column_integer: $rightmost_subject_column_integer
    ### rightmost_subject_column: $rightmost_subject_column
    my $range = $sheet->Range( make_range( 'A', 1, $rightmost_subject_column, $row_counter ) );
    $range->Columns(1)->{ColumnWidth} = 10;
    $range->Columns(2)->{ColumnWidth} = 35;
    $range->Columns(3)->{ColumnWidth} = 10;
    $range->Columns(4)->{ColumnWidth} = 10;
    $range->Columns(5)->{ColumnWidth} = 20;

    {    # Insert headers:
        enter_in_cell( COLUMN_FOR_SET(),                $row_counter, "SET" );
        enter_in_cell( COLUMN_FOR_SEQUENCE(),           $row_counter, "SEQUENCE" );
        enter_in_cell( COLUMN_FOR_COUNT(),              $row_counter, "# times seen" );
        enter_in_cell( COLUMN_FOR_UNDERSTANDING_TIME(), $row_counter, "TIME TO UNDERSTAND" );
        enter_in_cell( COLUMN_FOR_PERCENT_CORRECT(),    $row_counter, '% WRONG' );
        $row_counter++;
    }

    for my $i ( $used_up_columns + 1 .. $used_up_columns + scalar(@Results) ) {
        $range->Columns($i)->{ColumnWidth} = 4;
    }

    my @sets = keys %ExperimentConfig;
    ## sets: @sets
    my @sorted_sets = sort keys %ExperimentConfig;
    ## sorted sets: @sorted_sets
    my @PerSubjectTimeWhenCorrect;
    my @PerSubjectTime;
    my @PerSubjectNumberCorrect;
    my @PerSubjectNumberSeen;
    my @PerSubjectNumberSeenThatHaveAnswers;

    for my $set (@sorted_sets) {
        my $values = $ExperimentConfig{$set};
        my $is_extend;
        next if $set eq '';

        my $type = $values->{Type};
        next unless $set =~ /extend/i;
        $set =~ s#^\s*extend\s*##i;

        my $expected_difficulty = $values->{ExpectedDifficulty} || '';
        $expected_difficulty =~ s#\s+##g;
        $expected_difficulty = [ map { [ split( /,/, $_ ) ] } split( /;/, $expected_difficulty ) ];

        enter_in_cell( COLUMN_FOR_SET, $row_counter, $set );
        $row_counter++;

        my @sequences_in_set = @{ $values->{seq} } or die "No sequences for set $set!";
        my $first_row_in_set = $row_counter;
        for my $sequence_and_ext (@sequences_in_set) {
            $sequence_and_ext =~ m/^\s* (.*?) \s* \| \s* (.*?) \s* $/x
                or confess "Extension missing for $sequence_and_ext";
            my ( $sequence, $ext ) = ( $1, $2 );
            my $section_in_results_file = "extend $sequence";
            my @ext = split( /\s+/, $ext );
            enter_in_cell( COLUMN_FOR_SEQUENCE, $row_counter, $sequence );
            my $column_counter           = $used_up_columns;
            my $subject_number           = 0;
            my $count_of_correct_times   = 0;
            my $sum_of_correct_times     = 0;
            my $count_of_encountered     = 0;
            my $sequence_extension_known = (@ext) ? 1 : 0;

            for my $result_for_a_file (@Results) {
                $column_counter++;
                next unless exists $result_for_a_file->{$section_in_results_file};

                $count_of_encountered++;
                $PerSubjectNumberSeenThatHaveAnswers[$subject_number]++ if @ext;
                $PerSubjectNumberSeen[$subject_number]++;

                my $section_data = $result_for_a_file->{$section_in_results_file};
                my $is_correct = IsExtensionCorrect( $section_data->{extension}, \@ext );
                my $formatting =
                      ( $is_correct == -1 ) ? {comment => $section_data->{extension}}
                    : ( $is_correct == 1 ) ? { background => 4, comment => $section_data->{extension} }
                    :                        { background => 3, comment => $section_data->{extension} };
                if ( $is_correct == 1 ) {
                    $count_of_correct_times++;
                    $PerSubjectNumberCorrect[$subject_number]++;
                    $sum_of_correct_times                       += $section_data->{understanding};
                    $PerSubjectTimeWhenCorrect[$subject_number] += $section_data->{understanding};
                }
                $PerSubjectTime[$subject_number] += $section_data->{understanding};

                enter_in_cell(
                    integer_to_column($column_counter), $row_counter,
                    sprintf( "%3.1f", $section_data->{understanding} ), $formatting,
                );
                $subject_number++;
            }

            enter_in_cell( COLUMN_FOR_COUNT(), $row_counter, $count_of_encountered );
            if ( $sequence_extension_known and $count_of_encountered ) {
                my $fraction_correct = $count_of_correct_times / $count_of_encountered;
                my $time_when_correct =
                    $count_of_correct_times
                    ? ( $sum_of_correct_times / $count_of_correct_times )
                    : '';
                enter_in_cell( COLUMN_FOR_UNDERSTANDING_TIME(),
                    $row_counter, sprintf( "%5.2f", $time_when_correct ) );
                enter_in_cell( COLUMN_FOR_PERCENT_CORRECT(),
                    $row_counter, sprintf( "%5.2f", 100 - $fraction_correct * 100 ) );
            }
            $row_counter++;
        }
        my $last_row_in_set = $row_counter - 1;
        {
            my $range = $sheet->Range(
                make_range( COLUMN_FOR_UNDERSTANDING_TIME, $first_row_in_set,
                    COLUMN_FOR_UNDERSTANDING_TIME, $last_row_in_set
                )
            ) or next;

            $range->FormatConditions()->AddDataBar();

            #$range->BorderAround(width => 4, ColorIndex => 4);
        }
        {
            my $range = $sheet->Range(
                make_range( COLUMN_FOR_PERCENT_CORRECT, $first_row_in_set,
                    COLUMN_FOR_PERCENT_CORRECT, $last_row_in_set
                )
            ) or next;

            $range->FormatConditions()->AddDataBar();

            #$range->BorderAround(width => 4, ColorIndex => 4);
        }
    }

    # Now enter subject Data:
    for my $subject_number ( 0 .. $#Results ) {
        my $avg_time_when_correct = $PerSubjectTimeWhenCorrect[$subject_number]
            / $PerSubjectNumberCorrect[$subject_number];
        my $avg_time = $PerSubjectTime[$subject_number]/$PerSubjectNumberSeen[$subject_number];
        my $fraction_correct = $PerSubjectNumberCorrect[$subject_number]
            / $PerSubjectNumberSeenThatHaveAnswers[$subject_number];
        enter_in_cell(
            integer_to_column( $subject_number + $used_up_columns + 1 ),
            2, sprintf( "%5.3f", $avg_time_when_correct ),
        );
        enter_in_cell(
            integer_to_column( $subject_number + $used_up_columns + 1 ),
            1, sprintf( "%5.3f", $avg_time ),
        );
        enter_in_cell(
            integer_to_column( $subject_number + $used_up_columns + 1 ),
            3, sprintf( "%5.2f", 100 * $fraction_correct ),
        );
    }
    enter_in_cell(integer_to_column($used_up_columns), 2,
                  "Time when corr. ==> "
                      );
    enter_in_cell(integer_to_column($used_up_columns), 1,
                  "Time ==> ",
                      );
    enter_in_cell(integer_to_column($used_up_columns), 3,
                  "Percent Correct ==>",
                      );

    {
        my $range = $sheet->Range(
            make_range( $leftmost_subject_column, 1,
                $rightmost_subject_column, 1
            )
        ) or next;

        $range->FormatConditions()->AddDataBar();
    }

    {
        my $range = $sheet->Range(
            make_range( $leftmost_subject_column, 2,
                $rightmost_subject_column, 2
            )
        ) or next;

        $range->FormatConditions()->AddDataBar();
    }

}

sub make_range {
    if ( @_ == 2 ) {
        return "$_[0]$_[1]";
    }
    else {
        return "$_[0]$_[1]:$_[2]$_[3]";
    }
}

sub enter_in_cell {
    my ( $col, $row, $value, $options_ref ) = @_;
    my $range = $sheet->Range( make_range( $col, $row ) );
    $range->{Value} = $value;

    # print "In ($col, $row) entered $value\n";
    if ($options_ref) {
        if ( exists $options_ref->{background} ) {

            #$range->{Interior}{Pattern}      = 1;                            #Solid
            #$range->{Interior}{PatternColor} = $options_ref->{background};
            $range->{Interior}{ColorIndex} = $options_ref->{background};
        }

        if (exists $options_ref->{comment}) {
            $range->AddComment($options_ref->{comment});
        }
    }
}

sub integer_to_column {
    my ($int) = @_;
    $int ||= 26;
    return ( 'A' .. 'Z' )[ $int - 1 ] if $int <= 26;
    return integer_to_column( int( ($int - 1) / 26 ) ) . integer_to_column( $int % 26 );
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

# Widths: 10, 40, 4, 10, 10, and 4s

#  TEST: {
#      my $data_range = 'A3:C6';
#

#      my $data = [[13, 15, 17],
#                  [19, undef, 23],
#                  [25..27],
#                  [28..30],
#                      ];

#      my $range = $sheet->Range($data_range);
#      $range->{Value} = $data;
#      my $databar_format = $range->FormatConditions()->AddDataBar();
#      $range->Columns(1)->{ColumnWidth} = 30;
#      $range->Columns(3)->{ColumnWidth} = 3;
#      #$databar_format->MinPoint()->Modify(newtype=>0, newvalue=> 0); # xlConditionValueNumber is 0
#      $databar_format->{MaxPoint}->Modify(newtype=>0, newvalue=> 500); # xlConditionValueNumber is 0
#  }

sub ReadResults {
    my ($directory) = @_;
    my @results;
    for my $file (<$directory/*>) {
        read_config $file => my %data_for_file;
        my %results_for_file;
        while ( my ( $section, $content ) = each %data_for_file ) {
            next if ( not( $section =~ /^ extend \s* [\d\s-]+ $/x ) );
            my $extension = join( ', ', @{ $content->{next_terms_entered} } );
            $results_for_file{$section} = {
                extension     => $extension,
                understanding => $content->{time_to_understand},
                typing_first  => $content->{typing_times}[0],
            };
        }
        push @results, \%results_for_file;
    }
    return @results;
}

