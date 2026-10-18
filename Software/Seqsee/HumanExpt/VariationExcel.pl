use strict;
use Smart::Comments;
use List::Util;
use Carp;
use OLE;
use Win32::OLE::Const 'Microsoft.Excel';     # wd  constants
use Win32::OLE::Const 'Microsoft Office';    # mso constants

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
my $excel = CreateObject OLE 'Excel.Application' or die $!;
$excel->{Visible} = 1;
my $workbook = $excel->Workbooks->Add;
my $sheet    = $workbook->ActiveSheet();

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

    {
        my $range = $sheet->Range('A1:B1');
        $range->Columns(1)->{ColumnWidth} = 35;
        $range->Columns(2)->{ColumnWidth} = 35;
    }

    my $row_counter = 2;

    while ( my ( $set, $values ) = each %ExperimentConfig ) {
        my $is_extend;
        next if $set eq '';

        my $type = $values->{Type};
        next unless $set =~ /variation/i;

        my $expected_difficulty = $values->{ExpectedDifficulty} || '';
        $expected_difficulty =~ s#\s+##g;
        $expected_difficulty = [map {[split(/,/, $_)]} split(/;/, $expected_difficulty)];

        # print "NEXT SET: [$type]\n";
        my @sequences_in_set = @{ $values->{seq} } or die "No sequences for set $set!";
        my @difficulty_seen_measure;
        for my $sequence_and_ext (@sequences_in_set) {
            $sequence_and_ext =~ m/^\s* (.*?) \s* \| \s* (.*?) \s* $/x
                or confess "Extension missing for $sequence_and_ext";
            my ( $sequence, $ext ) = ( $1, $2 );
            my @ext = split( /\s+/, $ext );
            #print "   SEQ: $sequence... ($ext)\n";
            enter_in_cell('A', $row_counter, $sequence);
            $row_counter++;
            if ( exists $results_ref->{"variation $sequence"} ) {
                my @total_times;
                my @correct_total_times;
                for my $extension (sort keys %{ $results_ref->{"variation $sequence"}}) {
                    my $times = $results_ref->{"variation $sequence"}{$extension};
                    my @times = @{ $times->{$display_what} };
                    enter_in_cell('B', $row_counter, $extension);
                    my $column_counter = 3;
                    for (@times) {
                        enter_in_cell(integer_to_column($column_counter),
                                      $row_counter,
                                      sprintf("%5.3f", $_)
                                          );
                        $column_counter++;
                    }
                    $row_counter++;
                    push @total_times, @times;
                }
            }
        }
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
    }
}

sub integer_to_column {
    my ($int) = @_;
    $int ||= 26;
    return ( 'A' .. 'Z' )[ $int - 1 ] if $int <= 26;
    return integer_to_column( int( $int - 1 / 26 ) ) . integer_to_column( $int % 26 );
}
