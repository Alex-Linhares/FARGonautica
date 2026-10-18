use threads;
use threads::shared;
use strict;
use Tk;
use lib 'lib';
use Test::Seqsee;
use Global;
use List::Util qw{min max sum};
use Time::HiRes qw{time};

use Getopt::Long;

my $StartTime = time();

my %options = (
    f => sub {
        my ( $ignored, $feature_name ) = @_;
        print "$feature_name will be turned on\n";
        unless ( $Global::PossibleFeatures{$feature_name} ) {
            print "No feature $feature_name. Typo?\n";
            exit;
        }
        $Global::Feature{$feature_name} = 1;
    }
);
GetOptions(
    \%options,
    "seq=s",
    "view=i",    # ignored
    "times=i",
    "f=s",
);

my $times : shared;
my $terms : shared;

( $times, $terms ) = @options{qw{times seq}};
my @selected_feature_set : shared;
@selected_feature_set = map {"-f=$_"} keys %Global::Feature;
my $TIMES_TO_RUN =$times ;

my $MW = new MainWindow();

my $Text = $MW->Scrolled( 'Text', -scrollbars => 'se', -width => 100 )->pack();
$Text->tagConfigure( 'success',           -background => 'blue',    -foreground => 'white' );
$Text->tagConfigure( 'qualified_success', -background => '#7777FF', -foreground => 'white' );
$Text->tagConfigure( 'error',             -background => 'red',     -foreground => 'white' );
$Text->tagConfigure( 'nosuccess',         -background => 'yellow',  -foreground => 'white' );

$Text->insert( 'end', "Please wait...." );
$Text->update();
my @RESULTS : shared;
my @WALLCLOCK_TIME : shared;
my @EFFECTIVE_CODELET_RATE : shared;    # not *actual*, as contaminated by startup time.

threads->create('StartRun');
$MW->repeat(
    1000,
    sub {
        Update();
    }
);
$MW->MainLoop();

use Storable;

sub StartRun {
    my ( $seq, $continuation ) = split( /\|/, $terms );

    my @cmd;
    if ($OSNAME eq 'MSWin32') {
        @cmd = ( $EXECUTABLE_NAME,   'util/RunTestOnce.pl' );
    } else {
        @cmd = ('perl', 'util/RunTestOnce.pl');
    }

    push @cmd, (
        qq{--seq="$seq"},     qq{--continuation="$continuation"},
        qq{-max_steps=10000}, qq{--min_extension=3},
        qq{--max_false=3},    qq{--tempfilename=foo},
                @selected_feature_set,
    );

    # my $cmd = join(" ", @cmd);
    for ( 1 .. $TIMES_TO_RUN ) {
        print ">>@cmd<<\n";

        unlink 'foo';

        my $time_before = time();
        system @cmd;
        my $time_taken = time() - $time_before;
        push @WALLCLOCK_TIME, $time_taken;

        open( my $RESULT, '<', "foo" ) or confess "Unable to open file >>foo<<: $! ";
        my $result_str = join( '', <$RESULT> );
        #my $result_object = Storable::thaw($result_str)
        #    or confess "Unable to thaw: >>$result_str<<!!";
        push @RESULTS, $result_str;
        my $effective_codelet_rate = Storable::thaw($result_str)->get_steps() / $time_taken;
        push @EFFECTIVE_CODELET_RATE, $effective_codelet_rate;
        print "RESULT ADDED\n";
    }
}

my $ResultCountAtLastUpdate = 0;
sub Update {
    my $elapsed_time = time() - $StartTime;
    $MW->configure(-title => 'Multiple Seqsee Runs. Time elapsed: '. sprintf('%4d', $elapsed_time) . ' seconds');
    if (@RESULTS) {
        return unless scalar(@RESULTS) > $ResultCountAtLastUpdate;
        $ResultCountAtLastUpdate = scalar(@RESULTS);
        my @RESULTS2 = map { Storable::thaw($_) } @RESULTS;
        $Text->delete( '0.0', 'end' );
        my @top_row = map { find_tag( $_->get_status() ) } @RESULTS2;
        $Text->insert( 'end', @top_row, scalar(@RESULTS), '', "/$TIMES_TO_RUN", '', "\n" );

        $Text->insert( 'end', "Times in seconds:    ",
            '', ( map { sprintf( '%6.2f', $_ ), '', ' ', '' } @WALLCLOCK_TIME ), "\n" );
        $Text->insert( 'end', "Codelets per second: ",
            '', ( map { sprintf( '%6.2f', $_ ), '', ' ', '' } @EFFECTIVE_CODELET_RATE ), "\n" );
        $Text->insert( 'end', "Codelets run:        ",
            '', ( map { sprintf( '%6d', $_->get_steps() ), '', ' ', '' } @RESULTS2 ), "\n" );

        my @times_when_successful
            = map { $_->get_steps() } grep { $_->get_status()->IsSuccess } @RESULTS2;
        my $sucess_percent
            = sprintf( '%5.2f', 100 * scalar(@times_when_successful) / scalar(@RESULTS) );
        if ( $sucess_percent > 0 ) {
            $Text->insert( 'end', "$sucess_percent% successful\n" );
            $Text->insert(
                'end', "Steps needed when correct: ",
                '', join( ', ', sort { $a <=> $b } @times_when_successful ),
                '', "\n"
            );
            $Text->insert(
                'end',
                "\nMinimum steps: " . min(@times_when_successful),
                '',
                "\nMaximum steps: " . max(@times_when_successful),
                '',
                "\nAverage:       "
                    . sprintf(
                    '%5.3f', sum(@times_when_successful) / scalar(@times_when_successful)
                    ),
                '', "\n"
            );
        }

        for (@RESULTS2) {
            $Text->insert( 'end', "\n............\n", '', find_tag( $_->get_status() ),
                "\t", '', $_->get_steps(), '', "\n\n" );
            $Text->insert( 'end', $_->get_error() ) if $_->get_error();
        }
        $MW->update();
    }
}

sub find_tag {
    my ($status) = @_;
    my $string = $status->get_status_string;

    # print $status;
    if ( $status->IsSuccess() ) {
        return ( " OK ", ["success"] );
    }
    elsif ( $status->IsACrash() ) {
        return ( " CRASH! ", ["error"] );
    }
    elsif ( $status->IsAtLeastAnExtension() ) {
        return ( " $string ", ["qualified_success"] );
    }
    else {
        return ( " X ", ["nosuccess"] );
    }
}

sub Display_Selected {
    my ($arr_ref) = @_;
    my @arr = @$arr_ref;

    my @string = map { ( " X ", find_tag($_) ) } @arr;
    $Text->insert( 'end', @string, " \n " );

    for my $line (@arr) {
        ## line: $line
        $Text->insert( 'end', " == == == \n \n " );
        $Text->insert( 'end', $line, '', " \n " );
    }
}

