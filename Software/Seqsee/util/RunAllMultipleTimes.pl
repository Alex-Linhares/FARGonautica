use threads;
use threads::shared;
use strict;
use Tk;
use lib 'lib';
use Test::Seqsee;
use Global;
use List::Util qw{min max sum};
use Time::HiRes qw{time};
use English qw{-no_match_vars};

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
  "steps=i",
  "f=s",
  "filename=s",
);

my $times : shared;
my $MaxSteps : shared;

($times) = @options{qw{times}};
my @selected_feature_set : shared;
@selected_feature_set = map { "-f=$_" } keys %Global::Feature;
my $TIMES_TO_RUN = ( $times ||= 10 );
$MaxSteps = $options{steps} || 10000;
my $SequencesRunSoFar : shared;
my $TotalSequences : shared;
$SequencesRunSoFar = 0;
$TotalSequences    = '?';

my $MW = new MainWindow();

my $ResultsSummaryText =
$MW->Scrolled( 'Text', -scrollbars => 'se', -width => 130 )
->pack( -side => 'top' );
my $ResultsText =
$MW->Scrolled( 'Text', -scrollbars => 'se', -width => 130, -height => 10 )
->pack( -side => 'top' );
my $Text =
$MW->Scrolled( 'Text', -scrollbars => 'se', -width => 130, -height => 10 )
->pack( -side => 'bottom' );

for ( $Text, $ResultsText, $ResultsSummaryText ) {
  $_->tagConfigure( 'success', -background => 'blue', -foreground => 'white' );
  $_->tagConfigure(
    'qualified_success',
    -background => '#7777FF',
    -foreground => 'white'
  );
  $_->tagConfigure( 'error', -background => 'red', -foreground => 'white' );
  $_->tagConfigure(
    'nosuccess',
    -background => 'yellow',
    -foreground => 'white'
  );
  $_->tagConfigure(
    'sequence',
    -foreground => 'red',
    -font       => '{Lucida Bright} -18 bold'
  );

  $_->insert( 'end', "Please wait...." );
  $_->update();
}
my %RESULTS : shared;
my %WALLCLOCK_TIME : shared;
my %EFFECTIVE_CODELET_RATE :
shared;    # not *actual*, as contaminated by startup time.

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
  my $sequence_list_filename = $options{filename}
  // 'config/sequence_list_for_multiple';
  open my $LIST, '<', $sequence_list_filename or die "Failed to open list";
  my @sequence_list = <$LIST>;
  close $LIST;
  @sequence_list = grep { $_ } map { s#^\s*##; s#\s*$##; $_ } @sequence_list;

  $TotalSequences = scalar(@sequence_list) * $TIMES_TO_RUN;
  for my $terms (@sequence_list) {
    my @features_for_this_seq;
    if ($terms =~ m/(.*?):(.*)/) {
      my $features = $1;
      $terms = $2;
      $terms =~ s#^\s*##;
      @features_for_this_seq = map {"--f=$_"} split(/,/, $features);
    }
    my ( $seq, $continuation ) = split( /\|/, $terms );
    $RESULTS{$terms}                = &share( [] );
    $WALLCLOCK_TIME{$terms}         = &share( [] );
    $EFFECTIVE_CODELET_RATE{$terms} = &share( [] );
    my @cmd;
    if ( $OSNAME eq 'MSWin32' ) {
      @cmd = ( $EXECUTABLE_NAME, 'util/RunTestOnce.pl' );
    }
    else {
      @cmd = ( 'perl', 'util/RunTestOnce.pl' );
    }

    push @cmd,
    (
      qq{--seq="$seq"},         qq{--continuation="$continuation"},
      qq{-max_steps=$MaxSteps}, qq{--min_extension=3},
      qq{--max_false=3},        qq{--tempfilename=foo},
      @features_for_this_seq,
    );

    # my $cmd = join(" ", @cmd);
    for ( 1 .. $TIMES_TO_RUN ) {
      print ">>@cmd<<\n";

      unlink 'foo';
      my $time_before = time();
      system @cmd;
      my $time_taken = time() - $time_before;
      push @{ $WALLCLOCK_TIME{$terms} }, $time_taken;

      open( my $RESULT, '<', "foo" ) or confess "Unable to open file >>foo<<";
      my $result_str = join( '', <$RESULT> );

      #my $result_object = Storable::thaw($result_str)
      #    or confess "Unable to thaw: >>$result_str<<!!";
      push @{ $RESULTS{$terms} }, $result_str;
      my $effective_codelet_rate =
      Storable::thaw($result_str)->get_steps() / $time_taken;
      push @{ $EFFECTIVE_CODELET_RATE{$terms} }, $effective_codelet_rate;
      print "RESULT ADDED\n";
      $SequencesRunSoFar++;
    }
  }

  # Save to file.
  open my $OUT, '>', 'results.dat';
  my %Results_untied = map { $_ => [ @{ $RESULTS{$_} } ] } keys %RESULTS;
  print $OUT Storable::freeze( \%Results_untied ), "\n";
  close $OUT;
}

my $ResultCountAtLastUpdate = 0;
my $Finished                = 0;

sub Update {
  my $elapsed_time = time() - $StartTime;
  $MW->configure( -title => 'Multiple Seqsee Runs. Time elapsed: '
    . sprintf( '%4d', $elapsed_time )
    . ' seconds. Sequences completed: '
    . $SequencesRunSoFar . '/'
    . $TotalSequences )
  unless $Finished;
  $Finished = 1 if $SequencesRunSoFar == $TotalSequences;
  if ( $SequencesRunSoFar > $ResultCountAtLastUpdate ) {
    $ResultCountAtLastUpdate = $SequencesRunSoFar;
    $Text->delete( '0.0', 'end' );
    $ResultsText->delete( '0.0', 'end' );
    $ResultsSummaryText->delete( '0.0', 'end' );
    for my $seq ( keys %RESULTS ) {
      $Text->insert( 'end', "$seq\n", 'sequence' );
      $ResultsText->insert( 'end', "$seq\n", 'sequence' );
      $ResultsSummaryText->insert( 'end', "$seq\n", 'sequence' );
      my @RESULTS2 = map { Storable::thaw($_) } @{ $RESULTS{$seq} };
      next unless @RESULTS2;
      my @WALLCLOCK_TIME         = @{ $WALLCLOCK_TIME{$seq} };
      my @EFFECTIVE_CODELET_RATE = @{ $EFFECTIVE_CODELET_RATE{$seq} };
      my @top_row = map { find_tag( $_->get_status() ) } @RESULTS2;
      $ResultsText->insert( 'end', @top_row, scalar(@RESULTS2), '',
        "/$TIMES_TO_RUN", '', "\n" );
      $ResultsSummaryText->insert( 'end', @top_row, scalar(@RESULTS2), '',
        "/$TIMES_TO_RUN", '', "\n" );

      $ResultsText->insert( 'end', "Times in seconds:    ",
        '', ( map { sprintf( '%6.2f', $_ ), '', ' ', '' } @WALLCLOCK_TIME ),
        "\n" );
      $ResultsText->insert(
        'end',
        "Codelets per second: ",
        '',
        ( map { sprintf( '%6.2f', $_ ), '', ' ', '' } @EFFECTIVE_CODELET_RATE ),
        "\n"
      );
      $ResultsText->insert( 'end', "Codelets run:        ",
        '',
        ( map { sprintf( '%6d', $_->get_steps() ), '', ' ', '' } @RESULTS2 ),
        "\n" );

      my @times_when_successful =
      map { $_->get_steps() } grep { $_->get_status()->IsSuccess } @RESULTS2;
      my $sucess_percent = sprintf( '%5.2f',
        100 * scalar(@times_when_successful) / scalar(@RESULTS2) );
      if ( $sucess_percent > 0 ) {
        $ResultsText->insert( 'end', "$sucess_percent% successful\n" );
        $ResultsText->insert(
          'end', "Steps needed when correct: ",
          '', join( ', ', sort { $a <=> $b } @times_when_successful ),
          '', "\n"
        );
        my ( $min, $max, $avg ) = (
          min(@times_when_successful),
          max(@times_when_successful),
          sprintf( '%5.3f',
            sum(@times_when_successful) / scalar(@times_when_successful) ),
        );
        $ResultsText->insert(
          'end', "\nMinimum steps: $min",   '', "\nMaximum steps: $max",
          '',    "\nAverage:     : $avg  ", '', "\n"
        );
        $ResultsSummaryText->insert( 'end', "[$min/$avg/$max]\n" );
      }

      for (@RESULTS2) {
        $Text->insert( 'end', "\n............\n", '',
          find_tag( $_->get_status() ),
          "\t", '', $_->get_steps(), '', "\n\n" );
        $Text->insert( 'end', $_->get_error() ) if $_->get_error();
      }
      $MW->update();
    }
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
