package Seqsee;
use strict;
use S;
use version; our $VERSION = version->new("0.91");

use English qw(-no_match_vars);
use List::Util qw(min max);
use Carp;
use Smart::Comments;
use Config::Std;
use Getopt::Long;
use Time::HiRes qw( sleep );
use Class::Multimethods;
use SUtil;
use Sanity;

multimethod 'SanityCheck';

sub run {
  my (@sequence) = @_;
  SWorkspace->clear();
  SWorkspace->init(@sequence);
  $Global::MainStream->clear();
  $Global::MainStream->init();
  SCoderack->clear();
  SCoderack->init();
  SLTM->init();

  _SeqseeMainLoop();

}

# method: do_background_activity
{
  my $TimeLastProgressCheckerLaunched = 0;

  sub do_background_activity {

    if ( $Global::Feature{CodeletTree} ) {
      print {$Global::CodeletTreeLogHandle} "Background\n";
    }

    SCoderack->add_codelet( SCodelet->new( "FocusOn", 50, {} ) )
    if SUtil::toss(0.3);

    my $time_since_last_addn =
    $Global::Steps_Finished - $Global::TimeOfNewStructure;
    my $time_since_last_checker =
    $Global::Steps_Finished - $TimeLastProgressCheckerLaunched;

    if ( $time_since_last_checker > 20
      and SUtil::toss( $time_since_last_addn / 150 ) )
    {
      $TimeLastProgressCheckerLaunched = $Global::Steps_Finished;
      SCodelet->new( "CheckProgress", 100, {} )->schedule();
    }

    if ( $Global::Steps_Finished % 10 == 0 ) {
      SLTM::DecayAll();
      SWorkspace::__UpdateObjectStrengths();
    }
  }
}

sub already_rejected_by_user {
  my ($aref) = @_;
  my @a      = @$aref;
  my $cnt    = scalar @a;
  for my $i ( 0 .. $cnt - 1 ) {
    my $substr = join( ", ", @a[ 0 .. $i ] );
    ## Chekin for user rejection: $substr
    return 1 if $Global::ExtensionRejectedByUser{$substr};
  }
  return 0;
}

sub Seqsee_Step {
  $Global::Steps_Finished++;
  SLTM->LogActivations()
  if ( $Global::Feature{LogActivations}
    and not( $Global::Steps_Finished % 10 ) );
  unless ( $Global::Steps_Finished % 100 ) {
    $Global::AcceptableTrustLevel -= 0.002;
    print '@', $Global::Steps_Finished, "\n"
    unless $Global::Steps_Finished % 1000;
  }
  sleep( $Global::InterstepSleep / 1000 );

  #main::message($Global::InterstepSleep);
  do_background_activity();

  # Global::ClearHilit();

  ## $Global::Steps_Finished
  my $runnable = SCoderack->get_next_runnable();
  return unless $runnable;    # prog not yet finished!

  if ( $runnable->isa("SCodelet") )
  {
    if ( $Global::Feature{CodeletTree} ) {
      print {$Global::CodeletTreeLogHandle} "Chose $runnable\n";
    }
    $Global::CurrentRunnableString = "Seqsee::SCF::" . $runnable->[0];
    $runnable->run();
  }
  else {
    SErr::Fatal->throw("Runnable object is $runnable: expected a SCodelet");
  }
  if ($Global::Sanity) {
    SanityCheck();
  }
  return;
}

# method: Interaction_step_n
# Takes upto n steps
#
#    Updates display after update_after
#
#    usage:
#       Interaction_step_n( $options_ref )
#
#    parameter list:
#        n - steps to take
#        update_after - update display every so many steps
#
#    return value:
#      bool, whether program has finished
#
#    possible exceptions:

sub Interaction_step_n {
  my $opts_ref = shift;
  ## In Interaction_step_n: $opts_ref

  my $steps_left_to_take = $opts_ref->{n} or confess "Need n";
  $steps_left_to_take =
  min( $steps_left_to_take, $opts_ref->{max_steps} - $Global::Steps_Finished );
  return 1 unless $steps_left_to_take > 0;    # i.e, okay to stop now!

  my $update_after = $opts_ref->{update_after} || $steps_left_to_take;

  my $change_after_last_display = 0;          #to prevent repeats at end
  my $program_finished          = 0;

  STEP_LOOP: for my $steps_executed ( 1 .. $steps_left_to_take ) {
    $Global::Break_Loop = 0;

    ## Interaction_step_n executing step number: $steps_executed
    $program_finished = Seqsee_Step();
    ## Interaction_step_n finished step: $steps_executed
    $change_after_last_display = 1;

    if ( not( $steps_executed % $update_after ) ) {
      main::update_display();
      $change_after_last_display = 0;
    }
    last if $program_finished;
    last if $Global::Break_Loop;
  }

  main::update_display() if $change_after_last_display;
  return $program_finished;
}

# var: %DEFAULTS
# Defaults for configuration
#
# used if not spec'd in config file or on the command line.
my %DEFAULTS = (
  seed => int( rand() * 32000 ),
  update_interval => 0,    # If default used, carps when interactive
);

sub _read_commandline {
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
    "seed=i",
    "seq=s",
    "update_interval=i",
    "max_steps=i", "n=i",    # same option!
    'f=s',
    'gui_config=s', 'gui=s',    # same option!
    'sanity!',
    'view=i',
  );
  $options{max_steps}  ||= $options{n}   if exists $options{n};
  $options{gui_config} ||= $options{gui} if exists $options{gui};

  $Global::debugMAX = 1 if exists $Global::Feature{debugMAX};

  return %options;
}

# method: _read_config_and_commandline
# reads in config/commandline/defaults
#
# Reads the configuration (conf/seqsee.conf), updates what it sees using the commandline arguments, sets defaults, and returns the whole thing in a HASH
#
#    return value:
#       The OptionsRef

sub _read_config {
  my %options    = @_;
  my $RETURN_ref = {};
  read_config 'config/seqsee.conf' => my %config;

  for (
    qw{seed max_steps
    update_interval

    UseScheduledThoughtProb ScheduledThoughtVanishProb
    DecayRate

    view

    gui_config
    }
  )
  {
    my $val =
     exists( $options{$_} )        ? $options{$_}
    :exists( $config{seqsee}{$_} ) ? $config{seqsee}{$_}
    :exists( $DEFAULTS{$_} )       ? $DEFAULTS{$_}
    :  confess "Option '$_' not set either on command line, conf file or defauls";
    $RETURN_ref->{$_} = $val;
  }

  $RETURN_ref->{seq} = $options{seq};    # or confess "Sequence not set!";

  # SANITY CHECKING: SEQ
  my $seq = $RETURN_ref->{seq};
  unless ( $seq =~ /^[\d\s,]*$/ ) {
    confess
    "The option --seq must be a space or comma separated list of integers; I got '$seq' instead";
  }
  for ($seq) { s/^\s*//; s/\s*$//; }
  my @seq = split( /[\s,]+/, $seq );
  $RETURN_ref->{seq} = [@seq];

  print "View: $RETURN_ref->{view}!\n";

  return $RETURN_ref;
}

1;
