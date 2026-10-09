# Oracle for item 048: a headless Seqsee.pl run (seqsee/__main__.py).
#
# Seqsee.pl needs Tk, so this script runs its steps without the display:
#   options = _read_config(_read_commandline()) from a local @ARGV,
#   srand(seed)          -- deliberate addition: Seqsee.pl never seeds (PERL-QUIRK),
#   INITIALIZE           -- SCoderack, MainStream and SWorkspace clear/init, SLTM init,
#   Interaction_continue -- called again after each Break_Loop (the user pressing "continue"),
#                           until it returns true (max_steps reached) or a solution is accepted.
# UI/Graphical.pm's main::ask_user_extension is copied; $SGUI::Commentary is a fake that
# answers every choice "Yes" and every boolean question either "yes" (no continuation given:
# "--answer yes") or, with a continuation, yes iff the asked terms are the next known terms.
# The asked terms are taken from SErr::ElementsBeyondKnownSought::Ask's next_elements or
# main::ask_user_extension's items (both wrapped to stash them in @pending), not the text. Asking past the known terms ends the run (status out_of_terms).
# An accepted solution (SolutionConfirmation::SetAcceptedSolution) sets Break_Loop, so the run
# ends after that step (status accepted).
#
# Each run is a separate Perl process (Seqsee.pl handles one run per process): the default
# mode spawns `cli.pl --one SEED SEQ MAX_STEPS [CONTINUATION]` per case and records its JSON.
use 5.10.0;
use strict;
use warnings;
use JSON::PP;

if ( @ARGV and $ARGV[0] eq '--one' ) {
  shift @ARGV;
  one_run(@ARGV);
  exit 0;
}

require Oracle;
Oracle->import;

# [seq, seeds, max_steps, continuation (undef: answer yes)]
my @CASES = (
  [ "1 1 2 1 2 3",         [ 1 .. 6 ],  600 ],
  [ "1 2 3 4 5",           [ 1 .. 6 ],  600 ],
  [ "5",                   [ 1 .. 3 ],  100 ],
  [ "1 1 2 1 2 3",         [ 1 .. 12 ], 1000, "1 2 3 4 1 2 3 4 5" ],
  [ "1 2 3 4 5",           [ 1 .. 12 ], 600,  "6 7 8 9 10" ],
  [ "2 4 6 8",             [ 1 .. 12 ], 600,  "10 12 14 16" ],
  [ "1 7 1 7 1 7",         [ 1 .. 12 ], 600,  "1 7 1 7" ],
  [ "1 2 2 3 3 3 4 4 4 4", [ 1 .. 8 ],  1000, "5 5 5 5 5" ],
);

for my $case (@CASES) {
  my ( $seq, $seeds, $steps, $cont ) = @$case;
  my $cont_arg = defined($cont) ? qq{ "$cont"} : '';
  for my $seed (@$seeds) {
    my $out = `python/oracle/run_perl.sh python/oracle/cli.pl --one $seed "$seq" $steps$cont_arg 2>/dev/null`;
    my ($line) = grep {/^\{/} split /\n/, $out;
    die "No result for $seed '$seq'" unless $line;
    record( %{ decode_json($line) } );
  }
}
emit();

our ( @asked, @responses, $accepted, @known, @pending );

sub one_run {
  my ( $seed, $seq, $max_steps, $cont ) = @_;
  require S;
  require Seqsee;
  S->import;

  @known = defined($cont) ? ( split( ' ', $seq ), split( ' ', $cont ) ) : ();

  no warnings 'redefine', 'once';
  *main::message               = sub { };
  *main::debug_message         = sub { };
  *main::update_display        = sub { };
  *main::ask_for_more_terms    = sub { };
  *main::default_error_handler = sub { die $_[0] };

  # UI/Graphical.pm
  *main::ask_user_extension = sub {
    my ( $arr_ref, $msg_suffix ) = @_;
    local @pending = @$arr_ref;
    return if Seqsee::already_rejected_by_user($arr_ref);
    my $cnt = scalar(@$arr_ref);
    my $msg = ( $cnt == 1 ) ? "Is the next term @$arr_ref?" : "Are the next terms: @$arr_ref?";
    my $ok =
      $Global::Feature{debug}
      ? $SGUI::Commentary->MessageRequiringBooleanResponse( $msg, '', $msg_suffix, ['debug'] )
      : $SGUI::Commentary->MessageRequiringBooleanResponse($msg);
    $Global::AtLeastOneUserVerification = 1 if $ok;
    return $ok;
  };

  $SGUI::Commentary = bless {}, 'FakeCommentary';

  my $orig_ask = \&SErr::ElementsBeyondKnownSought::Ask;
  *SErr::ElementsBeyondKnownSought::Ask = sub {
    local @pending = @{ $_[0]->next_elements() };
    $orig_ask->(@_);
  };

  my $orig_accept = \&SolutionConfirmation::SetAcceptedSolution;
  *SolutionConfirmation::SetAcceptedSolution = sub {
    $accepted           = 1;
    $Global::Break_Loop = 1;
    $orig_accept->(@_);
  };

  open my $NULL, '>', '/dev/null' or die;
  my $result = {
    seed         => $seed + 0,
    seq          => $seq,
    max_steps    => $max_steps + 0,
    continuation => $cont,
  };
  my $error;
  {
    my $old = select($NULL);
    eval {
      local @ARGV = ( '--seq', $seq, '--seed', $seed, '--max_steps', $max_steps );
      my $OPTIONS_ref = $Global::Options_ref = Seqsee::_read_config( Seqsee::_read_commandline() );
      srand( $OPTIONS_ref->{seed} );
      SCoderack->clear();
      SCoderack->init($OPTIONS_ref);
      $Global::MainStream->clear();
      $Global::MainStream->init($OPTIONS_ref);
      SWorkspace->clear();
      SWorkspace->init($OPTIONS_ref);
      SLTM->Load('memory_dump.dat') if $Global::Feature{LTM};
      SLTM->init();
      $Global::InterstepSleep = 0;
      until (
        Seqsee::Interaction_step_n(
          {
            n            => $OPTIONS_ref->{max_steps},
            update_after => $OPTIONS_ref->{update_interval},
            max_steps    => $OPTIONS_ref->{max_steps},
          }
        )
        or $accepted
        )
      {
      }
      1;
    } or $error = $@;
    select($old);
  }

  my @mags    = map { $_->get_mag() } SWorkspace->GetElements();
  my $initial = scalar( split ' ', $seq );
  $result->{elements}  = [ map { $_ + 0 } @mags ];
  $result->{extension} = [ map { $_ + 0 } @mags[ $initial .. $#mags ] ];
  $result->{asked}     = [@asked];
  $result->{responses} = [@responses];
  $result->{steps}     = $Global::Steps_Finished + 0;
  $result->{status} =
      ( defined($error) and "$error" eq "OUT_OF_TERMS\n" ) ? 'out_of_terms'
    : defined($error) ? 'error'
    : $accepted       ? 'accepted'
    :                   'max_steps';
  $result->{error} = $result->{status} eq 'error' ? ( split /\n/, "$error" )[0] : undef;
  say JSON::PP->new->canonical->encode($result);
}

package FakeCommentary;

sub MessageRequiringBooleanResponse {
  my $q = "$_[1]";
  push @main::asked, $q;
  return 1 unless @main::known;
  my @terms = @main::pending or die "No pending terms: $q";
  no warnings 'once';
  my $at = $SWorkspace::ElementCount;
  die "OUT_OF_TERMS\n" if $at + @terms > @main::known;
  for my $i ( 0 .. $#terms ) {
    return 0 unless $main::known[ $at + $i ] == $terms[$i];
  }
  return 1;
}

sub MessageRequiringAResponse { push @main::responses, "$_[2]"; return 'Yes' }
sub MessageRequiringNoResponse { }
