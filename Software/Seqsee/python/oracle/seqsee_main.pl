# Oracle for Seqsee.pm (item 047): run, do_background_activity, Seqsee_Step,
# Interaction_step_n, _read_commandline, _read_config.
# Output: tests/golden/seqsee_main.json
#
# Each case has a "case" name; tests/test_seqsee_main.py rebuilds the same situation by name.
# Codelet runs go to probe families: Seqsee::SCF::Probe (args t, brk, die) and stubbed
# FocusOn / CheckProgress, which only record that they ran. That keeps the step loop
# deterministic: its only draws are the background tosses and the coderack choice.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine', 'once';
use Oracle;
use PadWalker qw(closed_over);

BEGIN {
  *CORE::GLOBAL::exit = sub { die "EXIT\n" };
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require S;
  require Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

our @RAN;
our $UPDATES  = 0;
our $SANITIES = 0;
our $DECAYS   = 0;
our $STRENGTHS = 0;

sub main::update_display { $UPDATES++ }

{
  package Seqsee::SCF::Probe;
  sub run {
    my ( $action_object, $args ) = @_;
    push @main::RAN, "Probe:" . $args->{t};
    $Global::Break_Loop = 1 if $args->{brk};
    die "probe died\n" if $args->{die};
    return 'ignored';
  }
  package Seqsee::SCF::FocusOn;
  sub run { push @main::RAN, "FocusOn" }
  package Seqsee::SCF::CheckProgress;
  sub run { push @main::RAN, "CheckProgress" }
}

my $real_decay    = \&SLTM::DecayAll;
my $real_strength = \&SWorkspace::__UpdateObjectStrengths;
my $real_sanity   = \&Seqsee::SanityCheck;
*SLTM::DecayAll = sub { $DECAYS++; $real_decay->(@_) };
*SWorkspace::__UpdateObjectStrengths = sub { $STRENGTHS++; $real_strength->(@_) };
*Seqsee::SanityCheck = sub { $SANITIES++; $real_sanity->(@_) };

my $checker_ref = closed_over( \&Seqsee::do_background_activity )->{'$TimeLastProgressCheckerLaunched'};

sub err {
  my ($e) = @_;
  return undef unless $e;
  $e =~ s/\s*at (?:constructor |\S+ line \d+\.?\n).*//s;
  return $e;
}

# Run code with STDOUT captured; returns (output, [results], error).
sub capture {
  my ($code) = @_;
  open( my $saved, '>&', \*STDOUT ) or die;
  my $out = '';
  close STDOUT;
  open( STDOUT, '>', \$out ) or die;
  my @r = eval { $code->() };
  my $e = $@;
  close STDOUT;
  open( STDOUT, '>&', $saved ) or die;
  return ( $out, \@r, err($e) );
}

sub reset_state {
  my (%o) = @_;
  $Global::Steps_Finished     = $o{steps} // 0;
  $Global::TimeOfNewStructure = $o{tons}  // 0;
  $$checker_ref               = $o{checker} // 0;
  $Global::AcceptableTrustLevel = 0.5;
  $Global::Break_Loop         = undef;
  $Global::Sanity             = $o{sanity} // 1;
  $Global::CurrentRunnableString = '';
  %Global::Feature = ();
  SLTM->Clear();
  capture( sub { SWorkspace->init( { seq => $o{seq} // [ 1, 2, 3 ] } ) } );
  SCoderack->clear();
  $SCoderack::LastSelectedRunnable = undef;
  @RAN = ();
  $UPDATES = $SANITIES = $DECAYS = $STRENGTHS = 0;
  for my $p ( @{ $o{probes} // [] } ) {
    my ( $t, $u, %extra ) = @$p;
    SCoderack->add_codelet( SCodelet->new( 'Probe', $u, { t => $t, %extra } ) );
  }
  srand( $o{seed} // 1 );
}

sub coderack_view {
  return [ map { [ $_->[0], $_->[1], $_->[3]{t} ] } @SCoderack::CODELETS ];
}

sub state {
  return (
    ran        => [@RAN],
    steps      => $Global::Steps_Finished,
    coderack   => coderack_view(),
    checker    => $$checker_ref,
    trust      => $Global::AcceptableTrustLevel,
    updates    => $UPDATES,
    sanities   => $SANITIES,
    decays     => $DECAYS,
    strengths  => $STRENGTHS,
    current    => $Global::CurrentRunnableString,
    break_loop => $Global::Break_Loop,
    next_draw  => rand(),
  );
}

# ---- _read_config -------------------------------------------------------------------------
my @config_cases = (
  [ 'config_empty' ],
  [ 'config_spaces', seq => '1 2 3' ],
  [ 'config_commas', seq => ' 1, 2,,3 ' ],
  [ 'config_leading_comma', seq => ',1,2' ],
  [ 'config_trailing_newline', seq => "1 2\n" ],
  [ 'config_zero', seq => '0' ],
  [ 'config_blank', seq => '   ' ],
  [ 'config_letters', seq => '1 a' ],
  [ 'config_negative', seq => '-1 2' ],
  [ 'config_decimal', seq => '1.5' ],
  [ 'config_overrides', seq => '4', seed => 7, max_steps => 50, update_interval => 3, view => 2,
    gui_config => 'X', DecayRate => 0.5, UseScheduledThoughtProb => 1, ScheduledThoughtVanishProb => 0 ],
  [ 'config_undef_override', seq => '4', max_steps => undef ],
  [ 'config_extra_ignored', seq => '5', extra => 5, n => 3 ],
);
for my $c (@config_cases) {
  my ( $name, @opts ) = @$c;
  my ( $out, $r, $e ) = capture( sub { Seqsee::_read_config(@opts) } );
  my %result = $r->[0] ? %{ $r->[0] } : ();
  my $seed = delete $result{seed};
  my %opts = @opts;
  record(
    case        => $name,
    result      => ( $e ? undef : \%result ),
    seed_given  => ( exists $opts{seed} ? $seed : undef ),
    seed_is_int => ( defined $seed and $seed =~ /^\d+$/ and $seed < 32000 ) ? 1 : 0,
    output      => $out,
    error       => $e,
  );
}

# ---- _read_commandline --------------------------------------------------------------------
my @cmd_cases = (
  [ 'cmd_none' ],
  [ 'cmd_seed_seq', '--seed', '5', '--seq', '1 2 3' ],
  [ 'cmd_single_dash_eq', '-seed=5', '-n', '100' ],
  [ 'cmd_max_steps_and_n', '--max_steps', '10', '--n', '20' ],
  [ 'cmd_n_only', '--n', '20' ],
  [ 'cmd_gui', '--gui', 'X' ],
  [ 'cmd_gui_config_and_gui', '--gui_config', 'A', '--gui', 'B' ],
  [ 'cmd_sanity', '--sanity' ],
  [ 'cmd_nosanity', '--nosanity' ],
  [ 'cmd_no_dash_sanity', '--no-sanity' ],
  [ 'cmd_view_extra', '--view', '3', 'extra', 'args' ],
  [ 'cmd_features', '-f', 'LTM', '-f', 'debugMAX' ],
  [ 'cmd_feature_typo', '-f', 'Bogus', '--seed', '3' ],
  [ 'cmd_bad_int', '--seed', 'abc' ],
  [ 'cmd_unknown', '--bogus', '--view', '1' ],
  [ 'cmd_ambiguous', '--se', '5' ],
  [ 'cmd_abbrev', '--max', '7', '--upd', '4' ],
  [ 'cmd_seq_eq', '--seq=1,2' ],
  [ 'cmd_negative_int', '--seed', '-3' ],
  [ 'cmd_double_dash', '--view', '2', '--', '--seed', '5' ],
  [ 'cmd_missing_value', '--seed' ],
  [ 'cmd_repeat', '--seed', '1', '--seed', '2' ],
  [ 'cmd_plus_int', '--seed', '+4' ],
  [ 'cmd_case', '--SEED', '6' ],
);
for my $c (@cmd_cases) {
  my ( $name, @argv ) = @$c;
  local @ARGV = @argv;
  %Global::Feature  = ();
  $Global::debugMAX = undef;
  my @warnings;
  local $SIG{__WARN__} = sub { push @warnings, $_[0] };
  my ( $out, $r, $e ) = capture( sub { my %o = Seqsee::_read_commandline(); \%o } );
  my %o = %{ $r->[0] // {} };
  delete $o{f};
  record(
    case     => $name,
    options  => \%o,
    argv     => [@ARGV],
    features => [ sort keys %Global::Feature ],
    debugMAX => $Global::debugMAX,
    output   => $out,
    warnings => \@warnings,
    error    => $e,
  );
}

# ---- do_background_activity ---------------------------------------------------------------
my @bg_cases = (
  [ 'bg_early',      steps => 5,   tons => 0,  checker => 0 ],
  [ 'bg_checker_toss', steps => 30, tons => 0, checker => 0 ],
  [ 'bg_checker_sure', steps => 200, tons => 0, checker => 0 ],
  [ 'bg_checker_recent', steps => 41, tons => 0, checker => 30 ],
  [ 'bg_checker_21', steps => 51, tons => 0, checker => 30 ],
  [ 'bg_new_structure', steps => 61, tons => 61, checker => 0 ],
  [ 'bg_decay', steps => 10, tons => 0, checker => 0 ],
  [ 'bg_decay_zero', steps => 0, tons => 0, checker => 0 ],
);
for my $c (@bg_cases) {
  my ( $name, %o ) = @$c;
  for my $seed ( 1 .. 6 ) {
    reset_state( %o, seed => $seed );
    my ( $out, $r, $e ) = capture( sub { Seqsee::do_background_activity() } );
    record( case => "${name}_$seed", seed => $seed, %o, state(), error => $e );
  }
}
{
  reset_state( steps => 3, seed => 2 );
  $Global::Feature{CodeletTree} = 1;
  my $log = '';
  open( my $fh, '>', \$log ) or die;
  $Global::CodeletTreeLogHandle = $fh;
  my ( $out, $r, $e ) = capture( sub { Seqsee::do_background_activity() } );
  close $fh;
  $Global::CodeletTreeLogHandle = undef;
  my $masked = $log;
  $masked =~ s/SCodelet=HASH\(0x[0-9a-f]+\)/SCodelet/g;
  record( case => 'bg_codelet_tree', state(), log => $masked, error => $e );
}

# ---- Seqsee_Step / Interaction_step_n -----------------------------------------------------
sub step_case {
  my ( $name, $state, $calls ) = @_;
  reset_state(@$state);
  my @results;
  for my $call (@$calls) {
    my ( $out, $r, $e ) = capture( sub { Seqsee::Interaction_step_n($call) } );
    push @results, { ret => $r->[0], output => $out, error => $e, ran => [@RAN], steps => $Global::Steps_Finished };
  }
  record( case => $name, calls => $calls, results => \@results, state() );
}

my @probes3 = ( [ 'a', 10 ], [ 'b', 20 ], [ 'c', 30 ] );
for my $seed ( 1 .. 4 ) {
  step_case( "step_basic_$seed", [ probes => \@probes3, seed => $seed ],
    [ { n => 5, max_steps => 100, update_after => 2 } ] );
}
step_case( 'step_need_n', [ probes => \@probes3 ], [ { max_steps => 100 } ] );
step_case( 'step_n_zero', [ probes => \@probes3 ], [ { n => 0, max_steps => 100 } ] );
step_case( 'step_max_limits', [ probes => \@probes3, seed => 5 ],
  [ { n => 10, max_steps => 4 }, { n => 10, max_steps => 4 } ] );
step_case( 'step_no_max', [ probes => \@probes3 ], [ { n => 3 } ] );
step_case( 'step_update_default', [ probes => \@probes3, seed => 6 ], [ { n => 3, max_steps => 10 } ] );
step_case( 'step_update_every', [ probes => \@probes3, seed => 6 ],
  [ { n => 3, max_steps => 10, update_after => 1 } ] );
step_case( 'step_update_3_of_4', [ probes => \@probes3, seed => 6 ],
  [ { n => 4, max_steps => 10, update_after => 3 } ] );
step_case( 'step_break',
  [ probes => [ [ 'x', 1000, brk => 1 ], [ 'y', 1 ] ], seed => 7 ],
  [ { n => 5, max_steps => 100, update_after => 2 } ] );
step_case( 'step_die', [ probes => [ [ 'z', 1000, die => 1 ] ], seed => 8 ],
  [ { n => 5, max_steps => 100 } ] );
step_case( 'step_trust_100', [ steps => 99, probes => \@probes3, seed => 9 ],
  [ { n => 2, max_steps => 1000 } ] );
step_case( 'step_print_1000', [ steps => 999, probes => \@probes3, seed => 10 ],
  [ { n => 2, max_steps => 2000 } ] );
step_case( 'step_empty_coderack', [ seed => 11 ], [ { n => 12, max_steps => 100 } ] );
step_case( 'step_no_sanity', [ probes => \@probes3, seed => 12, sanity => 0 ],
  [ { n => 3, max_steps => 100 } ] );
step_case( 'step_checker', [ steps => 25, probes => [ [ 'p', 5 ], [ 'q', 5 ] ], seed => 13 ],
  [ { n => 6, max_steps => 100 } ] );
for my $seed ( 14 .. 17 ) {
  step_case( "step_long_$seed", [ probes => [ map { [ "p$_", 3 * $_ ] } 1 .. 8 ], seed => $seed ],
    [ { n => 40, max_steps => 100, update_after => 7 } ] );
}

# The return value of Seqsee_Step itself.
{
  reset_state( probes => \@probes3, seed => 3 );
  my ( $out, $r, $e ) = capture( sub { my $x = Seqsee::Seqsee_Step(); [$x] } );
  record( case => 'seqsee_step_return', ret => $r->[0][0], defined => ( defined $r->[0][0] ? 1 : 0 ), state(), error => $e );
}

# ---- run ----------------------------------------------------------------------------------
{
  reset_state();
  my ( $out, $r, $e ) = capture( sub { Seqsee::run( 1, 2, 3 ) } );
  record( case => 'run_list', output => $out, error => $e,
    elements => $SWorkspace::ElementCount, codelets => scalar(@SCoderack::CODELETS) );
}
{
  reset_state();
  my ( $out, $r, $e ) = capture( sub { Seqsee::run( { seq => [ 4, 5 ] } ) } );
  record( case => 'run_hashref', output => $out, error => $e,
    elements => $SWorkspace::ElementCount, real => [@Global::RealSequence],
    codelets => coderack_view() );
}
{
  reset_state();
  my ( $out, $r, $e ) = capture( sub { Seqsee::run() } );
  record( case => 'run_empty', output => $out, error => $e, elements => $SWorkspace::ElementCount );
}

emit();
