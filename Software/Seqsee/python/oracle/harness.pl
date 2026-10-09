# Oracle for Test/Seqsee.pm (Test::Seqsee), Test::Stochastic (CPAN, used by it) and
# Seqsee/ResultOfTestRun.pm (item 049).
# Output: tests/golden/harness.json
#
# Each case has a "case" name; tests/test_harness.py rebuilds the same situation by name.
# Test::More's results are read from Test::Builder's details (output goes to a scalar).
# Error messages are normalised: " at FILE line N." and stack traces are dropped.
# RunSeqsee/RegTestHelper scenarios replace Seqsee::Interaction_step_n by a scripted stub,
# so they are deterministic. The "real_*" cases are genuine short RunSeqsee runs, each in
# its own Perl process (`harness.pl --real SEED SEQ CONT STEPS MAX_FALSE MIN_EXT`).
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine', 'once', 'misc';
use Oracle;
use File::Temp qw(tempdir);
use Cwd qw(getcwd);
use JSON::PP;

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

my $builder = Test::More->builder;
my ( $tap, $tap_err ) = ( '', '' );
$builder->output( \$tap );
$builder->failure_output( \$tap_err );
$builder->todo_output( \$tap_err );
$builder->no_ending(1);

sub norm {
  my ($msg) = @_;
  return undef unless defined $msg;
  $msg = "$msg";
  $msg =~ s/\n\t.*//s;                            # confess traces
  $msg =~ s/ at \S+ line \d+\.?(\n|$)/$1/g;
  $msg =~ s/=(HASH|ARRAY|SCALAR|CODE)\(0x[0-9a-f]+\)/=$1/g;
  $msg =~ s/\b(ARRAY|HASH)\(0x[0-9a-f]+\)/$1(0x)/g;
  return $msg;
}

# Results of Test::More calls made by $code: list of [ok, name].
sub tap_of {
  my ($code) = @_;
  my $before = scalar( my @d = $builder->details );
  my @ret    = $code->();
  my @all    = $builder->details;
  my @new    = @all[ $before .. $#all ];
  return ( [ map { [ $_->{ok} ? 1 : 0, norm( $_->{name} ) ] } @new ], @ret );
}

sub capture_stdout {
  my ($code) = @_;
  my $buf = '';
  open( my $fh, '>', \$buf ) or die;
  my $old = select($fh);
  my @ret;
  my $err;
  {
    local *STDOUT = $fh;
    @ret = eval { $code->() };
    $err = $@;
  }
  select($old);
  close $fh;
  return ( $buf, $err, @ret );
}

sub capture_stderr {
  my ($code) = @_;
  my $buf = '';
  my @ret;
  my $err;
  {
    local *STDERR;
    open( STDERR, '>', \$buf ) or die;
    @ret = eval { $code->() };
    $err = $@;
  }
  return ( $buf, $err, @ret );
}

if ( @ARGV and $ARGV[0] eq '--real' ) {
  shift @ARGV;
  my ( $seed, $seq, $cont, $steps, $max_false, $min_ext ) = @ARGV;
  my ( $out, $err, $r ) = capture_stdout(
    sub {
      srand($seed);
      RunSeqsee( [ split ' ', $seq ], [ split ' ', $cont ], $steps, $max_false, $min_ext );
    }
  );
  die $err if $err;
  print JSON::PP->new->canonical->encode(
    {
      status   => $r->get_status->get_status_string,
      steps    => $r->get_steps + 0,
      error    => norm( $r->get_error ),
      elements => [ map { $_->get_mag + 0 } SWorkspace::GetElements() ],
      stdout   => $out,
    }
  ), "\n";
  exit 0;
}

# ---------------------------------------------------------------- fakes
{
  package FakeErr;    # an exception with a payload method
  sub new { my ( $c, $p ) = @_; bless { p => $p }, $c }
  sub payload { $_[0]{p} }
  sub throw { die $_[0]->new( $_[1] ) }
  use overload '""' => sub { "FakeErr(" . ( ref( $_[0]{p} ) || 'none' ) . ")" }, fallback => 1;

  package Payload;
  use overload '""' => sub { "P:" . ref( $_[0] ) }, fallback => 1;
  sub new { bless {}, $_[0] }
  package SThought::AreRelated;    our @ISA = ('Payload');
  package SThought::Special;       our @ISA = ('SThought::AreRelated');
  package Seqsee::SCF::FocusOn::P; our @ISA = ('Payload');
  package Other::Thing;            our @ISA = ('Payload');

  package FakeCodelet;    # run() dies with a FakeErr carrying a payload, or a string, or lives
  sub new { my ( $c, %o ) = @_; bless {%o}, $c }
  sub run {
    my ($self) = @_;
    die $self->{string} if defined $self->{string};
    FakeErr->throw( $self->{payload} ) if exists $self->{payload};
    return 'lived';
  }

  package FakeCatObj;
  use overload '""' => sub { "OBJ" }, fallback => 1;
  sub new { bless { cats => { map { $_ => 1 } @_[ 1 .. $#_ ] } }, $_[0] }
  sub instance_of_cat { $_[0]{cats}{ $_[1] } ? 1 : 0 }

  package FakeFringe;    # get_fringe/get_extended_fringe/get_actions answer from rand
  sub new { bless { k => $_[1] }, $_[0] }
  sub get_fringe {
    my @f = ( [ 'a', 10 ] );
    push @f, [ 'b', 5 ] if rand() < 0.5;
    push @f, [ 'z', 1 ] if $_[0]{k};
    return \@f;
  }
  sub get_extended_fringe {
    my @f = ( [ 'x', 10 ] );
    push @f, [ 'y', 5 ] if rand() < 0.3;
    return \@f;
  }
  sub get_actions {
    my @a = ( bless( {}, 'SAction' ) );
    push @a, bless( {}, 'SCodelet' ) if rand() < 0.5;
    return @a;
  }
}

sub payload_obj {
  my ($cls) = @_;
  return undef unless $cls;
  return $cls->new;
}

# ---------------------------------------------------------------- ParseSeq_
for my $s ( "1 2 3 | 4 5", "  1 2  3|4  5 6  ", "1 2 | 3 4 | 5", " 1 2  ", "|", "", "7|" ) {
  my ( $a, $b ) = ParseSeq_($s);
  record( case => "parse_seq", input => $s, seq => $a, continuation => $b );
}

# ---------------------------------------------------------------- TestOutputStatus
for my $name (qw(Successful RanOutOfTerms InitialBlemish ExtendedABit NotEvenExtended Crashed)) {
  no strict 'refs';
  my $st = ${"TestOutputStatus::$name"};
  record(
    case                    => "status",
    name                    => $name,
    status_string           => $st->get_status_string,
    is_success              => $st->IsSuccess,
    is_at_least_an_extension => $st->IsAtLeastAnExtension,
    is_a_crash              => $st->IsACrash,
  );
}
{
  my $st = TestOutputStatus->new( { status_string => 'Weird' } );
  my $old = $st->set_status_string('Successful');
  record( case => "status_new", old => $old, now => $st->get_status_string,
          is_success => $st->IsSuccess, shared => ( $st == $TestOutputStatus::Successful ? 1 : 0 ) );
  record( case => "status_missing",
          error => norm( eval { TestOutputStatus->new( {} ); 1 } ? undef : $@ ) );
}

# ---------------------------------------------------------------- ResultOfTestRun
{
  my $r = Seqsee::ResultOfTestRun->new(
    { status => $TestOutputStatus::Crashed, steps => 12, error => undef } );
  my $old = $r->set_steps(15);
  record( case => "result", status => $r->get_status->get_status_string, steps => $r->get_steps,
          old_steps => $old, error => $r->get_error );
  for my $args ( { steps => 1, error => 'e' }, {}, { status => 1, steps => 2 } ) {
    record( case => "result_missing", args => [ sort keys %$args ],
            error => norm( eval { Seqsee::ResultOfTestRun->new($args); 1 } ? undef : $@ ) );
  }
  my $rs = ResultsOfTestRuns->new(
    { times => [1], results => [], rate => 0.5, terms => 'x', features => 'f', version => 3 } );
  record( case => "results", is_ltm_result => $rs->get_is_ltm_result, context => $rs->get_context,
          rate => $rs->get_rate, version => $rs->get_version );
  $rs = ResultsOfTestRuns->new(
    { times => [1], results => [], rate => 0.5, terms => 'x', features => 'f', version => 3,
      is_ltm_result => 1, context => 'ctx' } );
  my $old_ctx = $rs->set_context('new');
  record( case => "results_given", is_ltm_result => $rs->get_is_ltm_result, context => $rs->get_context,
          old_context => $old_ctx );
  record( case => "results_missing",
          error => norm( eval { ResultsOfTestRuns->new( { times => 1 } ); 1 } ? undef : $@ ) );
}

# ---------------------------------------------------------------- Test::Stochastic
for my $p ( [ 0.5, 1000, 0.2 ], [ 0.1, 1000, 0.2 ], [ 0.9, 1000, 0.2 ], [ 0.33, 100, 0.1 ], [ 0, 50, 0.2 ] ) {
  record( case => "range", args => $p, range => [ Test::Stochastic::_get_acceptable_range(@$p) ] );
}

our $CALLS;
sub counting { my ($code) = @_; return sub { $CALLS++; $code->() } }
my %SUBS = (
  rand3     => sub { int( rand(3) ) },
  rand2     => sub { int( rand(2) ) },
  skewed    => sub { rand() < 0.9 ? 'a' : 'b' },
  const     => sub { 'k' },
  undef_ret => sub { rand() < 0.5 ? undef : 'u' },
);
my @STOCH = (
  [ 'all_seen_ok',         'rand3',  [ 0, 1, 2 ] ],
  [ 'all_seen_ok',         'rand3',  [ 0, 1, 5 ] ],
  [ 'all_seen_ok',         'const',  [] ],
  [ 'all_seen_ok',         'undef_ret', [ '', 'u' ] ],
  [ 'all_seen_nok',        'rand3',  [ 0, 1, 2 ] ],
  [ 'all_seen_nok',        'rand3',  [ 3 ] ],
  [ 'all_and_only_ok',     'rand3',  [ 0, 1, 2 ] ],
  [ 'all_and_only_ok',     'rand3',  [ 0, 1 ] ],
  [ 'all_and_only_ok',     'rand2',  [ 0, 1, 7 ] ],
  [ 'all_and_only_nok',    'rand3',  [ 0, 1, 2 ] ],
  [ 'all_and_only_nok',    'rand3',  [ 0, 2 ] ],
  [ 'ok',                  'rand2',  { 0 => 0.5, 1 => 0.5 } ],
  [ 'ok',                  'skewed', { a => 0.5 } ],
  [ 'ok',                  'skewed', { b => 0.5 } ],
  [ 'ok',                  'skewed', { c => 0.1 } ],
  [ 'ok',                  'skewed', { a => 0.9, b => 0.1 } ],
  [ 'nok',                 'skewed', { a => 0.9 } ],
  [ 'nok',                 'skewed', { a => 0.1 } ],
);
my $i = 0;
for my $t (@STOCH) {
  my ( $fn, $subname, $arg ) = @$t;
  for my $order ( 'sub_first', 'arg_first' ) {
    next if $order eq 'arg_first' and $i % 3;
    srand( 100 + $i );
    local $CALLS = 0;
    my $code = counting( $SUBS{$subname} );
    no strict 'refs';
    my $f = \&{"Test::Stochastic::stochastic_$fn"};
    my ($res) = tap_of( sub { $order eq 'sub_first' ? $f->( $code, $arg, ( $i % 2 ? "msg$i" : () ) )
                                                    : $f->( $arg, $code ) } );
    record( case => "stochastic", fn => $fn, sub => $subname, arg => $arg, order => $order,
            seed => 100 + $i, msg => ( $i % 2 && $order eq 'sub_first' ? "msg$i" : undef ),
            tap => $res, calls => $CALLS, next_rand => rand() );
  }
  $i++;
}
{
  Test::Stochastic::setup( times => 50, tolerence => 0.1 );
  srand(7);
  local $CALLS = 0;
  my ($res) = tap_of( sub { Test::Stochastic::stochastic_ok( counting( $SUBS{rand2} ), { 0 => 0.5 } ) } );
  record( case => "stochastic_setup", tap => $res, calls => $CALLS, next_rand => rand() );
  my $err = eval { Test::Stochastic::setup( bogus => 1 ); 1 } ? undef : norm($@);
  record( case => "stochastic_setup_bad", error => $err );
  Test::Stochastic::setup( times => 1000, tolerence => 0.2 );
}

# ---------------------------------------------------------------- _wrap_to_get_payload_type
my @WRAP = (
  [ 'thought',   sub { FakeErr->throw( payload_obj('SThought::AreRelated') ) }, undef ],
  [ 'thought_sub', sub { FakeErr->throw( payload_obj('SThought::Special') ) }, undef ],
  [ 'scf',       sub { FakeErr->throw( payload_obj('Seqsee::SCF::FocusOn::P') ) }, undef ],
  [ 'other',     sub { FakeErr->throw( payload_obj('Other::Thing') ) }, undef ],
  [ 'nopayload', sub { FakeErr->throw(undef) }, undef ],
  [ 'string',    sub { die "plain\n" }, undef ],
  [ 'lives',     sub { 1 }, undef ],
  [ 'check_ok',  sub { 1 }, sub { 1 } ],
  [ 'check_bad', sub { 1 }, sub { 0 } ],
  [ 'check_ignored_on_throw', sub { FakeErr->throw( payload_obj('SThought::AreRelated') ) }, sub { 0 } ],
);
for my $w (@WRAP) {
  my ( $name, $code, $check ) = @$w;
  my $wrapped = main::_wrap_to_get_payload_type( $code, $check );
  my $ret = eval { $wrapped->() };
  record( case => "wrap", name => $name, ret => $ret, error => ( $@ ? norm($@) : undef ) );
}

# a sub that picks one of several outcomes at random
sub picker {
  my (@outs) = @_;
  return sub {
    $CALLS++;
    my $o = $outs[ int( rand(@outs) ) ];
    return 1 if $o eq '';
    die "boom\n" if $o eq 'die';
    FakeErr->throw( payload_obj($o) );
  };
}
my @CTS = (
  [ 'ok',               [ '', 'SThought::AreRelated', 'Seqsee::SCF::FocusOn::P' ], [ '', 'AreRelated', 'FocusOn::P' ] ],
  [ 'ok',               [ '', 'SThought::AreRelated' ], [ 'AreRelated', 'Special' ] ],
  [ 'ok',               [ '', 'die' ], [ '' , 'x'] ],
  [ 'nok',              [ '', 'SThought::AreRelated' ], [ '', 'AreRelated' ] ],
  [ 'nok',              [ '', 'SThought::AreRelated' ], [ 'Special' ] ],
  [ 'all_and_only_ok',  [ '', 'SThought::AreRelated' ], [ '', 'AreRelated' ] ],
  [ 'all_and_only_ok',  [ '', 'SThought::AreRelated' ], [ '' ] ],
  [ 'all_and_only_nok', [ '', 'SThought::AreRelated' ], [ '', 'AreRelated' ] ],
  [ 'all_and_only_nok', [ '', 'SThought::AreRelated' ], [ 'AreRelated' ] ],
);
$i = 0;
for my $c (@CTS) {
  my ( $fn, $outs, $expect ) = @$c;
  srand( 300 + $i );
  local $CALLS = 0;
  no strict 'refs';
  my $f = \&{"main::code_throws_stochastic_$fn"};
  my ($res) = tap_of( sub { $f->( picker(@$outs), $expect ) } );
  record( case => "code_throws", fn => $fn, outs => $outs, expect => $expect, seed => 300 + $i,
          tap => $res, calls => $CALLS, next_rand => rand() );
  $i++;
}
{
  # check_sub: code_throws_stochastic_ok with a check that fails once the sub lives
  srand(400);
  local $CALLS = 0;
  my ($res) = tap_of( sub { code_throws_stochastic_ok( picker( '', 'SThought::AreRelated' ), [ '', 'AreRelated' ], sub { 0 } ) } );
  record( case => "code_throws_check_fails", tap => $res, calls => $CALLS, next_rand => rand() );
  srand(401);
  $CALLS = 0;
  ($res) = tap_of( sub { code_throws_stochastic_all_and_only_ok( picker( '', 'SThought::AreRelated' ), [ '', 'AreRelated' ], sub { 1 } ) } );
  record( case => "code_throws_all_and_only_check", tap => $res, calls => $CALLS, next_rand => rand() );
}

# ---------------------------------------------------------------- throws_thought_ok & co.
my @TT = (
  [ 'match',       { payload => payload_obj('SThought::AreRelated') }, 'AreRelated' ],
  [ 'match_full',  { payload => payload_obj('SThought::AreRelated') }, 'SThought::AreRelated' ],
  [ 'match_list',  { payload => payload_obj('SThought::AreRelated') }, [ 'Foo', 'AreRelated' ] ],
  [ 'match_isa',   { payload => payload_obj('SThought::Special') }, 'AreRelated' ],
  [ 'wrong',       { payload => payload_obj('SThought::AreRelated') }, [ 'Foo', 'SThought::Bar' ] ],
  [ 'no_thought',  {}, 'AreRelated' ],
  [ 'no_payload',  { payload => undef }, 'AreRelated' ],
  [ 'string',      { string => "oops\n" }, 'AreRelated' ],
);
for my $t (@TT) {
  my ( $name, $opts, $type ) = @$t;
  my ( $res, $ret );
  my $died = eval { ( $res, $ret ) = tap_of( sub { throws_thought_ok( FakeCodelet->new(%$opts), $type ) } ); 1 } ? undef : norm($@);
  record( case => "throws_thought", name => $name, type => $type, tap => $res,
          ret => ( ref($ret) || undef ), died => $died );
}
for my $t ( [ 'lives', {} ], [ 'string', { string => "x\n" } ], [ 'thought', { payload => payload_obj('SThought::AreRelated') } ] ) {
  my ( $name, $opts ) = @$t;
  my ($res) = tap_of( sub { throws_no_thought_ok( FakeCodelet->new(%$opts) ) } );
  record( case => "throws_no_thought", name => $name, tap => $res );
}

# ---------------------------------------------------------------- undef_ok, instance_of_cat_ok
for my $t ( [ undef, undef ], [ 0, undef ], [ '', undef ], [ 5, undef ], [ undef, 'custom' ], [ 5, 'custom' ] ) {
  my ($res) = tap_of( sub { undef_ok(@$t) } );
  record( case => "undef_ok", args => $t, tap => $res );
}
for my $t ( [ 'cat1', undef ], [ 'cat2', undef ], [ 'cat1', 'mine' ], [ 'cat2', 'mine' ] ) {
  my ($res) = tap_of( sub { instance_of_cat_ok( FakeCatObj->new('cat1'), @$t ) } );
  record( case => "instance_of_cat_ok", args => $t, tap => $res );
}

# ---------------------------------------------------------------- output_contains & co.
sub lister {
  return sub {
    $CALLS++;
    my @r = ('always');
    push @r, 'half' if rand() < 0.5;
    push @r, 'rare' if rand() < 0.05;
    push @r, 'always', 'always';    # duplicates count once per call
    return \@r;
  };
}
my @OC = (
  [ always => ['always'] ], [ always => ['half'] ], [ always => ['missing'] ],
  [ never => ['missing'] ], [ never => ['half'] ],
  [ sometimes => ['half'] ], [ sometimes => ['missing'] ], [ sometimes => ['always'] ],
  [ sometimes_but_not_always => ['half'] ], [ sometimes_but_not_always => ['always'] ],
  [ sometimes_but_not_always => ['missing'] ],
  [ always => [ 'always', 'half', 'missing' ] ],
  [ always => ['always'], never => ['missing'], sometimes => ['half'], msg => 'multi' ],
  [ msg => 'empty' ],
);
$i = 0;
for my $scope (@OC) {
  srand( 500 + $i );
  local $CALLS = 0;
  my ($res) = tap_of( sub { output_contains( lister(), @$scope ) } );
  record( case => "output_contains", scope => $scope, seed => 500 + $i, tap => $res, calls => $CALLS,
          next_rand => rand() );
  $i++;
}
{
  srand(550);
  my $err = eval { output_contains( lister(), bogus => ['x'] ); 1 } ? undef : norm($@);
  record( case => "output_contains_bad", error => $err );
}
for my $w ( [ 'always', 'always' ], [ 'always', [ 'always', 'half' ] ], [ 'never', 'rare' ],
            [ 'sometimes', 'half' ], [ 'sometimes_but_not_always', ['half'] ] ) {
  my ( $kind, $arg ) = @$w;
  srand( 600 + $i );
  local $CALLS = 0;
  no strict 'refs';
  my $f = \&{"main::output_${kind}_contains"};
  my ($res) = tap_of( sub { $f->( lister(), $arg ) } );
  record( case => "output_wrapper", kind => $kind, arg => $arg, seed => 600 + $i, tap => $res,
          calls => $CALLS, next_rand => rand() );
  $i++;
}
for my $w ( [ 'fringe', 'object', [ always => ['a'], sometimes_but_not_always => ['b'], never => ['z'] ] ],
            [ 'fringe', 'setup', [ always => [ 'a', 'z' ] ] ],
            [ 'fringe', 'object', [ always => ['b'] ] ],
            [ 'extended_fringe', 'object', [ always => ['x'], sometimes => ['y'] ] ],
            [ 'extended_fringe', 'setup', [ never => ['y'] ] ],
            [ 'action', 'object', [ always => ['SAction'], sometimes => ['SCodelet'] ] ],
            [ 'action', 'setup', [ never => ['SCodelet'] ] ] ) {
  my ( $kind, $how, $scope ) = @$w;
  srand( 700 + $i );
  my $setups = 0;
  my $target = $how eq 'object' ? FakeFringe->new(0) : sub { $setups++; FakeFringe->new(1) };
  no strict 'refs';
  my $f = \&{"main::${kind}_contains"};
  my ($res) = tap_of( sub { $f->( $target, @$scope ) } );
  record( case => "fringe", kind => $kind, how => $how, scope => $scope, seed => 700 + $i, tap => $res,
          setups => $setups, next_rand => rand() );
  $i++;
}

# ---------------------------------------------------------------- INITIALIZE_for_testing (at load)
{
  my %o = %{$Global::TestingOptionsRef};
  record( case => "initialize", testing_mode => $Global::TestingMode,
          current_runnable_string => $Global::CurrentRunnableString,
          steps_finished => $Global::Steps_Finished,
          options => { map { $_ => $o{$_} } grep { !ref $o{$_} or ref $o{$_} eq 'ARRAY' } keys %o } );
}

# ---------------------------------------------------------------- RunSeqsee / RegTestHelper (stubbed)
our @STUB;     # actions for successive stub calls
our @CALLED;
my $real_step = \&Seqsee::Interaction_step_n;
sub install_stub {
  *Seqsee::Interaction_step_n = sub {
    my ($opts) = @_;
    push @CALLED, { map { $_ => $opts->{$_} } qw(n max_steps update_after) };
    my $a = shift(@STUB) // { ret => 1 };
    $Global::Steps_Finished += $a->{steps} // 0;
    SWorkspace->insert_elements( @{ $a->{insert} } ) if $a->{insert};
    IncrementFailedRequests() for 1 .. ( $a->{fail} // 0 );
    $Global::ExtensionRejectedByUser{ $a->{reject} } = 1 if $a->{reject};
    my $t = $a->{throw} // '';
    SErr::FinishedTest->throw( got_it => 1 ) if $t eq 'got_it';
    SErr::FinishedTest->throw( got_it => 0 ) if $t eq 'not_got_it';
    SErr::NotClairvoyant->throw()            if $t eq 'clairvoyant';
    SErr::FinishedTestBlemished->throw()     if $t eq 'blemished';
    SErr->throw('objerr')                    if $t eq 'serr';
    die "boom\n"                             if $t eq 'die';
    die "no newline"                         if $t eq 'die_nonl';
    return $a->{ret};
  };
}
sub state_after {
  return (
    called        => [@CALLED],
    elements      => [ map { $_->get_mag + 0 } SWorkspace::GetElements() ],
    real_sequence => [ map { $_ + 0 } @Global::RealSequence ],
    failed        => GetFailedRequests(),
    read_head     => $SWorkspace::ReadHead,
    steps         => $Global::Steps_Finished,
    rejected      => [ sort keys %Global::ExtensionRejectedByUser ],
  );
}
sub fresh {
  SUtil::clear_all();
  @Global::RealSequence = ();
  %Global::ExtensionRejectedByUser = ();
  $Global::Steps_Finished = 0;
  $SWorkspace::ReadHead = 7;
  @CALLED = ();
}

my @RUNS = (
  [ 'natural_none',  [ { steps => 4 }, { ret => 1 } ], 3 ],
  [ 'natural_ret_first', [ { steps => 4, ret => 1 } ], 3 ],
  [ 'natural_extended', [ { steps => 9, insert => [ 4, 5, 6, 7 ] }, { ret => 1 } ], 3 ],
  [ 'natural_exactly_min', [ { steps => 9, insert => [ 4, 5, 6 ] }, { ret => 1 } ], 3 ],
  [ 'natural_min0',  [ { steps => 2, insert => [4] }, { ret => 1 } ], 0 ],
  [ 'three_calls',   [ { steps => 1 }, { steps => 2, ret => 0 }, { ret => 'yes' } ], 3 ],
  [ 'got_it',        [ { steps => 5 }, { steps => 3, throw => 'got_it' } ], 3 ],
  [ 'not_got_it',    [ { steps => 5, throw => 'not_got_it' } ], 3 ],
  [ 'clairvoyant',   [ { steps => 6, insert => [4], throw => 'clairvoyant' } ], 3 ],
  [ 'blemished',     [ { steps => 2, throw => 'blemished' } ], 3 ],
  [ 'serr',          [ { steps => 2, throw => 'serr' } ], 3 ],
  [ 'die',           [ { steps => 2, fail => 4, throw => 'die' } ], 3 ],
  [ 'die_nonl',      [ { steps => 2, fail => 1, reject => '9, 9', throw => 'die_nonl' } ], 3 ],
);
install_stub();
for my $r (@RUNS) {
  my ( $name, $stub, $min_ext ) = @$r;
  fresh();
  @STUB = map { {%$_} } @$stub;
  my ( $out, $err, $res ) = capture_stdout(
    sub { RunSeqsee( [ 1, 2, 3 ], [ 4, 5, 6, 7 ], 50, 2, $min_ext ) } );
  record( case => "run_seqsee", name => $name, stub => $stub, min_extension => $min_ext,
          stdout => $out, died => ( $err ? norm($err) : undef ),
          ( $res ? ( status => $res->get_status->get_status_string, result_steps => $res->get_steps,
                     error => norm( $res->get_error ) ) : () ),
          state_after() );
}
{
  # LTM on: SLTM->Load fails (missing file) -> Crashed, and no Dump. The file name is redirected
  # (Perl reads memory_dump.dat from the cwd).
  my $real_load = \&SLTM::Load;
  my @dumped;
  local *SLTM::Load = sub { $real_load->( $_[0], '/nonexistent/memory_dump.dat' ) };
  local *SLTM::Dump = sub { push @dumped, $_[1] };
  local $Global::Feature{LTM} = 1;
  fresh();
  @STUB = ( { steps => 3 }, { ret => 1 } );
  my ( $out, $err, $res ) = capture_stdout( sub { RunSeqsee( [ 1, 2, 3 ], [4], 50, 2, 3 ) } );
  record( case => "run_seqsee_ltm_missing", stdout => $out, status => $res->get_status->get_status_string,
          result_steps => $res->get_steps, error => norm( $res->get_error ), dumped => \@dumped, state_after() );
}

my @REG = (
  [ 'got_it',      [ { steps => 5, throw => 'got_it' } ], 3 ],
  [ 'not_got_it',  [ { steps => 5, throw => 'not_got_it' } ], 3 ],
  [ 'clairvoyant', [ { steps => 6, throw => 'clairvoyant' } ], 3 ],
  [ 'blemished',   [ { steps => 2, throw => 'blemished' } ], 3 ],
  [ 'too_many',    [ { steps => 2, fail => 3, reject => '8, 9', throw => 'die' } ], 3 ],
  [ 'not_too_many', [ { steps => 2, fail => 2, throw => 'die' } ], 3 ],
  [ 'too_many_object', [ { steps => 2, fail => 3, throw => 'serr' } ], 3 ],
  [ 'rethrow_object', [ { steps => 2, throw => 'serr' } ], 3 ],
  [ 'extended',    [ { steps => 9, insert => [ 4, 5, 6, 7 ] } ], 3 ],
  [ 'not_extended', [ { steps => 9, insert => [ 4, 5, 6 ] } ], 3 ],
);
for my $r (@REG) {
  my ( $name, $stub, $min_ext ) = @$r;
  fresh();
  @STUB = map { {%$_} } @$stub;
  my ( $errtxt, $err, @ret );
  my ($out) = capture_stdout( sub {
    ( $errtxt, $err, @ret ) = capture_stderr(
      sub { RegTestHelper( { seq => [ 1, 2, 3 ], continuation => [ 4, 5, 6, 7 ], max_false => 2,
                             max_steps => 40, min_extension => $min_ext } ) } );
  } );
  record( case => "reg_test_helper", name => $name, stub => $stub, min_extension => $min_ext,
          stdout => $out, stderr => $errtxt, died => ( $err ? norm($err) : undef ), ret => \@ret,
          state_after() );
}
{
  fresh();
  my $err = eval { RegTestHelper( { seq => [1], continuation => [], max_false => 1, max_steps => 1 } ); 1 } ? undef : norm($@);
  record( case => "reg_test_helper_missing", error => $err );
}
*Seqsee::Interaction_step_n = $real_step;

# ---------------------------------------------------------------- RegStat (RegTestHelper stubbed)
my $tmp = tempdir( CLEANUP => 1 );
my $cwd = getcwd();
{
  my @SCRIPT;
  my @SEEN_OPTS;
  local *main::RegTestHelper = sub {
    push @SEEN_OPTS, join( ',', map { "$_=" . ( ref $_[0]{$_} ? "@{$_[0]{$_}}" : $_[0]{$_} ) } sort keys %{ $_[0] } );
    my $s = shift @SCRIPT;
    die $s->[1] if $s->[0] eq 'die';
    return @$s;
  };
  my @RS = (
    [ 'mixed', [ [ 'GotIt', 100 ], [ 'Extended', 50 ], [ 'GotIt', 30 ], [ 'NotEvenExtended', 9 ],
                 [ 'die', "kaput\n" ], [ 'TooManyFalseQueries', 0 ], [ 'BlemishedGotIt', 3 ],
                 [ 'ExtendedWithoutGettingIt', 4 ], [ 'die', "kaput\n" ], [ 'GotIt', 20 ] ] ],
    [ 'none', [ map { [ 'NotEvenExtended', $_ ] } 1 .. 10 ] ],
  );
  chdir $tmp;
  for my $rs (@RS) {
    my ( $name, $script ) = @$rs;
    @SCRIPT = @$script;
    @SEEN_OPTS = ();
    open my $fh, '>', 'foo' or die;
    print {$fh} Data::Dumper->Dump( [ { seq => [ 1, 2 ], continuation => [3], max_false => 4,
                                        max_steps => 10, min_extension => 2 } ], ['opts_ref'] );
    close $fh;
    my ( $out, $err, $res ) = capture_stdout( sub { my ( $e, $x, $r ) = capture_stderr( sub { RegStat() } ); die $x if $x; $r } );
    my @lines = split /\n/, $out;
    record( case => "reg_stat", name => $name, script => $script, died => ( $err ? norm($err) : undef ),
            outputs => { map { $_ => $res->{$_} } grep { $_ ne 'RESULTS' } keys %$res },
            results => [ map { norm($_) } @{ $res->{RESULTS} } ], stdout_lines => \@lines,
            seen_opts => [@SEEN_OPTS] );
  }
  chdir $cwd;
}

# ---------------------------------------------------------------- RegHarness (RegStatShell stubbed)
{
  my $SHELL_OUT;
  my @SHELL_OPTS;
  local *main::RegStatShell = sub {
    my ($o) = @_;
    push @SHELL_OPTS, { map { $_ => $o->{$_} } keys %$o };
    return { %$SHELL_OUT, RESULTS => [ @{ $SHELL_OUT->{RESULTS} || [] } ] };
  };
  my @RH = (
    # name, input (hash or file text), shell output, earlier .last_res text (undef: none)
    [ 'hash_first', { seq => '1 2 3 | 4 5' }, { GotIt => 3, RESULTS => [ 'SUCCESS: GotIt\t5' ], avgcc => 5 }, undef ],
    [ 'hash_opts', { seq => ' 1 2|3 ', max_false => 2, max_steps => 50, min_extension => 0 },
      { RESULTS => [] }, undef ],
    # RegHarness($file) (as util/regtest.pl calls it) reads the file from the *second* argument
    [ 'file', "seq = 1 1 2 | 1 2 3\nmax_steps = 99\n", { GotIt => 5, RESULTS => [ 'a', 'b' ] }, undef ],
    [ 'file_second', "seq = 1 1 2 | 1 2 3\nmax_steps = 99\n# comment\n\nmin_extension: 4\n",
      { GotIt => 5, RESULTS => [ 'a', 'b' ] }, undef, 1 ],
    [ 'file_bad', "seq = 1 | 2\nnonsense line\n", { GotIt => 5, RESULTS => [] }, undef, 1 ],
    ( map { [ "earlier_$_->[0]_now_$_->[1]", { seq => '1 | 2' }, { GotIt => $_->[1], RESULTS => [] },
              "GotIt = $_->[0]\nRESULTS = ARRAY(0x1)\n" ] }
      [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 2 ], [ 1, 3 ], [ 3, 1 ], [ 3, 2 ], [ 3, 5 ], [ 5, 7 ], [ 9, 7 ],
      [ 9, 8 ], [ 10, 9 ], [ 10, 10 ], [ 10, 11 ], [ 12, 0 ], [ 12, 1 ], [ 8, 9 ] ),
    [ 'earlier_no_gotit', { seq => '1 | 2' }, { GotIt => 0, RESULTS => [] }, "avgcc = 3\n" ],
    [ 'earlier_unparsable', { seq => '1 | 2' }, { GotIt => 0, RESULTS => [] }, "[[[ not config\n" ],
  );
  for my $rh (@RH) {
    my ( $name, $input, $shell, $earlier, $second ) = @$rh;
    my $dir = tempdir( CLEANUP => 1 );
    chdir $dir;
    $SHELL_OUT  = $shell;
    @SHELL_OPTS = ();
    if ( defined $earlier ) { open my $fh, '>', '.last_res' or die; print {$fh} $earlier; close $fh }
    my $arg = $input;
    if ( !ref $input ) { open my $fh, '>', 'in.reg' or die; print {$fh} $input; close $fh; $arg = 'in.reg' }
    my ( $out, $err, @ret ) = capture_stdout( sub { local $_; $second ? RegHarness( 'x', $arg ) : RegHarness($arg) } );
    my $read = sub { my $f = shift; return undef unless -e $f; open my $fh, '<', $f; my @l = map { chomp; $_ } <$fh>; \@l };
    my $log = $read->('.log_res');
    $out =~ s/Processing time: \d+/Processing time: N/;
    record( case => "reg_harness", name => $name, input => $input, shell => $shell, earlier => $earlier,
            second => $second, died => ( $err ? norm($err) : undef ), stdout => $out,
            improved => $ret[0], worse => $ret[1], results => $ret[2], opts => ( $ret[3] ? { %{ $ret[3] } } : undef ),
            shell_opts => [@SHELL_OPTS],
            last_res => [ sort map { norm($_) } @{ $read->('.last_res') || [] } ],
            log_res => ( $log ? [ map { /^\[\d+\]$/ ? '[T]' : norm($_) } @$log ] : undef ) );
    chdir $cwd;
  }
}

# ---------------------------------------------------------------- real short runs (own process each)
for my $r ( [ 1, '1 2 3 4 5', '6 7 8', 1, 3, 3 ], [ 2, '1 2 3 4 5', '6 7 8', 5, 3, 3 ],
            [ 3, '1 1 2 1 2 3', '1 2 3 4', 20, 3, 3 ], [ 4, '1 1 2 2 3 3', '4 4 5 5', 10, 3, 3 ] ) {
  my ( $seed, $seq, $cont, $steps, $mf, $me ) = @$r;
  my $out = `python/oracle/run_perl.sh python/oracle/harness.pl --real $seed "$seq" "$cont" $steps $mf $me 2>/dev/null`;
  my ($line) = grep {/^\{/} split /\n/, $out;
  record( case => "real", seed => $seed, seq => $seq, continuation => $cont, max_steps => $steps,
          max_false => $mf, min_extension => $me, %{ decode_json($line) } );
}

emit();
