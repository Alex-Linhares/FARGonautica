# Oracle for the codelet machinery (item 035): SCodeletBase.pm, SCodelet.pm, SAction.pm,
# MooseX/SCF.pm (Codelet_Family, ACTION) and Seqsee/SCF.pm (ContinueWith).
# Output: tests/golden/codelet.json
#
# Each case has a "case" name; tests/test_codelet.py rebuilds the same situation by name.
# Two test families are defined here: Seqsee::SCF::TFam (one attribute of every spec kind
# used in Seqsee/SCF_MX) and Seqsee::SCF::Empty (no attributes). Their bodies log the
# arguments they receive.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine', 'once';
use Oracle;
use S;

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

our @BODY_LOG;
our @MESSAGES;
*main::message = sub { my ($m) = @_; push @MESSAGES, ref($m) eq 'ARRAY' ? [@$m] : $m; };

package Seqsee::SCF::TFam;
use MooseX::SCF;
Codelet_Family(
  attributes => [
    a => { required => 1 },
    b => { default  => 0 },
    c => { optional => 1 },
    d => {},
    e => { defualt => "" },
  ],
  body => sub { push @main::BODY_LOG, [ map { main::v($_) } @_ ]; return "ret"; }
);

package Seqsee::SCF::Empty;
use MooseX::SCF;
Codelet_Family( attributes => [], body => sub { push @main::BODY_LOG, [ scalar(@_) ]; return 7; } );

package main;

my %name_of;

sub v {
  my ($x) = @_;
  return undef unless defined $x;
  return $x unless ref $x;
  return $name_of{$x} if exists $name_of{$x};
  return ref($x);
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  $e =~ s/\s*at (?:constructor |\S+ line \d+\.?\n).*//s;
  return $e;
}

sub reset_state {
  $Global::Steps_Finished = 0;    # before init: the elements' histories record the step
  SLTM->Clear();
  SWorkspace->init( { seq => [ 1 .. 6 ] } );
  %Global::Feature = ();
  $Global::debugMAX = undef;
  $Global::CurrentCodelet = undef;
  $Global::CurrentCodeletFamily = undef;
  @BODY_LOG = @MESSAGES = ();
  %name_of = ();
  srand(1);
}

sub elements {
  my @e = SWorkspace::GetElements();
  $name_of{ $e[$_] } = "e$_" for 0 .. $#e;
  return @e;
}

# Run $code; return (result, error) with the result in scalar context.
sub attempt {
  my ($code) = @_;
  my $r = eval { $code->() };
  return ( $@ ? undef : v($r), err($@) );
}

sub codelet_view {
  my ($c) = @_;
  my @a = @$c;
  return [ $a[0], $a[1], $a[2], ref( $a[3] ) eq 'HASH' ? [ sort keys %{ $a[3] } ] : v( $a[3] ) ];
}

# ---- SCodelet->new, the @{} overload, as_text -----------------------------------------------
reset_state();
for my $spec (
  [ 'new_basic',      'TFam', 50,  { a => 1 } ],
  [ 'new_no_args',    'Foo',  10 ],
  [ 'new_empty_str',  'Foo',  10,  '' ],
  [ 'new_zero_args',  'Foo',  10,  0 ],
  [ 'new_float_urg',  'Foo',  0.5, { x => 'y' } ],
  [ 'new_undef_urg',  'Foo',  undef, { x => undef } ],
  [ 'new_num_family', 42,     3,   { x => 2 } ],
  )
{
  my ( $name, $fam, $urg, @rest ) = @$spec;
  for my $steps ( 0, 7 ) {
    reset_state();
    $Global::Steps_Finished = $steps;
    my $c = SCodelet->new( $fam, $urg, @rest );
    record(
      case    => $name,
      steps   => $steps,
      view    => codelet_view($c),
      as_text => $c->as_text,
      family  => $c->family,
      urgency => $c->urgency,
      ctime   => $c->creation_time,
    );
  }
}

reset_state();
{
  my @e = elements();
  my $c = SCodelet->new( 'Foo', 20, { obj => $e[2] } );
  record( case => 'as_text_object', as_text => $c->as_text );
  $c = SCodelet->new( 'Foo', 20, { items => [ $e[1], $e[2] ] } );
  record( case => 'as_text_array', as_text => $c->as_text );
  $c = SCodelet->new( 'Foo', 20, { n => 3 } );
  record( case => 'as_text_number', as_text => $c->as_text );
}

for my $spec ( [ 'new_undef_family', undef, 5 ], [ 'new_only_family', 'X' ], ) {
  my ( $name, @args ) = @$spec;
  reset_state();
  my ( $r, $e ) = attempt( sub { SCodelet->new(@args); 1 } );
  record( case => $name, ok => $r, error => $e );
}

# ---- SCodelet run: validated_list and the family body -----------------------------------------
my @RUN_SPECS = (
  [ 'all_given',     'TFam',  { a => 1, d => 4, e => 5 } ],
  [ 'missing_a',     'TFam',  { d => 4, e => 5 } ],
  [ 'missing_e',     'TFam',  { a => 1, d => 4 } ],
  [ 'missing_many',  'TFam',  {} ],
  [ 'extra_one',     'TFam',  { a => 1, d => 4, e => 5, zz => 1 } ],
  [ 'extra_missing', 'TFam',  { zz => 1 } ],
  [ 'optional_set',  'TFam',  { a => 1, d => 4, e => 5, c => 3, b => 7 } ],
  [ 'undef_values',  'TFam',  { a => undef, d => undef, e => undef, b => undef } ],
  [ 'empty_family',  'Empty', {} ],
  [ 'empty_extra',   'Empty', { x => 1 } ],
  [ 'unknown_fam',   'Nope',  {} ],
  [ 'unknown_args',  'Nope',  { x => 1 } ],
);
for my $spec (@RUN_SPECS) {
  my ( $name, $fam, $args ) = @$spec;
  reset_state();
  my $c = SCodelet->new( $fam, 50, $args );
  my ( $r, $e ) = attempt( sub { $c->run } );
  record(
    case            => "run_$name",
    ret             => $r,
    error           => $e,
    body            => [@BODY_LOG],
    current_family  => $Global::CurrentCodeletFamily,
    current_is_self => ( ( $Global::CurrentCodelet // 0 ) == $c ? 1 : 0 ),
  );
}

# Two unknown parameters: the XS validator names only one of them.
{
  reset_state();
  my $c = SCodelet->new( 'TFam', 50, { a => 1, d => 4, e => 5, zz => 1, yy => 2 } );
  my ( $r, $e ) = attempt( sub { $c->run } );
  record( case => 'run_extra_two', error => $e );
}

# Non-hash arguments.
for my $spec ( [ 'array', [1] ], [ 'string', '5' ] ) {
  my ( $name, $args ) = @$spec;
  reset_state();
  my $c = SCodelet->new( 'TFam', 50, $args );
  my ( $r, $e ) = attempt( sub { $c->run } );
  record( case => "run_nonhash_$name", error => $e, body => [@BODY_LOG],
    current_family => $Global::CurrentCodeletFamily );
}

# ---- Freshness ----------------------------------------------------------------------------------
# Steps_Finished is '' when only `use S` has run (SHistory.pm's `||= ''`).
{
  reset_state();
  $Global::Steps_Finished = '';
  my $c = SCodelet->new( 'TFam', 50, { a => 1, d => 2, e => 3 } );
  my ( $r, $e ) = attempt( sub { $c->run } );
  record( case => 'fresh_steps_empty_string', ret => $r, error => $e, body => [@BODY_LOG] );
  @BODY_LOG = ();
  $c = SCodelet->new( 'Empty', 50, {} );
  ( $r, $e ) = attempt( sub { $c->run } );
  record( case => 'fresh_steps_empty_string_noargs', ret => $r, error => $e, body => [@BODY_LOG] );
}

# Scenarios: [name, sub building args (gets elements), sub run between creation and run]
my @FRESH = (
  [ 'element',   sub { { a => $_[0], d => $_[1], e => $_[2] } }, sub { } ],
  [ 'element_changed', sub { { a => $_[0], d => 1, e => 1 } },
    sub { $Global::Steps_Finished = 5; $_[0]->AddHistory("poke") } ],
  [ 'misc_values', sub { { a => [ 1, 2 ], d => { x => 1 }, e => 'str' } }, sub { } ],
  [ 'group_unchanged', sub { { a => $_[6], d => 1, e => 1 } }, sub { $Global::Steps_Finished = 5 } ],
  [ 'group_changed', sub { { a => $_[6], d => 1, e => 1 } },
    sub { $Global::Steps_Finished = 5; $_[6]->AddHistory("poke") } ],
  [ 'group_changed_same_step', sub { { a => $_[6], d => 1, e => 1 } },
    sub { $_[6]->AddHistory("poke") } ],
  [ 'reln_unchanged', sub { { a => $_[7], d => 1, e => 1 } }, sub { $Global::Steps_Finished = 5 } ],
  [ 'reln_first_changed', sub { { a => $_[7], d => 1, e => 1 } },
    sub { $Global::Steps_Finished = 5; $_[0]->AddHistory("poke") } ],
  [ 'reln_second_changed', sub { { a => $_[7], d => 1, e => 1 } },
    sub { $Global::Steps_Finished = 5; $_[1]->AddHistory("poke") } ],
  [ 'reln_self_changed', sub { { a => $_[7], d => 1, e => 1 } },
    sub { $Global::Steps_Finished = 5; $_[7]->AddHistory("poke") } ],
);
for my $spec (@FRESH) {
  my ( $name, $mk, $between ) = @$spec;
  reset_state();
  my @e = elements();
  $Global::Steps_Finished = 2;
  my $g = Seqsee::Anchored->create( @e[ 3, 4 ] );
  $name_of{$g} = 'G';
  my $rel = SRelation->new(
    { first => $e[0], second => $e[1], type => Mapping::Numeric->create( 'succ', $S::NUMBER ) } );
  $name_of{$rel} = 'R';
  $Global::Steps_Finished = 3;
  my @pool = ( @e, $g, $rel );    # 0..5 elements, 6 = G, 7 = R
  my $c = SCodelet->new( 'TFam', 50, $mk->(@pool) );
  $between->(@pool);
  my ( $r, $e ) = attempt( sub { $c->run } );
  record( case => "fresh_$name", ret => $r, error => $e, body => [@BODY_LOG] );
}

# ---- debugMAX messages ---------------------------------------------------------------------------
{
  reset_state();
  $Global::debugMAX = 1;
  my $c = SCodelet->new( 'Empty', 50, { } );
  my ( $r, $e ) = attempt( sub { $c->run } );
  record( case => 'debug_codelet', ret => $r, error => $e, messages => [@MESSAGES] );
  @MESSAGES = ();
  my $a = SAction->new( { family => 'Empty', urgency => 100, arguments => {} } );
  ( $r, $e ) = attempt( sub { $a->run } );
  record( case => 'debug_action', ret => $r, error => $e, messages => [@MESSAGES] );
  @MESSAGES = ();
  $Global::debugMAX = 0;
  ( $r, $e ) = attempt( sub { $a->run } );
  record( case => 'debug_off', ret => $r, messages => [@MESSAGES] );
}

# ---- SAction -------------------------------------------------------------------------------------
for my $spec (
  [ 'action_new_empty',      {} ],
  [ 'action_new_no_urgency', { family => 'X', arguments => {} } ],
  [ 'action_new_no_args',    { family => 'X', urgency => 3 } ],
  [ 'action_new_undef_fam',  { family => undef, urgency => 3, arguments => {} } ],
  [ 'action_new_ok',         { family => 'X', urgency => 3, arguments => [] } ],
  )
{
  my ( $name, $h ) = @$spec;
  reset_state();
  my ( $r, $e ) = attempt( sub { my $a = SAction->new($h); [ $a->family, $a->urgency ] } );
  record( case => $name, ok => ( $e ? 0 : 1 ), error => $e );
}

# Seeded conditionally_run / ACTION: which of a series of actions ran, and the next draw.
for my $seed ( 1, 7, 42 ) {
  for my $urg ( 0, 30, 50, 90, 100, undef ) {
    reset_state();
    srand($seed);
    my @ran;
    for ( 1 .. 6 ) {
      @BODY_LOG = ();
      my $r = SAction->new( { family => 'Empty', urgency => $urg, arguments => {} } )->conditionally_run();
      push @ran, scalar(@BODY_LOG) ? v($r) : undef;
    }
    record( case => 'conditionally_run', seed => $seed, urgency => $urg, ran => \@ran, next => rand() );
  }
}
for my $seed ( 3, 11 ) {
  reset_state();
  srand($seed);
  my @ran;
  for my $urg ( 10, 60, 95 ) {
    @BODY_LOG = ();
    my $r = MooseX::SCF::ACTION( $urg, 'TFam', { a => $urg, d => 1, e => 2 } );
    push @ran, [ v($r), [@BODY_LOG] ];
  }
  record( case => 'action_sub', seed => $seed, ran => \@ran, next => rand() );
}

# SAction does not check freshness: a stale group still runs.
{
  reset_state();
  my @e = elements();
  my $g = Seqsee::Anchored->create( @e[ 3, 4 ] );
  $name_of{$g} = 'G';
  $Global::Steps_Finished = 5;
  $g->AddHistory("poke");
  my ( $r, $e ) = attempt(
    sub { SAction->new( { family => 'TFam', urgency => 100, arguments => { a => $g, d => 1, e => 2 } } )->run } );
  record( case => 'action_stale_runs', ret => $r, error => $e, body => [@BODY_LOG] );
}

for my $spec ( [ 'array', [1] ], [ 'string', '5' ], [ 'undef', undef ] ) {
  my ( $name, $args ) = @$spec;
  reset_state();
  my ( $r, $e ) = attempt(
    sub { SAction->new( { family => 'TFam', urgency => 100, arguments => $args } )->run } );
  record( case => "action_nonhash_$name", error => $e, current_family => $Global::CurrentCodeletFamily );
}
{
  reset_state();
  my ( $r, $e ) = attempt( sub { SAction->new( { family => 'Nope', urgency => 100, arguments => {} } )->run } );
  record( case => 'action_unknown_family', error => $e );
}

# ---- Codelet_Family option checks ----------------------------------------------------------------
for my $spec (
  [ 'cf_no_attributes', [ body => sub { } ] ],
  [ 'cf_no_body', [ attributes => [] ] ],
  [ 'cf_neither', [] ],
  [ 'cf_undef_attributes', [ attributes => undef, body => sub { } ] ],
  )
{
  my ( $name, $opts ) = @$spec;
  my ( $r, $e ) = attempt( sub { MooseX::SCF::Codelet_Family( 'Seqsee::SCF::ZZTmp', @$opts ); 1 } );
  record( case => $name, error => $e );
}

# ---- ContinueWith ---------------------------------------------------------------------------------
{
  reset_state();
  my @e = elements();
  for my $spec (
    [ 'cw_none',    [] ],
    [ 'cw_two',     [ 1, 2 ] ],
    [ 'cw_string',  ['x'] ],
    [ 'cw_undef',   [undef] ],
    [ 'cw_element', [ $e[0] ] ],
    )
  {
    my ( $name, $args ) = @$spec;
    my ( $r, $e ) = attempt( sub { Seqsee::SCF::ContinueWith(@$args); 1 } );
    record( case => $name, error => $e );
  }
}

binmode STDOUT, ':utf8';    # SUtil.pm's StringifyForCarp uses raw latin-1 « »
emit();
