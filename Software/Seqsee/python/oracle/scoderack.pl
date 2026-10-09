# Oracle for SCoderack.pm (item 036).
# Output: tests/golden/scoderack.json
#
# Each case has a "case" name; tests/test_scoderack.py rebuilds the same situation by name.
# Codelets are described as [family, urgency, tag], where tag is the "t" argument (or undef).
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

my %name_of;

sub err {
  my ($e) = @_;
  return undef unless $e;
  $e =~ s/\s*at (?:constructor |\S+ line \d+\.?\n).*//s;
  return $e;
}

sub attempt {
  my ($code) = @_;
  my $r = eval { $code->() };
  return ( $r, err($@) );
}

sub quiet {
  my ($code) = @_;
  open( my $saved, '>&', \*STDOUT ) or die;
  my $out = '';
  close STDOUT;
  open( STDOUT, '>', \$out ) or die;
  my @r = eval { $code->() };
  my $e = $@;
  close STDOUT;
  open( STDOUT, '>&', $saved ) or die;
  die $e if $e;
  return $out;
}

sub reset_state {
  $Global::Steps_Finished = 0;
  SLTM->Clear();
  SWorkspace->init( { seq => [ 1 .. 6 ] } );
  %Global::Feature = ();
  SCoderack->clear();
  $SCoderack::LastSelectedRunnable = undef;
  %name_of = ();
  srand(1);
}

sub elements {
  my @e = SWorkspace::GetElements();
  $name_of{ $e[$_] } = "e$_" for 0 .. $#e;
  return @e;
}

sub cl {
  my ( $family, $urgency, $tag ) = @_;
  return SCodelet->new( $family, $urgency, defined($tag) ? { t => $tag } : {} );
}

sub view {
  my ($c) = @_;
  return undef unless defined $c;
  return "str:$c" unless ref $c;
  return ref($c) unless Scalar::Util::blessed($c) and $c->isa('SCodelet');
  return [ $c->[0], $c->[1], $c->[3]{t} ];
}

sub state {
  return (
    codelets => [ map { view($_) } @SCoderack::CODELETS ],
    count    => SCoderack->get_codelet_count,
    sum      => SCoderack->get_urgencies_sum,
    history  => {%SCoderack::HistoryOfRunnable},
    last     => view($SCoderack::LastSelectedRunnable),
  );
}

# ---- clear / init ---------------------------------------------------------------------------------
{
  reset_state();
  record( case => 'empty', state() );
}
{
  reset_state();
  my $out = quiet( sub { SCoderack->init() } );
  record( case => 'init', output => $out, state() );
  $out = quiet( sub { SCoderack->init( { foo => 1 } ) } );
  record( case => 'init_twice', output => $out, state() );
  SCoderack->clear();
  record( case => 'init_cleared', state() );
}
{
  reset_state();
  $Global::Steps_Finished = undef;
  quiet( sub { SCoderack->init() } );
  record( case => 'init_steps_undef', steps => $Global::Steps_Finished,
          creation => [ map { $_->[2] } @SCoderack::CODELETS ] );
  $Global::Steps_Finished = 0;
}

# ---- add_codelet ------------------------------------------------------------------------------------
{
  reset_state();
  my @e = elements();
  for my $spec (
    [ 'add_undef',   sub { undef } ],
    [ 'add_string',  sub { 'SCodelet' } ],
    [ 'add_string2', sub { 'foo' } ],
    [ 'add_hash',    sub { {} } ],
    [ 'add_action',  sub { SAction->new( { family => 'FocusOn', urgency => 5, arguments => {} } ) } ],
    [ 'add_element', sub { $e[0] } ],
    )
  {
    my ( $name, $make ) = @$spec;
    SCoderack->clear();
    my ( $r, $e ) = attempt( sub { SCoderack->add_codelet( $make->() ); 1 } );
    record( case => $name, error => $e, state() );
  }
}
{
  reset_state();
  my @steps;
  my @urg = ( 5, 3, 7, 3, 9, 1, 4, 4, 8, 2, 6, 3, 1, 10, 2, 2, 5, 7, 1, 9, 3, 3, 6, 1, 2, 0.5, 1, 4, 3, 1 );
  for my $i ( 0 .. $#urg ) {
    SCoderack->add_codelet( cl( "F$i", $urg[$i], "c$i" ) );
    push @steps, { state() };
  }
  record( case => 'add_many', steps => \@steps );
}
{
  reset_state();
  SCoderack->add_codelet( cl( "A", 5, "x$_" ) ) for 0 .. 25;
  record( case => 'add_ties', state() );
}
{
  reset_state();
  SCoderack->add_codelet( cl( "A", $_ % 3 ? 10 : 2, "x$_" ) ) for 0 .. 27;
  record( case => 'add_ties2', state() );
}
{
  reset_state();
  SCoderack->add_codelet( cl( "A", "7", "s" ) );
  SCoderack->add_codelet( cl( "B", undef, "u" ) );
  record( case => 'add_odd_urgency', state() );
}

# ---- get_next_runnable --------------------------------------------------------------------------------
{
  reset_state();
  $Global::LogString = "xx";
  my $r     = SCoderack->get_next_runnable();
  my $after = rand();
  record(
    case      => 'next_empty',
    runnable  => view($r),
    isa       => ref($r),
    creation  => $r->[2],
    logstring => $Global::LogString,
    after     => $after,
    state()
  );
}
{
  reset_state();
  SCoderack->add_codelet( cl( "Old", 5, "old" ) );
  SCoderack->get_next_runnable();
  my $r = SCoderack->get_next_runnable();
  record( case => 'next_empty_keeps_last', runnable => view($r), state() );
}
for my $seed ( 1, 7, 42 ) {
  reset_state();
  srand($seed);
  my @urg = ( 5, 30, 7, 1, 12, 50, 3, 3, 20, 9 );
  SCoderack->add_codelet( cl( "F" . ( $_ % 3 ), $urg[$_], "c$_" ) ) for 0 .. $#urg;
  my @picked;
  push @picked, view( SCoderack->get_next_runnable() ) for 0 .. 11;
  record( case => "next_seeded_$seed", picked => \@picked, after => rand(), state() );
}
{
  reset_state();
  srand(3);
  my @picked;
  my $i = 0;
  for my $round ( 0 .. 30 ) {
    SCoderack->add_codelet( cl( "G" . ( $round % 4 ), 1 + ( $round * 7 ) % 13, "r$round" ) );
    if ( $round % 3 == 2 ) {
      push @picked, view( SCoderack->get_next_runnable() );
    }
  }
  record( case => 'next_interleaved', picked => \@picked, after => rand(), state() );
}
{
  reset_state();
  SCoderack->add_codelet( cl( "Z", 0, "z" ) );
  my ( $r, $e ) = attempt( sub { SCoderack->get_next_runnable(); 1 } );
  record( case => 'next_zero_sum', error => $e, after => rand(), state() );
}
{
  # Fractional urgencies: 1 + int(rand(1.5)) can be 2, which walks off the end of @CODELETS.
  # Perl then autovivifies urgency-0 entries forever (it never returns), so seeds 2, 3, 10 and 11
  # are left out; only the seeds that draw 1 are recorded.
  my @out;
  for my $seed ( 1, 4 .. 9, 12 ) {
    reset_state();
    srand($seed);
    SCoderack->add_codelet( cl( "H", 0.5, "a" ) );
    SCoderack->add_codelet( cl( "H", 1,   "b" ) );
    my ( $r, $e ) = attempt( sub { view( SCoderack->get_next_runnable() ) } );
    push @out, { seed => $seed, runnable => $r, error => $e, state() };
  }
  record( case => 'next_fractional', runs => \@out );
}
{
  reset_state();
  SCoderack->add_codelet( cl( "Neg", -5, "n" ) );
  SCoderack->add_codelet( cl( "Pos", 10, "p" ) );
  # Only one pick: with -5 left, a second pick would walk off the end and Perl never returns.
  my ( $r, $e ) = attempt( sub { view( SCoderack->get_next_runnable() ) } );
  record( case => 'next_negative', runnable => $r, error => $e, state() );
}

# ---- choose_codelet ----------------------------------------------------------------------------------
{
  reset_state();
  srand(5);
  my @u = ( 3, 1, 4, 1, 5, 9, 2, 6 );
  SCoderack->add_codelet( cl( "C", $_, "u" ) ) for @u;
  my @idx = map { SCoderack::_choose_codelet() } 1 .. 40;
  record( case => 'choose_seeded', indices => \@idx, state() );
  SCoderack->clear();
  record( case => 'choose_empty', index => SCoderack::_choose_codelet() );
}

# ---- AttentionDistribution ----------------------------------------------------------------------------
sub named_dist {
  my ($d) = @_;
  my %out;
  my %by_str = map { ( "$_" => $name_of{$_} ) } keys %name_of;
  while ( my ( $k, $v ) = each %$d ) {
    my $n = exists $by_str{$k} ? $by_str{$k} : $k;
    $out{$n} = $v;
  }
  return \%out;
}
{
  reset_state();
  record( case => 'attention_empty', dist => SCoderack->AttentionDistribution() );
}
{
  reset_state();
  my @e = elements();
  SCoderack->add_codelet( SCodelet->new( "X", 10, { a => $e[0], b => $e[1] } ) );
  SCoderack->add_codelet( SCodelet->new( "Y", 30, { a => $e[0], n => 7 } ) );
  SCoderack->add_codelet( SCodelet->new( "Z", 20, {} ) );
  record( case => 'attention_no_reader', dist => named_dist( SCoderack->AttentionDistribution() ) );
}
{
  reset_state();
  my @e = elements();
  my $g = Seqsee::Anchored->create( @e[ 2, 3 ] );
  SWorkspace->add_group($g);
  $name_of{$g} = 'g23';
  my $r = SRelation->new( { first => $e[4], second => $e[5], type => Mapping::Numeric->create( 'succ', $S::NUMBER ) } );
  $r->insert;
  $name_of{$r} = 'r45';
  SWorkspace::__UpdateObjectStrengths();
  SCoderack->add_codelet( SCodelet->new( "X", 10, { a => $e[0], b => $g } ) );
  SCoderack->add_codelet( SCodelet->new( "FocusOn", 40, {} ) );
  SCoderack->add_codelet( SCodelet->new( "FocusOn", 10, { what => $e[1] } ) );
  my ( $p, $o ) = SWorkspace::__GetObjectOrRelationChoiceProbabilityDistribution();
  record(
    case   => 'attention_reader',
    dist   => named_dist( SCoderack->AttentionDistribution() ),
    reader => { map { ( $name_of{ $o->[$_] } => $p->[$_] ) } 0 .. $#$o },
  );
}

# ---- clear_all_but_workspace ------------------------------------------------------------------------
{
  reset_state();
  SCoderack->add_codelet( cl( "A", 5, "a" ) );
  SCoderack->get_next_runnable();
  SCoderack->add_codelet( cl( "B", 6, "b" ) );
  SUtil::clear_all_but_workspace();
  record( case => 'clear_all_but_workspace', state(), elements => scalar( SWorkspace::GetElements() ) );
}

emit();
