# Oracle for SLTM.pm, part II: init, Dump, Load/Load_Helper, FindActiveFollowers,
# FindActiveCategories, LogActivations/PrintNode and Print.
# Output: tests/golden/sltm_persistence.json
#
# Harness notes:
# - CORE::GLOBAL::exit is overridden (before SLTM.pm compiles) so that Load's `exit` on an
#   SErr::LTM_LoadFailure dies "EXIT CALLED" instead of ending the oracle.
# - Output to STDOUT (say/print) is captured by selecting an in-memory handle; warnings are
#   captured with $SIG{__WARN__}.
# - Files go to a fresh temporary directory; recorded paths are relative to it ("DIR/...").
# - Error messages are normalized: " at FILE line N." (and any stack trace after it) is
#   removed, as are object addresses ("=HASH(0x...)").
BEGIN { *CORE::GLOBAL::exit = sub { die "EXIT CALLED\n" }; }
use strict;
no warnings;    # undef strings in the edge cases are intended
use Oracle;
use S;
use File::Temp;
use File::Slurp;

our @DEBUG;
{ no strict 'refs'; *{"main::debug_message"} = sub { push @DEBUG, [@_] }; }

my $DIR = File::Temp->newdir();
sub path { "$DIR/$_[0]" }
sub rel { my ($s) = @_; $s =~ s/\Q$DIR\E/DIR/g; $s }

sub norm {
  my ($e) = @_;
  $e = rel($e);
  $e =~ s/ at \S+ line \d+\.?\n.*?(?=\nNodes inserted so far)//s;
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/ at \(eval \d+\) line \d+\.?\n.*//s;
  $e =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)//g;
  $e;
}

# Run code with STDOUT captured; returns (captured, result-or-error, died).
sub capture {
  my ($code) = @_;
  my $buf = '';
  open my $fh, '>', \$buf;
  my $old = select($fh);
  my @warn;
  local $SIG{__WARN__} = sub { push @warn, $_[0] };
  my $ok = eval { $code->(); 1 };
  my $err = $ok ? '' : norm( ref($@) ? ( $@->can('what') ? $@->what : "$@" ) : $@ );
  select($old);
  close $fh;
  return ( rel($buf), $ok ? 0 : 1, $err, [ map { rel($_) } @warn ] );
}

sub P { SLTM::Platonic->create( $_[0] ) }

sub state {
  my @acts = map { [@$_] } @SLTM::ACTIVATIONS;
  my @links;
  for my $from ( 1 .. $#SLTM::OUT_LINKS ) {
    my $lr = $SLTM::OUT_LINKS[$from];
    for my $type ( 0 .. $#$lr ) {
      my $h = $lr->[$type] or next;
      for my $to ( sort { $a <=> $b } keys %$h ) {
        push @links, [ $from, $type, $to + 0, [ @{ $h->{$to} } ] ];
      }
    }
  }
  return {
    node_count  => $SLTM::NodeCount,
    nodes       => [ map { [ ref( $SLTM::MEMORY[$_] ), $SLTM::MEMORY[$_]->as_text ] } 1 .. $#SLTM::MEMORY ],
    activations => [ @acts[ 1 .. $#acts ] ],
    links       => \@links,
    link_count  => scalar(@SLTM::LINKS),
  };
}

# The file split into the node part (exact) and the link lines (sorted: hash order).
sub dumped {
  my ($text) = @_;
  my ( $nodes, $links ) = split( /#####\n/, $text, 2 );
  return { nodes => $nodes, links => [ sort split( /\n/, $links // '' ) ], whole => $text };
}

# A small LTM: 7 nodes, links of every type, one with a modifier, assorted numbers.
sub build_small {
  SLTM::Clear();
  SLTM::GetMemoryIndex($S::NUMBER);
  my ( $p1, $p2, $p3 ) = ( P(1), P("[1,2]"), P("[[1,2],3]") );
  SLTM::InsertISALink( $p1, $p2 );
  SLTM::InsertFollowsLink( $S::ASCENDING, Mapping::Numeric->create( "succ", $S::NUMBER ) );
  SLTM::GetMemoryIndex($METO_MODE::ALL);
  SLTM::GetMemoryIndex($POS_MODE::FORWARD);
  SLTM::__InsertLinkUnlessPresent( 3, 6, 7, SLTM::LTM_CAN_BE_SEEN_AS );
  SLTM::__InsertLinkUnlessPresent( 3, 2, 0, SLTM::LTM_IS );
  SLTM::__InsertLinkUnlessPresent( 2, 7, 0, SLTM::LTM_FOLLOWS );
  my $l = SLTM::__InsertLinkUnlessPresent( 7, 1, 2, SLTM::LTM_IS );
  @$l[ SLinkActivation::RAW_SIGNIFICANCE, SLinkActivation::STABILITY_RECIPROCAL ] = ( 123.456789, 0.000049 );
  SLTM::StrengthenLinkGivenNodes( $p1, $p2, SLTM::LTM_IS, 30 );
  SLTM::SpikeBy( 30, $p1 );
  SLTM::SetDepthReciprocalForIndex( 3, 0.5 );
  SLTM::SetDepthReciprocalForIndex( 4, "7" );
}

# ---- init ----
{
  my @r = capture( sub { SLTM::init() } );
  record( op => 'init', out => $r[0], died => $r[1] );
  local $Global::Feature{LogActivations} = 1;
  local $Global::ActivationsLogfile = path('act.log');
  local $Global::ActivationsLogHandle;
  @r = capture( sub { SLTM::init() } );
  my $opened = $Global::ActivationsLogHandle ? 1 : 0;
  my @r2 = capture( sub { SLTM::init() } );    # handle already open
  record( op => 'init_log', out => $r[0], opened => $opened, exists => ( -e path('act.log') ? 1 : 0 ),
    again => $r2[0] );
}

# ---- Dump ----
{
  SLTM::Clear();
  my @r = capture( sub { SLTM->Dump( path('empty.dat') ) } );
  record( op => 'dump_empty', out => $r[0], file => scalar( read_file( path('empty.dat') ) ) );

  build_small();
  my $before = state();
  @r = capture( sub { SLTM->Dump( path('small.dat') ) } );
  record( op => 'dump_small', out => $r[0], state => $before,
    file => dumped( scalar read_file( path('small.dat') ) ) );

  # A File::Temp object is accepted (and written to).
  my $tmp = File::Temp->new( DIR => "$DIR" );
  my $tmpname = $tmp->filename;
  @r = capture( sub { SLTM->Dump($tmp) } );
  record( op => 'dump_file_temp', died => $r[1], file => dumped( scalar read_file($tmpname) ) );

  # Any other reference confesses.
  @r = capture( sub { SLTM->Dump( [] ) } );
  record( op => 'dump_bad_ref', died => $r[1], error => $r[2] );
  @r = capture( sub { SLTM->Dump( bless {}, 'Foo' ) } );
  record( op => 'dump_bad_blessed', died => $r[1], error => $r[2] );

  # An undef depth reciprocal and odd link numbers.
  SLTM::Clear();
  SLTM::InsertISALink( P(5), P(6) );
  SLTM::SetDepthReciprocalForIndex( 1, undef );
  SLTM::SetDepthReciprocalForIndex( 2, "abc" );
  my $l = SLTM::__InsertLinkUnlessPresent( 1, 2, 0, SLTM::LTM_IS );
  @$l[ SLinkActivation::RAW_SIGNIFICANCE, SLinkActivation::STABILITY_RECIPROCAL ] = ( -12345.6789, 99999.123456 );
  @r = capture( sub { SLTM->Dump( path('odd.dat') ) } );
  record( op => 'dump_odd', file => dumped( scalar read_file( path('odd.dat') ) ) );

  # Numbers that need rounding/widening.
  for my $pair ( [ 0, 0 ], [ 1e-9, 1e-9 ], [ 0.00005, 0.000005 ], [ 1234567, 12345 ], [ "7", "0.3" ],
    [ undef, undef ], [ "x", "y" ], [ 2.5, 0.125 ] ) {
    SLTM::Clear();
    SLTM::InsertISALink( P(1), P(2) );
    my $l = SLTM::__InsertLinkUnlessPresent( 1, 2, 0, SLTM::LTM_IS );
    @$l[ SLinkActivation::RAW_SIGNIFICANCE, SLinkActivation::STABILITY_RECIPROCAL ] = @$pair;
    $l->[SLinkActivation::MODIFIER_NODE_INDEX] = undef if $pair->[0] && $pair->[0] eq "x";
    capture( sub { SLTM->Dump( path('n.dat') ) } );
    record( op => 'dump_number', sig => $pair->[0], stab => $pair->[1], undef_modifier => ( $pair->[0] && $pair->[0] eq "x" ) ? 1 : 0,
      file => scalar( read_file( path('n.dat') ) ) );
  }
}

# ---- Load_Helper: round trips ----
{
  # Round trip of a dump without Mapping::Numeric (whose create memo collides on reload).
  SLTM::Clear();
  my ( $p1, $p2, $p3 ) = ( P(1), P("[1,2]"), P("[[1,2],3]") );
  SLTM::InsertISALink( $p1, $p2 );
  SLTM::InsertFollowsLink( $S::ASCENDING, $p3 );
  SLTM::GetMemoryIndex($METO_MODE::ALLBUTONE);
  SLTM::GetMemoryIndex($POS_MODE::BACKWARD);
  SLTM::__InsertLinkUnlessPresent( 3, 5, 6, SLTM::LTM_CAN_BE_SEEN_AS );
  SLTM::StrengthenLinkGivenNodes( $p1, $p2, SLTM::LTM_IS, 30 );
  my $l = SLTM::__InsertLinkUnlessPresent( 5, 1, 2, SLTM::LTM_IS );
  @$l[ SLinkActivation::RAW_SIGNIFICANCE, SLinkActivation::STABILITY_RECIPROCAL ] = ( 42.123456, 0.123456 );
  SLTM::SetDepthReciprocalForIndex( 2, 0.75 );
  SLTM::SpikeBy( 20, $p3 );
  capture( sub { SLTM->Dump( path('rt.dat') ) } );
  my $text = read_file( path('rt.dat') );
  my @r = capture( sub { SLTM->Load_Helper( path('rt.dat') ) } );
  my $after = state();
  my $same_platonic = ( $SLTM::MEMORY[1] == $p1 ) ? 1 : 0;
  capture( sub { SLTM->Dump( path('rt2.dat') ) } );
  record( op => 'load_round_trip', file_text => $text, out => $r[0], died => $r[1], error => $r[2],
    state => $after, same_platonic => $same_platonic,
    redump => dumped( scalar read_file( path('rt2.dat') ) ) );

  # Load_Helper on the small dump (Mapping::Numeric memo collision → failure).
  @r = capture( sub { SLTM->Load_Helper( path('small.dat') ) } );
  record( op => 'load_small', died => $r[1], error => $r[2], node_count => $SLTM::NodeCount );
}

# ---- Load_Helper on hand-written files ----
my @FILES = (
  [ 'empty', '' ],
  [ 'only_sep', "#####\n" ],
  [ 'no_sep', "=== 1: SLTM::Platonic 0.2\n1\n=== 2: SLTM::Platonic 0.4\n2\n" ],
  [ 'whitespace', "\n\n  === 1: SLTM::Platonic   0.2  \n  [1, 2]  \n\n=== 9:   POS_MODE 0.1\nFORWARD\n\n#####\n\n   1    2  2    0  1.0000 0.02000\n\n\t2 1 3 1 5 0.5   \n\n" ],
  [ 'links_many', "=== 1: SLTM::Platonic 0.2\n1\n=== 2: SLTM::Platonic 0.2\n2\n=== 3: SLTM::Platonic 0.2\n3\n#####\n   1    2  2    0  1.0000 0.02000\n   1    2  2    3  9.0000 0.50000\n   1    3  1    2 17.5000 0.10000\n   2    1  3    0  0.0000 1.00000\n   3   99  2    0  2.0000 0.20000\n" ],
  [ 'link_short', "=== 1: SLTM::Platonic 0.2\n1\n#####\n1 2\n" ],
  [ 'no_depth', "=== 1: SLTM::Platonic\n7\n" ],
  [ 'multiline_val', "=== 1: SLTM::Platonic 0.2\n[1,\n2]\n" ],
  [ 'meto', "=== 1: METO_MODE 0.2\nSINGLE\n=== 2: METO_MODE 0.3\nNONE\n=== 3: SCategory::Ascending 0.2\nSCategory::Ascending->new()\n" ],
  [ 'unknown_type', "=== 1: SLTM::Platonic 0.2\n1\n=== 2: Foo::Bar 0.2\nwhatever\n" ],
  [ 'bad_meto', "=== 1: METO_MODE 0.2\nBOGUS\n" ],
  [ 'bad_pos', "=== 1: SLTM::Platonic 0.2\n1\n=== 2: POS_MODE 0.2\nBOGUS\n" ],
  [ 'bad_platonic', "=== 1: SLTM::Platonic 0.2\n[1\n" ],
  [ 'no_val', "=== 1: SLTM::Platonic 0.2\n" ],
  [ 'garbage_first', "garbage\n=== 1: SLTM::Platonic 0.2\n1\n" ],
  [ 'duplicate', "=== 1: SLTM::Platonic 0.2\n1\n=== 2: SLTM::Platonic 0.3\n1\n" ],
  [ 'non_pure_type', "=== 1: SInt 0.2\n3\n" ],
  [ 'empty_type', "=== 1:\n\n=== 2: SLTM::Platonic 0.2\n1\n" ],
  [ 'two_seps', "=== 1: SLTM::Platonic 0.2\n1\n#####\n1 1 2 0 1 1\n#####\n1 1 1 0 1 1\n" ],
  [ 'ind_independent', "=== 1: SCategory::Number 0.2\nSCategory::Number->new()\n=== 2: SCategory::Number 0.2\nSCategory::Number->new()\n" ],
  [ 'ind_bad', "=== 1: SCategory::Number 0.2\nnonsense\n" ],
);
for (@FILES) {
  my ( $name, $text ) = @$_;
  SLTM::Clear();
  write_file( path("$name.dat"), $text );
  my @r = capture( sub { SLTM->Load_Helper( path("$name.dat") ) } );
  record( op => 'load_file', name => $name, text => $text, out => $r[0], died => $r[1], error => $r[2],
    state => state() );
}

# Missing file.
{
  SLTM::Clear();
  my @r = capture( sub { SLTM->Load_Helper( path('missing.dat') ) } );
  record( op => 'load_missing', died => $r[1], error => $r[2] );
}

# ---- Load (the safe wrapper) ----
{
  SLTM::Clear();
  write_file( path('ok.dat'), "=== 1: SLTM::Platonic 0.2\n1\n#####\n" );
  my @r = capture( sub { SLTM->Load( path('ok.dat') ) } );
  record( op => 'load_wrapper_ok', out => $r[0], died => $r[1], error => $r[2], warn => $r[3],
    node_count => $SLTM::NodeCount );

  SLTM::Clear();
  @r = capture( sub { SLTM->Load( path('bad_pos.dat') ) } );
  my @w = map { ( split /\n/, $_ )[0] } @{ $r[3] };
  record( op => 'load_wrapper_failure', out => $r[0], died => $r[1], error => $r[2], warn_first_lines => \@w,
    node_count => $SLTM::NodeCount );

  SLTM::Clear();
  @r = capture( sub { SLTM->Load( path('missing.dat') ) } );
  record( op => 'load_wrapper_other', died => $r[1], error => $r[2], warn => $r[3] );
}

# ---- FindActiveFollowers ----
sub weighted { my ($set) = @_; [ sort { $a->[0] cmp $b->[0] } map { [ $_->[0]->as_text, $_->[1] ] } @$set ] }
{
  SLTM::Clear();
  my $succ = Mapping::Numeric->create( 'succ', $S::NUMBER );
  my $pred = Mapping::Numeric->create( 'pred', $S::NUMBER );
  my $same = Mapping::Numeric->create( 'same', $S::NUMBER );
  my $odd_succ = Mapping::Numeric->create( 'succ', $S::ODD );
  SLTM::InsertFollowsLink( $S::NUMBER, $succ );
  SLTM::InsertFollowsLink( $S::NUMBER, $pred );
  SLTM::InsertFollowsLink( $S::ODD,    $odd_succ );
  SLTM::InsertISALink( $S::NUMBER, $same );    # not a FOLLOWS link: ignored

  my $three = SInt->new(3);
  my @r = capture( sub { SLTM::FindActiveFollowers($three) } );
  record( op => 'followers_none', died => $r[1], error => $r[2] );
  my $res = SLTM::FindActiveFollowers($three);
  record( op => 'followers_no_categories', result => weighted($res), nodes => state()->{nodes} );

  $three->add_category( $S::NUMBER, SBindings->create( {}, {}, $three ) );
  $res = SLTM::FindActiveFollowers($three);
  record( op => 'followers_number', result => weighted($res), is_not_empty => $res->is_not_empty ? 1 : 0 );

  $three->add_category( $S::ODD, SBindings->create( {}, {}, $three ) );
  $three->add_category( $S::PRIME, SBindings->create( {}, {}, $three ) );
  $res = SLTM::FindActiveFollowers($three);
  record( op => 'followers_three_cats', result => weighted($res), state => state() );

  # A pred of 0 is still an SInt (-1); merge_keys merges equal followers.
  my $zero = SInt->new(0);
  $zero->add_category( $S::NUMBER, SBindings->create( {}, {}, $zero ) );
  SLTM::InsertFollowsLink( $S::NUMBER, Mapping::Numeric->create( 'succ', $S::NUMBER ) );
  $res = SLTM::FindActiveFollowers($zero);
  record( op => 'followers_zero', result => weighted($res) );
}

# Set::Weighted on SInt keys (SInt overloads "" and ne), as FindActiveFollowers returns them.
{
  my $txt = sub { join( ",", map { ( ref( $_->[0] ) ? $_->[0]->as_text : $_->[0] ) . ":$_->[1]" } sort { "$a->[0]" cmp "$b->[0]" } @{ $_[0] } ) };
  my $s = Set::Weighted->new( [ SInt->new(4), 1 ], [ SInt->new(4), 2 ], [ "x", 1 ], [ SInt->new(5), 1 ], [ "SInt(5)", 4 ] );
  $s->merge_keys();
  my $merged = $txt->($s);
  $s = Set::Weighted->new( [ SInt->new(4), 1 ], [ SInt->new(4), 2 ], [ "x", 1 ], [ SInt->new(5), 1 ] );
  $s->delete_key(4);
  my $del_num = $txt->($s);
  $s = Set::Weighted->new( [ SInt->new(4), 1 ], [ "4", 1 ], [ "x", 1 ] );
  $s->delete_key( SInt->new(4) );
  my $del_sint = $txt->($s);
  record( op => 'weighted_sint', merged => $merged, delete_number => $del_num, delete_sint => $del_sint );
}

# ---- FindActiveCategories ----
{
  SLTM::Clear();
  my $three = SInt->new(3);
  my $pure  = $three->get_pure;
  my $res   = SLTM::FindActiveCategories($three);
  record( op => 'categories_none', result => weighted($res), nodes => state()->{nodes} );

  SLTM::InsertISALink( $pure, $S::ODD );
  SLTM::InsertISALink( $pure, $S::PRIME );
  SLTM::InsertFollowsLink( $pure, $S::NUMBER );    # not an IS link
  SLTM::SpikeBy( 40, $S::PRIME );
  $res = SLTM::FindActiveCategories($three);
  record( op => 'categories_some', result => weighted($res) );

  $three->add_category( $S::ODD, SBindings->create( {}, {}, $three ) );
  my @r = capture( sub { $res = SLTM::FindActiveCategories($three) } );
  record( op => 'categories_with_current', died => $r[1], error => $r[2], result => $r[1] ? undef : weighted($res) );
}

# ---- LogActivations / PrintNode ----
{
  SLTM::Clear();
  my $buf = '';
  open my $fh, '>', \$buf;
  local $Global::ActivationsLogHandle = $fh;
  local $Global::Steps_Finished       = 17;
  my @p = map { P($_) } 1 .. 4;
  SLTM::GetMemoryIndex($_) for @p;
  SLTM::SpikeBy( 10, $p[1] );
  SLTM::SpikeBy( 50, $p[3] );
  SLTM::LogActivations();
  $Global::Steps_Finished = 18;
  SLTM::SpikeBy( 30, $p[0] );
  SLTM::LogActivations();
  SLTM::PrintNode( 99, "hello" );
  $Global::Steps_Finished = 19;
  SLTM::LogActivations();
  close $fh;
  record( op => 'log_activations', log => $buf, state => state() );
}

# ---- Print ----
{
  build_small();
  my @r = capture( sub { SLTM::Print() } );
  record( op => 'print_small', out => $r[0], died => $r[1] );
  SLTM::Clear();
  @r = capture( sub { SLTM::Print() } );
  record( op => 'print_empty', out => $r[0] );
}

binmode STDOUT, ':utf8';    # the separators chr(129..133) appear in dumped files
emit();
