# Oracle for SWorkspace.pm, part I (item 031): clear/init/insert_elements, GetElements,
# bar lines and scanning. Output: tests/golden/sworkspace_basic.json
use strict;
use warnings;
no warnings 'uninitialized', 'numeric';
use Oracle;
use S;

# Test::Seqsee runs INITIALIZE_for_testing at load, which prints "View: 1!".
BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    return { class => ref($e), message => '' . ( $e->can('message') ? $e->message : $e ) };
  }
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/\(0x[0-9a-f]+\)/(0x)/g;
  return $e;
}

sub num_or_undef { defined $_[0] ? 0 + $_[0] : undef }

# The observable state of the workspace after an operation.
sub state {
  my @e = SWorkspace::GetElements();
  return {
    count         => 0 + $SWorkspace::ElementCount,
    mags          => [ map { $_->get_mag } @e ],
    edges         => [ map { [ map { num_or_undef($_) } $_->get_edges ] } @e ],
    left_edge_of  => [ map { num_or_undef( SWorkspace::__GetPositionStructure($_) ) } @e ],
    live          => [ map { SWorkspace::__CheckLiveness($_) ? 1 : 0 } @e ],
    supergroups   => [ map { scalar( SWorkspace->GetSuperGroups($_) ) } @e ],
    strengths     => [ map { 0 + $_->get_strength } @e ],
    as_text       => [ map { $_->as_text } @e ],
    ltm_nodes     => 0 + SLTM::GetNodeCount(),
    real_sequence => [@Global::RealSequence],
    initial_terms => num_or_undef($Global::InitialTermCount),
    read_head     => 0 + $SWorkspace::ReadHead,
    relations     => scalar( keys %SWorkspace::relations ),
    elements_var  => scalar(@SWorkspace::elements),
  };
}

# ---- init ----
SLTM->Clear();
SWorkspace->init( { seq => [ 1, 1, 2, 1, 2, 3 ] } );
record( kind => 'init', seq => [ 1, 1, 2, 1, 2, 3 ], state => state() );

SWorkspace->init( { seq => [ 7, 8 ] } );
record( kind => 'init', seq => [ 7, 8 ], state => state() );

SWorkspace->init( { seq => [] } );
record( kind => 'init', seq => [], state => state() );

# init keeps the strings it was given in RealSequence.
SWorkspace->init( { seq => [ '4', '5.0', ' 6' ] } );
record( kind => 'init', seq => [ '4', '5.0', ' 6' ], state => state() );

# init that dies part way: elements before the bad one stay, globals untouched.
@Global::RealSequence = (99);
$Global::InitialTermCount = 42;
my $ok = eval { SWorkspace->init( { seq => [ 3, 'x', 4 ] } ); 1 };
record( kind => 'init_dies', seq => [ 3, 'x', 4 ], ok => $ok ? 1 : 0, error => err($@),
        state => state() );

# ---- clear ----
SWorkspace->init( { seq => [ 1, 2, 3 ] } );
my @old = SWorkspace::GetElements();
$SWorkspace::ReadHead = 5;
%SWorkspace::relations = ( a => 1, b => 2 );
@SWorkspace::elements = ( 1, 2 );
SWorkspace->clear();
record(
  kind          => 'clear',
  state         => state(),
  old_live      => [ map { SWorkspace::__CheckLiveness($_) ? 1 : 0 } @old ],
  old_live_once => [ map { SWorkspace::__CheckLivenessAtSomePoint($_) ? 1 : 0 } @old ],
  old_left_edge => [ map { num_or_undef( SWorkspace::__GetPositionStructure($_) ) } @old ],
  old_edges     => [ map { [ $_->get_edges ] } @old ],
);

# ---- insert_elements ----
SWorkspace->clear();
$Global::Steps_Finished       = 17;
$Global::TimeOfLastNewElement = 3;
$Global::TimeOfNewStructure   = 4;
SWorkspace->insert_elements( 5, 6 );
record( kind => 'insert_times', state => state(),
        last_new_element => $Global::TimeOfLastNewElement,
        new_structure => $Global::TimeOfNewStructure );
$Global::Steps_Finished = 20;
SWorkspace->insert_elements();
record( kind => 'insert_times_empty', state => state(),
        last_new_element => $Global::TimeOfLastNewElement,
        new_structure => $Global::TimeOfNewStructure );
$Global::Steps_Finished = 25;
$ok = eval { SWorkspace->insert_elements( 7, 'y', 8 ); 1 };
record( kind => 'insert_times_dies', state => state(), ok => $ok ? 1 : 0, error => err($@),
        last_new_element => $Global::TimeOfLastNewElement,
        new_structure => $Global::TimeOfNewStructure );
$Global::Steps_Finished = 0;

# Single values: numbers (dispatched as '#', used as is) and strings ('$', int()).
my @values = (
  [ 'num',  3 ],       [ 'num',  3.7 ],     [ 'num',  -2 ],    [ 'num', 2.5 ],
  [ 'num',  0 ],       [ 'num',  3.0 ],     [ 'num',  1e20 ],
  [ 'str',  '3' ],     [ 'str',  '3.7' ],   [ 'str',  ' 3' ],  [ 'str', '3 ' ],
  [ 'str',  '3abc' ],  [ 'str',  'abc' ],   [ 'str',  '' ],    [ 'str', '-2' ],
  [ 'str',  '-2.9' ],  [ 'str',  '1e2' ],   [ 'str',  '0x10' ], [ 'str', 'Inf' ],
  [ 'str',  '0' ],     [ 'str',  '+4' ],    [ 'str', '0 but true' ],
  [ 'undef', undef ],  [ 'array', [1] ],
);
for my $v (@values) {
  my ( $type, $x ) = @$v;
  SWorkspace->clear();
  my $ok = eval { SWorkspace->insert_elements($x); 1 };
  my $e = err($@);
  $e = 'No viable candidate for call to multimethod _insert_element(ARRAY)'
    if $type eq 'array' and $e =~ /No viable candidate/;
  record( kind => 'insert_value', type => $type,
          value => ( ref $x ? undef : $x ), ok => $ok ? 1 : 0, error => $e,
          state => state() );
}

# Inserting Element objects: edges get overwritten, the object itself is stored.
SWorkspace->clear();
my $el  = Seqsee::Element->create( 9, 5 );
my $el2 = Seqsee::Element->create( 4, 0 );
$el2->set_edges( 7, 8 );
SWorkspace->insert_elements( 1, $el, $el2, 2 );
my @e = SWorkspace::GetElements();
record( kind => 'insert_objects', state => state(),
        same_object => [ ( $e[1] == $el ? 1 : 0 ), ( $e[2] == $el2 ? 1 : 0 ) ] );

# The same element object twice.
SWorkspace->clear();
SWorkspace->insert_elements( $el, $el );
record( kind => 'insert_same_twice', state => state() );

# Inserting an element clears rejected extensions.
SWorkspace->clear();
%Global::ExtensionRejectedByUser = ( '3, 4' => 1, '5' => 1 );
SWorkspace->insert_elements(3);
record( kind => 'insert_clears_rejected',
        rejected => [ sort keys %Global::ExtensionRejectedByUser ] );

# ---- scanning ----
SWorkspace->init( { seq => [ 1, 1, 2, 1, 2, 3, 1, 2, 3, 4 ] } );
my @hunts = ( [1], [2], [4], [5], [ 1, 2 ], [ 1, 2, 3 ], [ 2, 3, 4 ], [ 3, 4 ], [ 1, 2, 3, 4 ],
  [ 4, 1 ], [ 1, 1, 2, 1, 2, 3, 1, 2, 3, 4 ], [ 1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 5 ], [],
  [0], [ 4, 0 ], [ '2.0', '3' ] );
for my $h (@hunts) {
  for my $start ( -3, -1, 0, 1, 2, 3, 5, 8, 9, 10, 11, 13 ) {
    my $r = SWorkspace::__ScanRightwardForElements( $start, $h );
    my $l = SWorkspace::__ScanLeftwardForElements( $start, $h );
    my $c;
    my $cok = eval { $c = SWorkspace::__CheckMagnitudesRightwards( $start, $h ); 1 };
    record( kind => 'scan', seq => [ 1, 1, 2, 1, 2, 3, 1, 2, 3, 4 ], start => $start,
            mags => $h, right => num_or_undef($r), left => num_or_undef($l),
            check => num_or_undef($c), check_error => ( $cok ? undef : err($@) ),
            check_next => ( $cok ? undef : [ @{ $@->next_elements } ] ) );
  }
}

# ---- bar lines ----
SWorkspace::__ClearBarLines();
record( kind => 'barlines', ops => [], barlines => [ SWorkspace::GetBarLines() ] );
my @barline_ops = ( [ 5, 2, 9, 2 ], [10], [ '3', 0 ], [] );
my @done;
for my $op (@barline_ops) {
  SWorkspace::__AddBarLines(@$op);
  push @done, $op;
  record( kind => 'barlines', ops => [@done], barlines => [ SWorkspace::GetBarLines() ] );
}
SWorkspace::__ClearBarLines();
record( kind => 'barlines_cleared', barlines => [ SWorkspace::GetBarLines() ] );

my @barline_sets = ( [], [3], [ 0, 4, 8 ], [ 2, 2, 5 ], [ 0, 10 ], [ 1, 2, 3, 4, 5 ] );
for my $set (@barline_sets) {
  SWorkspace::__ClearBarLines();
  SWorkspace::__AddBarLines(@$set);
  for my $index ( -1 .. 11 ) {
    my $l = SWorkspace::__ClosestBarLineToLeftGivenIndex($index);
    my $r = SWorkspace::__ClosestBarLineToRightGivenIndex($index);
    my @ll = SWorkspace::__ClosestBarLineToLeftGivenIndex($index);
    record( kind => 'closest', barlines => $set, index => $index,
            left => num_or_undef($l), right => num_or_undef($r),
            left_list_len => scalar(@ll) );
  }
}

# Crossing check on registered groups of a 10-element workspace.
SWorkspace->init( { seq => [ 1 .. 10 ] } );
@e = SWorkspace::GetElements();
my @spans = ( [ 0, 1 ], [ 0, 3 ], [ 2, 5 ], [ 3, 4 ], [ 4, 7 ], [ 1, 8 ], [ 0, 9 ], [ 5, 5 ],
  [ 2, 3 ], [ 8, 9 ], [ 4, 4 ] );
for my $span (@spans) {
  my ( $l, $r ) = @$span;
  my $gp = $l == $r ? $e[$l] : Seqsee::Anchored->create( @e[ $l .. $r ] );
  SWorkspace->add_group($gp) unless $l == $r;
  for my $set ( @barline_sets, [ 4, 6 ], [ 2, 4, 6 ], [ 4, 8 ], [ 3, 5 ], [ 2, 6 ], [ 0, 5 ] ) {
    SWorkspace::__ClearBarLines();
    SWorkspace::__AddBarLines(@$set);
    my $res = SWorkspace::__CheckIfCrossesBarLinesInappropriately($gp);
    record( kind => 'crosses', left => $l, right => $r, barlines => $set,
            result => num_or_undef($res) );
  }
  SWorkspace->remove_gp($gp) unless $l == $r;
}
SWorkspace::__ClearBarLines();

emit();
