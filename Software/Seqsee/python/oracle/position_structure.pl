# Oracle for PositionStructure.pm. Output: tests/golden/position_structure.json
use strict;
use Oracle;
use S;

# Test::Seqsee runs INITIALIZE_for_testing at load, which prints "View: 1!".
BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}
SWorkspace->init( { seq => [ 1, 1, 2, 1, 2, 3, 1, 2, 3, 4 ] } );
my @e = SWorkspace::GetElements();

# Shape of a group: nested lists of left edges (numbers), as __GetPositionStructure
# sees them. 'E' marks a bare element, so the Python fakes can be rebuilt.
sub shape {
  my ($obj) = @_;
  my $ref = ref($obj);
  if ( $ref eq 'Seqsee::Element' ) {
    my $x = SWorkspace::__GetPositionStructure($obj);
    return { E => ( defined($x) ? 0 + $x : undef ) };
  }
  return [ map { shape($_) } @$obj ];
}

sub ps_list { [ map { defined($_) ? "$_" : undef } @{ $_[0] } ] }

my $g12   = Seqsee::Anchored->create( @e[ 1, 2 ] );
my $g345  = Seqsee::Anchored->create( @e[ 3 .. 5 ] );
my $g6_9  = Seqsee::Anchored->create( @e[ 6 .. 9 ] );
my $big   = Seqsee::Anchored->create( $g12, $g345 );
my $g01   = Seqsee::Anchored->create( @e[ 0, 1 ] );
my $mixed = Seqsee::Anchored->create( $g01, $e[2] );
my $deep  = Seqsee::Anchored->create( $big, $g6_9 );
my $loose = Seqsee::Element->create( 7, 0 );    # never inserted: no left edge

my %groups = (
  'e0'           => [ $e[0] ],
  'all_elements' => [@e],
  'e0,g12,g345'  => [ $e[0], $g12, $g345 ],
  'g12,g345'     => [ $g12, $g345 ],
  'g12,g345,g6_9' => [ $g12, $g345, $g6_9 ],
  'big'          => [$big],
  'big,g6_9'     => [ $big, $g6_9 ],
  'mixed'        => [$mixed],
  'deep'         => [$deep],
  'deep,mixed'   => [ $deep, $mixed ],
  'empty'        => [],
  'loose'        => [$loose],
  'g12,loose'    => [ $g12, $loose ],
);
for my $name ( sort keys %groups ) {
  my $group = $groups{$name};
  my $ps    = PositionStructure->Create($group);
  record(
    kind   => 'create',
    name   => $name,
    shape  => shape($group),
    result => ps_list($ps),
    is_ps  => ( ref($ps) eq 'PositionStructure' ? 1 : 0 ),
  );
}
record( kind => 'as_string', name => 'big',
        result => SWorkspace::__GetPositionStructureAsString($big) );

# IsASubsetOf over hand-built structures (Create just blesses an array).
my @lists = (
  [], ['0'], ['1'], [ '0', '1' ], [ '1', '2' ], [ '0', '1', '2' ], [ '2', '1' ],
  [ '[1, 2]', '[3, 4, 5]' ], [ '[3, 4, 5]' ], [ '0', '[1, 2]', '[3, 4, 5]' ],
  [ '1.0', '2' ], [ undef, '1' ], [ '', '1' ], [undef], [''], [ '1', '1' ],
  [ '1', '1', '1' ], [ '1', '2', '1', '2', '3' ], [ '1', '2', '3' ],
);
for my $a (@lists) {
  for my $b (@lists) {
    my $pa = bless [@$a], 'PositionStructure';
    my $pb = bless [@$b], 'PositionStructure';
    my $scalar = $pa->IsASubsetOf($pb);
    my @list   = $pa->IsASubsetOf($pb);
    record(
      kind   => 'subset',
      a      => $a,
      b      => $b,
      scalar => $scalar,
      list   => \@list,
    );
  }
}

# Subset checks between real Created structures.
my @real = map { [ $_, PositionStructure->Create( $groups{$_} ) ] }
  ( 'g12,g345', 'e0,g12,g345', 'g12,g345,g6_9', 'big', 'big,g6_9', 'e0', 'all_elements' );
for my $x (@real) {
  for my $y (@real) {
    record(
      kind   => 'subset_real',
      a      => $x->[0],
      b      => $y->[0],
      scalar => scalar( $x->[1]->IsASubsetOf( $y->[1] ) ),
    );
  }
}

emit();
