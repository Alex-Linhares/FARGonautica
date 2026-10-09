# Oracle for SWorkspace.pm, part II (item 032): add_group/remove_gp, conflicts, group
# bookkeeping, supergroups and liveness. Output: tests/golden/sworkspace_groups.json
#
# Each case is a scenario: a list of ops and the result of each op. The Python test
# replays the same ops (tests/test_sworkspace_groups.py), so the op language below is
# mirrored there. Objects are referred to by name: e0, e1, ... are the elements of the
# last init; other names are given by "gp"/"reln" ops.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric';
use Oracle;
use S;
use PadWalker qw(closed_over);

# Test::Seqsee runs INITIALIZE_for_testing at load, which prints "View: 1!".
BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

# The workspace's lexical hashes.
my %h1 = %{ closed_over( \&SWorkspace::__UpdateGroup ) };
my %h2 = %{ closed_over( \&SWorkspace::__DoGroupAddBookkeeping ) };
my ( $SUPER, $LEFT, $RIGHT, $SPAN ) =
  @h1{qw(%SuperGroups_of %LeftEdge_of %RightEdge_of %Span_of)};
my ( $OBJECTS, $NONELT, $LIVE_ONCE ) = @h2{qw(%Objects %NonEltObjects %LiveAtSomePoint)};
die "PadWalker" unless $SUPER and $LEFT and $OBJECTS and $LIVE_ONCE;

my ( %obj, %name_of );

sub reg {
  my ( $name, $o ) = @_;
  $obj{$name} = $o;
  $name_of{$o} = $name if ref $o;
}

sub nm {
  my ($o) = @_;
  return undef unless defined $o;
  return $o if $o eq '';
  return $name_of{$o} // ( '?' . ( ref($o) && $o->can('as_text') ? $o->as_text : "$o" ) );
}
sub names { [ sort map { nm($_) } @_ ] }
sub O     { map { $obj{$_} } @_ }

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    return { class => ref($e), message => '' . ( $e->can('message') ? $e->message : $e ) };
  }
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/=HASH\(0x[0-9a-f]+\)/=HASH/g;
  return $e;
}

sub num_or_undef { defined $_[0] ? 0 + $_[0] : undef }

sub dir_name {
  my ($d) = @_;
  for (qw(LEFT RIGHT UNKNOWN NEITHER)) {
    no strict 'refs';
    return $_ if $d eq ${"DIR::$_"};
  }
  return nm($d);
}

sub state {
  my @live = sort { nm($a) cmp nm($b) } values %$OBJECTS;
  return {
    live => [
      map {
        my $o = $_;
        [
          nm($o), num_or_undef( $LEFT->{$o} ), num_or_undef( $RIGHT->{$o} ),
          num_or_undef( $SPAN->{$o} ),
          ( exists $SUPER->{$o} ? names( values %{ $SUPER->{$o} } ) : undef ),
          ( exists $NONELT->{$o} ? 1 : 0 ),
        ]
      } @live
    ],
    counts => [ map { scalar keys %$_ } $OBJECTS, $NONELT, $LEFT, $RIGHT, $SPAN, $SUPER ],
    live_once => names( grep { exists $LIVE_ONCE->{$_} } values %obj ),
    relations => names( values %SWorkspace::relations ),
  };
}

my %OPS = (
  init => sub {
    SLTM->Clear();
    SWorkspace->init( { seq => $_[0] } );
    SWorkspace::__ClearBarLines();
    %Global::Feature = ();
    $Global::Steps_Finished = 0;
    %obj = %name_of = ();
    my @e = SWorkspace::GetElements();
    reg( "e$_", $e[$_] ) for 0 .. $#e;
    return scalar(@e);
  },
  srand   => sub { srand( $_[0] ); return undef },
  feature => sub { $Global::Feature{ $_[0] } = $_[1]; return undef },
  steps   => sub { $Global::Steps_Finished = $_[0]; return undef },
  gp      => sub {
    my ( $name, @items ) = @_;
    my $g = Seqsee::Anchored->create( O(@items) );
    reg( $name, $g );
    return [ $g->as_text, 0 + $g->get_strength ];
  },
  reln => sub {
    my ( $name, $f, $s, $type ) = @_;
    my $r = SRelation->new(
      { first => $obj{$f}, second => $obj{$s}, type => Mapping::Numeric->create( $type, $S::NUMBER ) } );
    reg( $name, $r );
    return undef;
  },
  insert_rel => sub { my $v = $obj{ $_[0] }->insert(); return num_or_undef($v) },
  add_rel    => sub { my @r = SWorkspace->AddRelation( $obj{ $_[0] } ); return [ map { nm($_) } @r ] },
  remove_rel => sub { SWorkspace->RemoveRelation( $obj{ $_[0] } ); return undef },
  has_rel    => sub { return $obj{ $_[0] }->get_relation( $obj{ $_[1] } ) ? 1 : 0 },
  rel_ends   => sub { return scalar keys %SWorkspace::relations_by_ends },
  add        => sub {
    $Global::TimeOfNewStructure = -1;
    my @r = SWorkspace->add_group( $obj{ $_[0] } );
    return [ [ map { num_or_undef($_) } @r ], num_or_undef($Global::TimeOfNewStructure) ];
  },
  add_internal => sub {
    $Global::TimeOfNewStructure = -1;
    my @r = SWorkspace::__AddGroup( $obj{ $_[0] } );
    return [ [ map { num_or_undef($_) } @r ], num_or_undef($Global::TimeOfNewStructure) ];
  },
  remove     => sub { SWorkspace->remove_gp( $obj{ $_[0] } ); return undef },
  delete     => sub { SWorkspace::__DeleteGroup( $obj{ $_[0] } ); return undef },
  live       => sub { return SWorkspace::__CheckLiveness( O(@_) ) ? 1 : 0 },
  live_once  => sub { return SWorkspace::__CheckLivenessAtSomePoint( O(@_) ) ? 1 : 0 },
  grep_live  => sub { return [ map { nm($_) } SWorkspace::__GrepLiveness( O(@_) ) ] },
  diagnose   => sub { return SWorkspace::__CheckLivenessAndDiagnose( O(@_) ) },
  exactly    => sub { return names( SWorkspace::__GetObjectsWithEndsExactly(@_) ) },
  beyond     => sub { return names( SWorkspace::__GetObjectsWithEndsBeyond(@_) ) },
  notbeyond  => sub { return names( SWorkspace::__GetObjectsWithEndsNotBeyond(@_) ) },
  exact_obj  => sub { return nm( scalar SWorkspace::__GetExactObjectIfPresent( $obj{ $_[0] } ) ) },
  partial    => sub { return names( SWorkspace::__GetGroupsThatPartiallyOverlap( $obj{ $_[0] } ) ) },
  sort_lr_left  => sub { return [ map { nm($_) } SWorkspace::__SortLtoRByLeftEdge( O(@_) ) ] },
  sort_rl_left  => sub { return [ map { nm($_) } SWorkspace::__SortRtoLByLeftEdge( O(@_) ) ] },
  sort_lr_right => sub { return [ map { nm($_) } SWorkspace::__SortLtoRByRightEdge( O(@_) ) ] },
  sort_rl_right => sub { return [ map { nm($_) } SWorkspace::__SortRtoLByRightEdge( O(@_) ) ] },
  conflict2 => sub { return num_or_undef( SWorkspace::__CheckTwoGroupsForConflict( O(@_) ) ) },
  in_conflict => sub {
    my @r = SWorkspace->AreGroupsInConflict( O(@_) );
    return [ map { num_or_undef($_) } @r ];
  },
  find_conflicts => sub {
    my $c = SWorkspace::__FindGroupsConflictingWith( $obj{ $_[0] } );
    return {
      challenger  => nm( $c->challenger ),
      exact       => nm( $c->exact_conflict ),
      overlapping => names( @{ $c->overlapping_conflicts } ),
      bool        => ( $c ? 1 : 0 ),
    };
  },
  find_conflicts_list => sub {
    my ( $exact, @rest ) = SWorkspace->FindGroupsConflictingWith( $obj{ $_[0] } );
    return [ nm($exact), names(@rest) ];
  },
  direction => sub { return dir_name( SWorkspace::__FindObjectSetDirection( O(@_) ) ) },
  holes     => sub { return num_or_undef( SWorkspace::__AreThereHolesOrOverlap( O(@_) ) ) },
  sanity    => sub { return num_or_undef( SWorkspace::__GroupAddSanityCheck( O(@_) ) ) },
  bookkeep  => sub { SWorkspace::__DoGroupAddBookkeeping( $obj{ $_[0] } ); return undef },
  update    => sub { SWorkspace::__UpdateGroup( $obj{ $_[0] } ); return undef },
  rm_super  => sub { SWorkspace::__RemoveFromSupergroups_of( O(@_) ); return undef },
  supergroups => sub { return names( SWorkspace->GetSuperGroups( $obj{ $_[0] } ) ) },
  supersuper  => sub { return num_or_undef( SWorkspace->AreThereAnySuperSuperGroups( $obj{ $_[0] } ) ) },
  groups => sub {
    my @g = SWorkspace->GetGroups();
    return [ [ map { 0 + $SPAN->{$_} } @g ], names(@g) ];
  },
  overlapping_sets => sub {
    return [ sort { join( ',', @$a ) cmp join( ',', @$b ) }
        map { names(@$_) } SWorkspace::__FindSetsOfObjectsWithOverlappingSubgroups( O(@_) ) ];
  },
  barlines        => sub { SWorkspace::__AddBarLines(@_); return undef },
  remove_crossing => sub { SWorkspace::__RemoveGroupsCrossingBarLines(); return undef },
  fight => sub {
    return num_or_undef( SWorkspace->FightUntoDeath( { challenger => $obj{ $_[0] }, incumbent => $obj{ $_[1] } } ) );
  },
  lock         => sub { $obj{ $_[0] }->set_is_locked_against_deletion(1); return undef },
  set_strength => sub { $obj{ $_[0] }->set_strength( $_[1] ); return undef },
  state        => sub { return state() },
);

sub scenario {
  my ( $name, @ops ) = @_;
  my @results;
  for my $op (@ops) {
    my ( $kind, @args ) = @$op;
    my $code = $OPS{$kind} or die "unknown op $kind";
    my $value;
    my $ok = eval { $value = $code->(@args); 1 };
    push @results, $ok ? { value => $value } : { error => err($@) };
  }
  record( scenario => $name, ops => \@ops, results => \@results );
}

my @SEQ8 = ( 1 .. 8 );

scenario(
  'add_remove_supergroups',
  [ init => \@SEQ8 ],
  [ steps => 17 ],
  [ gp => 'A', 'e0', 'e1' ],
  [ add => 'A' ],
  ['state'],
  [ gp => 'A2', 'e0', 'e1' ],
  [ add => 'A2' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ add_internal => 'B' ],
  [ gp => 'C', 'A', 'B' ],
  [ add => 'C' ],
  ['state'],
  [ supergroups => 'A' ],
  [ supergroups => 'e0' ],
  [ supergroups => 'C' ],
  [ supersuper  => 'e0' ],
  [ supersuper  => 'A' ],
  [ supersuper  => 'C' ],
  ['groups'],
  [ remove => 'A' ],
  ['state'],
  [ live      => 'A' ],
  [ live      => 'B' ],
  [ live      => 'C' ],
  [ live      => 'B', 'e0' ],
  [ live      => 'B', 'C' ],
  ['live'],
  [ live_once => 'A', 'C' ],
  [ live_once => 'A2' ],
  [ supergroups => 'B' ],
  [ supersuper  => 'e0' ],
  [ add => 'C' ],
  [ remove => 'C' ],
  [ remove => 'C' ],
  ['state'],
);

scenario(
  'groups_order',
  [ init => \@SEQ8 ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e2', 'e3', 'e4' ],
  [ gp => 'C', 'e5', 'e6' ],
  [ gp => 'D', 'A', 'B', 'C' ],
  [ gp => 'E', 'e7' ],
  ['groups'],
  [ add => 'A' ],
  [ add => 'B' ],
  [ add => 'C' ],
  [ add => 'D' ],
  [ add => 'E' ],
  ['groups'],
  ['state'],
  [ supersuper => 'A' ],
  [ supersuper => 'e0' ],
  [ supersuper => 'e5' ],
  [ delete => 'B' ],
  ['groups'],
  ['state'],
);

scenario(
  'conflicts',
  [ init => \@SEQ8 ],
  [ gp => 'A', 'e0', 'e1' ],
  [ add => 'A' ],
  [ gp => 'B', 'e0', 'e1', 'e2' ],
  [ gp => 'A3', 'e0', 'e1' ],
  [ gp => 'D', 'e1', 'e2' ],
  [ gp => 'F', 'e2', 'e3' ],
  [ conflict2 => 'B', 'A' ],
  [ conflict2 => 'A', 'B' ],
  [ conflict2 => 'A', 'A' ],
  [ conflict2 => 'B', 'B' ],
  [ conflict2 => 'A3', 'A' ],
  [ conflict2 => 'D', 'A' ],
  [ conflict2 => 'F', 'A' ],
  [ conflict2 => 'e0', 'A' ],
  [ conflict2 => 'A', 'e0' ],
  [ conflict2 => 'B', 'e0' ],
  [ in_conflict => 'B', 'A' ],
  [ in_conflict => 'A', 'B' ],
  [ in_conflict => 'D', 'A' ],
  [ find_conflicts => 'B' ],
  [ find_conflicts => 'A' ],
  [ find_conflicts => 'A3' ],
  [ find_conflicts => 'D' ],
  [ find_conflicts => 'F' ],
  [ find_conflicts => 'e0' ],
  [ find_conflicts_list => 'B' ],
  [ find_conflicts_list => 'A' ],
  [ find_conflicts_list => 'A3' ],
  [ find_conflicts_list => 'D' ],
  [ exact_obj => 'A3' ],
  [ exact_obj => 'A' ],
  [ exact_obj => 'B' ],
  [ exact_obj => 'e1' ],
  [ partial => 'D' ],
  [ partial => 'B' ],
  [ partial => 'F' ],
  [ exactly => 0, 1 ],
  [ exactly => 0, undef ],
  [ exactly => undef, 1 ],
  [ exactly => 1, undef ],
  [ exactly => undef, undef ],
  [ exactly => 5, 4 ],
  [ beyond => 1, 1 ],
  [ beyond => 0, 0 ],
  [ beyond => undef, 1 ],
  [ beyond => 3, undef ],
  [ notbeyond => 0, 2 ],
  [ notbeyond => 1, undef ],
  [ notbeyond => undef, 0 ],
  [ notbeyond => undef, undef ],
  [ feature => 'NoGpOverlap', 1 ],
  [ find_conflicts => 'D' ],
  [ find_conflicts => 'F' ],
  [ find_conflicts_list => 'D' ],
  [ add => 'A3' ],
  [ lock => 'A' ],
  [ add => 'B' ],
  [ add => 'D' ],
  [ add => 'F' ],
  ['state'],
);

# Two groups with the same span but different structure: the other one shows up twice.
scenario(
  'same_span_conflicts',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ gp => 'C', 'A', 'B' ],
  [ gp => 'D', 'e0', 'e1', 'e2', 'e3' ],
  [ add => 'A' ],
  [ add => 'B' ],
  [ add => 'C' ],
  [ find_conflicts => 'D' ],
  [ find_conflicts_list => 'D' ],
  [ conflict2 => 'D', 'C' ],
  [ conflict2 => 'C', 'A' ],
  [ conflict2 => 'D', 'A' ],
  [ lock => 'C' ],
  [ set_strength => 'A', 0 ],
  [ set_strength => 'B', 0 ],
  [ add => 'D' ],
  ['state'],
  [ gp => 'G', 'A', 'e2', 'e3' ],
  [ conflict2 => 'G', 'C' ],
  [ conflict2 => 'G', 'A' ],
  [ find_conflicts => 'G' ],
  [ gp => 'H', 'e1', 'e2', 'e3', 'e4' ],
  [ conflict2 => 'H', 'C' ],
  [ conflict2 => 'H', 'B' ],
  [ find_conflicts => 'H' ],
);

# Seeded fights: a challenger against one incumbent.
for my $seed ( 1 .. 8 ) {
  scenario(
    "seeded_add_$seed",
    [ init => [ 1 .. 6 ] ],
    [ gp => 'A', 'e0', 'e1' ],
    [ add => 'A' ],
    [ gp => 'B', 'e0', 'e1', 'e2' ],
    [ srand => $seed ],
    [ add => 'B' ],
    [ live => 'A' ],
    [ live => 'B' ],
  );
  scenario(
    "seeded_partial_$seed",
    [ init => [ 1 .. 6 ] ],
    [ feature => 'NoGpOverlap', 1 ],
    [ gp => 'A', 'e0', 'e1' ],
    [ add => 'A' ],
    [ gp => 'D', 'e1', 'e2' ],
    [ srand => $seed ],
    [ add => 'D' ],
    ['state'],
  );
}

scenario(
  'fights',
  [ init => [ 1 .. 6 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e0', 'e1', 'e2' ],
  [ gp => 'C', 'e3', 'e4' ],
  [ fight => 'B', 'A' ],
  [ add => 'A' ],
  [ add => 'C' ],
  [ srand => 3 ],
  [ fight => 'B', 'A' ],
  [ fight => 'B', 'A' ],
  [ fight => 'B', 'A' ],
  [ fight => 'B', 'A' ],
  [ live => 'A' ],
  [ add => 'A' ],
  [ lock => 'A' ],
  [ fight => 'B', 'A' ],
  [ set_strength => 'B', 0 ],
  [ set_strength => 'C', 0 ],
  [ fight => 'B', 'C' ],
  [ set_strength => 'B', 50 ],
  [ srand => 5 ],
  [ fight => 'B', 'C' ],
  [ live => 'C' ],
  [ fight => 'B', 'C' ],
);

scenario(
  'liveness',
  [ init => [ 5, 6, 7, 8, 9 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ gp => 'N', 'e3', 'e4' ],
  [ add => 'A' ],
  [ add => 'B' ],
  [ diagnose => 'A', 'B', 'e4' ],
  ['diagnose'],
  [ diagnose => 'N' ],
  [ diagnose => 'A', 'N', 'B' ],
  [ remove => 'B' ],
  [ diagnose => 'B' ],
  [ diagnose => 'N', 'B' ],
  [ grep_live => 'A', 'B', 'N', 'e0', 'A' ],
  ['grep_live'],
  [ live_once => 'A', 'B' ],
  [ live_once => 'N' ],
  [ live_once => 'e4' ],
  [ add => 'B' ],
  [ sort_lr_left  => 'B', 'e4', 'A', 'e2', 'e0' ],
  [ sort_rl_left  => 'B', 'e4', 'A', 'e2', 'e0' ],
  [ sort_lr_right => 'B', 'e4', 'A', 'e1', 'e3' ],
  [ sort_rl_right => 'B', 'e4', 'A', 'e1', 'e3' ],
  [ sort_lr_left  => 'A', 'e0' ],
  [ sort_lr_left  => 'e0', 'A' ],
  [ sort_rl_left  => 'A', 'e0' ],
  [ sort_rl_left  => 'e0', 'A' ],
  [ sort_lr_right => 'B', 'e3' ],
  [ sort_rl_right => 'e3', 'B' ],
  ['sort_lr_left'],
  [ sort_lr_left  => 'A', 'N' ],
  [ sort_rl_right => 'N' ],
  [ direction => 'A', 'B' ],
  [ direction => 'B', 'A' ],
  [ direction => 'A', 'e1', 'B' ],
  [ direction => 'e0', 'A' ],
  [ direction => 'A', 'B', 'e0' ],
  [ direction => 'e4', 'B', 'A' ],
  [ direction => 'B', 'e4', 'A' ],
  [ direction => 'A' ],
  ['direction'],
  [ direction => 'A', 'N' ],
  [ holes => 'A', 'B' ],
  [ holes => 'B', 'A' ],
  [ holes => 'A', 'B', 'e4' ],
  [ holes => 'A', 'e3' ],
  [ holes => 'A', 'e1' ],
  [ holes => 'e0', 'A' ],
  [ holes => 'e4', 'e3', 'e2' ],
  [ holes => 'e4', 'e2' ],
  [ holes => 'e4', 'B', 'e1', 'e0' ],
  [ holes => 'A', 'B', 'e0' ],
  [ holes => 'A' ],
  [ holes => 'A', 'N' ],
  [ sanity => 'A', 'B' ],
  ['sanity'],
  [ sanity => 'N' ],
);

scenario(
  'bookkeeping',
  [ init => [ 1, 2, 3, 4 ] ],
  [ steps => 9 ],
  [ gp => 'A', 'e0', 'e1' ],
  [ bookkeep => 'A' ],
  ['state'],
  [ gp => 'A2', 'e0', 'e1' ],
  [ bookkeep => 'A2' ],
  ['state'],
  [ supergroups => 'e0' ],
  [ update => 'A' ],
  [ gp => 'C', 'A', 'e2' ],
  [ add => 'C' ],
  [ update => 'A' ],
  [ supergroups => 'A' ],
  [ rm_super => 'A', 'C' ],
  [ supergroups => 'A' ],
  [ update => 'A' ],
  [ rm_super => 'A', 'C' ],
  [ rm_super => 'e3', 'C' ],
  ['state'],
  [ update => 'C' ],
  [ supergroups => 'e2' ],
  [ delete => 'A2' ],
  [ supergroups => 'e0' ],
  [ delete => 'A' ],
  ['state'],
  [ delete => 'A' ],
  ['state'],
  [ bookkeep => 'A' ],
  ['state'],
);

scenario(
  'relations',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ add => 'A' ],
  [ add => 'B' ],
  [ reln => 'R', 'A', 'B', 'succ' ],
  [ insert_rel => 'R' ],
  [ reln => 'Q', 'e4', 'e5', 'succ' ],
  [ insert_rel => 'Q' ],
  [ reln => 'S', 'e3', 'e4', 'succ' ],
  [ insert_rel => 'S' ],
  ['rel_ends'],
  [ has_rel => 'B', 'A' ],
  [ has_rel => 'e4', 'e3' ],
  ['state'],
  [ reln => 'R2', 'A', 'B', 'succ' ],
  [ add_rel => 'R2' ],
  ['rel_ends'],
  [ remove => 'A' ],
  [ has_rel => 'B', 'A' ],
  ['rel_ends'],
  ['state'],
  [ add_rel => 'R2' ],
  ['rel_ends'],
  ['state'],
  [ remove_rel => 'R2' ],
  [ remove_rel => 'R2' ],
  ['rel_ends'],
  [ remove_rel => 'Q' ],
  [ has_rel => 'e5', 'e4' ],
  ['state'],
  [ gp => 'G', 'e3', 'e4' ],
  [ add => 'G' ],
  [ reln => 'T', 'G', 'e5', 'succ' ],
  [ insert_rel => 'T' ],
  [ delete => 'G' ],
  [ has_rel => 'e5', 'G' ],
  [ has_rel => 'e4', 'e3' ],
  ['state'],
);

scenario(
  'bar_lines',
  [ init => \@SEQ8 ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ gp => 'C', 'e1', 'e2' ],
  [ gp => 'D', 'A', 'B' ],
  [ gp => 'F', 'e4', 'e5' ],
  [ gp => 'H', 'e5', 'e6', 'e7' ],
  [ add => 'A' ],
  [ add => 'B' ],
  [ add => 'C' ],
  [ add => 'D' ],
  [ add => 'F' ],
  [ add => 'H' ],
  ['remove_crossing'],
  ['state'],
  [ barlines => 2 ],
  ['remove_crossing'],
  ['state'],
  [ barlines => 4, 6 ],
  ['remove_crossing'],
  ['state'],
  [ add => 'D' ],
  [ add => 'C' ],
  [ barlines => 0 ],
  ['remove_crossing'],
  ['state'],
);

scenario(
  'overlapping_sets',
  [ init => \@SEQ8 ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e1', 'e2' ],
  [ gp => 'C', 'e2', 'e3' ],
  [ gp => 'D', 'e5', 'e6' ],
  [ gp => 'E', 'A', 'C' ],
  [ gp => 'F', 'A', 'e2', 'e3' ],
  [ overlapping_sets => 'A', 'B', 'C', 'D' ],
  [ overlapping_sets => 'A', 'D' ],
  [ overlapping_sets => 'E', 'F', 'A', 'C' ],
  [ overlapping_sets => 'A', 'A' ],
  ['overlapping_sets'],
);

emit();
