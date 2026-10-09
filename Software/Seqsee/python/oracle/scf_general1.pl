# Oracle for Seqsee/SCF_MX/General.pm, first half (item 040): the codelet families
# LookForSimilarGroups, MergeGroups, CleanUpGroup, DoTheSameThing and CreateGroup.
# Output: tests/golden/scf_general1.json
#
# Same scenario style as sthought_sobject.pl: each case is a list of ops and one result per
# op; tests/test_scf_general1.py replays them. e0, e1, ... are the elements of the last
# init; other names come from "gp"/"reln"/"relnf" ops.
#
# "run" calls the family's installed run sub directly (no freshness check), with arguments
# given as [key, spec] pairs (see argval). After a run the oracle records the workspace
# state, the relations and the coderack (sorted: some choices follow hash order).
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine';
use Oracle;
use S;
use PadWalker qw(closed_over);

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

my %h1 = %{ closed_over( \&SWorkspace::__UpdateGroup ) };
my %h2 = %{ closed_over( \&SWorkspace::__DoGroupAddBookkeeping ) };
my ( $LEFT, $RIGHT ) = @h1{qw(%LeftEdge_of %RightEdge_of)};
my ($OBJECTS) = @h2{qw(%Objects)};
die "PadWalker" unless $LEFT and $OBJECTS;

my ( %obj, %name_of );

sub reg {
  my ( $name, $o ) = @_;
  $obj{$name} = $o;
  $name_of{$o} = $name if ref $o;
}

sub nm {
  my ($o) = @_;
  return undef unless defined $o;
  return $o if !ref $o;
  return $name_of{$o} // ( '?' . ( $o->can('as_text') ? $o->as_text : ref($o) ) );
}
sub O { map { $obj{$_} } @_ }

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

sub cat {
  my ($n) = @_;
  my %c = (
    ascending  => $S::ASCENDING,
    descending => $S::DESCENDING,
    sameness   => $S::SAMENESS,
    number     => $S::NUMBER,
    prime      => $S::PRIME,
    odd        => $S::ODD,
    even       => $S::EVEN,
    mountain   => $S::MOUNTAIN,
  );
  return $c{$n} // die "unknown cat $n";
}

sub mapping {
  my ( $name, $catname ) = @_;
  return Mapping::Numeric->create( $name, cat( $catname // 'number' ) );
}

sub argval {
  my ( $kind, @v ) = @{ $_[0] };
  return $obj{ $v[0] }               if $kind eq 'obj';
  return [ O(@v) ]                   if $kind eq 'list';
  return cat( $v[0] )                if $kind eq 'cat';
  return mapping(@v)                 if $kind eq 'map';
  return $obj{ $v[0] }->get_type     if $kind eq 'type';
  return $v[0] eq 'LEFT' ? $DIR::LEFT : $DIR::RIGHT if $kind eq 'dir';
  return $v[0]                       if $kind eq 'val';
  return undef                       if $kind eq 'undef';
  return SCategory::MappingBased->Create( $obj{ $v[0] }->get_type ) if $kind eq 'mbcat';
  die "unknown arg kind $kind";
}

sub summarize {
  my ($v) = @_;
  return [ map { nm($_) } @$v ] if ref($v) eq 'ARRAY';
  return nm($v);
}

sub codelet {
  my ($cl) = @_;
  my $args = $cl->[3];
  return [ $cl->[0], 0 + $cl->[1], [ map { [ $_, summarize( $args->{$_} ) ] } sort keys %$args ] ];
}

sub state {
  my @live = sort { nm($a) cmp nm($b) } values %$OBJECTS;
  return [
    map {
      my $ul = $_->get_underlying_reln;
      [ nm($_), num_or_undef( $LEFT->{$_} ), num_or_undef( $RIGHT->{$_} ),
        [ sort map { $_->as_text } @{ $_->get_categories() } ],
        ( $ul ? $ul->get_rule->get_transform->as_text : undef ),
      ]
    } @live
  ];
}

sub relations {
  return [ sort map { join( ' ', nm( $_->get_first ), nm( $_->get_second ), $_->get_type->as_text ) }
      values %SWorkspace::relations ];
}

sub coderack {
  return [ sort { join( ',', @$a ) cmp join( ',', @$b ) }
      map { my $c = codelet($_); [ $c->[0], $c->[1], map { join( '=', $_->[0], ref $_->[1] ? join( '+', @{ $_->[1] } ) : $_->[1] ) } @{ $c->[2] } ] }
      @SCoderack::CODELETS ];
}

my %OPS = (
  init => sub {
    SLTM->Clear();
    SWorkspace->init( { seq => $_[0] } );
    SWorkspace::__ClearBarLines();
    SCoderack->clear;
    %Global::Feature = ();
    $Global::Steps_Finished = 0;
    %obj = %name_of = ();
    srand(1);
    my @e = SWorkspace::GetElements();
    reg( "e$_", $e[$_] ) for 0 .. $#e;
    return scalar(@e);
  },
  srand    => sub { srand( $_[0] ); return undef },
  rand     => sub { return rand() },
  describe => sub {
    my $r = $obj{ $_[0] }->describe_as( cat( $_[1] ) );
    return defined($r) ? 1 : 0;
  },
  gp => sub {
    my ( $name, @items ) = @_;
    my $g = Seqsee::Anchored->create( O(@items) );
    reg( $name, $g );
    return $g->as_text;
  },
  add    => sub { my @r = SWorkspace->add_group( $obj{ $_[0] } ); return [ map { num_or_undef($_) } @r ] },
  remove => sub { SWorkspace->remove_gp( $obj{ $_[0] } ); return undef },
  reln   => sub {
    my ( $name, $a, $b, $t, $c ) = @_;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => mapping( $t, $c ) } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  relnf => sub {
    my ( $name, $a, $b ) = @_;
    # FindMapping on groups needs active categories: spike $a's, then reseed.
    SLTM::SpikeBy( 100, $_ ) for @{ $obj{$a}->get_categories() };
    srand(1);
    my $t = Seqsee::Object::FindMapping( $obj{$a}, $obj{$b} ) or return undef;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => $t } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  ruleapp => sub {
    my ( $g, $r ) = @_;
    my $ra = $obj{$g}->set_underlying_ruleapp( $obj{$r} );
    return defined($ra) ? ref($ra) : undef;
  },
  spike => sub {
    SLTM::SpikeBy( $_[1], cat( $_[0] ) );
    return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) );
  },
  # Make $_[1] a metonym of $_[0]: $_[0]'s is_a_metonym is set, as SMetonym's new does.
  is_a_metonym => sub { $obj{ $_[0] }->set_is_a_metonym( $obj{ $_[1] } ); return undef },
  state     => sub { return state() },
  relations => sub { return relations() },
  coderack  => sub { return coderack() },
  run       => sub {
    my ( $family, @pairs ) = @_;
    my %args = map { ( $_->[0] => argval( $_->[1] ) ) } @pairs;
    no strict 'refs';
    "Seqsee::SCF::${family}::run"->( undef, \%args );
    return undef;
  },
  # The new objects a run made (live and unnamed), named in left-edge order.
  name_new => sub {
    my ($prefix) = @_;
    my @new = sort { $LEFT->{$a} <=> $LEFT->{$b} or $RIGHT->{$a} <=> $RIGHT->{$b} }
      grep { !exists $name_of{$_} } values %$OBJECTS;
    my $i = 0;
    reg( $prefix . $i++, $_ ) for @new;
    return [ map { $_->as_text } @new ];
  },
  shouldicontinue => sub { return 0 + Seqsee::SCF::FindIfRelated::ShouldIContinue(@_) },
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

my @after = ( ['state'], ['relations'], ['coderack'], ['rand'] );

# --- LookForSimilarGroups ------------------------------------------------------------------
for my $seed ( 1 .. 3 ) {
  scenario(
    "look_for_similar_$seed",
    [ init => [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] ],
    [ gp => 'A', 'e0', 'e1' ],
    [ add => 'A' ],
    [ srand => $seed ],
    [ run => 'LookForSimilarGroups', [ group => [ obj => 'A' ] ] ],
    [ describe => 'A', 'ascending' ],
    [ run => 'LookForSimilarGroups', [ group => [ obj => 'A' ] ] ],
    @after,
    [ gp => 'B', 'e3', 'e4' ],
    [ add => 'B' ],
    [ describe => 'B', 'ascending' ],
    [ srand => $seed ],
    [ run => 'LookForSimilarGroups', [ group => [ obj => 'A' ] ] ],
    @after,
    [ gp => 'C', 'e6', 'e7' ],
    [ add => 'C' ],
    [ describe => 'C', 'ascending' ],
    [ gp => 'D', 'e9', 'e10' ],
    [ add => 'D' ],
    [ describe => 'D', 'ascending' ],
    [ srand => $seed ],
    [ run => 'LookForSimilarGroups', [ group => [ obj => 'A' ] ] ],
    @after,
    [ run => 'LookForSimilarGroups' ],
    [ run => 'CleanUpGroup' ],
    [ run => 'LookForSimilarGroups', [ group => [ obj => 'A' ] ], [ other => [ val => 1 ] ] ],
  );
}

# --- MergeGroups ---------------------------------------------------------------------------
scenario(
  'merge_basic',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'A', 'ascending' ],
  [ describe => 'B', 'ascending' ],
  [ describe => 'C', 'ascending' ],
  [ relnf => 'R1', 'A', 'B' ],
  [ relnf => 'R2', 'B', 'C' ],
  [ gp => 'G1', 'A', 'B' ],
  [ add => 'G1' ],
  [ gp => 'G2', 'B', 'C' ],
  [ add => 'G2' ],
  ['state'],
  [ run => 'MergeGroups', [ a => [ obj => 'G1' ] ], [ b => [ obj => 'G1' ] ] ],
  [ run => 'MergeGroups', [ a => [ obj => 'G1' ] ], [ b => [ obj => 'G2' ] ] ],
  ['state'],
  [ ruleapp => 'G1', 'R1' ],
  [ describe => 'G1', 'ascending' ],
  [ run => 'MergeGroups', [ a => [ obj => 'G1' ] ], [ b => [ obj => 'G2' ] ] ],
  [ name_new => 'M' ],
  @after,
  [ run => 'MergeGroups', [ a => [ obj => 'G1' ] ] ],
);

scenario(
  'merge_reversed_and_dead',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'A', 'ascending' ],
  [ describe => 'B', 'ascending' ],
  [ describe => 'C', 'ascending' ],
  [ relnf => 'R1', 'A', 'B' ],
  [ gp => 'G1', 'A', 'B' ],
  [ add => 'G1' ],
  [ gp => 'G2', 'B', 'C' ],
  [ add => 'G2' ],
  [ ruleapp => 'G2', 'R1' ],
  [ describe => 'G2', 'ascending' ],
  [ run => 'MergeGroups', [ a => [ obj => 'G2' ] ], [ b => [ obj => 'G1' ] ] ],
  [ name_new => 'M' ],
  @after,
  [ gp => 'H', 'e0', 'e1' ],
  [ run => 'MergeGroups', [ a => [ obj => 'H' ] ], [ b => [ obj => 'G1' ] ] ],
  ['state'],
);

scenario(
  'merge_holes',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'A', 'ascending' ],
  [ describe => 'B', 'ascending' ],
  [ relnf => 'R1', 'A', 'B' ],
  [ gp => 'X', 'e0', 'e1' ],
  [ add => 'X' ],
  [ gp => 'Y', 'e4', 'e5' ],
  [ add => 'Y' ],
  [ ruleapp => 'A', 'R1' ],
  [ run => 'MergeGroups', [ a => [ obj => 'A' ] ], [ b => [ obj => 'C' ] ] ],
  [ run => 'MergeGroups', [ a => [ obj => 'X' ] ], [ b => [ obj => 'Y' ] ] ],
  ['state'],
);

# Overlapping element groups: A = e0 e1 e2, B = e2 e3 e4.
scenario(
  'merge_overlap_elements',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e2', 'e3', 'e4' ],
  [ add => 'B' ],
  [ describe => 'A', 'ascending' ],
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ ruleapp => 'A', 'R' ],
  [ run => 'MergeGroups', [ a => [ obj => 'A' ] ], [ b => [ obj => 'B' ] ] ],
  [ name_new => 'M' ],
  @after,
);

# --- CleanUpGroup --------------------------------------------------------------------------
scenario(
  'cleanup',
  [ init => [ 1, 2, 3, 4, 5, 6, 7, 8 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ add => 'A' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ add => 'B' ],
  [ gp => 'X', 'e1', 'e2' ],
  [ add => 'X' ],
  [ gp => 'Y', 'e3', 'e4' ],
  [ add => 'Y' ],
  [ gp => 'Z', 'e5', 'e6' ],
  [ add => 'Z' ],
  [ gp => 'G', 'A', 'B' ],
  [ add => 'G' ],
  ['state'],
  [ gp => 'D', 'e6', 'e7' ],
  [ run => 'CleanUpGroup', [ group => [ obj => 'D' ] ] ],
  ['state'],
  [ run => 'CleanUpGroup', [ group => [ obj => 'G' ] ] ],
  @after,
  [ run => 'CleanUpGroup', [ group => [ obj => 'e0' ] ] ],
  ['state'],
);

# --- DoTheSameThing ------------------------------------------------------------------------
my @dts_setup = (
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ describe => 'A', 'ascending' ],
  [ describe => 'B', 'ascending' ],
  [ relnf => 'R1', 'A', 'B' ],
);

for my $seed ( 1 .. 4 ) {
  scenario(
    "dts_right_$seed",
    [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
    @dts_setup,
    [ srand => $seed ],
    [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ], [ transform => [ type => 'R1' ] ],
      [ direction => [ dir => 'RIGHT' ] ] ],
    [ name_new => 'N' ],
    @after,
    [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ], [ transform => [ type => 'R1' ] ] ],
    [ name_new => 'P' ],
    @after,
  );
}

for my $seed ( 1 .. 4 ) {
  scenario(
    "dts_category_$seed",
    [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 6 ] ],
    @dts_setup,
    [ gp => 'C', 'e6', 'e7', 'e8' ],
    [ add => 'C' ],
    [ describe => 'C', 'ascending' ],
    [ srand => $seed ],
    [ run => 'DoTheSameThing', [ category => [ cat => 'ascending' ] ], [ transform => [ type => 'R1' ] ] ],
    [ name_new => 'N' ],
    @after,
    [ srand => $seed ],
    [ run => 'DoTheSameThing', [ transform => [ type => 'R1' ] ], [ direction => [ dir => 'LEFT' ] ] ],
    [ name_new => 'P' ],
    @after,
    [ run => 'DoTheSameThing', [ category => [ cat => 'descending' ] ], [ transform => [ type => 'R1' ] ] ],
    @after,
  );
}

scenario(
  'dts_left',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'B', 'ascending' ],
  [ describe => 'C', 'ascending' ],
  [ relnf => 'R1', 'B', 'C' ],
  [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'LEFT' ] ] ],
  [ name_new => 'N' ],
  @after,
  [ run => 'DoTheSameThing', [ group => [ obj => 'N0' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'LEFT' ] ] ],
  @after,
);

scenario(
  'dts_misc',
  [ init => [ 1, 2, 3, 2, 3, 4, 7, 8, 9, 3, 4 ] ],
  @dts_setup,
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'C', 'ascending' ],
  # mismatch: 3 4 5 expected, 7 8 9 present.
  [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  # beyond the known elements: 8 9 10 expected, 3 4 present, then nothing.
  [ run => 'DoTheSameThing', [ group => [ obj => 'C' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ gp => 'D', 'e9', 'e10' ],
  [ add => 'D' ],
  [ describe => 'D', 'ascending' ],
  [ run => 'DoTheSameThing', [ group => [ obj => 'D' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  # both group and category.
  [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ], [ category => [ cat => 'ascending' ] ],
    [ transform => [ type => 'R1' ] ] ],
  [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ] ],
  # from the left end, leftward.
  [ run => 'DoTheSameThing', [ group => [ obj => 'A' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'LEFT' ] ] ],
  @after,
);

scenario(
  'dts_numeric',
  [ init => [ 1, 2, 3, 4, 7, 7 ] ],
  [ run => 'DoTheSameThing', [ group => [ obj => 'e0' ] ], [ transform => [ map => 'succ' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ run => 'DoTheSameThing', [ group => [ obj => 'e2' ] ], [ transform => [ map => 'pred' ] ],
    [ direction => [ dir => 'LEFT' ] ] ],
  @after,
  [ run => 'DoTheSameThing', [ group => [ obj => 'e4' ] ], [ transform => [ map => 'same' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ run => 'DoTheSameThing', [ group => [ obj => 'e3' ] ], [ transform => [ map => 'succ', 'even' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ run => 'DoTheSameThing', [ category => [ cat => 'prime' ] ], [ transform => [ map => 'succ' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
);

# A group of groups: the transform maps groups to groups.
scenario(
  'dts_group_of_groups',
  [ init => [ 1, 1, 2, 2, 3, 3, 4, 4 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ add => 'A' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ add => 'B' ],
  [ describe => 'A', 'sameness' ],
  [ describe => 'B', 'sameness' ],
  [ relnf => 'R1', 'A', 'B' ],
  [ run => 'DoTheSameThing', [ group => [ obj => 'B' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  [ name_new => 'N' ],
  @after,
  [ run => 'DoTheSameThing', [ group => [ obj => 'N0' ] ], [ transform => [ type => 'R1' ] ],
    [ direction => [ dir => 'RIGHT' ] ] ],
  [ name_new => 'P' ],
  @after,
);

# --- CreateGroup ---------------------------------------------------------------------------
scenario(
  'create_group',
  [ init => [ 1, 2, 3, 4, 7, 7, 7, 3, 1, 2, 9 ] ],
  [ run => 'CreateGroup', [ items => [ list => 'e0', 'e1', 'e2' ] ] ],
  [ run => 'CreateGroup', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ category => [ cat => 'ascending' ] ],
    [ transform => [ map => 'succ' ] ] ],
  [ run => 'CreateGroup', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ transform => [ cat => 'ascending' ] ] ],
  ['state'],
  [ run => 'CreateGroup', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ transform => [ map => 'succ' ] ] ],
  [ name_new => 'A' ],
  @after,
  # covered now.
  [ run => 'CreateGroup', [ items => [ list => 'e1', 'e2' ] ], [ category => [ cat => 'ascending' ] ] ],
  [ run => 'CreateGroup', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ category => [ cat => 'ascending' ] ] ],
  ['state'],
  [ run => 'CreateGroup', [ items => [ list => 'e4', 'e5', 'e6' ] ], [ category => [ cat => 'sameness' ] ] ],
  [ name_new => 'S' ],
  # describe_as fails.
  [ run => 'CreateGroup', [ items => [ list => 'e7', 'e8', 'e9' ] ], [ category => [ cat => 'ascending' ] ] ],
  ['state'],
  # a numeric transform not over NUMBER: MappingBased category.
  [ run => 'CreateGroup', [ items => [ list => 'e8', 'e9' ] ], [ transform => [ map => 'succ', 'odd' ] ] ],
  [ name_new => 'O' ],
  @after,
  [ run => 'CreateGroup', [ items => [ list => 'e7', 'e8' ] ], [ transform => [ map => 'pred' ] ] ],
  [ name_new => 'D' ],
  @after,
  [ run => 'CreateGroup', [ items => [ list => 'e3' ] ], [ category => [ cat => 'ascending' ] ] ],
  ['state'],
  [ run => 'CreateGroup', [ items => [ list => 'e3', 'e10' ] ], [ category => [ cat => 'ascending' ] ] ],
  ['state'],
);

scenario(
  'create_group_structural',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
  @dts_setup,
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'C', 'ascending' ],
  [ run => 'CreateGroup', [ items => [ list => 'A', 'B', 'C' ] ], [ transform => [ type => 'R1' ] ] ],
  [ name_new => 'G' ],
  @after,
  [ remove => 'G0' ],
  [ run => 'CreateGroup', [ items => [ list => 'A', 'B', 'C' ] ], [ category => [ mbcat => 'R1' ] ] ],
  [ name_new => 'H' ],
  @after,
);

# GetConcreteObject: an item that is a metonym stands for the object it is a metonym of.
scenario(
  'create_group_metonym',
  [ init => [ 1, 2, 3, 4 ] ],
  [ gp => 'X', 'e1', 'e2' ],
  [ add => 'X' ],
  [ gp => 'Y', 'e0', 'e1' ],
  [ is_a_metonym => 'e3', 'X' ],
  [ run => 'CreateGroup', [ items => [ list => 'e0', 'e3' ] ], [ category => [ cat => 'ascending' ] ] ],
  [ name_new => 'M' ],
  @after,
  [ is_a_metonym => 'e2', 'Y' ],
  [ run => 'MergeGroups', [ a => [ obj => 'X' ] ], [ b => [ obj => 'X' ] ] ],
  ['state'],
);

emit();
