# Oracle for SThought.pm, SThought/SCat.pm and SThought/Relations.pm (item 037).
# Output: tests/golden/sthought.json
#
# Same scenario style as sworkspace_rest.pl: each case is a list of ops and one result per
# op; tests/test_sthought.py replays them. e0, e1, ... are the elements of the last init;
# other names come from "gp"/"reln"/"create" ops.
#
# Cores are given as specs: [obj => NAME], [cat => NAME] ("interlaced3" is
# SCategory::Interlaced->Create(3)), [mapping => NAME], [str => S] or [undef].
#
# Hash order: SThought::SCat's actions come from `values %Objects` and from a hash of
# subgroups, so they are recorded sorted, with the codelet's argument names sorted too.
# SThought::SRelation's actions have a fixed order and its random draws match draw for draw.
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
  return SCategory::Interlaced->Create($1) if $n =~ /^interlaced(\d+)$/;
  my %c = (
    ascending  => $S::ASCENDING,
    descending => $S::DESCENDING,
    sameness   => $S::SAMENESS,
    number     => $S::NUMBER,
  );
  return $c{$n} // die "unknown cat $n";
}

sub core {
  my ( $kind, $v ) = @{ $_[0] };
  return $obj{$v}                                  if $kind eq 'obj';
  return cat($v)                                   if $kind eq 'cat';
  return Mapping::Numeric->create( $v, $S::NUMBER ) if $kind eq 'mapping';
  return $v                                        if $kind eq 'str';
  return undef                                     if $kind eq 'undef';
  die "unknown core kind $kind";
}

sub summarize {
  my ($v) = @_;
  return [ map { nm($_) } @$v ] if ref($v) eq 'ARRAY';
  return nm($v);
}

sub codelet {
  my ($cl) = @_;
  my $args = $cl->[3];
  return [ ref($cl), $cl->[0], 0 + $cl->[1], [ map { [ $_, summarize( $args->{$_} ) ] } sort keys %$args ] ];
}

sub state {
  my @live = sort { nm($a) cmp nm($b) } values %$OBJECTS;
  return [
    map {
      [ nm($_), num_or_undef( $LEFT->{$_} ), num_or_undef( $RIGHT->{$_} ),
        [ sort map { $_->as_text } @{ $_->get_categories() } ] ]
    } @live
  ];
}

my %OPS = (
  init => sub {
    SLTM->Clear();
    SWorkspace->init( { seq => $_[0] } );
    SWorkspace::__ClearBarLines();
    %Global::Feature = ();
    $Global::Steps_Finished = 0;
    %obj = %name_of = ();
    srand(1);
    my @e = SWorkspace::GetElements();
    reg( "e$_", $e[$_] ) for 0 .. $#e;
    return scalar(@e);
  },
  srand   => sub { srand( $_[0] ); return undef },
  rand    => sub { return rand() },
  feature => sub { $Global::Feature{ $_[0] } = $_[1]; return undef },
  gp      => sub {
    my ( $name, @items ) = @_;
    my $g = Seqsee::Anchored->create( O(@items) );
    reg( $name, $g );
    return $g->as_text;
  },
  add    => sub { my @r = SWorkspace->add_group( $obj{ $_[0] } ); return [ map { num_or_undef($_) } @r ] },
  addcat => sub {
    $obj{ $_[0] }->add_category( cat( $_[1] ), SBindings->create( {}, {}, $obj{ $_[0] } ) );
    return undef;
  },
  reln => sub {
    my ( $name, $a, $b, $t ) = @_;
    my $r = SRelation->new(
      { first => $obj{$a}, second => $obj{$b}, type => Mapping::Numeric->create( $t, $S::NUMBER ) } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  spike => sub {
    SLTM::SpikeBy( $_[1], cat( $_[0] ) );
    return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) );
  },
  activation => sub {
    my $c = $_[0] eq 'type' ? $obj{ $_[1] }->get_type : cat( $_[1] );
    return 0 + SLTM::GetRealActivationsForOneConcept($c);
  },
  state => sub { return state() },

  # SThought
  create => sub {
    my ( $name, $spec ) = @_;
    my $t = SThought->create( core($spec) );
    my $same = $name_of{$t};
    reg( $name, $t );
    return [ ref($t), $t->as_text, $same, nm( $t->core ) ];
  },
  create_list => sub {
    my ( $name, $spec ) = @_;
    my ($t) = SThought->create( core($spec) );
    my $same = $name_of{$t};
    reg( $name, $t );
    return [ ref($t), $t->as_text, $same ];
  },
  new => sub {
    my ( $class, $spec ) = @_;
    my $t = $class->new( defined $spec ? { core => core($spec) } : {} );
    return [ ref($t), $t->as_text, defined( $t->stored_fringe ) ? 1 : 0 ];
  },
  stored_fringe => sub {
    my ( $name, @v ) = @_;
    $obj{$name}->stored_fringe(@v) if @v;
    return $obj{$name}->stored_fringe;
  },
  fringe => sub {
    return [ map { [ nm( $_->[0] ), 0 + $_->[1] ] } @{ $obj{ $_[0] }->get_fringe } ];
  },
  actions => sub {
    my @a = $obj{ $_[0] }->get_actions;
    return [ map { codelet($_) } @a ];
  },
  actions_sorted => sub {
    my @a = map { codelet($_) } $obj{ $_[0] }->get_actions;
    for (@a) { $_->[3] = [ map { [ 'arg', $_->[1] ] } @{ $_->[3] } ]; $_->[3] = [ sort { $a->[1] cmp $b->[1] } @{ $_->[3] } ]; }
    return [ sort { join( ',', map { $_->[1] } @{ $a->[3] } ) cmp join( ',', map { $_->[1] } @{ $b->[3] } ) } @a ];
  },
  schedule => sub { $obj{ $_[0] }->schedule; return undef },
  force    => sub { $obj{ $_[0] }->force_to_be_next_runnable; return undef },
  name     => sub { no strict 'refs'; return ${"$_[0]::NAME"} },
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

scenario(
  'create',
  [ init => [ 1 .. 6 ] ],
  [ name => 'SThought::SCat' ],
  [ name => 'SThought::SRelation' ],
  [ create => 't_undef', ['undef'] ],
  [ create => 't_foo',   [ str => 'Foo' ] ],
  [ create => 't_5',     [ str => 5 ] ],
  [ create => 't_0',     [ str => '0' ] ],
  [ create => 't_empty', [ str => '' ] ],
  [ create => 't_map',   [ mapping => 'succ' ] ],
  [ create => 'asc1',    [ cat => 'ascending' ] ],
  [ create => 'asc2',    [ cat => 'ascending' ] ],
  [ create_list => 'asc3', [ cat => 'ascending' ] ],
  [ create_list => 'asc4', [ cat => 'ascending' ] ],
  [ create => 'desc',    [ cat => 'descending' ] ],
  [ create => 'num',     [ cat => 'number' ] ],
  [ create => 'il3',     [ cat => 'interlaced3' ] ],
  [ gp => 'A', 'e1', 'e2' ],
  [ add => 'A' ],
  [ reln => 'R', 'e0', 'e3', 'succ' ],
  [ create => 'tr',  [ obj => 'R' ] ],
  [ create => 'tr2', [ obj => 'R' ] ],
  [ new => 'SThought::SCat' ],
  [ new => 'SThought::SRelation' ],
  [ new => 'SThought::SCat', [ cat => 'sameness' ] ],
  [ new => 'SThought::SRelation', [ obj => 'R' ] ],
  [ stored_fringe => 'asc1' ],
  [ stored_fringe => 'asc1', 7 ],
  [ stored_fringe => 'asc2' ],
  [ stored_fringe => 'asc3' ],
  [ schedule => 'asc1' ],
  [ force    => 'asc1' ],
  [ schedule => 'tr' ],
  [ force    => 'tr' ],
);

scenario(
  'scat_fringe_and_actions',
  [ init => [ 1 .. 8 ] ],
  [ create => 'asc', [ cat => 'ascending' ] ],
  [ create => 'il',  [ cat => 'interlaced2' ] ],
  [ fringe => 'asc' ],
  [ fringe => 'il' ],
  [ actions => 'asc' ],
  [ actions => 'il' ],
  [ gp => 'A', 'e1', 'e2' ],
  [ add => 'A' ],
  [ addcat => 'A', 'ascending' ],
  [ actions => 'asc' ],
  [ gp => 'S1', 'A', 'e3' ],
  [ add => 'S1' ],
  [ gp => 'S2', 'e0', 'A' ],
  [ add => 'S2' ],
  [ addcat => 'S1', 'ascending' ],
  [ actions => 'asc' ],
  [ addcat => 'S2', 'ascending' ],
  ['state'],
  [ actions_sorted => 'asc' ],
  [ addcat => 'S1', 'interlaced2' ],
  [ addcat => 'S2', 'interlaced2' ],
  [ actions => 'il' ],
  [ create => 'desc', [ cat => 'descending' ] ],
  [ actions => 'desc' ],
  ['rand'],
);

scenario(
  'scat_two_sets',
  [ init => [ 1 .. 10 ] ],
  [ gp => 'A', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'S1', 'A', 'e3' ],
  [ add => 'S1' ],
  [ gp => 'S2', 'e0', 'A' ],
  [ add => 'S2' ],
  [ gp => 'B', 'e6', 'e7' ],
  [ add => 'B' ],
  [ gp => 'T1', 'B', 'e8' ],
  [ add => 'T1' ],
  [ gp => 'T2', 'e5', 'B' ],
  [ add => 'T2' ],
  [ addcat => 'S1', 'sameness' ],
  [ addcat => 'S2', 'sameness' ],
  [ addcat => 'T1', 'sameness' ],
  [ addcat => 'T2', 'sameness' ],
  [ addcat => 'S1', 'descending' ],
  ['state'],
  [ create => 'same', [ cat => 'sameness' ] ],
  [ actions_sorted => 'same' ],
  [ create => 'desc', [ cat => 'descending' ] ],
  [ actions => 'desc' ],
);

scenario(
  'relation_contiguous_sameness',
  [ init => [ 1, 1, 2, 3 ] ],
  [ reln => 'R', 'e0', 'e1', 'same' ],
  [ create => 't', [ obj => 'R' ] ],
  [ fringe => 't' ],
  [ activation => 'type', 'R' ],
  [ actions => 't' ],
  [ activation => 'type', 'R' ],
  [ actions => 't' ],
  [ activation => 'type', 'R' ],
  ['state'],
  ['rand'],
);

scenario(
  'relation_contiguous_succ',
  [ init => [ 1, 2, 3, 4, 5 ] ],
  [ reln => 'R', 'e1', 'e2', 'succ' ],
  [ create => 't', [ obj => 'R' ] ],
  [ fringe => 't' ],
  [ actions => 't' ],
  [ gp => 'G', 'e3', 'e4' ],
  [ add => 'G' ],
  [ actions => 't' ],
  [ gp => 'H', 'e0', 'e1', 'e2' ],
  [ add => 'H' ],
  [ actions => 't' ],
  [ reln => 'R2', 'e3', 'e4', 'pred' ],
  [ create => 't2', [ obj => 'R2' ] ],
  [ actions => 't2' ],
  [ reln => 'Rl', 'e2', 'e1', 'pred' ],
  [ create => 'tl', [ obj => 'Rl' ] ],
  [ fringe => 'tl' ],
  [ actions => 'tl' ],
  ['state'],
  ['rand'],
);

# Relation between groups, contiguous: ends are groups.
scenario(
  'relation_groups',
  [ init => [ 1, 2, 1, 2, 1, 2 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ add => 'A' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ add => 'B' ],
  [ gp => 'C', 'e4', 'e5' ],
  [ add => 'C' ],
  [ reln => 'R', 'A', 'B', 'same' ],
  [ create => 't', [ obj => 'R' ] ],
  [ fringe => 't' ],
  [ actions => 't' ],
  [ reln => 'R2', 'A', 'C', 'same' ],
  [ create => 't2', [ obj => 'R2' ] ],
  [ actions => 't2' ],
  ['state'],
  ['rand'],
);

# Ends that are not in the workspace.
scenario(
  'relation_dead_ends',
  [ init => [ 1, 2, 3, 4, 5 ] ],
  [ gp => 'A', 'e0', 'e1' ],
  [ gp => 'B', 'e3', 'e4' ],
  [ reln => 'R', 'A', 'B', 'same' ],
  [ create => 't', [ obj => 'R' ] ],
  [ actions => 't' ],
);

# Gaps: the ad hoc Interlaced category.
my @GAP = (
  [ init => [ 1, 7, 8, 1, 9, 10, 1 ] ],
  [ reln => 'R', 'e0', 'e3', 'same' ],
  [ create => 't', [ obj => 'R' ] ],
);

scenario(
  'relation_gap_inactive',
  @GAP,
  [ activation => 'cat', 'interlaced3' ],
  [ actions => 't' ],
  [ activation => 'cat', 'interlaced3' ],
  ['state'],
  ['rand'],
);

for my $seed ( 1 .. 8 ) {
  scenario(
    "relation_gap_active_$seed",
    @GAP,
    [ spike => 'interlaced3', 100 ],
    [ spike => 'interlaced3', 100 ],
    [ spike => 'interlaced3', 100 ],
    [ srand => $seed ],
    [ actions => 't' ],
    [ activation => 'cat', 'interlaced3' ],
    ['state'],
    ['rand'],
    [ actions => 't' ],
    ['state'],
    ['rand'],
  );
}

for my $seed ( 1 .. 3 ) {
  scenario(
    "relation_gap_no_interlaced_$seed",
    @GAP,
    [ spike => 'interlaced3', 100 ],
    [ spike => 'interlaced3', 100 ],
    [ spike => 'interlaced3', 100 ],
    [ feature => 'NoInterlaced', 1 ],
    [ srand => $seed ],
    [ actions => 't' ],
    ['state'],
    ['rand'],
  );
}

# Overshoot: the intervening objects can't tile the gap exactly, so there is no ad hoc group
# (and no draw), however active the category. M only counts once it has a (non-ad-hoc)
# category; before that the gap is tiled by elements.
scenario(
  'relation_gap_overshoot',
  [ init => [ 1, 7, 8, 1, 9 ] ],
  [ gp => 'M', 'e2', 'e3' ],
  [ add => 'M' ],
  [ addcat => 'M', 'ascending' ],
  [ reln => 'R', 'e0', 'e3', 'same' ],
  [ create => 't', [ obj => 'R' ] ],
  [ spike => 'interlaced3', 100 ],
  [ spike => 'interlaced3', 100 ],
  [ spike => 'interlaced3', 100 ],
  [ srand => 5 ],
  [ actions => 't' ],
  [ activation => 'cat', 'interlaced3' ],
  ['state'],
  ['rand'],
);

# One intervening object (a group): the ad hoc category is Interlaced_2.
for my $seed ( 1 .. 4 ) {
  scenario(
    "relation_gap_group_$seed",
    [ init => [ 1, 5, 6, 1, 7 ] ],
    [ gp => 'M', 'e1', 'e2' ],
    [ add => 'M' ],
    [ addcat => 'M', 'ascending' ],
    [ reln => 'R', 'e0', 'e3', 'same' ],
    [ create => 't', [ obj => 'R' ] ],
    [ spike => 'interlaced2', 100 ],
    [ spike => 'interlaced2', 100 ],
    [ spike => 'interlaced2', 100 ],
    [ srand => $seed ],
    [ activation => 'cat', 'interlaced2' ],
    [ actions => 't' ],
    ['state'],
    ['rand'],
    [ actions => 't' ],
    ['state'],
    ['rand'],
  );
}

emit();
