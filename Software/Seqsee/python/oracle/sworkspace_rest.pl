# Oracle for SWorkspace.pm, part IV (item 034): choice distributions, __ReadObjectOrRelation,
# _saccade, __UpdateObjectStrengths, read_relation, _get_some_object_at, __ChooseByStrength,
# __GetObjectsBelongingToCategory, __GetObjectsBelongingToSimilarCategories,
# are_there_holes_here, SErr::AskUser::WorthAsking/Ask, DeleteObjectsInconsistentWith and
# __DeleteNonSubgroupsOfFrom.
# Output: tests/golden/sworkspace_rest.json
#
# Same scenario style as sworkspace_groups.pl / sworkspace_more.pl: each case is a list of
# ops and one result per op; tests/test_sworkspace_rest.py replays them. e0, e1, ... are the
# elements of the last init; other names come from "gp"/"reln" ops.
#
# Hash order: Perl iterates %Objects/%relations in hash order, Python in insertion order.
# Distributions are recorded as sorted [name, value] pairs. Seeded choices are only made
# where the candidate list order is fixed (one candidate, or one object plus one relation).
# Uniform choices over several candidates record the set of results over many draws.
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

# Scripted UI.
my @ANSWERS;
my @UI_LOG;
*main::ask_user_extension = sub {
  my ( $next, $msg ) = @_;
  push @UI_LOG, [ 'ask', [ map { 0 + $_ } @$next ], $msg ];
  return shift(@ANSWERS);
};
*main::update_display = sub { push @UI_LOG, ['update_display']; };

package FakeRuleApp;
sub new { my ( $c, %bad ) = @_; bless {%bad}, $c }
sub CheckConsitencyOfGroup { my ( $self, $g ) = @_; return $self->{ main::nm($g) } ? 0 : 1 }

package main;

my ( %obj, %name_of );
my $BY_ENDS_AT_INIT = 0;    # Perl's clear keeps %relations_by_ends; count relative to init.

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

sub cat {
  my ($n) = @_;
  my %c = (
    ascending  => $S::ASCENDING,
    descending => $S::DESCENDING,
    sameness   => $S::SAMENESS,
    number     => $S::NUMBER,
    prime      => $S::PRIME,
  );
  return $c{$n} // die "unknown cat $n";
}

sub pairs {
  my ( $values, $objects ) = @_;
  return [ sort { $a->[0] cmp $b->[0] } map { [ nm( $objects->[$_] ), 0 + $values->[$_] ] } 0 .. $#$objects ];
}

sub state {
  my @live = sort { nm($a) cmp nm($b) } values %$OBJECTS;
  return {
    objects   => [ map { [ nm($_), num_or_undef( $LEFT->{$_} ), num_or_undef( $RIGHT->{$_} ) ] } @live ],
    relations => names( values %SWorkspace::relations ),
    by_ends   => scalar( keys %SWorkspace::relations_by_ends ) - $BY_ENDS_AT_INIT,
  };
}

sub strengths {
  my @all = ( values %$OBJECTS, values %SWorkspace::relations );
  return [ sort { $a->[0] cmp $b->[0] } map { [ nm($_), num_or_undef( $_->get_strength ) ] } @all ];
}

my %OPS = (
  init => sub {
    SLTM->Clear();
    SWorkspace->init( { seq => $_[0] } );
    SWorkspace::__ClearBarLines();
    %Global::Feature = ();
    $Global::Steps_Finished = 0;
    $Global::AcceptableTrustLevel = 0.5;
    $Global::Break_Loop = undef;
    %Global::ExtensionRejectedByUser = ();
    %obj = %name_of = ();
    @ANSWERS = @UI_LOG = ();
    srand(1);
    $BY_ENDS_AT_INIT = scalar( keys %SWorkspace::relations_by_ends );
    my @e = SWorkspace::GetElements();
    reg( "e$_", $e[$_] ) for 0 .. $#e;
    return scalar(@e);
  },
  srand => sub { srand( $_[0] ); return undef },
  rand  => sub { return rand() },
  gp    => sub {
    my ( $name, @items ) = @_;
    my $g = Seqsee::Anchored->create( O(@items) );
    reg( $name, $g );
    return $g->as_text;
  },
  obj => sub {
    my ( $name, @spec ) = @_;
    my $o = Seqsee::Object->create(@spec);
    reg( $name, $o );
    return $o->as_text;
  },
  add      => sub { my @r = SWorkspace->add_group( $obj{ $_[0] } ); return [ map { num_or_undef($_) } @r ] },
  describe => sub { return $obj{ $_[0] }->describe_as( cat( $_[1] ) ) ? 1 : 0 },
  strength => sub { $obj{ $_[0] }->set_strength( $_[1] ); return undef },
  reln     => sub {
    my ( $name, $a, $b, $t ) = @_;
    my $r = SRelation->new(
      { first => $obj{$a}, second => $obj{$b}, type => Mapping::Numeric->create( $t, $S::NUMBER ) } );
    reg( $name, $r );
    $r->insert;
    return [ $r->as_text, exists $SWorkspace::relations{$r} ? 1 : 0 ];
  },
  spike => sub {
    SLTM::SpikeBy( $_[1], cat( $_[0] ) );
    return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) );
  },
  state     => sub { return state() },
  strengths => sub { return strengths() },
  readhead  => sub {
    $SWorkspace::ReadHead = $_[0] if @_;
    return num_or_undef($SWorkspace::ReadHead);
  },

  # distributions and reading
  update_strengths => sub { SWorkspace::__UpdateObjectStrengths(); return strengths() },
  obj_dist => sub { return pairs( SWorkspace::__GetObjectChoiceProbabilityDistribution() ) },
  rel_dist => sub { return pairs( SWorkspace::__GetRelationChoiceProbabilityDistribution() ) },
  both_dist => sub {
    my ( $v, $o ) = SWorkspace::__GetObjectOrRelationChoiceProbabilityDistribution();
    return [ pairs( $v, $o ), scalar(@$o) ];
  },
  read => sub {
    my $c = SWorkspace::__ReadObjectOrRelation();
    return [ nm($c), num_or_undef($SWorkspace::ReadHead) ];
  },
  saccade => sub { my $r = SWorkspace::_saccade(); return [ num_or_undef($r), num_or_undef($SWorkspace::ReadHead) ] },

  # uniform choosers
  read_relation   => sub { return nm( SWorkspace->read_relation() ) },
  some_object_at  => sub { return nm( SWorkspace::_get_some_object_at( $_[0] ) ) },
  choose_by_strength => sub { return nm( SWorkspace::__ChooseByStrength( O(@_) ) ) },
  some_object_at_set => sub {
    my %seen;
    $seen{ nm( SWorkspace::_get_some_object_at( $_[0] ) ) // 'undef' } = 1 for 1 .. 60;
    return [ sort keys %seen ];
  },
  read_relation_set => sub {
    my %seen;
    $seen{ nm( SWorkspace->read_relation() ) // 'undef' } = 1 for 1 .. 60;
    return [ sort keys %seen ];
  },

  # categories
  in_category => sub { return names( SWorkspace::__GetObjectsBelongingToCategory( cat( $_[0] ) ) ) },
  similar     => sub {
    my @r = SWorkspace::__GetObjectsBelongingToSimilarCategories( $obj{ $_[0] } );
    return [] unless @r;
    return [ sort { $a->[0] cmp $b->[0] or $a->[1] <=> $b->[1] } map { [ nm( $_->[0] ), 0 + $_->[1] ] } @{ $r[0] } ];
  },

  holes => sub { return SWorkspace->are_there_holes_here( O(@_) ) },
  holes_raw => sub { return SWorkspace->are_there_holes_here(@_) },

  # SErr::AskUser
  worth_asking => sub {
    my ( $matched, $next, $trust, $acceptable ) = @_;
    $Global::AcceptableTrustLevel = $acceptable;
    my $e = SErr::AskUser->new( already_matched => $matched, next_elements => $next );
    return num_or_undef( $e->WorthAsking($trust) );
  },
  ask => sub {
    my ( $matched, $next, $msg, $answer, $object, $from, $dir ) = @_;
    @ANSWERS = ($answer);
    @UI_LOG  = ();
    no strict 'refs';
    my $e = SErr::AskUser->new(
      already_matched => $matched,
      next_elements   => $next,
      ( defined $object ? ( object => $obj{$object}, from_position => $from, direction => ${"DIR::$dir"} ) : () ),
    );
    my $r = $e->Ask($msg);
    return {
      answer     => $r,
      ui         => [@UI_LOG],
      count      => 0 + $SWorkspace::ElementCount,
      trust      => 0 + $Global::AcceptableTrustLevel,
      break_loop => $Global::Break_Loop,
      rejected   => [ sort keys %Global::ExtensionRejectedByUser ],
    };
  },

  # deletions
  delete_inconsistent => sub {
    my $app = FakeRuleApp->new( map { $_ => 1 } @_ );
    SWorkspace::DeleteObjectsInconsistentWith($app);
    return state();
  },
  delete_non_subgroups => sub {
    my ( $of, $from ) = @_;
    SWorkspace::__DeleteNonSubgroupsOfFrom(
      { ( defined $of ? ( of => [ O(@$of) ] ) : () ), ( defined $from ? ( from => [ O(@$from) ] ) : () ) } );
    return state();
  },
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

# 1 2 3 4 5 6 7 8: A = 2 3 4 (ascending), B = 6 7, S = [A, 5] supergroup.
my @BASE = (
  [ init => [ 1 .. 8 ] ],
  [ gp => 'A', 'e1', 'e2', 'e3' ],
  [ add => 'A' ],
  [ describe => 'A', 'ascending' ],
  [ gp => 'B', 'e5', 'e6' ],
  [ add => 'B' ],
  [ gp => 'S', 'A', 'e4' ],
  [ add => 'S' ],
  [ gp => 'L', 'e0', 'e1', 'e2', 'e3', 'e4', 'e5' ],
);

scenario(
  'strengths_and_distributions',
  @BASE,
  ['strengths'],
  [ reln => 'R1', 'e0', 'e1', 'succ' ],
  [ reln => 'R2', 'B', 'e7', 'succ' ],
  [ reln => 'R3', 'e2', 'e3', 'succ' ],
  [ reln => 'R4', 'e0', 'e1', 'pred' ],
  ['state'],
  ['strengths'],
  [ spike => 'ascending', 50 ],
  ['update_strengths'],
  [ readhead => 0 ],
  ['obj_dist'],
  ['rel_dist'],
  ['both_dist'],
  [ readhead => 3 ],
  ['obj_dist'],
  [ readhead => 5 ],
  ['obj_dist'],
  ['both_dist'],
  [ readhead => 7 ],
  ['obj_dist'],
  [ readhead => 8 ],
  ['obj_dist'],
  ['both_dist'],
  [ strength => 'e5', 0 ],
  [ strength => 'e6', 0 ],
  [ strength => 'e7', 0 ],
  [ readhead => 5 ],
  ['obj_dist'],
  [ strength => 'R2', 0 ],
  [ strength => 'R3', 0 ],
  ['rel_dist'],
  [ strength => 'R1', 0 ],
  [ strength => 'R4', 0 ],
  ['rel_dist'],
  ['both_dist'],
);

# Long groups (span > 4) get doubled strength.
scenario(
  'long_group_distribution',
  [ init => [ 1 .. 7 ] ],
  [ gp => 'L', 'e0', 'e1', 'e2', 'e3', 'e4' ],
  [ add => 'L' ],
  [ gp => 'M', 'e5', 'e6' ],
  [ add => 'M' ],
  [ gp => 'N', 'L', 'M' ],
  [ add => 'N' ],
  [ strength => 'L', 10 ],
  [ strength => 'M', 10 ],
  [ strength => 'N', 10 ],
  [ readhead => 0 ],
  ['obj_dist'],
  ['both_dist'],
);

# Seeded reads: one object candidate (others strength 0), and optionally one relation.
for my $seed ( 1 .. 6 ) {
  scenario(
    "read_$seed",
    [ init => [ 1 .. 6 ] ],
    [ gp => 'A', 'e1', 'e2' ],
    [ add => 'A' ],
    [ gp => 'B', 'e3', 'e4', 'e5' ],
    [ add => 'B' ],
    ( map { [ strength => "e$_", 0 ] } 0 .. 5 ),
    [ strength => 'A', 40 ],
    [ strength => 'B', 40 ],
    [ reln => 'R', 'e0', 'A', 'succ' ],
    [ strength => 'R', 25 ],
    [ srand => $seed ],
    [ readhead => 1 ],
    ['both_dist'],
    ['read'],
    ['read'],
    [ readhead => 3 ],
    ['read'],
    ['read'],
    ['read'],
    [ readhead => 6 ],
    ['both_dist'],
    ['read'],
    ['read'],
    [ saccade => () ],
    [ saccade => () ],
    [ saccade => () ],
    ['rand'],
  );
}

scenario(
  'read_nothing',
  [ init => [ 1, 2 ] ],
  [ strength => 'e0', 0 ],
  [ strength => 'e1', 0 ],
  [ readhead => 0 ],
  ['both_dist'],
  ['read'],
  ['readhead'],
  ['rand'],
  [ init => [] ],
  [ srand => 2 ],
  [ saccade => () ],
  [ saccade => () ],
  [ saccade => () ],
);

scenario(
  'uniform_choosers',
  @BASE,
  [ srand => 3 ],
  ['read_relation'],
  ['rand'],
  [ reln => 'R1', 'e0', 'e1', 'succ' ],
  [ srand => 3 ],
  ['read_relation'],
  ['rand'],
  [ reln => 'R2', 'e6', 'e7', 'succ' ],
  ['read_relation_set'],
  [ some_object_at => 0 ],
  [ some_object_at => 7 ],
  [ some_object_at => 8 ],
  [ some_object_at => -1 ],
  [ some_object_at_set => 2 ],
  [ some_object_at_set => 4 ],
  [ some_object_at_set => 6 ],
  [ srand => 4 ],
  [ choose_by_strength => 'A' ],
  [ choose_by_strength => () ],
  ['rand'],
);

scenario(
  'categories',
  @BASE,
  [ describe => 'B', 'ascending' ],
  [ describe => 'e2', 'number' ],
  [ describe => 'e3', 'prime' ],
  [ describe => 'e4', 'prime' ],
  [ describe => 'e4', 'number' ],
  [ in_category => 'ascending' ],
  [ in_category => 'number' ],
  [ in_category => 'prime' ],
  [ in_category => 'descending' ],
  [ similar => 'A' ],
  [ similar => 'e4' ],
  [ similar => 'e7' ],
  [ spike => 'ascending', 30 ],
  [ spike => 'prime', 60 ],
  [ similar => 'A' ],
  [ similar => 'e4' ],
  [ similar => 'e3' ],
);

scenario(
  'holes',
  @BASE,
  [ holes => () ],
  [ holes => 'A' ],
  [ holes => 'A', 'e4' ],
  [ holes => 'A', 'B' ],
  [ holes => 'e0', 'A', 'e4', 'B' ],
  [ holes => 'e7', 'e0' ],
  [ holes => 'A', 'e2' ],
  [ holes => 'S', 'B' ],
  [ holes => 'L', 'e0' ],
  [ holes_raw => 3 ],
);

for my $seed ( 1 .. 4 ) {
  scenario(
    "worth_asking_$seed",
    [ init => [ 1, 2, 3 ] ],
    [ srand => $seed ],
    [ worth_asking => [ 1, 2 ], [3], 0.5, 0.5 ],
    [ worth_asking => [], [ 3, 4 ], 0.3, 0.5 ],
    [ worth_asking => [], [ 3, 4 ], 0.6, 0.5 ],
    [ worth_asking => [1], [ 3, 4, 5 ], 0.2, 0.4 ],
    [ worth_asking => [ 1, 2, 3 ], [4], 0, 0.9 ],
    [ worth_asking => [ 1, 2, 3 ], [4], 0.9, 0.9 ],
    [ worth_asking => [], [4], 1, 1 ],
    ['rand'],
  );
}

scenario(
  'worth_asking_errors',
  [ init => [ 1, 2, 3 ] ],
  [ worth_asking => [], [], 0.5, 0.5 ],
);

scenario(
  'ask',
  [ init => [ 1, 2, 3 ] ],
  [ ask => [], [ 4, 5 ], 'Hm. ', 0 ],
  ['state'],
  [ ask => [ 2, 3 ], [ 4, 5 ], 'Hm. ', '' ],
  [ ask => [1], [4], 'Hm. ', 1 ],
  ['state'],
  [ gp => 'A', 'e1', 'e2' ],
  [ add => 'A' ],
  [ describe => 'A', 'ascending' ],
  [ obj => 'X', 5, 6 ],
  [ ask => [ 4 ], [ 5, 6 ], 'Q ', 'yes', 'X', 4, 'RIGHT' ],
  ['state'],
  [ obj => 'Y', 9, 9 ],
  [ ask => [], [ 8 ], 'Q ', 1, 'Y', 6, 'RIGHT' ],
  ['state'],
);

scenario(
  'delete_inconsistent',
  @BASE,
  [ reln => 'R1', 'e0', 'e1', 'succ' ],
  [ reln => 'R2', 'A', 'e4', 'succ' ],
  ['state'],
  ['delete_inconsistent'],
  [ reln => 'R3', 'e0', 'e1', 'succ' ],
  [ delete_inconsistent => 'B' ],
  [ delete_inconsistent => 'A' ],
  [ add => 'B' ],
  [ add => 'L' ],
  [ delete_inconsistent => 'L', 'B' ],
);

scenario(
  'delete_non_subgroups',
  @BASE,
  [ gp => 'C', 'e6', 'e7' ],
  [ delete_non_subgroups => ['S'], [ 'A', 'B', 'S', 'e7', 'C' ] ],
  [ delete_non_subgroups => [], [ 'S', 'e0' ] ],
  [ add => 'B' ],
  [ add => 'S' ],
  [ delete_non_subgroups => [ 'A', 'e0' ], [ 'S', 'B', 'A' ] ],
  [ delete_non_subgroups => undef, ['A'] ],
  [ delete_non_subgroups => ['A'], undef ],
  [ delete_non_subgroups => ['A'], [] ],
);

emit();
