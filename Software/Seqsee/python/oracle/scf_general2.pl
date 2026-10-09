# Oracle for Seqsee/SCF_MX/General.pm, second half (item 041): the codelet families
# FindIfRelatedRelations, CheckIfAlternating, FindIfRelated (+ ShouldIContinue) and
# AttemptExtensionOfRelation (+ EstimateAskability).
# Output: tests/golden/scf_general2.json
#
# Same op-driven style (and helpers) as scf_general1.pl; tests/test_scf_general2.py replays
# the scenarios op for op. e0, e1, ... are the elements of the last init.
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
  return $name_of{$o} // ( '?' . noaddr( $o->can('as_text') ? $o->as_text : ref($o) ) );
}

# An Alternating category's name stringifies its (Platonic) objects: drop the addresses.
sub noaddr {
  my ($s) = @_;
  $s =~ s/=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)/=REF/g;
  return $s;
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
        [ sort map { noaddr( $_->as_text ) } @{ $_->get_categories() } ],
        ( $ul ? $ul->get_rule->get_transform->as_text : undef ),
      ]
    } @live
  ];
}

sub relations {
  return [ sort map { join( ' ', nm( $_->get_first ), nm( $_->get_second ), noaddr( $_->get_type->as_text ) ) }
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
  feature  => sub {
    if ( $_[1] eq 'none' ) { delete $Global::Feature{ $_[0] } }
    else                   { $Global::Feature{ $_[0] } = $_[1] }
    return undef;
  },
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
  reln   => sub {
    my ( $name, $a, $b, $t, $c ) = @_;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => mapping( $t, $c ) } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  # A relation that is not inserted.
  reln_only => sub {
    my ( $name, $a, $b, $t, $c ) = @_;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => mapping( $t, $c ) } );
    reg( $name, $r );
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
  spike_map => sub {
    my ( $amount, @m ) = @_;
    SLTM::SpikeBy( $amount, mapping(@m) );
    return 0 + SLTM::GetRealActivationsForOneConcept( mapping(@m) );
  },
  spike_type => sub {
    SLTM::SpikeBy( $_[1], $obj{ $_[0] }->get_type );
    return 0 + SLTM::GetRealActivationsForOneConcept( $obj{ $_[0] }->get_type );
  },
  activation_map => sub { return 0 + SLTM::GetRealActivationsForOneConcept( mapping(@_) ) },
  activation_type => sub { return 0 + SLTM::GetRealActivationsForOneConcept( $obj{ $_[0] }->get_type ) },
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
  # EstimateAskability($relation, $transform, $end1, $end2), as AttemptExtensionOfRelation calls it.
  askability => sub {
    my $r = $obj{ $_[0] };
    my $v = Seqsee::SCF::AttemptExtensionOfRelation::EstimateAskability( $r, $r->get_type, $r->get_ends );
    return num_or_undef($v);
  },
  # Details of the AskIfThisIsTheContinuation codelets on the coderack.
  ask_details => sub {
    return [
      map {
        my $a = $_->[3];
        [ ref( $a->{exception} ), [ map { 0 + $_ } @{ $a->{exception}->next_elements } ],
          $a->{expected_object}->as_text, 0 + $a->{start_position}, 0 + $a->{known_term_count},
          nm( $a->{relation} ) ]
      } grep { $_->[0] eq 'AskIfThisIsTheContinuation' } @SCoderack::CODELETS
    ];
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

my @after = ( ['state'], ['relations'], ['coderack'], ['rand'] );

# --- FindIfRelatedRelations ----------------------------------------------------------------
scenario(
  'fir_relations',
  [ init => [ 1, 2, 3, 5, 4, 3, 7 ] ],
  [ reln => 'R01', 'e0', 'e1', 'succ' ],
  [ reln => 'R12', 'e1', 'e2', 'succ' ],
  [ reln => 'R23', 'e2', 'e3', 'pred' ],
  [ reln => 'R34', 'e3', 'e4', 'pred' ],
  [ reln => 'R45', 'e4', 'e5', 'pred' ],
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R01' ] ], [ b => [ obj => 'R12' ] ] ],
  @after,
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R45' ] ], [ b => [ obj => 'R34' ] ] ],
  @after,
  # not adjacent.
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R01' ] ], [ b => [ obj => 'R34' ] ] ],
  # different types, no Alternating feature.
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R12' ] ], [ b => [ obj => 'R23' ] ] ],
  @after,
  # R01's succ was created before NUMBER was in the LTM, so its memo key differs and it is
  # a different object from R12's succ (no CreateGroup above). These two share one.
  [ reln_only => 'Q01', 'e0', 'e1', 'succ' ],
  [ reln_only => 'Q12', 'e1', 'e2', 'succ' ],
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'Q01' ] ], [ b => [ obj => 'Q12' ] ] ],
  @after,
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R01' ] ] ],
  [ run => 'FindIfRelatedRelations' ],
);

for my $seed ( 1 .. 2 ) {
  scenario(
    "fir_relations_alternating_$seed",
    [ init => [ 1, 2, 1, 2, 4, 4 ] ],
    [ reln => 'R01', 'e0', 'e1', 'succ' ],
    [ reln => 'R12', 'e1', 'e2', 'pred' ],
    [ reln => 'R23', 'e2', 'e3', 'succ' ],
    [ reln => 'R34', 'e3', 'e4', 'succ', 'even' ],
    [ reln => 'R45', 'e4', 'e5', 'same' ],
    [ feature => 'Alternating', 1 ],
    [ srand => $seed ],
    [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R01' ] ], [ b => [ obj => 'R12' ] ] ],
    @after,
    # different categories.
    [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R23' ] ], [ b => [ obj => 'R34' ] ] ],
    @after,
    # same category, no alternation (2 4 4).
    [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R34' ] ], [ b => [ obj => 'R45' ] ] ],
    [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R23' ] ], [ b => [ obj => 'R45' ] ] ],
    @after,
    [ feature => 'Alternating', 0 ],
    [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R12' ] ], [ b => [ obj => 'R23' ] ] ],
    @after,
  );
}

# Relations between groups.
scenario(
  'fir_relations_groups',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5 ] ],
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
  [ run => 'FindIfRelatedRelations', [ a => [ obj => 'R1' ] ], [ b => [ obj => 'R2' ] ] ],
  @after,
);

# --- CheckIfAlternating --------------------------------------------------------------------
for my $seed ( 1 .. 2 ) {
  scenario(
    "check_if_alternating_$seed",
    [ init => [ 1, 2, 3, 1, 2, 1, 7, 2, 9, 9 ] ],
    [ srand => $seed ],
    [ run => 'CheckIfAlternating', [ first => [ obj => 'e0' ] ], [ second => [ obj => 'e1' ] ],
      [ third => [ obj => 'e2' ] ] ],
    @after,
    [ run => 'CheckIfAlternating', [ first => [ obj => 'e3' ] ], [ second => [ obj => 'e4' ] ],
      [ third => [ obj => 'e5' ] ] ],
    @after,
    [ run => 'CheckIfAlternating', [ first => [ obj => 'e5' ] ], [ second => [ obj => 'e6' ] ],
      [ third => [ obj => 'e7' ] ] ],
    @after,
    [ run => 'CheckIfAlternating', [ first => [ obj => 'e7' ] ], [ second => [ obj => 'e8' ] ],
      [ third => [ obj => 'e9' ] ] ],
    @after,
    [ run => 'CheckIfAlternating', [ first => [ obj => 'e0' ] ], [ second => [ obj => 'e1' ] ] ],
  );
}

scenario(
  'check_if_alternating_groups',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'A', 'ascending' ],
  [ describe => 'B', 'ascending' ],
  [ describe => 'C', 'ascending' ],
  [ spike => 'ascending', 100 ],
  [ srand => 1 ],
  [ run => 'CheckIfAlternating', [ first => [ obj => 'A' ] ], [ second => [ obj => 'B' ] ],
    [ third => [ obj => 'C' ] ] ],
  @after,
);

# --- FindIfRelated -------------------------------------------------------------------------
scenario(
  'shouldicontinue',
  [ shouldicontinue => 0.5, 0.2, 4 ],
  [ shouldicontinue => 1, 0, 0 ],
  [ shouldicontinue => 0.3, 1, 9 ],
  [ shouldicontinue => 1, 0, 2 ],
  [ shouldicontinue => 0, 0, 0 ],
);

for my $seed ( 1 .. 4 ) {
  scenario(
    "find_if_related_$seed",
    [ init => [ 1, 2, 9, 3, 5, 8, 9, 9 ] ],
    [ srand => $seed ],
    # adjacent: distance 0, so it always continues.
    [ run => 'FindIfRelated', [ a => [ obj => 'e1' ] ], [ b => [ obj => 'e0' ] ] ],
    [ activation_map => 'succ' ],
    @after,
    # now related: FocusOn the existing relation.
    [ run => 'FindIfRelated', [ a => [ obj => 'e0' ] ], [ b => [ obj => 'e1' ] ] ],
    [ activation_map => 'succ' ],
    @after,
    # distance 1 and 2.
    [ run => 'FindIfRelated', [ a => [ obj => 'e1' ] ], [ b => [ obj => 'e3' ] ] ],
    @after,
    [ run => 'FindIfRelated', [ a => [ obj => 'e0' ] ], [ b => [ obj => 'e3' ] ] ],
    @after,
    [ run => 'FindIfRelated', [ a => [ obj => 'e5' ] ], [ b => [ obj => 'e2' ] ] ],
    @after,
    # unrelated.
    [ run => 'FindIfRelated', [ a => [ obj => 'e3' ] ], [ b => [ obj => 'e5' ] ] ],
    @after,
    [ run => 'FindIfRelated', [ a => [ obj => 'e6' ] ], [ b => [ obj => 'e7' ] ] ],
    @after,
  );
}

# Far apart, ShouldIContinue is well below 1: whether the relation is made depends on the draw.
for my $seed ( 1 .. 6 ) {
  scenario(
    "find_if_related_far_$seed",
    [ init => [ 1, 7, 7, 7, 7, 7, 7, 7, 7, 2, 9, 9, 9, 9, 3 ] ],
    [ srand => $seed ],
    [ run => 'FindIfRelated', [ a => [ obj => 'e0' ] ], [ b => [ obj => 'e9' ] ] ],
    @after,
    [ spike_map => 100, 'succ' ],
    [ run => 'FindIfRelated', [ a => [ obj => 'e14' ] ], [ b => [ obj => 'e9' ] ] ],
    @after,
  );
}

scenario(
  'find_if_related_groups',
  [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 6 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],
  [ add => 'C' ],
  [ describe => 'A', 'ascending' ],
  [ describe => 'B', 'ascending' ],
  [ describe => 'C', 'ascending' ],
  [ spike => 'ascending', 100 ],
  [ srand => 1 ],
  [ run => 'FindIfRelated', [ a => [ obj => 'A' ] ], [ b => [ obj => 'B' ] ] ],
  @after,
  [ run => 'FindIfRelated', [ a => [ obj => 'C' ] ], [ b => [ obj => 'A' ] ] ],
  @after,
  # dead object.
  [ gp => 'D', 'e8', 'e9' ],
  [ run => 'FindIfRelated', [ a => [ obj => 'C' ] ], [ b => [ obj => 'D' ] ] ],
  @after,
  [ run => 'FindIfRelated', [ a => [ obj => 'A' ] ] ],
);

# Overlapping groups: MergeGroups when both have the same rule and share their last item.
scenario(
  'find_if_related_overlap',
  [ init => [ 1, 2, 3, 4, 5, 6, 7 ] ],
  # Put NUMBER in the LTM first, so that R and S share their succ mapping (and rule).
  [ spike => 'number', 10 ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e2', 'e3', 'e4' ],
  [ add => 'B' ],
  [ gp => 'X', 'e1', 'e2', 'e3' ],
  [ add => 'X' ],
  [ run => 'FindIfRelated', [ a => [ obj => 'A' ] ], [ b => [ obj => 'B' ] ] ],
  @after,
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ reln => 'S', 'e3', 'e4', 'succ' ],
  [ reln => 'T', 'e4', 'e5', 'pred' ],
  [ ruleapp => 'A', 'R' ],
  [ run => 'FindIfRelated', [ a => [ obj => 'A' ] ], [ b => [ obj => 'B' ] ] ],
  @after,
  [ ruleapp => 'B', 'S' ],
  [ run => 'FindIfRelated', [ a => [ obj => 'B' ] ], [ b => [ obj => 'A' ] ] ],
  @after,
  [ ruleapp => 'X', 'T' ],
  [ run => 'FindIfRelated', [ a => [ obj => 'A' ] ], [ b => [ obj => 'X' ] ] ],
  @after,
);

# --- AttemptExtensionOfRelation ------------------------------------------------------------
scenario(
  'aer_elements',
  [ init => [ 1, 2, 3, 4, 5, 9, 1, 9, 2, 9, 3 ] ],
  [ reln => 'R12', 'e1', 'e2', 'succ' ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R12' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R12' ] ], [ direction => [ dir => 'LEFT' ] ] ],
  @after,
  # again: the relations exist already.
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R12' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  # mismatch: 6 expected, 9 present.
  [ reln => 'R34', 'e3', 'e4', 'succ' ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R34' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  # from the left end.
  [ reln => 'R01', 'e0', 'e1', 'succ' ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R01' ] ], [ direction => [ dir => 'LEFT' ] ] ],
  @after,
  # at a distance: 1 _ 2 _ 3.
  [ reln => 'R68', 'e6', 'e8', 'succ' ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R68' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ reln => 'R8A', 'e8', 'e10', 'succ' ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R8A' ] ], [ direction => [ dir => 'LEFT' ] ] ],
  @after,
  # a non-inserted relation still extends.
  [ reln_only => 'Q', 'e2', 'e3', 'succ' ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'Q' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R12' ] ] ],
);

# Beyond the known elements: EstimateAskability decides whether to ask.
for my $seed ( 1 .. 6 ) {
  scenario(
    "aer_beyond_$seed",
    [ init => [ 1, 2, 3 ] ],
    [ reln => 'R12', 'e1', 'e2', 'succ' ],
    [ spike_type => 'R12', 100 ],
    [ spike_type => 'R12', 100 ],
    [ srand => $seed ],
    [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R12' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    @after,
    ['ask_details'],
    [ askability => 'R12' ],
    [ askability => 'R12' ],
    [ reln => 'R00', 'e0', 'e0', 'same' ],
    [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R00' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    @after,
  );
}

for my $seed ( 1 .. 4 ) {
  scenario(
    "aer_groups_$seed",
    [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5 ] ],
    [ gp => 'A', 'e0', 'e1', 'e2' ],
    [ add => 'A' ],
    [ gp => 'B', 'e3', 'e4', 'e5' ],
    [ add => 'B' ],
    [ describe => 'A', 'ascending' ],
    [ describe => 'B', 'ascending' ],
    [ relnf => 'R1', 'A', 'B' ],
    [ spike_type => 'R1', 60 ],
    [ srand => $seed ],
    [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R1' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    [ name_new => 'N' ],
    @after,
    [ relnf => 'R2', 'B', 'N0' ],
    [ spike_type => 'R2', 100 ],
    [ spike_type => 'R2', 100 ],
    [ srand => $seed ],
    [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R2' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    @after,
    ['ask_details'],
    # with a supergroup: penalty 0.6.
    [ gp => 'G', 'B', 'N0' ],
    [ add => 'G' ],
    [ srand => $seed ],
    [ askability => 'R2' ],
    [ askability => 'R2' ],
    # with a super-supergroup: never.
    [ gp => 'H', 'A', 'G' ],
    [ add => 'H' ],
    [ askability => 'R2' ],
    ['rand'],
    [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R1' ] ], [ direction => [ dir => 'LEFT' ] ] ],
    @after,
  );
}

emit();
