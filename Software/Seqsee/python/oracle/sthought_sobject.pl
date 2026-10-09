# Oracle for SThought/SObject.pm (item 038): SThought::Seqsee::Element and
# SThought::Seqsee::Anchored. Output: tests/golden/sthought_sobject.json
#
# Same scenario style as sthought.pl: each case is a list of ops and one result per op;
# tests/test_sthought_sobject.py replays them. e0, e1, ... are the elements of the last init;
# other names come from "gp"/"reln"/"relnf"/"create" ops.
#
# Hash order: an object's categories come from a hash, so a fringe with more than one
# category is recorded sorted ("fringe_sorted"). Seeded scenarios that draw (toss,
# SpikeAndChoose, Set::Weighted choose) keep to one category per object, or to draws that
# don't depend on the order, so they match draw for draw.
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

sub core {
  my ( $kind, $v ) = @{ $_[0] };
  return $obj{$v} if $kind eq 'obj';
  return cat($v)  if $kind eq 'cat';
  die "unknown core kind $kind";
}

sub summarize {
  my ($v) = @_;
  return [ map { nm($_) } @$v ] if ref($v) eq 'ARRAY';
  return nm($v);
}

sub codelet {
  my ($cl) = @_;
  return [ 'NONREF', $cl ] unless ref $cl;    # ExtendFromMemory can return a feature value
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

sub fringe_entries {
  return [ map { [ nm( $_->[0] ), 0 + $_->[1] ] } @{ $_[0] } ];
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
  feature => sub {
    if   ( $_[1] eq 'none' ) { delete $Global::Feature{ $_[0] } }
    else                     { $Global::Feature{ $_[0] } = $_[1] }
    return undef;
  },
  describe => sub {
    my $r = $obj{ $_[0] }->describe_as( cat( $_[1] ) );
    return defined($r) ? 1 : 0;
  },
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
  # A category whose bindings have one slippage at index $_[2] (metonymy mode SINGLE).
  addcat_meto => sub {
    my ( $name, $catname, $index ) = @_;
    my $o = $obj{$name};
    my $m = SMetonym->new(
      { category => $S::SAMENESS, name => 'each', info_loss => { length => 2 },
        starred => Seqsee::Element->create( 1, -1 ), unstarred => $o->[$index] } );
    $o->add_category( cat($catname), SBindings->create( { $index => $m }, {}, $o ) );
    my $b = $o->GetBindingForCategory( cat($catname) );
    return $b->get_metonymy_mode->as_text;
  },
  # Give the object an (active) metonym whose starred part is a new element of magnitude $_[1].
  set_metonym => sub {
    my ( $name, $mag ) = @_;
    my $o = $obj{$name};
    my $m = SMetonym->new(
      { category => $S::SAMENESS, name => 'each', info_loss => { length => scalar(@$o) },
        starred => Seqsee::Element->create( $mag, -1 ), unstarred => $o } );
    $o->SetMetonym($m);
    $o->SetMetonymActiveness(1);
    return $o->GetEffectiveObject->as_text;
  },
  reln => sub {
    my ( $name, $a, $b, $t, $c ) = @_;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => mapping( $t, $c ) } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  # A relation whose type is FindMapping's.
  relnf => sub {
    my ( $name, $a, $b ) = @_;
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
  spike_obj => sub {
    SLTM::SpikeBy( $_[1], $obj{ $_[0] } );
    return 0 + SLTM::GetRealActivationsForOneConcept( $obj{ $_[0] } );
  },
  activation => sub {
    my ( $kind, $v ) = @_;
    my $c =
        $kind eq 'type'      ? $obj{$v}->get_type
      : $kind eq 'obj'       ? $obj{$v}
      : $kind eq 'transform' ? $obj{$v}->get_underlying_reln->get_rule->get_transform
      :                        cat($v);
    return 0 + SLTM::GetRealActivationsForOneConcept($c);
  },
  # LTM links.
  follows => sub {
    my ( $catname, $mname, $mcat, $amount ) = @_;
    SLTM::InsertFollowsLink( cat($catname), mapping( $mname, $mcat ) )->Spike($amount);
    return undef;
  },
  follows_type => sub {
    my ( $catname, $r, $amount ) = @_;
    SLTM::InsertFollowsLink( cat($catname), $obj{$r}->get_type )->Spike($amount);
    return undef;
  },
  isa => sub {
    my ( $o, $catname, $amount ) = @_;
    SLTM::InsertISALink( $obj{$o}, cat($catname) )->Spike($amount);
    return undef;
  },
  link_activation => sub {
    my ( $kind, $from, $to ) = @_;
    my ( $f, $t, $type );
    if ( $kind eq 'isa' ) { ( $f, $t, $type ) = ( $obj{$from}, cat($to), SLTM::LTM_IS() ) }
    else { ( $f, $t, $type ) = ( cat($from), $obj{$to}, SLTM::LTM_FOLLOWS() ) }
    my $fi = SLTM::GetMemoryIndex($f);
    my $ti = SLTM::GetMemoryIndex($t);
    my $l  = $SLTM::OUT_LINKS[$fi][$type]{$ti};
    return defined($l) ? [ 0 + $l->[0], 0 + $l->[3] ] : undef;    # raw and real activation
  },
  node_count => sub { return 0 + $SLTM::NodeCount },
  state      => sub { return state() },
  set_metonym_activeness => sub {
    $obj{ $_[0] }->set_metonym_activeness( $_[1] );
    return undef;
  },

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
  magnitude => sub {
    my ( $name, @v ) = @_;
    $obj{$name}->magnitude(@v) if @v;
    return $obj{$name}->magnitude;
  },
  fringe        => sub { return fringe_entries( $obj{ $_[0] }->get_fringe ) },
  fringe_sorted => sub {
    my $f = fringe_entries( $obj{ $_[0] }->get_fringe );
    return [ sort { "$a->[0] $a->[1]" cmp "$b->[0] $b->[1]" } @$f ];
  },
  actions => sub {
    my @a = $obj{ $_[0] }->get_actions;
    return [ map { codelet($_) } @a ];
  },
  upslope => sub {
    return nm( SThought::Seqsee::Anchored::IsThisAMountainUpslope( $obj{ $_[0] } ) );
  },
  strengthen => sub {
    my @r = SThought::Seqsee::Anchored::StrengthenLink( O(@_) );
    return scalar(@r);
  },
  add_from_memory => sub {
    return [ map { codelet($_) } SThought::Seqsee::Anchored::AddCategoriesFromMemory( $obj{ $_[0] } ) ];
  },
  extend_from_memory => sub {
    return [ map { codelet($_) } SThought::Seqsee::Anchored::ExtendFromMemory( $obj{ $_[0] } ) ];
  },
  name => sub { no strict 'refs'; return ${"$_[0]::NAME"} },
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

# --- create, NAME, Moose new, magnitude -------------------------------------------------
scenario(
  'create',
  [ init => [ 2, 3, 5, 7 ] ],
  [ name => 'SThought::Seqsee::Element' ],
  [ name => 'SThought::Seqsee::Anchored' ],
  [ create => 't0', [ obj => 'e0' ] ],
  [ create => 't0b', [ obj => 'e0' ] ],
  [ create_list => 't0c', [ obj => 'e0' ] ],
  [ create_list => 't0d', [ obj => 'e0' ] ],
  [ create => 't3', [ obj => 'e3' ] ],
  [ gp => 'A', 'e1', 'e2' ],
  [ add => 'A' ],
  [ create => 'tA', [ obj => 'A' ] ],
  [ create => 'tA2', [ obj => 'A' ] ],
  [ create_list => 'tA3', [ obj => 'A' ] ],
  [ gp => 'B', 'e2', 'e3' ],
  [ create => 'tB', [ obj => 'B' ] ],
  [ new => 'SThought::Seqsee::Element' ],
  [ new => 'SThought::Seqsee::Anchored' ],
  [ new => 'SThought::Seqsee::Element', [ obj => 'e1' ] ],
  [ new => 'SThought::Seqsee::Anchored', [ obj => 'A' ] ],
  [ new => 'SThought::Seqsee::Anchored', [ obj => 'e1' ] ],
  [ magnitude => 't0' ],
  [ magnitude => 't3' ],
  [ magnitude => 't3', 42 ],
  [ magnitude => 't3' ],
  [ magnitude => 't0c' ],
);

# --- element fringe ---------------------------------------------------------------------
scenario(
  'element_fringe',
  [ init => [ 2, 3, 8, 97, 1, 0 ] ],
  [ create => 't0', [ obj => 'e0' ] ],
  [ create => 't1', [ obj => 'e1' ] ],
  [ create => 't2', [ obj => 'e2' ] ],
  [ create => 't3', [ obj => 'e3' ] ],
  [ create => 't4', [ obj => 'e4' ] ],
  [ create => 't5', [ obj => 'e5' ] ],
  [ fringe => 't1' ],
  [ node_count => ],
  [ addcat => 'e0', 'prime' ],
  [ fringe_sorted => 't0' ],
  [ activation => 'cat', 'prime' ],
  [ addcat => 'e1', 'prime' ],
  [ addcat => 'e1', 'odd' ],
  [ fringe_sorted => 't1' ],
  [ activation => 'cat', 'prime' ],
  [ activation => 'cat', 'odd' ],
  [ activation => 'cat', 'number' ],
  [ addcat => 'e2', 'even' ],
  [ fringe_sorted => 't2' ],
  [ addcat => 'e3', 'prime' ],
  [ fringe_sorted => 't3' ],
  [ fringe => 't4' ],
  [ fringe => 't5' ],
  [ actions => 't0' ],
  [ actions => 't1' ],
  ['rand'],
);

# --- element actions with LTM -----------------------------------------------------------
#
# One FOLLOWS link and one active ISA link each: with two, the choice depends on hash
# order (Set::Weighted's merge_keys keys the followers by their addresses).
for my $expt ( 0, 1 ) {
  for my $m ( 'succ', 'pred' ) {
  for my $seed ( 1 .. 3 ) {
    scenario(
      "element_ltm_${expt}_${m}_$seed",
      [ init => [ 1, 2, 3, 2, 3, 4 ] ],
      [ feature => 'LTM', 1 ],
      [ feature => 'LTM_expt', $expt ],
      [ create => 't1', [ obj => 'e1' ] ],
      [ create => 't0', [ obj => 'e0' ] ],
      [ srand => $seed ],
      [ actions => 't1' ],
      [ activation => 'obj', 'e1' ],
      [ node_count => ],
      ['rand'],
      [ follows => 'number', $m, 'number', 50 ],
      [ isa => 'e1', 'prime', 10 ],
      [ isa => 'e1', 'odd', 10 ],
      [ spike => 'prime', 100 ],
      [ spike => 'prime', 100 ],
      [ spike => 'prime', 100 ],
      [ spike => 'prime', 100 ],
      [ spike => 'odd', 100 ],
      [ srand => $seed ],
      [ actions => 't1' ],
      [ activation => 'obj', 'e1' ],
      [ activation => 'cat', 'prime' ],
      [ activation => 'cat', 'odd' ],
      ['state'],
      ['rand'],
      [ srand => $seed ],
      [ extend_from_memory => 'e1' ],
      ['state'],
      ['rand'],
      [ srand => $seed ],
      [ add_from_memory => 'e1' ],
      ['rand'],
      [ srand => $seed ],
      [ actions => 't0' ],
      ['rand'],
    );
  }
  }
}

# --- group fringe -----------------------------------------------------------------------
scenario(
  'group_fringe',
  [ init => [ 1, 2, 3, 1, 1, 1, 7 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ create => 'tA', [ obj => 'A' ] ],
  [ fringe => 'tA' ],
  [ addcat => 'A', 'ascending' ],
  [ fringe => 'tA' ],
  [ activation => 'cat', 'ascending' ],
  [ fringe => 'tA' ],
  [ activation => 'cat', 'ascending' ],
  [ addcat => 'A', 'number' ],
  [ fringe_sorted => 'tA' ],
  [ gp => 'S', 'e3', 'e4', 'e5' ],
  [ add => 'S' ],
  [ create => 'tS', [ obj => 'S' ] ],
  [ addcat_meto => 'S', 'sameness', 1 ],
  [ fringe => 'tS' ],
  [ activation => 'cat', 'sameness' ],
  [ set_metonym => 'S', 1 ],
  [ fringe => 'tS' ],
  [ activation => 'cat', 'sameness' ],
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e1', 'e2' ],
  [ create => 'tG', [ obj => 'G' ] ],
  [ ruleapp => 'G', 'R' ],
  [ fringe => 'tG' ],
);

# --- group actions: the basic tosses ------------------------------------------------------
for my $seed ( 1 .. 6 ) {
  scenario(
    "group_actions_$seed",
    [ init => [ 1, 2, 3, 4, 5, 6, 7, 8 ] ],
    [ gp => 'A', 'e2', 'e3' ],
    [ add => 'A' ],
    [ gp => 'L', 'e0', 'e1' ],
    [ add => 'L' ],
    [ gp => 'R', 'e6', 'e7' ],
    [ add => 'R' ],
    [ create => 'tA', [ obj => 'A' ] ],
    [ create => 'tL', [ obj => 'L' ] ],
    [ create => 'tR', [ obj => 'R' ] ],
    [ srand => $seed ],
    [ actions => 'tA' ],
    ['rand'],
    [ actions => 'tL' ],
    ['rand'],
    [ actions => 'tR' ],
    ['rand'],
    [ feature => 'AllowSquinting', 1 ],
    [ actions => 'tA' ],
    [ actions => 'tA' ],
    [ set_metonym_activeness => 'e2', 1 ],
    [ set_metonym_activeness => 'e3', 1 ],
    [ actions => 'tA' ],
    [ actions => 'tA' ],
    ['rand'],
  );
}

# --- relation-suggested categories ----------------------------------------------------------
for my $seed ( 1 .. 2 ) {
  scenario(
    "group_suggest_$seed",
    [ init => [ 1, 2, 3, 3, 3, 9, 5, 7, 11 ] ],
    [ gp => 'A', 'e0', 'e1', 'e2' ],
    [ add => 'A' ],
    [ create => 'tA', [ obj => 'A' ] ],
    [ srand => $seed ],
    [ actions => 'tA' ],
    [ reln => 'R', 'e0', 'e1', 'succ' ],
    [ actions => 'tA' ],
    [ addcat => 'A', 'ascending' ],
    [ actions => 'tA' ],
    [ gp => 'S', 'e3', 'e4' ],
    [ add => 'S' ],
    [ reln => 'RS', 'e3', 'e4', 'same' ],
    [ create => 'tS', [ obj => 'S' ] ],
    [ actions => 'tS' ],
    [ gp => 'D', 'e5', 'e6' ],
    [ add => 'D' ],
    [ reln => 'RD', 'e5', 'e6', 'pred' ],
    [ create => 'tD', [ obj => 'D' ] ],
    [ actions => 'tD' ],
    [ gp => 'P', 'e6', 'e7', 'e8' ],
    [ add => 'P' ],
    [ reln => 'RP', 'e6', 'e7', 'succ', 'prime' ],
    [ create => 'tP', [ obj => 'P' ] ],
    [ actions => 'tP' ],
    ['state'],
    ['rand'],
  );
}

# --- large groups and FocusOn ---------------------------------------------------------------
for my $seed ( 1 .. 6 ) {
  scenario(
    "group_focus_$seed",
    [ init => [ 1, 2, 3, 4, 5 ] ],
    [ gp => 'A', 'e0', 'e1', 'e2' ],
    [ add => 'A' ],
    [ addcat => 'A', 'ascending' ],
    [ create => 'tA', [ obj => 'A' ] ],
    [ spike => 'ascending', 30 ],
    [ srand => $seed ],
    [ actions => 'tA' ],
    [ activation => 'cat', 'ascending' ],
    ['rand'],
    [ spike => 'ascending', 100 ],
    [ spike => 'ascending', 100 ],
    [ srand => $seed ],
    [ actions => 'tA' ],
    [ activation => 'cat', 'ascending' ],
    ['rand'],
  );
}

# --- groups of groups: underlying relation, DoTheSameThing, LTM -------------------------
for my $ltm ( 0, 1 ) {
  for my $seed ( 1 .. 3 ) {
    scenario(
      "group_underlying_${ltm}_$seed",
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
      [ spike => 'ascending', 100 ],
      [ srand => 1 ],
      [ relnf => 'R1', 'A', 'B' ],
      [ relnf => 'R2', 'B', 'C' ],
      [ gp => 'G', 'A', 'B', 'C' ],
      [ add => 'G' ],
      [ ruleapp => 'G', 'R1' ],
      [ create => 'tG', [ obj => 'G' ] ],
      [ fringe => 'tG' ],
      [ feature => 'LTM', $ltm ],
      [ srand => $seed ],
      [ actions => 'tG' ],
      [ activation => 'transform', 'G' ],
      [ activation => 'obj', 'G' ],
      [ link_activation => 'isa', 'A', 'ascending' ],
      [ link_activation => 'isa', 'C', 'ascending' ],
      [ link_activation => 'follows', 'ascending', 'R1' ],
      [ link_activation => 'follows', 'ascending', 'R2' ],
      [ node_count => ],
      ['rand'],
      [ create => 'tA', [ obj => 'A' ] ],
      [ srand => $seed ],
      [ actions => 'tA' ],
      ['rand'],
    );
  }
}

# --- ExtendFromMemory / AddCategoriesFromMemory on a group ------------------------------
for my $expt ( 'none', 0, 1 ) {
  for my $seed ( 1 .. 3 ) {
    scenario(
      "group_memory_${expt}_$seed",
      [ init => [ 1, 2, 3, 2, 3, 4, 3, 4, 5, 9 ] ],
      [ gp => 'A', 'e0', 'e1', 'e2' ],
      [ add => 'A' ],
      [ gp => 'B', 'e3', 'e4', 'e5' ],
      [ add => 'B' ],
      [ describe => 'A', 'ascending' ],
      [ describe => 'B', 'ascending' ],
      [ spike => 'ascending', 100 ],
      [ srand => 1 ],
      [ relnf => 'R1', 'A', 'B' ],
      [ follows_type => 'ascending', 'R1', 50 ],
      [ isa => 'A', 'mountain', 10 ],
      [ spike => 'mountain', 100 ],
      [ spike => 'mountain', 100 ],
      [ isa => 'A', 'sameness', 10 ],
      [ feature => 'LTM_expt', $expt ],
      [ srand => $seed ],
      [ extend_from_memory => 'A' ],
      ['state'],
      ['rand'],
      [ srand => $seed ],
      [ add_from_memory => 'A' ],
      ['rand'],
      [ feature => 'LTM', 1 ],
      [ create => 'tA', [ obj => 'A' ] ],
      [ srand => $seed ],
      [ actions => 'tA' ],
      ['state'],
      ['rand'],
    );
  }
}

# --- StrengthenLink -----------------------------------------------------------------
scenario(
  'strengthen_link',
  [ init => [ 1, 2, 3, 4 ] ],
  [ strengthen => 'e0', 'e1' ],
  [ node_count => ],
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ strengthen => 'e0', 'e1' ],
  [ link_activation => 'isa', 'e0', 'number' ],
  [ link_activation => 'isa', 'e1', 'number' ],
  [ strengthen => 'e1', 'e0' ],
  [ strengthen => 'e0', 'e1' ],
  [ link_activation => 'isa', 'e0', 'number' ],
  [ link_activation => 'follows', 'number', 'R' ],
  [ node_count => ],
);

# --- mountains ----------------------------------------------------------------------
for my $seed ( 1 .. 6 ) {
  scenario(
    "mountain_$seed",
    [ init => [ 1, 2, 3, 2, 1, 5 ] ],
    [ gp => 'U', 'e0', 'e1', 'e2' ],
    [ add => 'U' ],
    [ gp => 'D', 'e2', 'e3', 'e4' ],
    [ add => 'D' ],
    [ gp => 'D2', 'e2', 'e3' ],
    ['state'],
    [ upslope => 'U' ],
    [ addcat => 'U', 'ascending' ],
    [ upslope => 'U' ],
    [ addcat => 'D2', 'descending' ],
    [ upslope => 'U' ],
    [ addcat => 'D', 'descending' ],
    [ upslope => 'U' ],
    [ upslope => 'D' ],
    [ create => 'tU', [ obj => 'U' ] ],
    [ srand => $seed ],
    [ actions => 'tU' ],
    [ activation => 'cat', 'mountain' ],
    ['rand'],
    [ spike => 'mountain', 100 ],
    [ spike => 'mountain', 100 ],
    [ srand => $seed ],
    [ actions => 'tU' ],
    ['rand'],
    [ spike => 'mountain', 100 ],
    [ spike => 'mountain', 100 ],
    [ srand => $seed ],
    [ actions => 'tU' ],
    [ activation => 'cat', 'mountain' ],
    ['state'],
    ['rand'],
  );
}

# --- the Alternating feature --------------------------------------------------------
for my $seed ( 1 .. 3 ) {
  scenario(
    "alternating_$seed",
    [ init => [ 1, 2, 1, 2, 1, 2, 7 ] ],
    [ gp => 'A', 'e0', 'e1' ],
    [ add => 'A' ],
    [ gp => 'B', 'e2', 'e3' ],
    [ add => 'B' ],
    [ gp => 'C', 'e4', 'e5' ],
    [ add => 'C' ],
    [ feature => 'Alternating', 1 ],
    [ create => 'tB', [ obj => 'B' ] ],
    [ srand => $seed ],
    [ actions => 'tB' ],
    ['rand'],
    [ addcat => 'A', 'ascending' ],
    [ addcat => 'B', 'ascending' ],
    [ actions => 'tB' ],
    ['rand'],
    [ addcat => 'C', 'ascending' ],
    [ actions => 'tB' ],
    ['rand'],
    [ create => 'tA', [ obj => 'A' ] ],
    [ actions => 'tA' ],
    ['rand'],
  );
}

emit();
