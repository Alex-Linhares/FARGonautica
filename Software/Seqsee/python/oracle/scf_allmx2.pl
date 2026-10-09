# Oracle for Seqsee/SCF_MX/AllMX2.pm and LargeGp.pm (item 043): the codelet families
# AttemptExtensionOfGroup, TryToSquint, LargeGroup, MaybeStartBlemish,
# InterlacedInitialBlemish and ArbitraryInitialBlemish.
# Output: tests/golden/scf_allmx2.json
#
# Same op-driven style (and helpers) as scf_allmx.pl; tests/test_scf_allmx2.py replays the
# scenarios op for op. e0, e1, ... are the elements of the last init.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine', 'once';
use Oracle;
use S;
use PadWalker qw(closed_over peek_sub);

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

# AreWeDone's file lexical $LastSolutionDescriptionTime, and CheckProgress's state var.
my $LAST_SOLUTION = closed_over( \&Seqsee::SCF::AreWeDone::BelieveDone )->{'$LastSolutionDescriptionTime'};
my $progress_body = closed_over( \&Seqsee::SCF::CheckProgress::run )->{'%options'}{body};
my $LAST_PROGRESS = peek_sub($progress_body)->{'$last_time_progresschecker_run'};
die "PadWalker 2" unless $LAST_SOLUTION and $LAST_PROGRESS;

# The UI hooks (Test::Seqsee installs them only in INITIALIZE_for_testing).
my @PROBES;
sub main::message            { }
sub main::update_display     { }
sub main::ask_for_more_terms { push @PROBES, ['ask_for_more_terms'] }

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

sub argval {
  my ( $kind, @v ) = @{ $_[0] };
  return $obj{ $v[0] }               if $kind eq 'obj';
  return [ O(@v) ]                   if $kind eq 'list';
  return cat( $v[0] )                if $kind eq 'cat';
  return mapping(@v)                 if $kind eq 'map';
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
        [ map { nm($_) } @$_ ],
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

sub thought_name {
  my ($t) = @_;
  return '' unless $t;
  return ref($t) . ':' . nm( $t->core );
}

my %OPS = (
  init => sub {
    SLTM->Clear();
    SWorkspace->init( { seq => $_[0] } );
    SWorkspace::__ClearBarLines();
    SCoderack->clear;
    $Global::MainStream->clear();
    %Global::Feature = ();
    %Global::Hilit   = ();
    $Global::Steps_Finished             = 0;
    $Global::TimeOfLastNewElement       = 0;
    $Global::TimeOfNewStructure         = 0;
    $Global::AtLeastOneUserVerification = undef;
    $Global::TestingMode                = undef;
    $Global::RecentPromisingRuleApp     = undef;
    $Global::RecentPromisingRule        = undef;
    $Global::CurrentCodelet             = undef;
    $$LAST_SOLUTION                     = undef;
    $$LAST_PROGRESS                     = 0;
    @PROBES = ();
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
  global => sub {
    no strict 'refs';
    ${"Global::$_[0]"} = $_[1];
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
  relnf => sub {
    my ( $name, $a, $b ) = @_;
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
  # A rule app with the group's items as they are, or the given items (no consistency check).
  fake_ruleapp => sub {
    my ( $g, $r, @items ) = @_;
    my $ra = SRuleApp->new(
      { rule => SRule->create( $obj{$r} ), items => ( @items ? [ O(@items) ] : [ @{ $obj{$g} } ] ),
        direction => $DIR::RIGHT } );
    $obj{$g}->set_underlying_reln($ra);
    return undef;
  },
  spike => sub {
    SLTM::SpikeBy( $_[1], cat( $_[0] ) );
    return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) );
  },
  activation => sub { return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) ) },
  thought => sub {
    my ( $name, $what ) = @_;
    my $t = SThought->create( $obj{$what} );
    reg( $name, $t );
    return ref($t);
  },
  thought_cat => sub {
    my ( $name, $c ) = @_;
    my $t = SThought->create( cat($c) );
    reg( $name, $t );
    return ref($t);
  },
  stream => sub {
    my $s = $Global::MainStream;
    return [ thought_name( $s->{CurrentThought} ), 0 + $s->{OlderThoughtCount},
      [ map { thought_name($_) } @{ $s->{OlderThoughts} } ] ];
  },
  current_codelet => sub { $Global::CurrentCodelet = SCodelet->new( $_[0], 50, {} ); return undef },
  readhead     => sub { return 0 + $SWorkspace::ReadHead },
  set_readhead => sub { $SWorkspace::ReadHead = $_[0]; return undef },
  items        => sub { my $g = $obj{ $_[0] }; return [ [ map { nm($_) } @$g ], $g->get_left_edge, $g->get_right_edge ] },
  supergroups  => sub { return [ sort map { nm($_) } SWorkspace->GetSuperGroups( $obj{ $_[0] } ) ] },
  hilit        => sub { return [ sort { $a->[0] cmp $b->[0] } map { [ $name_of{$_} // '?', $Global::Hilit{$_} ] } keys %Global::Hilit ] },
  recent       => sub {
    return undef unless $Global::RecentPromisingRuleApp;
    return $Global::RecentPromisingRuleApp eq $obj{ $_[0] }->get_underlying_reln ? 1 : 0;
  },
  last_solution     => sub { return num_or_undef($$LAST_SOLUTION) },
  last_progress     => sub { return num_or_undef($$LAST_PROGRESS) },
  set_last_progress => sub { $$LAST_PROGRESS = $_[0]; return undef },
  desperation       => sub { return 0 + Seqsee::SCF::CheckProgress::CalculateDesperation(@_) },
  strength          => sub { return 0 + $obj{ $_[0] }->get_strength },
  # Replace a family's run with a probe that records its arguments.
  probe => sub {
    my ($family) = @_;
    no strict 'refs';
    *{"Seqsee::SCF::${family}::run"} = sub {
      my ( $action, $args ) = @_;
      push @PROBES, [ $family, map { [ $_, summarize( $args->{$_} ) ] } sort keys %$args ];
    };
    return undef;
  },
  probes    => sub { my @p = @PROBES; @PROBES = (); return \@p },
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
  name_new => sub {
    my ($prefix) = @_;
    my @new = sort { $LEFT->{$a} <=> $LEFT->{$b} or $RIGHT->{$a} <=> $RIGHT->{$b} }
      grep { !exists $name_of{$_} } values %$OBJECTS;
    my $i = 0;
    reg( $prefix . $i++, $_ ) for @new;
    return [ map { $_->as_text } @new ];
  },
  # An object's metonym: [activeness, category, name, starred structure string].
  metonym => sub {
    my $o = $obj{ $_[0] };
    my $m = $o->get_metonym or return [ num_or_undef( $o->get_metonym_activeness ) ];
    return [ num_or_undef( $o->get_metonym_activeness ), $m->get_category->as_text, $m->get_name,
      $m->get_starred->get_structure_string ];
  },
  categories => sub { return [ sort map { noaddr( $_->as_text ) } @{ $obj{ $_[0] }->get_categories() } ] },
  flush      => sub { my $g = $obj{ $_[0] }; return [ $g->IsFlushLeft ? 1 : 0, $g->IsFlushRight ? 1 : 0 ] },
  live       => sub { return SWorkspace::__CheckLiveness( $obj{ $_[0] } ) ? 1 : 0 },
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

# --- AttemptExtensionOfGroup ---------------------------------------------------------------
for my $seed ( 1 .. 4 ) {
  scenario(
    "attempt_extension_of_group_$seed",
    [ init => [ 1, 2, 3, 4, 5, 6, 7 ] ],
    [ spike => 'number', 10 ],
    [ reln => 'R', 'e2', 'e3', 'succ' ],
    [ gp => 'G', 'e2', 'e3', 'e4' ],
    [ add => 'G' ],
    [ describe => 'G', 'ascending' ],
    [ ruleapp => 'G', 'R' ],
    [ srand => $seed ],
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    [ items => 'G' ], [ strength => 'G' ], @after,
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'LEFT' ] ] ],
    [ items => 'G' ], [ strength => 'G' ], @after,
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'LEFT' ] ] ],
    [ items => 'G' ], @after,
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'LEFT' ] ] ],
    [ items => 'G' ], @after,
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    [ items => 'G' ], @after,
    # beyond the known elements.
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    [ items => 'G' ], @after,
  );
}

scenario(
  'attempt_extension_of_group_misc',
  [ init => [ 1, 2, 3, 4, 5, 6, 7 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e1', 'e2', 'e3' ],
  [ add => 'G' ],
  [ describe => 'G', 'ascending' ],
  # no rule app: no extension.
  [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ items => 'G' ], @after,
  # a dead group.
  [ gp => 'D', 'e4', 'e5' ],
  [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'D' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  # an element.
  [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'e5' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  @after,
  # out-of-sync rule app: the pre SanityCheck fails.
  [ fake_ruleapp => 'G', 'R', 'e1', 'e2' ],
  [ global => 'CurrentRunnableString', 'SCodelet' ],
  [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ current_codelet => 'AttemptExtensionOfGroup' ],
  [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ] ],
  [ run => 'AttemptExtensionOfGroup' ],
);

# A group of groups.
for my $seed ( 1 .. 3 ) {
  scenario(
    "attempt_extension_of_group_gpgp_$seed",
    [ init => [ 1, 1, 2, 2, 3, 3, 4, 4 ] ],
    [ gp => 'A', 'e0', 'e1' ],
    [ add => 'A' ],
    [ describe => 'A', 'sameness' ],
    [ gp => 'B', 'e2', 'e3' ],
    [ add => 'B' ],
    [ describe => 'B', 'sameness' ],
    [ relnf => 'R', 'A', 'B' ],
    [ gp => 'G', 'A', 'B' ],
    [ add => 'G' ],
    [ ruleapp => 'G', 'R' ],
    [ srand => $seed ],
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    [ items => 'G' ], [ name_new => 'N' ], @after,
    [ run => 'AttemptExtensionOfGroup', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    [ items => 'G' ], [ name_new => 'M' ], @after,
  );
}

# --- TryToSquint ---------------------------------------------------------------------------
scenario(
  'try_to_squint',
  [ init => [ 3, 3, 3, 1, 2, 3, 7 ] ],
  [ gp => 'G', 'e0', 'e1', 'e2' ],
  [ add => 'G' ],
  [ describe => 'G', 'sameness' ],
  [ gp => 'H', 'e3', 'e4', 'e5' ],
  [ add => 'H' ],
  [ describe => 'H', 'ascending' ],
  [ metonym => 'G' ],
  [ run => 'TryToSquint', [ actual => [ obj => 'G' ] ], [ intended => [ obj => 'e6' ] ] ],
  [ metonym => 'G' ], @after,
  [ run => 'TryToSquint', [ actual => [ obj => 'H' ] ], [ intended => [ obj => 'e5' ] ] ],
  [ metonym => 'H' ], @after,
  [ run => 'TryToSquint', [ actual => [ obj => 'e0' ] ], [ intended => [ obj => 'G' ] ] ],
  [ categories => 'e0' ], [ metonym => 'e0' ], @after,
  [ run => 'TryToSquint', [ actual => [ obj => 'G' ] ], [ intended => [ obj => 'e5' ] ] ],
  [ metonym => 'G' ], [ categories => 'G' ], [ items => 'G' ], @after,
  [ run => 'TryToSquint', [ actual => [ obj => 'G' ] ] ],
);

# --- LargeGroup ----------------------------------------------------------------------------
scenario(
  'large_group',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ gp => 'ALL', 'e0', 'e1', 'e2', 'e3', 'e4', 'e5' ],
  [ gp => 'L', 'e0', 'e1', 'e2', 'e3' ],
  [ gp => 'R', 'e2', 'e3', 'e4', 'e5' ],
  [ gp => 'M', 'e1', 'e2', 'e3', 'e4' ],
  [ flush => 'ALL' ], [ flush => 'L' ], [ flush => 'R' ], [ flush => 'M' ],
  [ run => 'LargeGroup', [ group => [ obj => 'ALL' ] ] ],
  [ run => 'LargeGroup', [ group => [ obj => 'L' ] ] ],
  [ run => 'LargeGroup', [ group => [ obj => 'R' ] ] ],
  [ run => 'LargeGroup', [ group => [ obj => 'M' ] ] ],
  @after,
  [ global => 'AtLeastOneUserVerification', 1 ],
  [ run => 'LargeGroup', [ group => [ obj => 'L' ] ] ],
  [ run => 'LargeGroup', [ group => [ obj => 'M' ] ] ],
  @after,
  [ run => 'LargeGroup', [ group => [ obj => 'R' ] ] ],
  @after,
  [ run => 'LargeGroup', [ group => [ obj => 'ALL' ] ] ],
  @after,
  [ run => 'LargeGroup' ],
);

# --- MaybeStartBlemish ---------------------------------------------------------------------
scenario(
  'maybe_start_blemish',
  [ init => [ 7, 1, 2, 3, 4, 5 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R', 'e1', 'e2', 'succ' ],
  # flush left: nothing.
  [ gp => 'F', 'e0', 'e1' ],
  [ run => 'MaybeStartBlemish', [ group => [ obj => 'F' ] ] ],
  @after,
  # extends leftward.
  [ gp => 'G', 'e2', 'e3', 'e4', 'e5' ],
  [ add => 'G' ],
  [ describe => 'G', 'ascending' ],
  [ ruleapp => 'G', 'R' ],
  [ run => 'MaybeStartBlemish', [ group => [ obj => 'G' ] ] ],
  [ items => 'G' ], @after,
  # no extension: blemish, numeric transform, flush right: ArbitraryInitialBlemish.
  [ run => 'MaybeStartBlemish', [ group => [ obj => 'G' ] ] ],
  [ items => 'G' ], @after,
  # not flush right: nothing.
  [ gp => 'H', 'e1', 'e2', 'e3' ],
  [ add => 'H' ],
  [ ruleapp => 'H', 'R' ],
  [ run => 'MaybeStartBlemish', [ group => [ obj => 'H' ] ] ],
  @after,
  # no rule app.
  [ gp => 'K', 'e3', 'e4', 'e5' ],
  [ run => 'MaybeStartBlemish', [ group => [ obj => 'K' ] ] ],
  @after,
  [ run => 'MaybeStartBlemish' ],
);

# --- ArbitraryInitialBlemish ---------------------------------------------------------------
for my $seed ( 1 .. 2 ) {
  scenario(
    "arbitrary_initial_blemish_$seed",
    [ init => [ 7, 1, 2, 3 ] ],
    [ probe => 'DescribeSolution' ],
    [ gp => 'G', 'e1', 'e2', 'e3' ],
    [ srand => $seed ],
    [ run => 'ArbitraryInitialBlemish', [ group => [ obj => 'G' ] ] ],
    ['probes'], ['rand'],
    [ global => 'TestingMode', 1 ],
    [ run => 'ArbitraryInitialBlemish', [ group => [ obj => 'G' ] ] ],
    ['probes'], ['rand'],
    [ run => 'ArbitraryInitialBlemish' ],
  );
}

# --- MaybeStartBlemish → InterlacedInitialBlemish ------------------------------------------
# Interlaced groups started on the wrong foot: 5 [1 6] [2 7] [3 8].
for my $seed ( 1 .. 3 ) {
  scenario(
    "interlaced_blemish_$seed",
    [ init => [ 5, 1, 6, 2, 7, 3, 8 ] ],
    [ spike => 'number', 10 ],
    [ gp => 'P1', 'e1', 'e2' ],
    [ add => 'P1' ],
    [ describe => 'P1', 'interlaced2' ],
    [ gp => 'P2', 'e3', 'e4' ],
    [ add => 'P2' ],
    [ describe => 'P2', 'interlaced2' ],
    [ gp => 'P3', 'e5', 'e6' ],
    [ add => 'P3' ],
    [ describe => 'P3', 'interlaced2' ],
    [ relnf => 'R', 'P1', 'P2' ],
    [ gp => 'G', 'P1', 'P2', 'P3' ],
    [ add => 'G' ],
    [ ruleapp => 'G', 'R' ],
    [ srand => $seed ],
    [ run => 'MaybeStartBlemish', [ group => [ obj => 'G' ] ] ],
    @after,
    [ run => 'InterlacedInitialBlemish', [ count => [ val => '2' ] ], [ group => [ obj => 'G' ] ],
      [ cat => [ cat => 'interlaced2' ] ] ],
    [ live => 'G' ], [ live => 'P1' ], [ live => 'P2' ], [ live => 'P3' ],
    [ name_new => 'N' ], ['stream'], ['hilit'], @after,
    # G is dead now.
    [ run => 'InterlacedInitialBlemish', [ count => [ val => '2' ] ], [ group => [ obj => 'G' ] ],
      [ cat => [ cat => 'interlaced2' ] ] ],
    @after,
    [ run => 'InterlacedInitialBlemish', [ group => [ obj => 'G' ] ] ],
  );
}

# Three parts of three, plus another live Interlaced_3 group that is deleted too.
scenario(
  'interlaced_blemish_three',
  [ init => [ 9, 1, 5, 7, 2, 6, 8, 3, 7, 9 ] ],
  [ spike => 'number', 10 ],
  [ gp => 'P1', 'e1', 'e2', 'e3' ],
  [ add => 'P1' ],
  [ describe => 'P1', 'interlaced3' ],
  [ gp => 'P2', 'e4', 'e5', 'e6' ],
  [ add => 'P2' ],
  [ describe => 'P2', 'interlaced3' ],
  [ gp => 'G', 'P1', 'P2' ],
  [ add => 'G' ],
  [ gp => 'Q', 'e7', 'e8', 'e9' ],
  [ add => 'Q' ],
  [ describe => 'Q', 'interlaced3' ],
  [ srand => 1 ],
  [ run => 'InterlacedInitialBlemish', [ count => [ val => 3 ] ], [ group => [ obj => 'G' ] ],
    [ cat => [ cat => 'interlaced3' ] ] ],
  [ live => 'G' ], [ live => 'P1' ], [ live => 'P2' ], [ live => 'Q' ],
  [ name_new => 'N' ], ['stream'], @after,
);

# Too few subparts for two new parts: nothing more after the deletions.
scenario(
  'interlaced_blemish_short',
  [ init => [ 1, 6, 2, 7 ] ],
  [ gp => 'P1', 'e0', 'e1' ],
  [ add => 'P1' ],
  [ gp => 'P2', 'e2', 'e3' ],
  [ add => 'P2' ],
  [ gp => 'G', 'P1', 'P2' ],
  [ add => 'G' ],
  [ run => 'InterlacedInitialBlemish', [ count => [ val => 2 ] ], [ group => [ obj => 'G' ] ],
    [ cat => [ cat => 'interlaced2' ] ] ],
  [ name_new => 'N' ], ['stream'], @after,
);

emit();
