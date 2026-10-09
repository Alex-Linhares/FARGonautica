# Oracle for Seqsee/SCF_MX/AllMX.pm (item 042): the codelet families CheckIfInstance, FocusOn,
# ActOnOverlappingThoughts, AreTheseGroupable, AreWeDone (+ BelieveDone), ConvulseEnd and
# CheckProgress (+ CalculateDesperation).
# Output: tests/golden/scf_allmx.json
#
# Same op-driven style (and helpers) as scf_general2.pl; tests/test_scf_allmx.py replays the
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

# --- CheckIfInstance -----------------------------------------------------------------------
scenario(
  'check_if_instance',
  [ init => [ 1, 2, 3, 3, 1, 2 ] ],
  [ gp => 'G', 'e0', 'e1', 'e2' ],
  [ add => 'G' ],
  [ gp => 'H', 'e3', 'e4', 'e5' ],
  [ add => 'H' ],
  [ run => 'CheckIfInstance', [ obj => [ obj => 'G' ] ], [ cat => [ cat => 'ascending' ] ] ],
  [ activation => 'ascending' ],
  @after,
  [ feature => 'LTM', 1 ],
  [ run => 'CheckIfInstance', [ obj => [ obj => 'H' ] ], [ cat => [ cat => 'ascending' ] ] ],
  [ activation => 'ascending' ],
  @after,
  [ run => 'CheckIfInstance', [ obj => [ obj => 'G' ] ], [ cat => [ cat => 'ascending' ] ] ],
  [ activation => 'ascending' ],
  @after,
  [ run => 'CheckIfInstance', [ obj => [ obj => 'e0' ] ], [ cat => [ cat => 'odd' ] ] ],
  [ activation => 'odd' ],
  @after,
  [ run => 'CheckIfInstance', [ obj => [ obj => 'G' ] ] ],
  [ run => 'CheckIfInstance' ],
);

# --- FocusOn -------------------------------------------------------------------------------
for my $seed ( 1 .. 3 ) {
  scenario(
    "focus_on_what_$seed",
    [ init => [ 1, 2, 3, 4, 7 ] ],
    [ reln => 'R01', 'e0', 'e1', 'succ' ],
    [ srand => $seed ],
    [ run => 'FocusOn', [ what => [ obj => 'e1' ] ] ],
    ['stream'], @after,
    [ run => 'FocusOn', [ what => [ obj => 'R01' ] ] ],
    ['stream'], @after,
    [ run => 'FocusOn', [ what => [ cat => 'ascending' ] ] ],
    ['stream'], @after,
    [ run => 'FocusOn', [ what => [ obj => 'e1' ] ] ],
    ['stream'], @after,
    [ run => 'FocusOn', [ what => [ val => 'foo' ] ] ],
    [ run => 'FocusOn', [ bogus => [ val => 1 ] ] ],
  );
}

for my $seed ( 1 .. 8 ) {
  scenario(
    "focus_on_reader_$seed",
    [ init => [ 1, 1, 1, 2, 3, 4, 4 ] ],
    [ srand => $seed ],
    [ run => 'FocusOn' ],
    ['readhead'], ['stream'], @after,
    [ run => 'FocusOn', [ what => [ val => 0 ] ] ],
    ['readhead'], ['stream'], @after,
    [ run => 'FocusOn', [ what => [ undef => 1 ] ] ],
    ['readhead'], ['stream'], @after,
    [ set_readhead => 5 ],
    [ run => 'FocusOn' ],
    ['readhead'], ['stream'], @after,
  );
}

# --- ActOnOverlappingThoughts --------------------------------------------------------------
for my $seed ( 1 .. 3 ) {
  scenario(
    "act_on_overlapping_thoughts_$seed",
    [ init => [ 1, 2, 3, 5, 4, 9 ] ],
    [ spike => 'number', 10 ],
    [ reln => 'R01', 'e0', 'e1', 'succ' ],
    [ reln => 'R12', 'e1', 'e2', 'succ' ],
    [ thought => 'T0', 'e0' ],
    [ thought => 'T1', 'e1' ],
    [ thought => 'T3', 'e3' ],
    [ thought => 'T4', 'e4' ],
    [ thought => 'TR01', 'R01' ],
    [ thought => 'TR12', 'R12' ],
    [ thought_cat => 'TA', 'ascending' ],
    [ srand => $seed ],
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'T3' ] ], [ b => [ obj => 'T4' ] ] ],
    @after,
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'T1' ] ], [ b => [ obj => 'T0' ] ] ],
    @after,
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'TR12' ] ], [ b => [ obj => 'TR01' ] ] ],
    @after,
    # mixed or other types: nothing (and no draw).
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'T0' ] ], [ b => [ obj => 'TR01' ] ] ],
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'TA' ] ], [ b => [ obj => 'T0' ] ] ],
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'TA' ] ], [ b => [ obj => 'TA' ] ] ],
    # not thoughts: no core.
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'e0' ] ], [ b => [ obj => 'e1' ] ] ],
    [ run => 'ActOnOverlappingThoughts', [ a => [ val => 'foo' ] ], [ b => [ obj => 'T0' ] ] ],
    @after,
    [ run => 'ActOnOverlappingThoughts', [ a => [ undef => 1 ] ], [ b => [ obj => 'T0' ] ] ],
    [ run => 'ActOnOverlappingThoughts', [ a => [ obj => 'T0' ] ] ],
  );
}

# --- AreTheseGroupable ---------------------------------------------------------------------
scenario(
  'are_these_groupable',
  [ init => [ 1, 2, 3, 4, 5, 6, 2, 4, 6, 7, 7, 3, 2, 1 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R01', 'e0', 'e1', 'succ' ],
  [ run => 'AreTheseGroupable', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ reln => [ obj => 'R01' ] ] ],
  @after,
  # exists already.
  [ run => 'AreTheseGroupable', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ reln => [ obj => 'R01' ] ] ],
  @after,
  # unsorted items: the sorted copy is not used.
  [ run => 'AreTheseGroupable', [ items => [ list => 'e5', 'e4', 'e3' ] ], [ reln => [ obj => 'R01' ] ] ],
  @after,
  [ run => 'AreTheseGroupable', [ items => [ list => ] ], [ reln => [ obj => 'R01' ] ] ],
  # not adjacent.
  [ run => 'AreTheseGroupable', [ items => [ list => 'e3', 'e5' ] ], [ reln => [ obj => 'R01' ] ] ],
  # dead item.
  [ gp => 'X', 'e3', 'e4' ],
  [ run => 'AreTheseGroupable', [ items => [ list => 'X', 'e5' ] ], [ reln => [ obj => 'R01' ] ] ],
  @after,
  [ reln => 'R9', 'e9', 'e10', 'same' ],
  [ run => 'AreTheseGroupable', [ items => [ list => 'e9', 'e10' ] ], [ reln => [ obj => 'R9' ] ] ],
  @after,
  [ reln => 'RP', 'e11', 'e12', 'pred' ],
  [ run => 'AreTheseGroupable', [ items => [ list => 'e11', 'e12', 'e13' ] ], [ reln => [ obj => 'RP' ] ] ],
  @after,
  # a relation that does not fit the items: describe_as fails, the group stays.
  [ run => 'AreTheseGroupable', [ items => [ list => 'e3', 'e4', 'e5' ] ], [ reln => [ obj => 'RP' ] ] ],
  @after,
  [ run => 'AreTheseGroupable', [ items => [ list => 'e0' ] ] ],
);

for my $seed ( 1 .. 6 ) {
  scenario(
    "are_these_groupable_conflict_$seed",
    [ init => [ 1, 2, 3, 4, 5 ] ],
    [ spike => 'number', 10 ],
    [ gp => 'B', 'e2', 'e3' ],
    [ add => 'B' ],
    [ describe => 'B', 'ascending' ],
    [ reln => 'R', 'e0', 'e1', 'succ' ],
    [ srand => $seed ],
    [ run => 'AreTheseGroupable', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ reln => [ obj => 'R' ] ] ],
    @after,
  );
}

scenario(
  'are_these_groupable_groups',
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
  [ run => 'AreTheseGroupable', [ items => [ list => 'A', 'B', 'C' ] ], [ reln => [ obj => 'R1' ] ] ],
  @after,
);

# --- AreWeDone -----------------------------------------------------------------------------
scenario(
  'are_we_done',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ spike => 'number', 10 ],
  [ probe => 'DescribeSolution' ],
  [ probe => 'AttemptExtensionOfGroup' ],
  [ reln => 'R01', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e0', 'e1', 'e2', 'e3', 'e4', 'e5' ],
  [ add => 'G' ],
  [ ruleapp => 'G', 'R01' ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['recent', 'G'], ['hilit'], ['probes'], ['last_solution'], ['rand'],
  [ global => 'AtLeastOneUserVerification', 1 ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['hilit'], ['probes'], ['last_solution'], ['rand'],
  # LastSolutionDescriptionTime is 0 (false): it fires again.
  [ global => 'Steps_Finished', 10 ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['probes'], ['last_solution'], ['rand'],
  # 10 > TimeOfLastNewElement (0): no.
  [ global => 'Steps_Finished', 30 ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['probes'], ['last_solution'], ['rand'],
  [ global => 'TimeOfLastNewElement', 20 ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['probes'], ['last_solution'], ['rand'],
  [ global => 'TestingMode', 1 ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['probes'], ['last_solution'], ['rand'],
  [ run => 'AreWeDone' ],
);

for my $seed ( 1 .. 4 ) {
  scenario(
    "are_we_done_partial_$seed",
    [ init => [ 1, 2, 3, 4, 5, 6 ] ],
    [ spike => 'number', 10 ],
    [ probe => 'DescribeSolution' ],
    [ probe => 'AttemptExtensionOfGroup' ],
    [ reln => 'R01', 'e0', 'e1', 'succ' ],
    [ gp => 'G', 'e0', 'e1', 'e2', 'e3', 'e4' ],
    [ add => 'G' ],
    [ ruleapp => 'G', 'R01' ],
    [ global => 'AtLeastOneUserVerification', 1 ],
    [ srand => $seed ],
    [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
    ['recent', 'G'], ['hilit'], ['probes'], ['rand'],
  );
}

scenario(
  'are_we_done_small',
  [ init => [ 1, 2, 3, 4, 5, 6, 7 ] ],
  [ spike => 'number', 10 ],
  [ probe => 'DescribeSolution' ],
  [ probe => 'AttemptExtensionOfGroup' ],
  [ global => 'AtLeastOneUserVerification', 1 ],
  [ reln => 'R12', 'e1', 'e2', 'succ' ],
  [ gp => 'G', 'e1', 'e2', 'e3', 'e4', 'e5', 'e6' ],
  [ add => 'G' ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['recent', 'G'], ['probes'], ['rand'],
  [ ruleapp => 'G', 'R12' ],
  [ run => 'AreWeDone', [ group => [ obj => 'G' ] ] ],
  ['recent', 'G'], ['probes'], ['rand'],
  [ gp => 'H', 'e0', 'e1', 'e2' ],
  [ run => 'AreWeDone', [ group => [ obj => 'H' ] ] ],
  ['probes'], ['rand'],
);

# --- ConvulseEnd ---------------------------------------------------------------------------
scenario(
  'convulse_end_plain',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R01', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e0', 'e1', 'e2', 'e3' ],
  [ add => 'G' ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  ['items', 'G'], @after,
  [ ruleapp => 'G', 'R01' ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  ['items', 'G'], @after,
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'LEFT' ] ] ],
  ['items', 'G'], @after,
  [ gp => 'D', 'e4', 'e5' ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'D' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ] ],
);

# The last item is replaced by a different object that fits the rule.
for my $seed ( 1 .. 4 ) {
  scenario(
    "convulse_end_replace_$seed",
    [ init => [ 1, 1, 2, 2, 2, 2, 7 ] ],
    [ gp => 'A', 'e0', 'e1' ],
    [ add => 'A' ],
    [ describe => 'A', 'sameness' ],
    [ gp => 'B', 'e2', 'e3' ],
    [ describe => 'B', 'sameness' ],
    [ relnf => 'R', 'A', 'B' ],
    [ gp => 'L', 'e2', 'e3', 'e4', 'e5' ],
    [ add => 'L' ],
    [ describe => 'L', 'sameness' ],
    [ gp => 'G', 'A', 'L' ],
    [ add => 'G' ],
    [ fake_ruleapp => 'G', 'R' ],
    ['items', 'G'],
    [ srand => $seed ],
    [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
    ['items', 'G'], ['supergroups', 'L'], [ name_new => 'N' ], @after,
  );
}

# Leftward, and with a supergroup (Extend may refuse; the ejected item is put back).
for my $seed ( 1 .. 4 ) {
  scenario(
    "convulse_end_left_$seed",
    [ init => [ 3, 3, 3, 3, 4, 4, 5, 5, 9 ] ],
    [ gp => 'L', 'e0', 'e1', 'e2', 'e3' ],
    [ add => 'L' ],
    [ describe => 'L', 'sameness' ],
    [ gp => 'B', 'e4', 'e5' ],
    [ add => 'B' ],
    [ describe => 'B', 'sameness' ],
    [ gp => 'C', 'e6', 'e7' ],
    [ add => 'C' ],
    [ describe => 'C', 'sameness' ],
    [ relnf => 'R', 'B', 'C' ],
    [ gp => 'G', 'L', 'B', 'C' ],
    [ add => 'G' ],
    [ fake_ruleapp => 'G', 'R' ],
    [ gp => 'S', 'G', 'e8' ],
    [ add => 'S' ],
    [ srand => $seed ],
    [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'LEFT' ] ] ],
    ['items', 'G'], ['supergroups', 'L'], ['supergroups', 'G'], [ name_new => 'N' ], @after,
  );
}

# Mismatched rule app: the pre-extension SanityCheck fails.
scenario(
  'convulse_end_insane',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R01', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e0', 'e1', 'e2', 'e3' ],
  [ add => 'G' ],
  [ fake_ruleapp => 'G', 'R01', 'e0', 'e1', 'e2' ],
  [ global => 'CurrentRunnableString', 'SCodelet' ],
  # Without a current codelet, SanityFail dies building its message.
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ current_codelet => 'ConvulseEnd' ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ fake_ruleapp => 'G', 'R01', 'e0', 'e1', 'e3', 'e2' ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  [ ruleapp => 'G', 'R01' ],
  [ run => 'ConvulseEnd', [ object => [ obj => 'G' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  ['items', 'G'],
);

# --- CheckProgress -------------------------------------------------------------------------
scenario(
  'calculate_desperation',
  map { [ desperation => @$_ ] }[ 0, 0 ], [ 199, 5000 ], [ 200, 0 ], [ 499, 9999 ], [ 500, 0 ],
  [ 799, 2500 ], [ 800, 2499 ], [ 800, 2500 ], [ 1499, 0 ], [ 1500, 0 ], [ 9999, 9999 ],
);

for my $seed ( 1 .. 6 ) {
  scenario(
    "check_progress_$seed",
    [ init => [ 1, 2, 3, 4, 5, 6 ] ],
    [ spike => 'number', 10 ],
    [ reln => 'R01', 'e0', 'e1', 'succ' ],
    [ gp => 'G', 'e2', 'e3', 'e4' ],
    [ add => 'G' ],
    [ describe => 'G', 'ascending' ],
    [ srand => $seed ],
    # Too soon after the last run (0).
    [ global => 'Steps_Finished', 99 ],
    [ run => 'CheckProgress' ],
    ['last_progress'], ['probes'], @after,
    # desperation 0.
    [ global => 'Steps_Finished', 150 ],
    [ global => 'TimeOfNewStructure', 100 ],
    [ run => 'CheckProgress' ],
    ['last_progress'], ['probes'], @after,
    # desperation 20: relations may be uninserted.
    [ global => 'Steps_Finished', 400 ],
    [ run => 'CheckProgress' ],
    ['last_progress'], ['probes'], @after,
    [ global => 'Steps_Finished', 450 ],
    [ run => 'CheckProgress' ],
    ['last_progress'],
    # desperation 40: a group is removed.
    [ global => 'Steps_Finished', 700 ],
    [ global => 'TimeOfNewStructure', 0 ],
    [ run => 'CheckProgress' ],
    ['last_progress'], ['probes'], @after,
    # desperation 80: ask for more terms.
    [ global => 'Steps_Finished', 1600 ],
    [ run => 'CheckProgress' ],
    ['last_progress'], ['probes'], @after,
    [ global => 'Steps_Finished', 4000 ],
    [ global => 'TimeOfLastNewElement', 1000 ],
    [ run => 'CheckProgress' ],
    ['last_progress'], ['probes'], @after,
    [ run => 'CheckProgress', [ x => [ val => 1 ] ] ],
  );
}

# AreTheseGroupable with a category other than NUMBER: MappingBased. Mapping::Numeric's create
# memo is keyed by LTM indices and survives SLTM->Clear, so "succ" on a category could come
# back as an older scenario's "succ" with the same index. This scenario comes last and gives
# EVEN a high index (no earlier "succ" key uses it), so nothing collides either way.
scenario(
  'are_these_groupable_even',
  [ init => [ 2, 4, 6, 8 ] ],
  ( map { [ spike => $_, 1 ] } qw(prime odd mountain sameness descending ascending even) ),
  [ reln => 'R01', 'e0', 'e1', 'succ', 'even' ],
  [ run => 'AreTheseGroupable', [ items => [ list => 'e0', 'e1', 'e2' ] ], [ reln => [ obj => 'R01' ] ] ],
  @after,
);

emit();
