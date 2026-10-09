# Oracle for SWorkspace.pm, part III (item 033): distance/position helpers, position
# structures, sameness groups, check_at_location, rapid_create_gp, __CopyAttributes,
# __PlonkIntoPlace, GetSomethingLike and LookForSomethingLike.
# Output: tests/golden/sworkspace_more.json
#
# Same scenario style as sworkspace_groups.pl: each case is a list of ops and one result
# per op; tests/test_sworkspace_more.py replays them. e0, e1, ... are the elements of the
# last init; other names come from "gp"/"obj"/"rapid"/"plonk" ops.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric';
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
my ( $SUPER, $LEFT, $RIGHT, $SPAN ) =
  @h1{qw(%SuperGroups_of %LeftEdge_of %RightEdge_of %Span_of)};
my ( $OBJECTS, $NONELT ) = @h2{qw(%Objects %NonEltObjects)};
die "PadWalker" unless $SUPER and $LEFT and $OBJECTS;

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
sub names { [ sort map { nm($_) } @_ ] }
sub O     { map { $obj{$_} } @_ }

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    my %r = ( class => ref($e), message => '' . ( $e->can('message') ? $e->message : $e ) );
    $r{next_elements} = [ map { 0 + $_ } @{ $e->next_elements } ] if $e->can('next_elements');
    return \%r;
  }
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/=HASH\(0x[0-9a-f]+\)/=HASH/g;
  return $e;
}

sub num_or_undef { defined $_[0] ? 0 + $_[0] : undef }

sub dir {
  my ($n) = @_;
  return undef unless defined $n;
  no strict 'refs';
  return ${"DIR::$n"};
}

sub dir_name {
  my ($d) = @_;
  for (qw(LEFT RIGHT UNKNOWN NEITHER)) {
    no strict 'refs';
    return $_ if $d eq ${"DIR::$_"};
  }
  return nm($d);
}

sub cat {
  my ($n) = @_;
  return SCategory::Interlaced->Create($1) if $n =~ /^interlaced(\d+)$/;
  my %c = (
    ascending  => $S::ASCENDING,
    descending => $S::DESCENDING,
    sameness   => $S::SAMENESS,
    number     => $S::NUMBER,
    mountain   => $S::MOUNTAIN,
  );
  return $c{$n} // die "unknown cat $n";
}

sub mode {
  my ($m) = @_;
  return undef unless defined $m;
  return $m eq 'group' ? $DISTANCE_MODE::GROUP : $DISTANCE_MODE::ELEMENT;
}

sub dist_out {
  my ($d) = @_;
  return [ 0 + $d->GetMagnitude, $d->[1]{mode} ];
}

sub cats_of { [ sort map { $_->get_name } @{ $_[0]->get_categories } ] }

sub describe_obj {
  my ($o) = @_;
  my $m = $o->get_metonym;
  return {
    text   => $o->as_text,
    cats   => cats_of($o),
    group_p => ( $o->get_group_p ? 1 : 0 ),
    meto   => ( $m ? [ $m->get_category->get_name, $m->get_name ] : undef ),
    meto_active => ( $o->get_metonym_activeness ? 1 : 0 ),
    live   => ( SWorkspace::__CheckLiveness($o) ? 1 : 0 ),
  };
}

# A rapid_create_gp spec is [cats, item, item, ...]; an item is a name or a nested spec.
sub rapid_args {
  my ($spec) = @_;
  my ( $cats, @items ) = @$spec;
  my @c = @$cats;
  my @out;
  while (@c) {
    my $next = shift @c;
    if ( $next eq 'metonym' ) { push @out, $next, cat( shift @c ), shift @c }
    else                      { push @out, cat($next) }
  }
  return ( \@out,
    map { ref($_) ? [ rapid_args($_) ] : $obj{$_} } @items );
}

sub state {
  my @live = sort { nm($a) cmp nm($b) } values %$OBJECTS;
  return [ map { [ nm($_), num_or_undef( $LEFT->{$_} ), num_or_undef( $RIGHT->{$_} ) ] } @live ];
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
  obj => sub {
    my ( $name, @spec ) = @_;
    my $o = Seqsee::Object->create(@spec);
    reg( $name, $o );
    return [ ref($o), $o->as_text ];
  },
  add      => sub { my @r = SWorkspace->add_group( $obj{ $_[0] } ); return [ map { num_or_undef($_) } @r ] },
  remove   => sub { SWorkspace->remove_gp( $obj{ $_[0] } ); return undef },
  describe => sub { return $obj{ $_[0] }->describe_as( cat( $_[1] ) ) ? 1 : 0 },
  metonym  => sub {
    my ( $name, $c, $mname ) = @_;
    my $o = $obj{$name};
    $o->AnnotateWithMetonym( cat($c), $mname );
    $o->SetMetonymActiveness(1);
    return undef;
  },
  meto_off => sub { $obj{ $_[0] }->SetMetonymActiveness(0); return undef },
  reln_scheme => sub { $obj{ $_[0] }->set_reln_scheme($RELN_SCHEME::CHAIN); return undef },
  info  => sub { return describe_obj( $obj{ $_[0] } ) },
  state => sub { return state() },

  # distances and positions
  distance => sub {
    my ( $a, $b, $m ) = @_;
    return dist_out( SWorkspace::__FindDistance( $obj{$a}, $obj{$b}, mode($m) ) );
  },
  distance_helper => sub {
    my ( $l, $r, $m ) = @_;
    return dist_out( SWorkspace::__FindDistanceHelper_( $l, $r, mode($m) ) );
  },
  pos_at => sub {
    my ( $from, $d, $mag, $m ) = @_;
    my $dist = $m eq 'group' ? DISTANCE::InGroups($mag) : DISTANCE::InElements($mag);
    my @r = SWorkspace::__GetPositionInDirectionAtDistance(
      { from_object => $obj{$from}, direction => dir($d), distance => $dist } );
    # A non-group distance on the right adds the DISTANCE ref's address.
    return [ map { !defined($_) ? undef : $_ > 1e6 ? 'huge' : 0 + $_ } @r ];
  },
  pos_at_raw => sub {
    my ( $from, $d, $distance ) = @_;
    my @r = SWorkspace::__GetPositionInDirectionAtDistance(
      { from_object => $obj{$from}, direction => dir($d), distance => $distance } );
    return [ map { num_or_undef($_) } @r ];
  },
  longest_exact => sub { return nm( scalar SWorkspace::__GetLongestNonAdHocWithEndsExactly(@_) ) },
  longest_lerb  => sub { return nm( scalar SWorkspace::__GetLongestNonAdHocWithLeftExactRightBelow(@_) ) },
  longest_start => sub { return nm( scalar SWorkspace->get_longest_non_adhoc_object_starting_at(@_) ) },
  longest_end   => sub { return nm( scalar SWorkspace->get_longest_non_adhoc_object_ending_at(@_) ) },
  intervening   => sub { return [ map { nm($_) } SWorkspace->get_intervening_objects(@_) ] },
  posstruct     => sub { return SWorkspace::__GetPositionStructure( $obj{ $_[0] } ) },
  posstruct_str => sub { return SWorkspace::__GetPositionStructureAsString( $obj{ $_[0] } ) },
  posstruct_obj => sub { return [ @{ PositionStructure->Create( $obj{ $_[0] } ) } ] },

  # sameness
  sameness_around => sub { return [ map { 0 + $_ } SWorkspace::__GetSamenessAround( $_[0] ) ] },
  sameness_group  => sub {
    my @before = values %$NONELT;
    my @r = SWorkspace::__CreateSamenessGroupAround( $_[0] );
    my %old = map { $_ => 1 } @before;
    my @new = grep { !$old{$_} } values %$NONELT;
    reg( $_[1], $new[0] ) if @new == 1 and defined $_[1];
    return [ [ map { num_or_undef($_) } @r ], [ map { describe_obj($_) } @new ] ];
  },

  # checking for elements
  check_at => sub {
    my ( $start, $d, $what ) = @_;
    my @r = SWorkspace->check_at_location( { start => $start, direction => dir($d), what => $obj{$what} } );
    return [ map { num_or_undef($_) } @r ];
  },
  check_rightward => sub {
    my ( $start, $mags ) = @_;
    my @r = SWorkspace::CheckElementsRightwardFromLocation( $start, $mags );
    return [ map { num_or_undef($_) } @r ];
  },

  rapid => sub {
    my ( $name, $spec ) = @_;
    my $o = SWorkspace->rapid_create_gp( rapid_args($spec) );
    reg( $name, $o );
    return describe_obj($o);
  },

  copy_attr => sub {
    my ( $from, $to ) = @_;
    my $r = SWorkspace::__CopyAttributes( { from => $obj{$from}, to => $obj{$to} } );
    return [ 0 + $r->success, describe_obj( $obj{$to} ) ];
  },
  copy_attr_missing => sub {
    my ( $from, $to ) = @_;
    SWorkspace::__CopyAttributes( { from => $obj{$from}, to => $obj{$to} } );
    return undef;
  },

  plonk => sub {
    my ( $name, $start, $d, $what ) = @_;
    my $r = SWorkspace::__PlonkIntoPlace( $start, dir($d), $obj{$what} );
    my $res = $r->resultant_object;
    reg( $name, $res ) if defined $res and !exists $name_of{$res};
    return {
      ok        => ( $r->PlonkWasSuccessful ? 1 : 0 ),
      resultant => nm($res),
      copy      => 0 + $r->attribute_copy_result->success,
      plonked   => nm( $r->object_being_plonked ),
      ( defined $res ? ( info => describe_obj($res) ) : () ),
    };
  },

  something_like => sub {
    my ( $what, $start, $d, $trust, $reason ) = @_;
    my @r = SWorkspace->GetSomethingLike(
      { object => $obj{$what}, start => $start, direction => dir($d), trust_level => $trust,
        reason => $reason } );
    return [ map { nm($_) } @r ];
  },
  look_like => sub {
    my ( $what, $start, $d ) = @_;
    my $r = SWorkspace->LookForSomethingLike(
      { object => $obj{$what}, start_position => $start, direction => dir($d) } );
    my $ask = $r->get_to_ask;
    my $lit = $r->get_literally_present;
    return {
      to_ask => (
        $ask
        ? { expected => nm( $ask->{expected_object} ), start => $ask->{start_position},
            exception => err( $ask->{exception} ) }
        : $ask
      ),
      literally_present => ( ref $lit ? [ 0 + $lit->[0], dir_name( $lit->[1] ), nm( $lit->[2] ) ] : $lit ),
      probable  => names( @{ $r->get_probable_matches } ),
      potential => names( @{ $r->get_potential_matches } ),
    };
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

# Workspace 1 2 3 4 5 6 7 8 9 10 with a few groups.
my @SEQ10 = ( 1 .. 10 );
my @GROUPS10 = (
  [ init => \@SEQ10 ],
  [ gp => 'A', 'e1', 'e2', 'e3' ],    # 2 3 4, ascending
  [ add => 'A' ],
  [ describe => 'A', 'ascending' ],
  [ gp => 'B', 'e5', 'e6' ],          # 6 7, no category (ad hoc)
  [ add => 'B' ],
  [ gp => 'C', 'e6', 'e7', 'e8' ],    # 7 8 9, conflicts with B? partial only
  [ add => 'C' ],
  [ describe => 'C', 'ascending' ],
  [ gp => 'I', 'e0', 'e1' ],
  [ add => 'I' ],
  [ describe => 'I', 'interlaced2' ],
  ['state'],
);

scenario(
  'longest_non_adhoc',
  @GROUPS10,
  [ info => 'I' ],
  [ longest_exact => 1, undef ],
  [ longest_exact => undef, 3 ],
  [ longest_exact => 5, undef ],
  [ longest_exact => undef, 6 ],
  [ longest_exact => 6, undef ],
  [ longest_exact => undef, 8 ],
  [ longest_exact => 0, undef ],
  [ longest_exact => undef, 1 ],
  [ longest_exact => 9, undef ],
  [ longest_exact => 12, undef ],
  [ longest_exact => -1, undef ],
  [ longest_exact => 1, 3 ],
  [ longest_exact => undef, undef ],
  [ longest_lerb => 1, 3 ],
  [ longest_lerb => 1, 2 ],
  [ longest_lerb => 1, 9 ],
  [ longest_lerb => 6, 8 ],
  [ longest_lerb => 6, 7 ],
  [ longest_lerb => 5, 9 ],
  [ longest_lerb => 0, 9 ],
  [ longest_lerb => 11, 12 ],
  [ longest_start => 1 ],
  [ longest_start => 0 ],
  [ longest_start => 5 ],
  [ longest_start => 6 ],
  [ longest_start => 9 ],
  [ longest_start => 10 ],
  [ longest_start => -2 ],
  [ longest_end => 3 ],
  [ longest_end => 1 ],
  [ longest_end => 6 ],
  [ longest_end => 8 ],
  [ longest_end => 10 ],
  [ longest_end => -1 ],
  [ intervening => 0, 9 ],
  [ intervening => 1, 4 ],
  [ intervening => 1, 2 ],
  [ intervening => 4, 8 ],
  [ intervening => 5, 6 ],
  [ intervening => 3, 2 ],
  [ intervening => 7, 10 ],
  [ intervening => 0, 0 ],
);

scenario(
  'distances',
  @GROUPS10,
  [ distance => 'e0', 'e1', 'element' ],
  [ distance => 'e0', 'e2', 'element' ],
  [ distance => 'e0', 'e9', 'element' ],
  [ distance => 'e9', 'e0', 'element' ],
  [ distance => 'e0', 'e9', 'group' ],
  [ distance => 'e9', 'e0', 'group' ],
  [ distance => 'e0', 'e5', 'group' ],
  [ distance => 'e4', 'e9', 'group' ],
  [ distance => 'e0', 'e4', 'group' ],
  [ distance => 'A', 'C', 'group' ],
  [ distance => 'A', 'C', 'element' ],
  [ distance => 'A', 'e1', 'group' ],
  [ distance => 'A', 'e4', 'element' ],
  [ distance => 'B', 'C', 'element' ],
  [ distance => 'I', 'B', 'group' ],
  [ distance => 'I', 'B', 'element' ],
  [ distance => 'e0', 'e0', 'group' ],
  [ srand => 3 ],
  [ distance => 'e0', 'e9' ],
  [ distance => 'e0', 'e9' ],
  [ distance => 'e0', 'e9' ],
  [ distance => 'e0', 'e9' ],
  [ distance => 'e0', 'e9' ],
  [ distance => 'e0', 'e9' ],
  [ distance => 'e0', 'e4' ],
  [ distance => 'e0', 'e4' ],
  [ distance => 'e0', 'e4' ],
  [ distance => 'e0', 'e4' ],
  [ distance => 'e0', 'e4' ],
  [ distance => 'e0', 'e4' ],
  [ distance => 'e0', 'e1' ],
  [ distance => 'e0', 'e1' ],
  ['rand'],
  [ distance_helper => 1, 8, 'group' ],
  [ distance_helper => 4, 8, 'group' ],
  [ distance_helper => 4, 4, 'group' ],
  [ distance_helper => 5, 4, 'group' ],
  [ distance_helper => 5, 4, 'element' ],
  [ distance_helper => 2, 7, 'element' ],
  [ remove => 'A' ],
  [ distance => 'A', 'e9', 'group' ],
  [ distance => 'e9', 'A', 'element' ],
);

scenario(
  'positions',
  @GROUPS10,
  [ pos_at => 'e0', 'RIGHT', 1, 'group' ],
  [ pos_at => 'e0', 'RIGHT', 2, 'group' ],
  [ pos_at => 'e0', 'RIGHT', 3, 'group' ],
  [ pos_at => 'e0', 'RIGHT', 0, 'group' ],
  [ pos_at => 'I', 'RIGHT', 1, 'group' ],
  [ pos_at => 'A', 'RIGHT', 1, 'group' ],
  [ pos_at => 'A', 'RIGHT', 2, 'group' ],
  [ pos_at => 'A', 'RIGHT', 4, 'group' ],
  [ pos_at => 'A', 'RIGHT', 6, 'group' ],
  [ pos_at => 'A', 'RIGHT', 7, 'group' ],
  [ pos_at => 'A', 'RIGHT', 1, 'element' ],
  [ pos_at => 'A', 'LEFT', 0, 'group' ],
  [ pos_at => 'A', 'LEFT', 1, 'group' ],
  [ pos_at => 'A', 'LEFT', 2, 'group' ],
  [ pos_at => 'e9', 'LEFT', 1, 'group' ],
  [ pos_at => 'e9', 'LEFT', 2, 'group' ],
  [ pos_at => 'e9', 'LEFT', 3, 'group' ],
  [ pos_at => 'e9', 'LEFT', 5, 'group' ],
  [ pos_at => 'e9', 'LEFT', 6, 'group' ],
  [ pos_at => 'e9', 'LEFT', 7, 'group' ],
  [ pos_at => 'e9', 'LEFT', 1, 'element' ],
  [ pos_at => 'e0', 'LEFT', 1, 'group' ],
  [ pos_at => 'e0', 'LEFT', 0, 'group' ],
  [ pos_at => 'e0', 'UNKNOWN', 1, 'group' ],
  [ pos_at_raw => 'e0', 'RIGHT', 2 ],
  [ pos_at_raw => 'e0', 'RIGHT', 0 ],
  [ pos_at_raw => 'e0', undef, 2 ],
  [ pos_at_raw => undef, 'RIGHT', 2 ],
  [ posstruct => 'A' ],
  [ posstruct => 'e4' ],
  [ posstruct_str => 'A' ],
  [ posstruct_str => 'e4' ],
  [ gp => 'N', 'A', 'e4' ],
  [ add => 'N' ],
  [ posstruct => 'N' ],
  [ posstruct_str => 'N' ],
  [ posstruct_obj => 'N' ],
  [ posstruct_obj => 'A' ],
  [ obj => 'X', 3, 4 ],
  [ posstruct => 'X' ],
);

scenario(
  'sameness',
  [ init => [ 1, 1, 1, 2, 3, 3, 4, 5, 5 ] ],
  [ sameness_around => 0 ],
  [ sameness_around => 1 ],
  [ sameness_around => 2 ],
  [ sameness_around => 3 ],
  [ sameness_around => 4 ],
  [ sameness_around => 5 ],
  [ sameness_around => 8 ],
  [ sameness_around => 7 ],
  [ sameness_around => 9 ],
  [ sameness_around => -1 ],
  [ sameness_group => 3 ],
  [ sameness_group => 1, 'S1' ],
  ['state'],
  [ sameness_group => 0 ],
  [ sameness_group => 4, 'S2' ],
  [ obj => 'X', 5, 5 ],
  [ metonym => 'e7', 'number', 'x' ],
  [ sameness_group => 8 ],
  [ srand => 1 ],
  [ sameness_group => 2 ],
  [ srand => 2 ],
  [ sameness_group => 2 ],
  [ srand => 4 ],
  [ sameness_group => 2 ],
  ['state'],
);

# Covered sameness runs: one toss(0.5) decides.
for my $seed ( 1 .. 6 ) {
  scenario(
    "sameness_covered_$seed",
    [ init => [ 7, 7, 7, 8 ] ],
    [ gp => 'A', 'e0', 'e1', 'e2', 'e3' ],
    [ add => 'A' ],
    [ srand => $seed ],
    [ sameness_group => 1 ],
    ['rand'],
    ['state'],
  );
}

scenario(
  'sameness_metonym',
  [ init => [ 2, 2, 2, 5, 5 ] ],
  [ gp => 'D', 'e3', 'e4' ],
  [ add => 'D' ],
  [ describe => 'D', 'sameness' ],
  [ metonym => 'D', 'sameness', 'each' ],
  [ info => 'D' ],
  [ sameness_group => 0 ],
  [ sameness_group => 3 ],
  [ meto_off => 'D' ],
  [ sameness_group => 3 ],
);

scenario(
  'check_at_location',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ obj => 'X', 2, 3 ],
  [ obj => 'Y', [ 4, 5 ], 6 ],
  [ obj => 'Z', 3, 2 ],
  [ obj => 'W', 6, 7, 8 ],
  [ gp => 'G', 'e2', 'e3' ],
  [ check_at => 1, 'RIGHT', 'X' ],
  [ check_at => 2, 'RIGHT', 'X' ],
  [ check_at => 2, 'LEFT', 'X' ],
  [ check_at => 1, 'LEFT', 'X' ],
  [ check_at => 0, 'LEFT', 'X' ],
  [ check_at => 3, 'RIGHT', 'Y' ],
  [ check_at => 5, 'LEFT', 'Y' ],
  [ check_at => 4, 'RIGHT', 'Y' ],
  [ check_at => 5, 'RIGHT', 'W' ],
  [ check_at => 6, 'RIGHT', 'W' ],
  [ check_at => 9, 'RIGHT', 'X' ],
  [ check_at => 4, 'RIGHT', 'W' ],
  [ check_at => 2, 'RIGHT', 'G' ],
  [ check_at => 3, 'LEFT', 'G' ],
  [ check_at => 2, 'RIGHT', 'e2' ],
  [ check_at => 0, 'LEFT', 'e0' ],
  [ check_at => -5, 'RIGHT', 'X' ],
  [ check_at => -3, 'RIGHT', 'Y' ],
  [ check_at => -9, 'RIGHT', 'X' ],
  [ check_at => 1, 'UNKNOWN', 'X' ],
  [ check_at => 1, undef, 'X' ],
  [ check_at => undef, 'RIGHT', 'X' ],
  [ check_rightward => 1, [ 2, 3, 4 ] ],
  [ check_rightward => 1, [ 2, 4 ] ],
  [ check_rightward => 4, [ 5, 6, 7, 8 ] ],
  [ check_rightward => 4, [ 5, 7, 7, 8 ] ],
  [ check_rightward => 6, [] ],
  [ check_rightward => 6, [1] ],
  [ check_rightward => -2, [ 5, 6 ] ],
);

scenario(
  'rapid_create',
  [ init => [ 1, 2, 3, 3, 3, 4, 5, 6 ] ],
  [ rapid => 'R1', [ ['ascending'], 'e0', 'e1', 'e2' ] ],
  [ rapid => 'R2', [ [ 'sameness', 'metonym', 'sameness', 'each' ], 'e3', 'e4' ] ],
  [ rapid => 'R3', [ [], 'e5', [ ['ascending'], 'e6', 'e7' ] ] ],
  ['state'],
  [ rapid => 'R4', [ ['ascending'], 'e0', 'e1' ] ],
  [ rapid => 'R5', [ ['descending'], 'e2', 'e3' ] ],
  ['state'],
  [ rapid => 'R6', [ [ 'metonym', 'number', 'x' ], 'e6', 'e7' ] ],
  [ rapid => 'R7', [ [ 'metonym', 'ascending', 'x' ], 'e5', 'e6' ] ],
);

scenario(
  'copy_attributes',
  [ init => [ 1, 2, 3, 4, 4, 4, 1, 2, 3 ] ],
  [ gp => 'A', 'e0', 'e1', 'e2' ],
  [ add => 'A' ],
  [ describe => 'A', 'ascending' ],
  [ reln_scheme => 'A' ],
  [ gp => 'B', 'e6', 'e7', 'e8' ],
  [ add => 'B' ],
  [ copy_attr => 'A', 'B' ],
  [ gp => 'S', 'e3', 'e4', 'e5' ],
  [ add => 'S' ],
  [ describe => 'S', 'sameness' ],
  [ metonym => 'S', 'sameness', 'each' ],
  [ gp => 'T', 'e3', 'e4' ],
  [ copy_attr => 'S', 'T' ],
  [ obj => 'X', 4, 4, 4 ],
  [ copy_attr => 'S', 'X' ],
  [ copy_attr => 'A', 'S' ],
  [ copy_attr => 'e0', 'e1' ],
  [ copy_attr_missing => 'A', 'nothing' ],
  [ copy_attr_missing => 'nothing', 'A' ],
  [ meto_off => 'S' ],
  [ obj => 'Y', 4, 4, 4 ],
  [ copy_attr => 'S', 'Y' ],
);

scenario(
  'plonk',
  [ init => [ 1, 2, 3, 4, 5, 6, 7 ] ],
  [ obj => 'X', 2, 3 ],
  [ obj => 'Y', [ 4, 5 ], 6 ],
  [ obj => 'E', 3 ],
  [ obj => 'Z', 3, 4, 9 ],
  [ obj => 'W', 7, 8 ],
  [ obj => 'Q', 6, 7 ],
  [ describe => 'X', 'ascending' ],
  [ describe => 'Y', 'ascending' ],
  [ plonk => 'pE', 2, 'RIGHT', 'E' ],
  [ plonk => 'pE2', 3, 'RIGHT', 'E' ],
  [ plonk => 'pE3', 2, 'LEFT', 'E' ],
  [ plonk => 'pE4', -5, 'RIGHT', 'E' ],
  [ plonk => 'pX', 1, 'RIGHT', 'X' ],
  ['state'],
  [ plonk => 'pX2', 1, 'RIGHT', 'X' ],
  [ plonk => 'pX3', 2, 'LEFT', 'X' ],
  [ plonk => 'pX4', 0, 'LEFT', 'X' ],
  [ plonk => 'pX5', 2, 'RIGHT', 'X' ],
  [ plonk => 'pY', 3, 'RIGHT', 'Y' ],
  ['state'],
  [ plonk => 'pY2', 5, 'LEFT', 'Y' ],
  [ plonk => 'pZ', 2, 'RIGHT', 'Z' ],
  [ plonk => 'pW', 6, 'RIGHT', 'W' ],
  [ plonk => 'pQ', 5, 'RIGHT', 'Q' ],
  [ plonk => 'pQ2', 6, 'LEFT', 'Q' ],
  ['state'],
  [ plonk => 'pE5', 9, 'RIGHT', 'E' ],
);

# Plonking where a conflicting group is locked: add_group fails.
scenario(
  'plonk_conflict',
  [ init => [ 1, 2, 3, 4, 5 ] ],
  [ gp => 'G', 'e1', 'e2', 'e3' ],
  [ add => 'G' ],
  [ obj => 'X', 2, 3 ],
  [ plonk => 'pX', 1, 'RIGHT', 'X' ],
  [ obj => 'Y', 1, 2, 3, 4 ],
  [ plonk => 'pY', 0, 'RIGHT', 'Y' ],
  ['state'],
);

for my $seed ( 1 .. 4 ) {
  scenario(
    "something_like_$seed",
    [ init => [ 1, 2, 3, 4, 5, 6 ] ],
    [ gp => 'A', 'e1', 'e2' ],
    [ add => 'A' ],
    [ obj => 'X', 2, 3 ],
    [ obj => 'Y', 4, 5 ],
    [ obj => 'Z', 5, 6, 7 ],
    [ obj => 'V', 9, 9 ],
    [ srand => $seed ],
    [ something_like => 'X', 1, 'RIGHT', 0.5 ],
    [ something_like => 'X', 2, 'LEFT', 0.5 ],
    [ something_like => 'Y', 3, 'RIGHT', 0.5 ],
    ['state'],
    [ something_like => 'Z', 4, 'RIGHT', 0, 'why' ],
    [ something_like => 'V', 4, 'RIGHT', 0.5 ],
    [ something_like => 'V', 4, 'LEFT', 0.5 ],
    ['rand'],
  );
}

scenario(
  'something_like_errors',
  [ init => [ 1, 2, 3 ] ],
  [ obj => 'X', 2, 3 ],
  [ something_like => undef, 1, 'RIGHT', 0.5 ],
  [ something_like => 'X', undef, 'RIGHT', 0.5 ],
  [ something_like => 'X', 1, undef, 0.5 ],
  [ something_like => 'X', 1, 'RIGHT', undef ],
  [ something_like => 'X', 1, 'UNKNOWN', 0.5 ],
);

scenario(
  'look_for_something_like',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ gp => 'A', 'e1', 'e2' ],
  [ add => 'A' ],
  [ gp => 'B', 'e3', 'e4', 'e5' ],
  [ add => 'B' ],
  [ obj => 'X', 2, 3 ],
  [ obj => 'Y', 4, 5 ],
  [ obj => 'Z', 5, 6, 7, 8 ],
  [ obj => 'T', 4, 5, 6 ],
  [ look_like => 'X', 1, 'RIGHT' ],
  [ look_like => 'X', 2, 'LEFT' ],
  [ look_like => 'Y', 3, 'RIGHT' ],
  [ look_like => 'T', 3, 'RIGHT' ],
  [ look_like => 'T', 5, 'LEFT' ],
  [ look_like => 'Z', 4, 'RIGHT' ],
  [ look_like => 'Y', 1, 'RIGHT' ],
  [ look_like => 'X', 0, 'RIGHT' ],
  [ look_like => 'X', 1, 'UNKNOWN' ],
  [ look_like => undef, 1, 'RIGHT' ],
  [ look_like => 'X', 1, undef ],
  ['state'],
);

emit();
