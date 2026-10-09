# Oracle for SRule.pm and SRuleApp.pm (item 025).
# Output: tests/golden/srule.json
#
# Objects are real Seqsee::Elements/Anchored groups; transforms are real mappings plus
# FakeMapping. Workspace/coderack/LTM pieces that aren't ported yet are recorders:
#   SWorkspace::__FindObjectSetDirection  (needs %LeftEdge_of; replaced by the same loop
#                                          over get_left_edge)
#   SWorkspace->GetSomethingLike, SWorkspace->check_at_location   (item 033)
#   SCoderack->add_codelet, SCodelet->new                          (items 035/036)
#   SLTM::GetRealActivationsForConcepts (group creation)           (item 029)
#   main::message (GUI)
use strict;
use Oracle;
use S;

our @LOG;
our $GSL = 'FOUND';     # what GetSomethingLike returns
our $CHECK = 0;         # what check_at_location returns
our $CHECK_DIES;        # check_at_location dies with this, if set

package FakeMapping;    # a Mapping whose FlippedVersion is scripted
our @ISA = ('Mapping');
sub new { my ( $p, %h ) = @_; bless {%h}, $p }
sub FlippedVersion { $_[0]->{flip} }
sub as_text        { 'fake ' . $_[0]->{name} }
sub CheckSanity    { exists $_[0]->{sane} ? $_[0]->{sane} : 1 }

package main;

sub D { my ($d) = @_; return defined($d) ? ( ref($d) eq 'DIR' ? $d->{text} : "$d" ) : undef }

sub T {    # text of an object/value for logs
  my ($x) = @_;
  return undef unless defined $x;
  return ref($x) ? ( $x->can('as_text') ? $x->as_text : ref($x) ) : $x;
}

{
  no warnings 'redefine';
  *SLTM::GetRealActivationsForConcepts = sub { return [ map {0} @{ $_[0] } ] };
  *main::message = sub { push @LOG, [ 'message', mask( $_[0] ) ] };
  *SWorkspace::__FindObjectSetDirection = sub {
    my (@objects) = @_;
    my @left_edges = map { defined($_) ? $_->get_left_edge : undef } @objects;
    my $how_many = scalar(@objects);
    Carp::confess "Need at least 2" if $how_many <= 1;
    my ( $leftward, $rightward );
    for ( 0 .. $how_many - 2 ) {
      my $diff = $left_edges[ $_ + 1 ] - $left_edges[$_];
      if    ( $diff > 0 ) { $rightward++ }
      elsif ( $diff < 0 ) { $leftward++ }
      else                { return $DIR::UNKNOWN }
    }
    return $DIR::NEITHER if ( $leftward and $rightward );
    return $DIR::LEFT    if $leftward;
    return $DIR::RIGHT   if $rightward;
    Carp::confess "huh?";
  };
  *SWorkspace::GetSomethingLike = sub {
    my ( $p, $o ) = @_;
    push @LOG, [ 'gsl', $o->{object}->get_structure_string, $o->{start}, D( $o->{direction} ),
      $o->{trust_level}, $o->{reason}, [ map { $_->as_text } @{ $o->{hilit_set} } ] ];
    return $GSL;
  };
  *SWorkspace::check_at_location = sub {
    my ( $p, $o ) = @_;
    push @LOG, [ 'check', $o->{start}, D( $o->{direction} ),
      ( defined $o->{what} ? $o->{what}->get_structure_string : undef ) ];
    die $CHECK_DIES if $CHECK_DIES;
    return $CHECK;
  };
  *SCoderack::add_codelet = sub {
    my ( $p, $c ) = @_;
    push @LOG, [ 'add_codelet', @$c ];
  };
  *SCodelet::new = sub {
    my ( $p, $family, $urgency, $args ) = @_;
    return [ $family, $urgency, [ sort keys %$args ], $args->{exception} ];
  };
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    my $m = $e->can('message') ? $e->message : "$e";
    $m =~ s/ at \S+ line \d+.*//s;
    $m =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
    return { class => ref($e), message => $m };
  }
  my $s = "$e";
  $s =~ s/ at \S+ line \d+.*//s;
  $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
  return { class => 'DIE', message => $s };
}

sub mask { my ($s) = @_; return undef unless defined $s; $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g; $s }
sub log_take { my @l = @LOG; @LOG = (); return \@l }

sub E { Seqsee::Element->create(@_) }
sub G { Seqsee::Anchored->create(@_) }

# Elements by name: "a<mag>_<pos>". The Python test builds the same ones.
my %OBJ;
sub O {
  my ($n) = @_;
  return undef unless defined $n;
  return $OBJ{$n} //= do {
    if    ( $n =~ /^a(\d+)_(\d+)$/ ) { E( $1, $2 ) }
    elsif ( $n =~ /^g\((.*)\)$/ )    { G( map { O($_) } split /,/, $1 ) }
    else                             { die "bad name $n" }
  };
}

my %TYPE = (
  succ  => Mapping::Numeric->create( 'succ', $S::NUMBER ),
  pred  => Mapping::Numeric->create( 'pred', $S::NUMBER ),
  same  => Mapping::Numeric->create( 'same', $S::NUMBER ),
  noflip   => FakeMapping->new( name => 'noflip' ),
  fakeflip => FakeMapping->new( name => 'y', flip => FakeMapping->new( name => 'yflip' ) ),
  insane   => FakeMapping->new( name => 'z', flip => FakeMapping->new( name => 'zflip', sane => 0 ) ),
);
$TYPE{struct} = Mapping::Structural->create(
  { category => $S::ASCENDING, meto_mode => $METO_MODE::NONE, direction_reln => Mapping::Dir->create('Same'),
    changed_bindings => { start => $TYPE{succ} }, slippages => {} } );
# struct's flip fails CheckSanity (ascending needs start and end), so SRule->create dies on it.
$TYPE{struct2} = Mapping::Structural->create(
  { category => $S::ASCENDING, meto_mode => $METO_MODE::NONE, direction_reln => Mapping::Dir->create('Same'),
    changed_bindings => { start => $TYPE{succ}, end => $TYPE{succ} }, slippages => {} } );
$TYPE{rel} = SRelation->new( { first => O('a1_0'), second => O('a2_1'), type => $TYPE{succ} } );
$TYPE{relpred} = SRelation->new( { first => O('a2_1'), second => O('a1_0'), type => $TYPE{pred} } );

sub V {
  my ($v) = @_;
  return $v unless defined $v;
  return $TYPE{$v} if exists $TYPE{$v};
  return $DIR::RIGHT   if $v eq 'RIGHT';
  return $DIR::LEFT    if $v eq 'LEFT';
  return $DIR::UNKNOWN if $v eq 'UNKNOWN';
  return [] if $v eq 'ARRAY';
  return O($v) if $v =~ /^(a\d|g\()/;
  return $v;
}

sub rule_desc {
  my ($r) = @_;
  return {
    ref     => ref($r),
    as_text => $r->as_text,
    transform => T( $r->get_transform ),
    flipped   => T( $r->get_flipped_transform ),
  };
}

sub app_desc {
  my ($a) = @_;
  my @items = $a->get_all_items;
  my %d = (
    ref       => ref($a),
    as_text   => mask( $a->as_text ),
    direction => D( $a->get_direction ),
    items     => [ map { T($_) } @items ],
    rule      => ( ref( $a->get_rule ) && $a->get_rule->can('as_text') ? $a->get_rule->as_text : T( $a->get_rule ) ),
  );
  if ( @items and ref $items[0] ) {
    $d{edges} = [ $a->get_edges ];
    $d{span}  = $a->get_span;
  }
  return \%d;
}

# --- SRule->create ------------------------------------------------------------------------
for my $t (qw(succ pred same struct struct2 noflip fakeflip insane rel relpred), 5, 'str', undef, 'RIGHT', 'ARRAY') {
  my $v = V($t);
  my @r = eval { SRule->create($v) };
  my $e = err($@);
  my %c = ( kind => 'create', type => $t, err => $e, n => scalar(@r), log => log_take() );
  if ( @r and $r[0] ) {
    $c{rule} = rule_desc( $r[0] );
    my $again = SRule->create($v);
    $c{memo} = ( $again == $r[0] ? 1 : 0 );
  }
  elsif (@r) {
    $c{value} = $r[0];
  }
  record(%c);
}
{
  # A relation's rule is the rule of its type.
  my $r1 = SRule->create( $TYPE{rel} );
  my $r2 = SRule->create( $TYPE{succ} );
  record( kind => 'create_rel_same', same => ( $r1 == $r2 ? 1 : 0 ) );
  my @r = eval { SRule->create() };
  record( kind => 'create', type => 'NOARGS', err => err($@), n => scalar(@r) );
  @r = eval { SRule->create( $TYPE{succ}, $TYPE{pred} ) };
  record( kind => 'create', type => 'TWOARGS', err => err($@), n => scalar(@r) );
}

# --- SRule->new (Class::Std) ----------------------------------------------------------------
for my $args ( [], [ transform => 'succ' ], [ flipped_transform => 'pred' ],
  [ transform => 'succ', flipped_transform => 'pred' ], [ transform => 5, flipped_transform => 6 ],
  [ transform => undef, flipped_transform => undef ] )
{
  my %h = @$args;
  $h{$_} = V( $h{$_} ) for keys %h;
  my $r = eval { SRule->new( \%h ) };
  my $e = err($@);
  my %c = ( kind => 'rule_new', args => $args, err => $e );
  if ($r) {
    $c{transform} = T( $r->get_transform );
    $c{flipped}   = T( $r->get_flipped_transform );
    $c{as_text}   = eval { $r->as_text };
    $c{as_text_err} = err($@);
  }
  record(%c);
}
{
  my $r = SRule->new( { transform => $TYPE{succ}, flipped_transform => $TYPE{pred} } );
  $r->set_transform( $TYPE{same} );
  $r->set_flipped_transform(undef);
  record( kind => 'rule_set', rule => rule_desc($r) );
}

# --- CreateApplication ----------------------------------------------------------------------
for my $args ( [ start => 'a1_0', direction => 'RIGHT' ], [ direction => 'RIGHT' ], [ start => 'a1_0' ],
  [ start => 0, direction => 'RIGHT' ], [ start => 'a1_0', direction => 'LEFT' ],
  [ start => 'a1_0', direction => 'right' ], [ start => 'x', direction => 'RIGHT' ], [] )
{
  my %h = @$args;
  $h{$_} = V( $h{$_} ) for keys %h;
  my $rule = SRule->create( $TYPE{succ} );
  my $a = eval { $rule->CreateApplication( \%h ) };
  my $e = err($@);
  my %c = ( kind => 'create_app', args => $args, err => $e );
  $c{app} = app_desc($a) if $a;
  $c{rule_is} = ( $a->get_rule == $rule ? 1 : 0 ) if $a;
  record(%c);
}

# --- SRuleApp->new (Moose) ------------------------------------------------------------------
for my $args ( [], [ rule => 'succ' ], [ direction => 'RIGHT' ], [ rule => 'succ', direction => 'RIGHT' ],
  [ rule => 'succ', direction => 'LEFT' ], [ rule => 'succ', direction => undef ],
  [ rule => 'succ', direction => 'right' ], [ rule => undef, direction => 'RIGHT' ],
  [ rule => 'succ', direction => 'RIGHT', items => 5 ], [ rule => 'succ', direction => 'LEFT', items => 5 ],
  [ items => 5 ], [ rule => 'succ', direction => 'RIGHT', items => 'ARRAY' ],
  [ rule => 'succ', direction => 'RIGHT', items => undef ], [ rule => 5, direction => 'RIGHT', items => 'a1_0' ] )
{
  my %h = @$args;
  $h{$_} = V( $h{$_} ) for keys %h;
  my $a = eval { SRuleApp->new( \%h ) };
  my $e = err($@);
  my %c = ( kind => 'app_new', args => $args, err => $e );
  if ($a) {
    $c{items_n}   = scalar( $a->get_all_items );
    $c{direction} = D( $a->get_direction );
    $c{rule}      = T( $a->get_rule );
  }
  record(%c);
}
{
  my $a = eval { SRuleApp->new( rule => 5, direction => $DIR::RIGHT ) };
  record( kind => 'app_new_list', err => err($@), ok => ( $a ? 1 : 0 ) );
}

# --- SRuleApp accessors ---------------------------------------------------------------------
{
  my $rule = SRule->create( $TYPE{succ} );
  my $a = SRuleApp->new( { rule => $rule, direction => $DIR::RIGHT, items => [ O('a1_0'), O('a2_1') ] } );
  my @steps;
  push @steps, [ 'init', app_desc($a) ];
  $a->push_item( O('a3_2') );
  push @steps, [ 'push', app_desc($a) ];
  $a->unshift_item( O('a0_5') );
  push @steps, [ 'unshift', app_desc($a) ];
  $a->set_items( [ O('a2_1') ] );
  push @steps, [ 'set_items', app_desc($a) ];
  eval { $a->set_items(5) };
  push @steps, [ 'set_items_bad', err($@), app_desc($a) ];
  $a->set_direction($DIR::LEFT);
  push @steps, [ 'set_direction', app_desc($a) ];
  $a->set_rule( SRule->create( $TYPE{pred} ) );
  push @steps, [ 'set_rule', app_desc($a) ];
  my $ref = $a->get_items;
  push @steps, [ 'get_items_ref', ref($ref), scalar(@$ref) ];
  my $empty = SRuleApp->new( { rule => $rule, direction => $DIR::RIGHT } );
  eval { $empty->get_edges };
  push @steps, [ 'empty_edges', err($@) ];
  eval { $empty->get_span };
  push @steps, [ 'empty_span', err($@) ];
  record( kind => 'accessors', steps => \@steps );
}

# --- CheckApplicability ---------------------------------------------------------------------
for my $case (
  [ succ => qw(a1_0 a2_1 a3_2) ],
  [ succ => qw(a1_0 a2_1) ],
  [ succ => qw(a1_0 a2_1 a4_2) ],
  [ succ => qw(a1_0 a3_2 a5_4) ],
  [ succ => qw(a1_0 a2_2 a3_5) ],
  [ succ => qw(a1_0) ],
  [ succ => () ],
  [ succ => qw(a1_4 a2_3 a3_2) ],
  [ succ => qw(a1_0 a2_0) ],
  [ succ => qw(a1_0 a2_3 a3_2) ],
  [ pred => qw(a3_0 a2_1 a1_2) ],
  [ pred => qw(a1_0 a2_1) ],
  [ same => qw(a2_0 a2_1 a2_2 a2_3) ],
  [ struct2 => qw(g(a1_0,a2_1) g(a2_2,a3_3) g(a3_4,a4_5)) ],
  [ struct2 => qw(g(a1_0,a2_1) g(a2_2,a3_3) g(a4_4,a5_5)) ],
  [ succ => qw(g(a1_0,a2_1) g(a2_2,a3_3)) ],
  [ fakeflip => qw(a1_0 a2_1) ],
  )
{
  my ( $t, @names ) = @$case;
  my $rule = SRule->create( $TYPE{$t} );
  my @r = eval { $rule->CheckApplicability( { objects => [ map { O($_) } @names ] } ) };
  my $e = err($@);
  my %c = ( kind => 'check_app', type => $t, objects => \@names, err => $e, n => scalar(@r),
    log => log_take() );
  if ( @r and $r[0] ) {
    $c{app} = app_desc( $r[0] );
    $c{rule_is} = ( $r[0]->get_rule == $rule ? 1 : 0 );
  }
  record(%c);
}
{
  my $rule = SRule->create( $TYPE{succ} );
  eval { $rule->CheckApplicability( {} ) };
  record( kind => 'check_app_noobjects', err => err($@) );
}

# --- FindExtension --------------------------------------------------------------------------
$SWorkspace::ElementCount = 6;
my @fe = (
  [ 'r3', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'RIGHT' } ],
  [ 'r3s0', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'RIGHT', skip_this_many_elements => 0 } ],
  [ 'r3s1', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'RIGHT', skip_this_many_elements => 1 } ],
  [ 'r3s2', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'RIGHT', skip_this_many_elements => 2 } ],
  [ 'r3s3', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'RIGHT', skip_this_many_elements => 3 } ],
  [ 'r3s9', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'RIGHT', skip_this_many_elements => 9 } ],
  [ 'l3', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'LEFT' } ],
  [ 'l3s1', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'LEFT', skip_this_many_elements => 1 } ],
  [ 'l3edge', 'succ', [qw(a1_0 a2_1 a3_2)], { direction_to_extend_in => 'LEFT' } ],
  [ 'pred_r', 'pred', [qw(a7_2 a6_3 a5_4)], { direction_to_extend_in => 'RIGHT' } ],
  [ 'pred_l', 'pred', [qw(a7_2 a6_3 a5_4)], { direction_to_extend_in => 'LEFT' } ],
  [ 'same_r', 'same', [qw(a3_1 a3_2)], { direction_to_extend_in => 'RIGHT' } ],
  [ 'nodir', 'succ', [qw(a5_2 a6_3 a7_4)], {} ],
  [ 'unknown', 'succ', [qw(a5_2 a6_3 a7_4)], { direction_to_extend_in => 'UNKNOWN' } ],
  [ 'groups_num', 'succ', [qw(g(a1_0,a2_1) g(a2_2,a3_3))], { direction_to_extend_in => 'RIGHT' } ],
  [ 'groups_struct', 'struct2', [qw(g(a1_0,a2_1) g(a2_2,a3_3))], { direction_to_extend_in => 'RIGHT' } ],
  [ 'groups_struct_l', 'struct2', [qw(g(a2_2,a3_3) g(a3_4,a4_5))], { direction_to_extend_in => 'LEFT' } ],
  [ 'fake_r', 'fakeflip', [qw(a5_2 a6_3)], { direction_to_extend_in => 'RIGHT' } ],
);
for my $fe (@fe) {
  my ( $name, $t, $names, $opts ) = @$fe;
  my %o = %$opts;
  $o{direction_to_extend_in} = V( $o{direction_to_extend_in} ) if exists $o{direction_to_extend_in};
  my $a = SRuleApp->new( { rule => SRule->create( $TYPE{$t} ), direction => $DIR::RIGHT,
      items => [ map { O($_) } @$names ] } );
  my @r = eval { $a->FindExtension( \%o ) };
  my $e = err($@);
  record( kind => 'find_ext', name => $name, err => $e, n => scalar(@r), ret => ( @r ? T( $r[0] ) : undef ),
    log => log_take() );
}
{
  # Odd rules: no flipped transform, a non-Mapping transform.
  my $rule = SRule->new( { transform => $TYPE{succ}, flipped_transform => undef } );
  my $a = SRuleApp->new( { rule => $rule, direction => $DIR::RIGHT, items => [ O('a5_2'), O('a6_3') ] } );
  for my $d (qw(LEFT RIGHT)) {
    my @r = eval { $a->FindExtension( { direction_to_extend_in => V($d) } ) };
    record( kind => 'find_ext_odd', name => "noflip_$d", err => err($@), n => scalar(@r), log => log_take() );
  }
  $rule->set_transform(5);
  my @r = eval { $a->FindExtension( { direction_to_extend_in => $DIR::RIGHT } ) };
  record( kind => 'find_ext_odd', name => 'strange', err => err($@), n => scalar(@r), log => log_take() );
  $rule->set_transform('');
  @r = eval { $a->FindExtension( { direction_to_extend_in => $DIR::RIGHT } ) };
  record( kind => 'find_ext_odd', name => 'strange_empty', err => err($@), n => scalar(@r), log => log_take() );
  local $GSL = undef;
  $rule->set_transform( $TYPE{succ} );
  @r = eval { $a->FindExtension( { direction_to_extend_in => $DIR::RIGHT } ) };
  record( kind => 'find_ext_odd', name => 'gsl_undef', err => err($@), n => scalar(@r), log => log_take() );
}
for my $ec ( 0, 2, 10 ) {
  local $SWorkspace::ElementCount = $ec;
  my $a = SRuleApp->new( { rule => SRule->create( $TYPE{succ} ), direction => $DIR::RIGHT,
      items => [ O('a5_2'), O('a6_3') ] } );
  $a->FindExtension( { direction_to_extend_in => $DIR::RIGHT } );
  record( kind => 'find_ext_count', count => $ec, log => log_take() );
}

# --- Seqsee::Object integration: set_underlying_ruleapp, Anchored FindExtension -----------------
for my $case ( [ succ => qw(a1_10 a2_11 a3_12) ], [ rel => qw(a1_10 a2_11 a3_12) ],
  [ pred => qw(a1_10 a2_11 a3_12) ], [ noflip => qw(a1_10 a2_11) ], [ insane => qw(a1_10 a2_11) ],
  [ struct => qw(g(a1_20,a2_21) g(a2_22,a3_23)) ],
  [ struct2 => qw(g(a1_20,a2_21) g(a2_22,a3_23)) ] )
{
  my ( $t, @names ) = @$case;
  my $g = G( map { O($_) } @names );
  my @r = eval { $g->set_underlying_ruleapp( $TYPE{$t} ) };
  my $e = err($@);
  my $u = $g->get_underlying_reln;
  my %c = ( kind => 'set_underlying', type => $t, objects => \@names, err => $e, n => scalar(@r),
    hist => [ map { mask($_) } grep {/Underlying/} @{ $g->get_history } ],
    has_reln => ( $u ? 1 : 0 ), log => log_take() );
  $c{app} = app_desc($u) if $u;
  if ($u) {
    for my $d (qw(RIGHT LEFT)) {
      for my $skip ( 0, 1 ) {
        my @x = eval { $g->FindExtension( V($d), $skip ) };
        push @{ $c{find} }, [ $d, $skip, err($@), scalar(@x), log_take() ];
      }
    }
  }
  record(%c);
}

# --- CheckConsitencyOfGroup -----------------------------------------------------------------
{
  my $ga = O('g(a1_30,a2_31)');
  my $gb = O('g(a3_32,a4_33)');
  my $nested = G( $ga, $gb );
  my $gc = O('g(a5_34,a6_35)');
  my $app = SRuleApp->new( { rule => SRule->create( $TYPE{struct2} ), direction => $DIR::RIGHT,
      items => [ $nested, $gc ] } );
  my $own = G( O('a2_31'), O('a3_32') );
  my %G = (
    nested => $nested, ga => $ga, gb => $gb, gc => $gc,
    straddle => $own,
    outside => O('g(a6_35,a7_36)'),
    left_out => O('g(a9_29,a1_30)'),
    whole => G( $nested, $gc ),
    elem => O('a1_30'),
  );
  for my $n ( sort keys %G ) {
    my @r = eval { $app->CheckConsitencyOfGroup( $G{$n} ) };
    record( kind => 'consistency', group => $n, err => err($@), ret => ( @r ? $r[0] : undef ) );
  }
  # A group whose underlying reln is the app itself.
  $own->set_underlying_reln($app);
  record( kind => 'consistency', group => 'straddle_own', ret => $app->CheckConsitencyOfGroup($own) );
}

# --- Extend* ----------------------------------------------------------------------------------
sub mkapp {
  my ($t) = @_;
  return SRuleApp->new( { rule => SRule->create( $TYPE{ $t // 'succ' } ), direction => $DIR::RIGHT,
      items => [ O('a1_0'), O('a2_1'), O('a3_2') ] } );
}
$SWorkspace::ElementCount = 6;
for my $sc (
  [ 'fwd_no', 'ExtendForward', undef, 0, undef ],
  [ 'fwd2_no', 'ExtendForward', 2, 0, undef ],
  [ 'fwd0', 'ExtendForward', 0, 0, undef ],
  [ 'fwd_yes', 'ExtendForward', 1, 1, undef ],
  [ 'back_no', 'ExtendBackward', 1, 0, undef ],
  [ 'back_yes', 'ExtendBackward', 1, 1, undef ],
  [ 'right_no', 'ExtendRight', 1, 0, undef ],
  [ 'left_no', 'ExtendLeft', 1, 0, undef ],
  [ 'left_max', 'ExtendLeftMaximally', undef, 0, undef ],
  [ 'fwd_die_str', 'ExtendForward', 1, 0, "boom\n" ],
  [ 'fwd_die_serr', 'ExtendForward', 1, 0, 'SErr' ],
  [ 'pred_no', 'ExtendForward', 1, 0, undef, 'pred' ],
  [ 'fake_no', 'ExtendForward', 1, 0, undef, 'fakeflip' ],
  )
{
  my ( $name, $m, $steps, $check, $dies, $t ) = @$sc;
  local $CHECK = $check;
  local $CHECK_DIES = ( defined $dies and $dies eq 'SErr' ) ? SErr->new('ws err') : $dies;
  my $a = mkapp($t);
  my @r = eval { $a->$m($steps) };
  my $e = err($@);
  my $s = eval { mkapp($t)->$m($steps) };    # scalar context
  record( kind => 'extend', name => $name, err => $e, ret => [@r], scalar => $s, app => app_desc($a),
    log => log_take() );
}
# ElementsBeyondKnownSought: toss with 0.5 * total span / ElementCount, then a codelet.
for my $seed ( 1, 2, 3, 4, 5, 6 ) {
  for my $ec ( 3, 12 ) {
    local $SWorkspace::ElementCount = $ec;
    local $CHECK_DIES = SErr::ElementsBeyondKnownSought->new( next_elements => [4] );
    my $a = mkapp();
    srand($seed);
    my @r = eval { $a->ExtendForward(2) };
    my $e = err($@);
    record( kind => 'extend_beyond', seed => $seed, count => $ec, err => $e, ret => [@r], log => log_take(),
      next_rand => rand() );
  }
}
# _ExtendOneStep option checks.
for my $opts ( {}, { items_ref => [] }, { items_ref => [], direction_to_extend_in => 'RIGHT' },
  { items_ref => [], direction_to_extend_in => 'RIGHT', object_at_end => 'a1_0' },
  { items_ref => [], direction_to_extend_in => 'RIGHT', object_at_end => 'a1_0', transform => 'succ' },
  { items_ref => [], direction_to_extend_in => 'RIGHT', object_at_end => 'a1_0', transform => 'succ',
    extend_at_start_or_end => 'middle' } )
{
  my %o = %$opts;
  for (qw(direction_to_extend_in object_at_end transform)) {
    $o{$_} = V( $o{$_} ) if exists $o{$_};
  }
  local $CHECK = 0;
  my @r = eval { SRuleApp::_ExtendOneStep( \%o ) };
  record( kind => 'extend_one', keys => [ sort keys %$opts ], err => err($@), n => scalar(@r),
    log => log_take() );
}

emit();
