# Oracle for Seqsee/Object.pm, part II (item 022): ResultOfCanBeSeenAs, metonyms,
# CanBeSeenAs, apply_blemish_at, relations, strength, underlying rule apps,
# squintability, get_pure, and FindMappingForCat/ApplyMapping on real objects.
# Output: tests/golden/seqsee_object2.json
#
# Leaves are real Seqsee::Elements (the Python test rebuilds them with a fake).
# SRelation->new, FindMapping (as seen from Object.pm), SRule->create,
# SLTM::GetRealActivationsForConcepts and SLTM::Platonic->create are replaced by
# recorders below; they belong to later items.
use strict;
use Oracle;
use S;
use Scalar::Util qw(refaddr);

our @LOG;

package FakeCat;
use Moose;
has name  => ( is => 'ro' );
has inst  => ( is => 'rw', default => sub { {} } );    # structure string => bindings
has metos => ( is => 'rw', default => sub { {} } );    # meto name => starred structure
sub Instancer { $_[0]->inst->{ $_[1]->get_structure_string } }
sub build                          { }
sub get_name                       { $_[0]->name }
sub as_text                        { 'fake ' . $_[0]->name }
sub AreAttributesSufficientToBuild { 1 }
sub get_meto_types                 { sort keys %{ $_[0]->metos } }
sub get_pure                       { $_[0] }
sub get_memory_dependencies        { () }
sub serialize                      { $_[0]->name }
sub deserialize                    { }
sub get_meto_finder {
  my ( $self, $name ) = @_;
  return sub {
    my ( $obj, $cat, $n, $b ) = @_;
    push @main::LOG, [ 'finder', $cat->get_name, $n, $b ];
    my $s = $cat->metos->{$n};
    return unless defined $s;
    return SMetonym->new(
      { category => $cat, name => $n, starred => Seqsee::Object->create($s), unstarred => $obj, info_loss => {} } );
  };
}
sub find_metonym {
  my ( $self, $obj, $name ) = @_;
  return $self->get_meto_finder($name)->( $obj, $self, $name, 'FIND' );
}
with 'SCategory';

package FakeEnd;
use Moose;
extends 'Seqsee::Object';
has bounds => ( is => 'ro' );
sub get_bounds_string { $_[0]->bounds }

package FakeReln;
use Moose;
has label => ( is => 'ro' );
has ends  => ( is => 'ro', default => sub { [] } );
has type  => ( is => 'ro' );
sub get_ends { @{ $_[0]->ends } }
sub get_type { $_[0]->type }
sub insert   { push @main::LOG, 'insert ' . $_[0]->label }
sub uninsert { push @main::LOG, 'uninsert ' . $_[0]->label }

package FakeType;
sub new          { my ( $p, %h ) = @_; bless {%h}, $p }
sub get_category { $_[0]->{cat} }

package FakeRCat;    # only FindMappingForCat is used
sub new { my ( $p, %h ) = @_; bless {%h}, $p }
sub FindMappingForCat {
  my ( $self, $f, $s ) = @_;
  my $k = $f->get_bounds_string . $s->get_bounds_string;
  push @main::LOG, "FindMappingForCat $k";
  return $self->{table}{$k};
}

package FakeRule;
our @ISA = ('SRule');
sub new { my ( $p, %h ) = @_; bless {%h}, $p }
sub CheckApplicability {
  my ( $self, $opts ) = @_;
  push @main::LOG,
    [ 'check', $self->{label}, [ map { $_->get_structure_string } @{ $opts->{objects} } ],
    ( $opts->{direction} eq $DIR::RIGHT ? 1 : 0 ), join( ',', sort keys %$opts ) ];
  return $self->{app};
}

package FakeApp;
sub new { bless {}, $_[0] }

package FakeSRel;
our @ISA = ('SRelation');
sub new { bless {}, $_[0] }

package main;

our $NEXT_RULE;
{
  no warnings 'redefine';
  *SRelation::new = sub {
    my ( $p, $o ) = @_;
    my $label = join( ',', $o->{first}->get_bounds_string, $o->{second}->get_bounds_string, $o->{type} // 'undef' );
    push @LOG, "new $label";
    return if ( $o->{type} // '' ) eq 'NONE';
    return FakeReln->new( label => $label, ends => [ $o->{first}, $o->{second} ], type => $o->{type} );
  };
  *Seqsee::Object::FindMapping = sub {
    my ( $a, $b ) = @_;
    my $k = $a->get_bounds_string . $b->get_bounds_string;
    push @LOG, "FindMapping $k";
    return { AB => 'TAB', BC => 'NONE', CD => undef }->{$k} // "T$k";
  };
  *SRule::create = sub {
    my ( $p, $r ) = @_;
    push @LOG, [ 'SRule->create', ref($r) ];
    return $NEXT_RULE;
  };
  our %ACT;
  *SLTM::GetRealActivationsForConcepts = sub {
    my ($cats) = @_;
    push @LOG, [ 'activations', [ map { $_->get_name } @$cats ] ];
    return [ map { $ACT{ $_->get_name } } @$cats ];
  };
  *SLTM::Platonic::create = sub { 'PLATONIC(' . $_[1] . ')' };
}

sub norm {
  my ($s) = @_;
  return $s unless defined $s;
  $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
  $s =~ s/\b(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/$1(REF)/g;
  return $s;
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    return { class => ref($e), message => norm( $e->message ) };
  }
  $e =~ s/ \(defined at .*//s;
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  return norm($e);
}

sub names { [ sort map { defined($_) ? $_->get_name : 'UNDEF' } @{ $_[0]->get_categories } ] }
sub same { ( defined( $_[0] ) && defined( $_[1] ) && ref( $_[0] ) && refaddr( $_[0] ) == refaddr( $_[1] ) ) ? 1 : 0 }
sub ss   { my ($o) = @_; return undef unless defined $o; return ref($o) ? $o->get_structure_string : "$o" }

# A ResultOfCanBeSeenAs, observably.
sub rd {
  my ($r) = @_;
  return { undef => 1 } unless defined $r;
  return { plain => $r } unless ref $r;
  my $parts = $r->GetPartsBlemished;
  return {
    success  => $r->success,
    bool     => ( $r ? 1 : 0 ),
    is_no    => same( $r, Seqsee::ResultOfCanBeSeenAs::NO() ),
    entire_p => ( $r->IsEntireBlemished ? 1 : 0 ),
    entire   => ( ref( $r->GetEntireBlemish ) ? ss( $r->GetEntireBlemish->get_starred ) : $r->GetEntireBlemish ),
    parts_p  => ( $r->ArePartsBlemished ? 1 : 0 ),
    parts    => ( $parts ? { map { $_ => ss( $parts->{$_}->get_starred ) } keys %$parts } : undef ),
    blemished => ( $r->IsBlemished ? 1 : 0 ),
  };
}

# --- ResultOfCanBeSeenAs -------------------------------------------------------------------
{
  my $NO = Seqsee::ResultOfCanBeSeenAs::NO();
  record( case => 'result', name => 'NO', r => rd($NO), again_same => same( $NO, Seqsee::ResultOfCanBeSeenAs->NO ) );
  my $u = Seqsee::ResultOfCanBeSeenAs->newUnblemished;
  record( case => 'result', name => 'unblemished', r => rd($u),
    fresh => ( same( $u, Seqsee::ResultOfCanBeSeenAs->newUnblemished ) ? 0 : 1 ) );
  my $m = SMetonym->new( { category => $S::SAMENESS, name => 'each', info_loss => { length => 2 },
      starred => Seqsee::Object->create(7), unstarred => Seqsee::Object->create(7, 7) } );
  record( case => 'result', name => 'entire', r => rd( Seqsee::ResultOfCanBeSeenAs->newEntireBlemish($m) ) );
  my $eu = Seqsee::ResultOfCanBeSeenAs->newEntireBlemish(undef);
  record( case => 'result', name => 'entire undef', r => rd($eu) );
  record( case => 'result', name => 'by part', r => rd( Seqsee::ResultOfCanBeSeenAs->newByPart( { 2 => $m } ) ) );
  record( case => 'result', name => 'by part empty', r => rd( Seqsee::ResultOfCanBeSeenAs->newByPart( {} ) ) );
  for my $c (
    [ 'no success',        {} ],
    [ 'success 2',         { success => 2 } ],
    [ 'success "0"',       { success => '0' } ],
    [ 'success undef',     { success => undef } ],
    [ 'part_blemish undef', { success => 1, part_blemish => undef } ],
    [ 'part_blemish array', { success => 1, part_blemish => [] } ],
    [ 'all bad',           { success => 2, part_blemish => 3 } ],
    [ 'entire 0',          { success => 1, entire_blemish => 0 } ],
    )
  {
    my ( $name, $args ) = @$c;
    my $r = eval { Seqsee::ResultOfCanBeSeenAs->new($args) };
    record( case => 'result new', name => $name, ( defined($r) ? ( r => rd($r) ) : ( error => err($@) ) ) );
  }
}

# --- objects used below --------------------------------------------------------------------
sub fresh_objects {
  my %o;
  $o{e5}   = Seqsee::Element->create( 5, 0 );
  $o{g123} = Seqsee::Object->create( 1, 2, 3 );
  $o{gn}   = Seqsee::Object->create( 1, [ 2, 3 ] );
  $o{one}  = Seqsee::Object->new( { group_p => 1, items => [ Seqsee::Element->create( 5, 0 ) ] } );
  $o{emp}  = Seqsee::Object->create();
  $o{bl}   = Seqsee::Object->create( 1, 2, 3 )->apply_blemish_at( $S::DOUBLE, SPos->new(2) );
  $o{blin} = Seqsee::Object->create( 1, 2, 3 )->apply_blemish_at( $S::DOUBLE, SPos->new(2) );
  $o{blin}->[1]->SetMetonymActiveness(0);
  $o{mid}   = $o{bl}->[1];
  $o{midin} = $o{blin}->[1];
  return \%o;
}

# --- apply_blemish_at ------------------------------------------------------------------------
sub item_info {
  my ($x) = @_;
  my $m = $x->get_metonym;
  return {
    ref        => ref($x),
    structure  => $x->get_structure_string,
    active     => $x->get_metonym_activeness,
    has_meto   => ( $m ? 1 : 0 ),
    starred    => ( $m ? ss( $m->get_starred ) : undef ),
    meto_name  => ( $m ? $m->get_name : undef ),
    meto_cat   => ( $m ? $m->get_category->get_name : undef ),
    info_loss  => ( $m ? $m->get_info_loss : undef ),
    unstarred_defined => ( $m ? ( defined( $m->get_unstarred ) ? 1 : 0 ) : undef ),
    starred_is_a_metonym_is_x => ( $m ? same( $m->get_starred->get_is_a_metonym, $x ) : undef ),
    cats       => names($x),
    history    => [ @{ $x->get_history } ],
    metonymed  => $x->IsThisAMetonymedObject,
    contains   => $x->ContainsAMetonym,
    concrete_is_self => same( $x->GetConcreteObject, $x ),
  };
}
{
  my $orig = Seqsee::Object->create( 1, 2, 3 );
  my $e2   = $orig->[1];
  my $bl   = $orig->apply_blemish_at( $S::DOUBLE, SPos->new(2) );
  record(
    case       => 'blemish',
    name       => 'middle',
    ref        => ref($bl),
    structure  => $bl->get_structure_string,
    annotated  => $bl->GetAnnotatedStructureString,
    effective  => $bl->GetEffectiveStructureString,
    effective_structure => $bl->GetEffectiveStructure,
    cats       => names($bl),
    history    => [ @{ $bl->get_history } ],
    items      => [ map { item_info($_) } @$bl ],
    orig_e2_is_a_metonym_is_mid => same( $e2->get_is_a_metonym, $bl->[1] ),
    orig_e2_metonymed => $e2->IsThisAMetonymedObject,
    orig_e2_concrete_is_mid => same( $e2->GetConcreteObject, $bl->[1] ),
    orig_contains => $orig->ContainsAMetonym,
    orig_structure => $orig->get_structure_string,
    contains   => $bl->ContainsAMetonym,
    slippages  => { map { $_ => ss( $bl->GetEffectiveSlippages->{$_}->get_starred ) } keys %{ $bl->GetEffectiveSlippages } },
    mid_effective_is_e2 => same( $bl->[1]->GetEffectiveObject, $e2 ),
    mid_effective_structure => $bl->[1]->GetEffectiveStructure,
    e2_effective_structure  => $e2->GetEffectiveStructure,
  );
  $bl->[1]->SetMetonymActiveness(0);
  record(
    case      => 'blemish',
    name      => 'middle, deactivated',
    annotated => $bl->GetAnnotatedStructureString,
    effective => $bl->GetEffectiveStructureString,
    slippages => [ keys %{ $bl->GetEffectiveSlippages } ],
    mid_effective_is_mid => same( $bl->[1]->GetEffectiveObject, $bl->[1] ),
    contains  => $bl->ContainsAMetonym,
    mid_history => [ @{ $bl->[1]->get_history } ],
  );

  for my $c ( [ 'first', 1 ], [ 'last (-1)', -1 ] ) {
    my $o = Seqsee::Object->create( 4, 5 );
    my $r = $o->apply_blemish_at( $S::DOUBLE, SPos->new( $c->[1] ) );
    record( case => 'blemish', name => $c->[0], structure => $r->get_structure_string,
      annotated => $r->GetAnnotatedStructureString, items => [ map { item_info($_) } @$r ] );
  }
  {
    my $o = Seqsee::Object->create( 1, [ 2, 3 ] );
    my $r = $o->apply_blemish_at( $S::DOUBLE, SPos->new(2) );
    record( case => 'blemish', name => 'group part', structure => $r->get_structure_string,
      annotated => $r->GetAnnotatedStructureString, effective => $r->GetEffectiveStructureString,
      items => [ map { item_info($_) } @$r ] );
  }
  {
    my $e = Seqsee::Element->create( 5, 0 );
    my $r = $e->apply_blemish_at( $S::DOUBLE, SPos->new(1) );
    record( case => 'blemish', name => 'element', ref => ref($r), structure => $r->get_structure_string,
      annotated => $r->GetAnnotatedStructureString, items => [ map { item_info($_) } @$r ],
      e_is_a_metonym_is_r0 => same( $e->get_is_a_metonym, $r->[0] ) );
  }
  for my $c ( [ 'out of range', 4 ], [ 'out of range 0 items', 1, 1 ] ) {
    my $o = $c->[2] ? Seqsee::Object->create() : Seqsee::Object->create( 4, 5 );
    my $ok = eval { $o->apply_blemish_at( $S::DOUBLE, SPos->new( $c->[1] ) ); 1 };
    record( case => 'blemish', name => $c->[0], error => ( $ok ? undef : err($@) ) );
  }
}

# --- CanBeSeenAs -----------------------------------------------------------------------------
{
  my $o = fresh_objects();
  my @structs = (
    [ '5',            n => 5 ],
    [ '2',            n => 2 ],
    [ '6',            n => 6 ],
    [ '-3',           n => -3 ],
    [ '"5"',          s => '5' ],
    [ '"-5"',         s => '-5' ],
    [ '"5.0"',        s => '5.0' ],
    [ '"abc"',        s => 'abc' ],
    [ 'undef',        s => undef ],
    [ '[5]',          a => [5] ],
    [ '[]',           a => [] ],
    [ '[1,2,3]',      a => [ 1, 2, 3 ] ],
    [ '[1,[2,2],3]',  a => [ 1, [ 2, 2 ], 3 ] ],
    [ '[1,[2,3]]',    a => [ 1, [ 2, 3 ] ] ],
    [ '[2,2]',        a => [ 2, 2 ] ],
    [ '[[1,2,3]]',    a => [ [ 1, 2, 3 ] ] ],
    [ '[1,2]',        a => [ 1, 2 ] ],
    [ '[[2,3]]',      a => [ [ 2, 3 ] ] ],
    [ 'obj g123',     o => 'g123' ],
    [ 'obj e5',       o => 'e5' ],
    [ 'obj bl',       o => 'bl' ],
    [ 'obj gn',       o => 'gn' ],
    [ 'obj emp',      o => 'emp' ],
  );
  my $other = fresh_objects();
  for my $oname ( sort keys %$o ) {
    for my $s (@structs) {
      my ( $sname, $kind, $v ) = @$s;
      my $arg =
          $kind eq 's' ? ( defined($v) ? join( '', $v ) : undef )
        : $kind eq 'o' ? $other->{$v}
        :                $v;
      my $r = eval { Seqsee::Object::CanBeSeenAs( $o->{$oname}, $arg ) };
      record( case => 'cbsa', obj => $oname, struct => $sname, ( $@ ? ( error => err($@) ) : ( r => rd($r) ) ) );
    }
  }
  for my $c ( [ '5 5', 5, 5 ], [ '5 6', 5, 6 ], [ '5 5.0', 5, 5.0 ], [ '5 [5]', 5, [5] ], [ '5 obj', 5, 'e5' ],
    [ '"5" 5', '5', 5 ] )
  {
    my ( $name, $x, $y ) = @$c;
    $y = $other->{e5} if $y eq 'e5';
    my $r = eval { Seqsee::Object::CanBeSeenAs( $x, $y ) };
    record( case => 'cbsa plain', name => $name, ( $@ ? ( error => err($@) ) : ( r => rd($r) ) ) );
  }
  # The helper subs, called directly.
  for my $oname (qw(g123 mid midin e5)) {
    for my $s ( [ '[2,2]', [ 2, 2 ] ], [ '2', 2 ], [ '[1,2,3]', [ 1, 2, 3 ] ], [ 'obj e5', $other->{e5} ] ) {
      my ( $sname, $v ) = @$s;
      my %r;
      for my $m (qw(CanBeSeenAs_Literal CanBeSeenAs_Literal0rMeto CanBeSeenAs_ByPart)) {
        my @r = eval { $o->{$oname}->$m($v) };
        $r{$m} = $@ ? { error => err($@) } : { n => scalar(@r), r => ( @r ? rd( $r[0] ) : undef ) };
      }
      record( case => 'cbsa helpers', obj => $oname, struct => $sname, %r );
    }
  }
  {
    my $ok = eval { $o->{g123}->CanBeSeenAs_Meto( [ 1, 2, 3 ], $o->{g123} ); 1 };
    my $e3 = $ok ? undef : err($@);
    my @r =eval { $o->{g123}->CanBeSeenAs_Meto( [ 1, 2, 3 ], $o->{g123}, 'M' ) };
    my @r2 = eval { $o->{g123}->CanBeSeenAs_Meto( [ 1, 2 ], $o->{g123}, 'M' ) };
    record( case => 'cbsa meto', three_args_error => $e3,match_n => scalar(@r),
      match_entire_is_M => ( $r[0]->GetEntireBlemish eq 'M' ? 1 : 0 ), nomatch_n => scalar(@r2) );
  }
  # The method form, as the categories call it.
  my $r = $o->{g123}->CanBeSeenAs( [ 1, 2, 3 ] );
  record( case => 'cbsa method', r => rd($r) );
}

# --- metonym management ------------------------------------------------------------------
{
  my $o = Seqsee::Object->create( 1, 2 );
  my @log;
  my $ok = eval { $o->SetMetonymActiveness(1); 1 };
  push @log, [ 'on without metonym', ( $ok ? undef : err($@) ), $o->get_metonym_activeness ];
  my @r = $o->SetMetonymActiveness(0);
  push @log, [ 'off without metonym', [@r],$o->get_metonym_activeness ];
  my $star = Seqsee::Object->create( 7, 7 );
  my $m = SMetonym->new( { category => $S::SAMENESS, name => 'each', info_loss => {}, starred => $star, unstarred => $o } );
  @r = $o->SetMetonym($m);
  push @log, [ 'SetMetonym', scalar(@r), same( $r[0], $m ), same( $o->get_metonym, $m ), same( $star->get_is_a_metonym, $o ) ];
  @r = $o->SetMetonymActiveness(1);
  push @log, [ 'on', [@r], $o->get_metonym_activeness ];
  @r = $o->SetMetonymActiveness('yes');
  push @log, [ 'on again', [@r], $o->get_metonym_activeness ];
  push @log, [ 'effective is star', same( $o->GetEffectiveObject, $star ) ];
  @r = $o->SetMetonymActiveness('');
  push @log, [ 'off', [@r], $o->get_metonym_activeness ];
  push @log, [ 'effective is self', same( $o->GetEffectiveObject, $o ) ];
  @r = $o->SetMetonymActiveness(undef);
  push @log, [ 'off again', [@r],$o->get_metonym_activeness ];
  push @log, [ 'history', [ @{ $o->get_history } ] ];

  my $m5 = SMetonym->new( { category => $S::SAMENESS, name => 'each', info_loss => {}, starred => '5', unstarred => $o } );
  $ok = eval { $o->SetMetonym($m5); 1 };
  push @log, [ 'SetMetonym scalar starred', ( $ok ? undef : err($@) ), same( $o->get_metonym, $m ) ];
  my $mh = SMetonym->new( { category => $S::SAMENESS, name => 'each', info_loss => {}, starred => { a => 1 }, unstarred => $o } );
  $ok = eval { $o->SetMetonym($mh); 1 };
  push @log, [ 'SetMetonym hash starred', ( $ok ? undef : err($@) ) ];
  record( case => 'metonym log', log => \@log );

  # IsThisAMetonymedObject / GetConcreteObject
  my $p = Seqsee::Object->create( 3, 4 );
  my @l2;
  push @l2, [ 'plain', $p->IsThisAMetonymedObject, same( $p->GetConcreteObject, $p ) ];
  $p->set_is_a_metonym($p);
  push @l2, [ 'self', $p->IsThisAMetonymedObject, same( $p->GetConcreteObject, $p ) ];
  $p->set_is_a_metonym($o);
  push @l2, [ 'other', $p->IsThisAMetonymedObject, same( $p->GetConcreteObject, $o ), $p->ContainsAMetonym ];
  $p->set_is_a_metonym(0);
  push @l2, [ 'zero', $p->IsThisAMetonymedObject, same( $p->GetConcreteObject, $p ) ];
  my $q = Seqsee::Object->new( { group_p => 1, items => [ Seqsee::Object->create( 1, 2 ), $star ] } );
  push @l2, [ 'contains via item', $q->ContainsAMetonym, $star->IsThisAMetonymedObject ];
  push @l2, [ 'element', Seqsee::Element->create( 3, 0 )->ContainsAMetonym ];
  record( case => 'metonymed log', log => \@l2 );
}

# AnnotateWithMetonym / MaybeAnnotateWithMetonym
{
  my $cat = FakeCat->new( name => 'am', inst => { '[1, 2]' => 'B12' }, metos => { ok => [ 7, 7 ], none => undef } );
  my @log;
  my $o  = Seqsee::Object->create( 1, 2 );
  my $o2 = Seqsee::Object->create( 3, 4 );
  @LOG = ();
  my @r  = eval { $o->AnnotateWithMetonym( $cat, 'ok' ) };
  push @log, [ 'annotate ok', ( $@ ? err($@) : undef ), scalar(@r), names($o), [ @{ $o->get_history } ],
    ss( $o->get_metonym->get_starred ), same( $o->get_metonym->get_starred->get_is_a_metonym, $o ), $o->get_metonym_activeness,
    [@LOG] ];
  @LOG = ();
  eval { $o2->AnnotateWithMetonym( $cat, 'ok' ) };
  push @log, [ 'annotate not of cat', err($@), names($o2), [ @{ $o2->get_history } ], [@LOG] ];
  my $before = $o->get_metonym;
  eval { $o->AnnotateWithMetonym( $cat, 'none' ) };
  push @log, [ 'annotate none', err($@), same( $o->get_metonym, $before ), scalar( @{ $o->get_history } ) ];
  @r = eval { $o->MaybeAnnotateWithMetonym( $cat, 'none' ) };
  push @log, [ 'maybe none', ( $@ ? err($@) : undef ), scalar(@r) ];
  @r = eval { $o->MaybeAnnotateWithMetonym( $cat, 'ok' ) };
  push @log, [ 'maybe ok', ( $@ ? err($@) : undef ), scalar( @{ $o->get_history } ), ss( $o->get_metonym->get_starred ),
    same( $o->get_metonym, $before ) ];
  eval { $o2->MaybeAnnotateWithMetonym( $cat, 'ok' ) };
  push @log, [ 'maybe not of cat', err($@) ];
  eval { $o2->MaybeAnnotateWithMetonym( $S::ASCENDING, 'ok' ) };
  push @log, [ 'maybe no find_metonym', err($@) ];
  record( case => 'annotate log', log => \@log );
}

# --- relations ----------------------------------------------------------------------------
{
  my ( $o, $a, $b ) = map { FakeEnd->new( group_p => 1, bounds => $_ ) } qw(O A B);
  my $r1 = FakeReln->new( label => 'r1', ends => [ $o, $a ] );
  my $r2 = FakeReln->new( label => 'r2', ends => [ $b, $o ] );
  my $r3 = FakeReln->new( label => 'r3', ends => [ $a, $b ] );
  my @log;
  @LOG = ();
  for my $step ( [ add => $r1 ], [ add => $r2 ], [ add => $r1 ], [ add => $r3 ], [ remove => $r1 ], [ remove => $r1 ],
    [ remove => $r3 ], [ add => $r1 ], [ removeall => undef ] )
  {
    my ( $what, $r ) = @$step;
    my @ret = eval {
        $what eq 'add'    ? $o->AddRelation($r)
      : $what eq 'remove' ? $o->RemoveRelation($r)
      :                     $o->RemoveAllRelations;
    };
    push @log, [ $what, ( $r ? $r->label : undef ), ( $@ ? err($@) : undef ),
      [ sort map { $_->label } $o->all_relations ],
      ( $o->relation_exists_to($a) ? 1 : 0 ), ( $o->relation_exists_to($b) ? 1 : 0 ) ];
  }
  record( case => 'relations log', log => \@log, history => [ @{ $o->get_history } ], calls => [@LOG] );

  # _get_other_end_of_reln
  record( case => 'other end', a => $o->_get_other_end_of_reln($r1)->get_bounds_string,
    b => $o->_get_other_end_of_reln($r2)->get_bounds_string,
    self => $o->_get_other_end_of_reln( FakeReln->new( label => 's', ends => [ $o, $o ] ) )->get_bounds_string );

  # recalculate_relations
  my $rcat = FakeRCat->new( table => { OA => 'NEWT', BO => undef } );
  my $t    = FakeType->new( cat => $rcat );
  my $p    = FakeEnd->new( group_p => 1, bounds => 'O' );
  $p->set_relation_to( $a, FakeReln->new( label => 'ra', ends => [ $p, $a ], type => $t ) );
  $p->set_relation_to( $b, FakeReln->new( label => 'rb', ends => [ $b, $p ], type => $t ) );
  @LOG = ();
  $p->recalculate_relations;
  record( case => 'recalculate_relations', calls => [ sort @LOG ], history => [ @{ $p->get_history } ] );
}

# apply_reln_scheme
{
  my @ends = map { FakeEnd->new( group_p => 1, bounds => $_ ) } qw(A B C D E);
  my $g = Seqsee::Object->new( { group_p => 1, items => [@ends] } );
  $ends[3]->set_relation_to( $ends[4], 'EXISTING' );
  $ends[2]->set_relation_to( $ends[3], 0 );
  for my $c ( [ 'undef', undef ], [ 'zero', 0 ], [ 'NONE', RELN_SCHEME::NONE() ], [ 'CHAIN', RELN_SCHEME::CHAIN() ],
    [ 'string', 'foo' ], [ 'number', 5 ] )
  {
    my ( $name, $scheme ) = @$c;
    @LOG = ();
    my $ok = do { no warnings; eval { $g->apply_reln_scheme($scheme); 1 } };
    record( case => 'apply_reln_scheme', name => $name, error => ( $ok ? undef : err($@) ), calls => [@LOG],
      history => [ @{ $g->get_history } ] );
  }
  my $one = Seqsee::Object->new( { group_p => 1, items => [ $ends[0] ] } );
  @LOG = ();
  $one->apply_reln_scheme( RELN_SCHEME::CHAIN() );
  record( case => 'apply_reln_scheme', name => 'one item', error => undef, calls => [@LOG], history => [ @{ $one->get_history } ] );
}

# --- UpdateStrength -----------------------------------------------------------------------
{
  our %ACT = ( c1 => 0.5, c2 => 1, c3 => undef, c4 => 0.123 );
  my @cats = map { FakeCat->new( name => $_, inst => { '[1, 2, 3]' => 'B', '[]' => 'E' } ) } qw(c1 c2 c3 c4);
  my $g = Seqsee::Object->create( 1, 2, 3 );
  my @log;
  my $step = sub {
    my ($name) = @_;
    @LOG = ();
    my @r = $g->UpdateStrength;
    push @log, [ $name, $g->get_strength, scalar(@r), "$r[0]", [@LOG] ];
  };
  $step->('no cats');
  $g->describe_as( $cats[0] );
  $step->('c1');
  $Global::GroupStrengthByConsistency{$g} = 30;
  $step->('c1 + consistency');
  $g->describe_as( $cats[1] );
  $step->('c1 c2 + consistency (capped)');
  delete $Global::GroupStrengthByConsistency{$g};
  $g->remove_category( $cats[0] );
  $g->remove_category( $cats[1] );
  $g->describe_as( $cats[2] );
  $step->('c3 undef activation');
  $g->describe_as( $cats[3] );
  $g->[0]->set_strength(33.3);
  $step->('c3 c4, float part');
  $g->[1]->set_strength(undef);
  $step->('undef part strength');
  $Global::GroupStrengthByConsistency{$g} = -500;
  $step->('negative consistency');
  record( case => 'UpdateStrength', log => \@log );
  my $e = Seqsee::Object->create();
  @LOG = ();
  $e->UpdateStrength;
  my $s1 = $e->get_strength;
  $e->describe_as( $cats[3] );
  $e->UpdateStrength;
  record( case => 'UpdateStrength empty', strengths => [ $s1, $e->get_strength ], calls => [@LOG] );
}

# --- set_underlying_ruleapp ---------------------------------------------------------------
{
  my $g   = Seqsee::Object->create( 1, [ 2, 3 ] );
  my $app = FakeApp->new;
  my @log;
  my $try = sub {
    my ( $name, $arg, $next ) = @_;
    @LOG       = ();
    $NEXT_RULE = $next;
    my $out = '';
    my @r;
    my $ok;
    {
      open( my $saved, '>&', \*STDOUT ) or die;
      close STDOUT;
      open( STDOUT, '>', \$out ) or die;
      $ok = eval { @r = $g->set_underlying_ruleapp($arg); 1 };
      close STDOUT;
      open( STDOUT, '>&', $saved ) or die;
    }
    push @log,
      [ $name, ( $ok ? undef : err($@) ), norm($out), scalar(@r), [@LOG], [ map { norm($_) } @{ $g->get_history } ],
      ( same( $g->get_underlying_reln, $app ) ? 'APP' : $g->get_underlying_reln ) ];
  };
  $try->( 'undef', undef );
  $try->( 'zero',  0 );
  $try->( 'rule with app', FakeRule->new( label => 'R1', app => $app ) );
  $try->( 'rule without app', FakeRule->new( label => 'R2' ) );
  $try->( 'srelation, rule', FakeSRel->new, FakeRule->new( label => 'R3', app => $app ) );
  $try->( 'srelation, no rule', FakeSRel->new, undef );
  $try->( 'mapping, rule', Mapping::Numeric->create( 'succ', $S::NUMBER ), FakeRule->new( label => 'R4', app => $app ) );
  $try->( 'string', 'foo' );
  $try->( 'mapping dir', $Mapping::Dir::Same );
  record( case => 'set_underlying_ruleapp', log => \@log );
}

# --- get_pure, GetAnnotatedStructureString ----------------------------------------------
{
  my $g = Seqsee::Object->create( 1, [ 2, 3 ] );
  record( case => 'get_pure', pure => $g->get_pure, element => Seqsee::Element->create( 4, 0 )->GetAnnotatedStructureString,
    group => $g->GetAnnotatedStructureString, effective => $g->GetEffectiveStructure,
    effective_string => $g->GetEffectiveStructureString,
    one_effective => Seqsee::Object->new( { group_p => 1, items => [ Seqsee::Element->create( 4, 0 ) ] } )->GetEffectiveStructure,
    empty_effective => Seqsee::Object->create()->GetEffectiveStructureString,
    empty_annotated => Seqsee::Object->create()->GetAnnotatedStructureString );
}

# --- CheckSquintability ---------------------------------------------------------------------
{
  my $c1 = FakeCat->new( name => 'sq1', inst => { '[1, 2, 3]' => 'B1' },
    metos => { m_a => [ 1, 2, 3 ], m_b => [9], m_c => undef, m_d => 4, m_e => [ 1, 2, 3 ] } );
  my $c2 = FakeCat->new( name => 'sq2', inst => { '[1, 2, 3]' => 'B2' }, metos => { x => [ 1, 2, 3 ], y => 4 } );
  my $c3 = FakeCat->new( name => 'sq3', inst => {}, metos => { z => [ 1, 2, 3 ] } );
  my $o = Seqsee::Object->create( 1, 2, 3 );
  $o->describe_as($_) for ( $c1, $c2, $c3 );
  for my $c ( [ 'group', Seqsee::Object->create( 1, 2, 3 ) ], [ 'element 4', Seqsee::Element->create( 4, 0 ) ],
    [ 'nothing', Seqsee::Object->create( 8, 8 ) ] )
  {
    @LOG = ();
    my @r = $o->CheckSquintability( $c->[1] );
    record( case => 'squint', name => $c->[0],
      types => [ sort map { $_->get_category->get_name . '/' . $_->get_name } @r ],
      calls => [ sort { "@$a" cmp "@$b" } @LOG ] );
  }
  @LOG = ();
  my @r = $o->CheckSquintabilityForCategory( '4', $c1 );
  record( case => 'squint for cat', types => [ map { $_->get_name } @r ], calls => [@LOG] );
  eval { $o->CheckSquintabilityForCategory( '4', $c3 ) };
  record( case => 'squint for cat', name => 'not an instance', error => err($@) );
}

# --- real objects: FindMappingForCat and ApplyMapping(Mapping::Structural, Seqsee::Object) ----
{
  for my $c ( [ 'asc 123 -> 1234', $S::ASCENDING, [ 1, 2, 3 ], [ 1, 2, 3, 4 ] ],
    [ 'asc 234 -> 123', $S::ASCENDING, [ 2, 3, 4 ], [ 1, 2, 3 ] ],
    [ 'desc 321 -> 4321', $S::DESCENDING, [ 3, 2, 1 ], [ 4, 3, 2, 1 ] ],
    [ 'same 22 -> 333', $S::SAMENESS, [ 2, 2 ], [ 3, 3, 3 ] ] )
  {
    my ( $name, $cat, $s1, $s2 ) = @$c;
    my $a = Seqsee::Object->create(@$s1);
    my $b = Seqsee::Object->create(@$s2);
    $a->describe_as($cat);
    $b->describe_as($cat);
    my $m = $cat->FindMappingForCat( $a, $b );
    my %cb = $m ? %{ $m->get_changed_bindings } : ();
    my $applied = $m ? Mapping::ApplyMapping( $m, $b ) : undef;
    record(
      case     => 'real mapping',
      name     => $name,
      found    => ( $m ? 1 : 0 ),
      ref      => ref($m),
      changed  => { map { $_ => ( ref( $cb{$_} ) ? $cb{$_}->get_name : $cb{$_} ) } keys %cb },
      applied  => ss($applied),
      applied_cats => ( $applied ? names($applied) : undef ),
      applied_reln_scheme => ( $applied && $applied->get_reln_scheme ? 'CHAIN' : undef ),
    );
  }
}

emit();
