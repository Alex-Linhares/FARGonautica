# Oracle for Seqsee/Object.pm, part I (item 021): construction, attributes, items,
# create, categories (describe_as & co.), structure basics and relation handles.
# Output: tests/golden/seqsee_object.json
#
# Real Seqsee::Elements are used as leaves (the Python test rebuilds them with a fake
# that does what Seqsee::Element->create does). Categories other than $S::NUMBER are
# FakeCats: SCategory consumers whose Instancer looks the object's structure string
# up in a table, so describe_as & co. run against the real SCategory::is_instance.
use strict;
use Oracle;
use S;

package FakeCat;
use Moose;
has name  => ( is => 'ro' );
has inst  => ( is => 'rw', default => sub { {} } );    # structure string => bindings
has calls => ( is => 'rw', default => 0 );
sub Instancer {
  my ( $self, $o ) = @_;
  $self->calls( $self->calls + 1 );
  return $self->inst->{ $o->get_structure_string };
}
sub build                          { }
sub get_name                       { $_[0]->name }
sub as_text                        { 'fake ' . $_[0]->name }
sub AreAttributesSufficientToBuild { 1 }
sub get_meto_types                 { () }
sub get_pure                       { $_[0] }
sub get_memory_dependencies        { () }
sub serialize                      { $_[0]->name }
sub deserialize                    { }
with 'SCategory';

package main;

# Strip Moose's "(defined at FILE line N) line M" and Perl's " at FILE line N." tails,
# and replace addresses.
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
sub norm {
  my ($s) = @_;
  return $s unless defined $s;
  $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
  return $s;
}

sub names { [ sort map { defined($_) ? $_->get_name : 'UNDEF' } @{ $_[0]->get_categories } ] }

sub describe {
  my ($o) = @_;
  my $ref = ref $o;
  return { ref => $ref, value => $o } unless $ref && $ref =~ /^Seqsee::/;
  return {
    ref       => $ref,
    structure => ( eval { $o->get_structure_string } // { error => err($@) } ),
    group_p   => $o->get_group_p,
    strength  => $o->get_strength,
    count     => $o->get_parts_count,
    cats      => names($o),
    history   => [ @{ $o->get_history } ],
    items     => [ map { describe($_) } grep { $_ ne $o } $o->get_items_array ],
    self_item => ( ( grep { $_ eq $o } $o->get_items_array ) ? 1 : 0 ),
  };
}

sub try_new {
  my ($args) = @_;
  my $o = eval { Seqsee::Object->new($args) };
  return { error => err($@) } unless $o;
  return { ok => describe($o) };
}

# --- construction ----------------------------------------------------------------------
{
  my $o = Seqsee::Object->new( { group_p => 1 } );
  record(
    case                   => 'defaults',
    strength               => $o->get_strength,
    group_p                => $o->get_group_p,
    metonym                => $o->get_metonym,
    metonym_activeness     => $o->get_metonym_activeness,
    is_a_metonym           => $o->get_is_a_metonym,
    direction_defined      => defined( $o->get_direction ) ? 1 : 0,
    reln_scheme            => $o->get_reln_scheme,
    underlying_reln        => $o->get_underlying_reln,
    parts                  => [ @{ $o->get_parts_ref } ],
    count                  => $o->get_parts_count,
    items_is_parts_ref     => ( $o->get_items == $o->get_parts_ref ) ? 1 : 0,
    relations              => [ $o->all_relations ],
    cats                   => names($o),
    history                => [ @{ $o->get_history } ],
    history_obj_ref        => ref( $o->get_history_obj ),
    true                   => ( $o ? 1 : 0 ),
  );
}

my @new_cases = (
  [ 'no args',               {} ],
  [ 'group_p 2',             { group_p => 2 } ],
  [ 'group_p undef',         { group_p => undef } ],
  [ 'group_p "0"',           { group_p => '0' } ],
  [ 'group_p ""',            { group_p => '' } ],
  [ 'group_p "1"',           { group_p => '1' } ],
  [ 'group_p 0.0',           { group_p => 0.5 } ],
  [ 'group_p "abc"',         { group_p => 'abc' } ],
  [ 'items string',          { group_p => 1, items => 'x' } ],
  [ 'items hash',            { group_p => 1, items => {} } ],
  [ 'items undef',           { group_p => 1, items => undef } ],
  [ 'items numbers',         { group_p => 0, items => [ 1, 2 ] } ],
  [ 'history_obj 3',         { group_p => 1, history_obj => 3 } ],
  [ 'metonym_activeness 5',  { group_p => 1, metonym_activeness => 5 } ],
  [ 'metonym_activeness 1',  { group_p => 1, metonym_activeness => 1 } ],
  [ 'reln_other_end array',  { group_p => 1, reln_other_end => [] } ],
  [ 'categories string',     { group_p => 1, categories => 'c' } ],
  [ 'strength string',       { group_p => 1, strength => 'abc' } ],
  [ 'strength 35',           { group_p => 1, strength => 35 } ],
  [ 'item (not init_arg)',   { group_p => 1, item => [1] } ],
  [ 'unknown key',           { group_p => 1, foo => 1 } ],
  [ 'missing + bad items',   { items => 'x' } ],
  [ 'all bad',               { group_p => 2, items => 'x', history_obj => 3, metonym_activeness => 5, reln_other_end => [], categories => 'c' } ],
  [ 'all bad but cats',      { group_p => 2, items => 'x', history_obj => 3, metonym_activeness => 5, reln_other_end => [] } ],
  [ 'bad h/i/m/r',           { group_p => 1, items => 'x', history_obj => 3, metonym_activeness => 5, reln_other_end => [] } ],
  [ 'bad i/m/r',             { group_p => 1, items => 'x', metonym_activeness => 5, reln_other_end => [] } ],
  [ 'bad m/r',               { group_p => 1, metonym_activeness => 5, reln_other_end => [] } ],
);
for (@new_cases) {
  my ( $name, $args ) = @$_;
  record( case => 'new', name => $name, %{ try_new($args) } );
}

{
  my $o = Seqsee::Object->new(
    group_p            => 0,
    strength           => 12,
    metonym            => 'M',
    metonym_activeness => 1,
    is_a_metonym       => 'IAM',
    direction          => $DIR::LEFT,
    reln_scheme        => 'RS',
    underlying_reln    => 'UR',
  );
  record(
    case               => 'new with values (list form)',
    strength           => $o->get_strength,
    group_p            => $o->get_group_p,
    metonym            => $o->get_metonym,
    metonym_activeness => $o->get_metonym_activeness,
    is_a_metonym       => $o->get_is_a_metonym,
    direction_is_left  => ( $o->get_direction eq $DIR::LEFT ) ? 1 : 0,
    reln_scheme        => $o->get_reln_scheme,
    underlying_reln    => $o->get_underlying_reln,
  );
  my @w;
  for my $pair ( [ 'set_group_p', 3 ], [ 'set_group_p', '' ], [ 'set_group_p', undef ], [ 'set_group_p', 1 ],
    [ 'set_metonym_activeness', 2 ], [ 'set_metonym_activeness', 0 ], [ 'set_strength', 'x' ],
    [ 'set_metonym', undef ], [ 'set_is_a_metonym', 7 ], [ 'set_reln_scheme', 0 ], [ 'set_underlying_reln', 'u' ],
    [ 'set_direction', undef ], [ 'set_history_obj', 4 ] )
  {
    my ( $m, $v ) = @$pair;
    my $ok = eval { $o->$m($v); 1 };
    push @w, { method => $m, value => $v, error => ( $ok ? undef : err($@) ) };
  }
  record(
    case               => 'writers',
    results            => \@w,
    strength           => $o->get_strength,
    group_p            => $o->get_group_p,
    metonym            => $o->get_metonym,
    metonym_activeness => $o->get_metonym_activeness,
    is_a_metonym       => $o->get_is_a_metonym,
    reln_scheme        => $o->get_reln_scheme,
    underlying_reln    => $o->get_underlying_reln,
    direction_defined  => defined( $o->get_direction ) ? 1 : 0,
  );
}

# --- history delegation --------------------------------------------------------------------
{
  local $Global::Steps_Finished = 3;
  my $o = Seqsee::Object->new( group_p => 1 );
  $Global::Steps_Finished = 5;
  $o->AddHistory("five");
  my $u4 = $o->UnchangedSince(4);
  my $u5 = $o->UnchangedSince(5);
  $Global::Steps_Finished = 9;
  record(
    case       => 'history',
    history    => [ @{ $o->get_history } ],
    unchanged4 => $u4,
    unchanged5 => $u5,
    age        => $o->GetAge,
    as_text    => $o->history_as_text,
    search     => [ $o->search_history(qr/five/) ],
  );
}

# --- relation handles ----------------------------------------------------------------------
{
  my $o = Seqsee::Object->new( group_p => 1 );
  my $a = Seqsee::Object->new( group_p => 1 );
  my $b = Seqsee::Object->new( group_p => 1 );
  my @log;
  push @log, [ 'exists a', $o->relation_exists_to($a) ];
  push @log, [ 'get a', $o->get_relation($a) ];
  push @log, [ 'set a', $o->set_relation_to( $a, 'RA' ) ];
  push @log, [ 'set b', $o->set_relation_to( $b, 'RB' ) ];
  push @log, [ 'exists a', $o->relation_exists_to($a) ];
  push @log, [ 'get a', $o->get_relation($a) ];
  push @log, [ 'all', [ sort( $o->all_relations ) ] ];
  push @log, [ 'remove a', $o->remove_reln_to($a) ];
  push @log, [ 'remove a again', $o->remove_reln_to($a) ];
  push @log, [ 'exists a', $o->relation_exists_to($a) ];
  push @log, [ 'all', [ sort( $o->all_relations ) ] ];
  push @log, [ 'set a undef', $o->set_relation_to( $a, undef ) ];
  push @log, [ 'exists a', $o->relation_exists_to($a) ];
  push @log, [ 'get a', $o->get_relation($a) ];
  push @log, [ 'history', [ @{ $o->get_history } ] ];
  record( case => 'relations', log => \@log );
}

# --- create ----------------------------------------------------------------------------------
my $UNDEF_CAT;
my $even_cat = FakeCat->new( name => 'evenish', inst => { '[2, 4]' => 'B24', '[1, [2, 3]]' => 'Bnest', '4' => 'B4' } );
my $odd_cat  = FakeCat->new( name => 'oddish',  inst => { '[2, 4]' => 'O24' } );

sub try_create {
  my ( $name, @args ) = @_;
  my $o = eval { Seqsee::Object->create(@args) };
  if ( !$o ) { record( case => 'create', name => $name, error => err($@) ); return }
  record( case => 'create', name => $name, ok => describe($o) );
  return $o;
}

try_create('no args');
try_create( 'one number', 5 );
try_create( 'zero', 0 );
try_create( 'empty array', [] );
try_create( 'array of one', [5] );
try_create( 'array of two', [ 1, 2 ] );
try_create( 'three numbers', 1, 2, 3 );
try_create( 'nested', 1, [ 2, 3 ], [ [4] ] );
try_create( 'nested arrays deep', [ [ [ 1, 2 ] ] ] );
try_create( 'array then empty', 1, [] );
try_create( 'unblessed hash', {} );
try_create( 'two unblessed hashes', {}, {} );

{
  my $e = Seqsee::Element->create( 4, 0 );
  $e->describe_as($even_cat);
  my $copy = try_create( 'copy of element', $e );
  record( case => 'copy identity', name => 'element', same => ( $copy eq $e ) ? 1 : 0 );

  my $g = Seqsee::Object->create( 2, 4 );
  $g->describe_as($even_cat);
  $g->describe_as($odd_cat);
  my $gc = try_create( 'copy of group with cats', $g );
  record(
    case       => 'copy identity',
    name       => 'group',
    same       => ( $gc eq $g ) ? 1 : 0,
    items_same => [ map { $gc->[$_] eq $g->[$_] ? 1 : 0 } 0 .. 1 ],
    calls      => [ $even_cat->calls, $odd_cat->calls ],
  );

  my $g1 = Seqsee::Object->new( { group_p => 1, items => [$e] } );
  try_create( 'copy of one-item group', $g1 );
  my $g0 = Seqsee::Object->new( { group_p => 1, items => [] } );
  try_create( 'copy of empty group', $g0 );
  my $gn = Seqsee::Object->create( 1, [ 2, 3 ] );
  $gn->describe_as($even_cat);
  my $gnc = try_create( 'copy of nested group', $gn );
  try_create( 'list of objects', $e, $g, 7 );
  try_create( 'array of objects', [ $e, $g ] );
}

# --- describe_as & co ------------------------------------------------------------------------
sub cat_state {
  my ( $o, @extra ) = @_;
  return ( cats => names($o), history => [ @{ $o->get_history } ], @extra );
}
{
  my $cat = FakeCat->new( name => 'c1', inst => { '[1, 2]' => 'B12', '[3, 4]' => 0 } );
  my $o = Seqsee::Object->create( 1, 2 );
  my $r1 = $o->describe_as($cat);
  my $c1 = $cat->calls;
  my $r2 = $o->describe_as($cat);
  record( case => 'describe_as', name => 'success then cached', r1 => $r1, r2 => $r2, calls => [ $c1, $cat->calls ],
    binding => $o->GetBindingForCategory($cat), is_of => $o->is_of_category_p($cat), cat_state($o) );

  my $p = Seqsee::Object->create( 3, 4 );
  my $r = $p->describe_as($cat);
  record( case => 'describe_as', name => 'false bindings', r => $r, cat_state($p) );

  my $q = Seqsee::Object->create( 5, 6 );
  $r = $q->describe_as($cat);
  record( case => 'describe_as', name => 'no bindings', r => $r, cat_state($q) );

  # annotate_with_cat
  my $ok = eval { $r = $o->annotate_with_cat($cat); 1 };
  record( case => 'annotate_with_cat', name => 'already', r => $r, error => ( $ok ? undef : err($@) ) );
  $ok = eval { $r = $q->annotate_with_cat($cat); 1 };
  record( case => 'annotate_with_cat', name => 'fails', error => ( $ok ? undef : err($@) ), cat_state($q) );
  my $s = Seqsee::Object->create( 1, 2 );
  $ok = eval { $r = $s->annotate_with_cat($cat); 1 };
  record( case => 'annotate_with_cat', name => 'succeeds', r => $r, error => ( $ok ? undef : err($@) ), cat_state($s) );

  # redescribe_as
  my $t = Seqsee::Object->create( 1, 2 );
  $r = $t->redescribe_as($cat);
  record( case => 'redescribe_as', name => 'success', r => $r, cat_state($t) );
  $r = $t->redescribe_as($cat);
  record( case => 'redescribe_as', name => 'success again', r => $r, cat_state($t) );
  $cat->inst( { '[1, 2]' => 'NEW' } );
  $r = $t->redescribe_as($cat);
  record( case => 'redescribe_as', name => 'new bindings', r => $r, binding => $t->GetBindingForCategory($cat), cat_state($t) );
  $cat->inst( {} );
  $r = $t->redescribe_as($cat);
  record( case => 'redescribe_as', name => 'failure removes', r => $r, binding => $t->GetBindingForCategory($cat), cat_state($t) );
  $r = $t->redescribe_as($cat);
  record( case => 'redescribe_as', name => 'failure when absent', r => $r, cat_state($t) );
}

# recalculate_categories
{
  my $cat = FakeCat->new( name => 'r1', inst => { '[1, 2]' => 'R12' } );
  my $o = Seqsee::Object->create( 1, 2 );
  $o->describe_as($cat);
  my $ok = eval { $o->recalculate_categories; 1 };
  record( case => 'recalculate_categories', name => 'keeps', error => ( $ok ? undef : err($@) ), cat_state($o) );
  $cat->inst( {} );
  $ok = eval { $o->recalculate_categories; 1 };
  record( case => 'recalculate_categories', name => 'loses all', error => ( $ok ? undef : err($@) ), cat_state($o) );
  my $ok2 = eval { $o->recalculate_categories; 1 };
  record( case => 'recalculate_categories', name => 'no categories', error => ( $ok2 ? undef : err($@) ), cat_state($o) );

  my $c2 = FakeCat->new( name => 'r2', inst => { '[1, 2]' => 'R2' } );
  $cat->inst( { '[1, 2]' => 'R12' } );
  my $p = Seqsee::Object->create( 1, 2 );
  $p->describe_as($cat);
  $p->describe_as($c2);
  $cat->inst( {} );
  $ok = eval { $p->recalculate_categories; 1 };
  record( case => 'recalculate_categories', name => 'loses one of two', error => ( $ok ? undef : err($@) ),
    cats => names($p), history_sorted => [ sort @{ $p->get_history } ] );

  my $e = Seqsee::Element->create( 9, 0 );
  $ok = eval { $e->recalculate_categories; 1 };
  record( case => 'recalculate_categories', name => 'element', error => ( $ok ? undef : err($@) ), cat_state($e) );
}

# --- structure basics ------------------------------------------------------------------------
{
  my $e1 = Seqsee::Element->create( 1, 0 );
  my $e2 = Seqsee::Element->create( 2, 0 );
  my $e3 = Seqsee::Element->create( 3, 0 );
  my $g23  = Seqsee::Object->new( { group_p => 1, items => [ $e2, $e3 ] } );
  my $g    = Seqsee::Object->new( { group_p => 1, items => [ $e1, $g23 ] } );
  my $one  = Seqsee::Object->new( { group_p => 1, items => [$g23] } );
  my $one1 = Seqsee::Object->new( { group_p => 1, items => [$e1] } );
  my $emp  = Seqsee::Object->new( { group_p => 1, items => [] } );
  my $wrap = Seqsee::Object->new( { group_p => 1, items => [ $emp, $e1 ] } );
  my %objs = ( e1 => $e1, g23 => $g23, g => $g, one => $one, one1 => $one1, emp => $emp, wrap => $wrap );
  for my $k ( sort keys %objs ) {
    my $o = $objs{$k};
    my $struct = $o->get_structure;
    my $span = eval { $o->get_span };
    record(
      case             => 'structure',
      name             => $k,
      structure        => $struct,
      structure_string => $o->get_structure_string,
      as_text          => ( ref($o) eq 'Seqsee::Object' ? $o->as_text : undef ),
      flattened        => $o->get_flattened,
      span             => $span,
      count            => $o->get_parts_count,
      deref_count      => scalar(@$o),
    );
  }
  my @names = sort keys %objs;
  for my $a (@names) {
    for my $b (@names) {
      my ( $x, $y ) = ( $objs{$a}, $objs{$b} );
      record(
        case       => 'has_as',
        a          => $a,
        b          => $b,
        item       => $x->HasAsItem($y),
        deep       => ( $x->HasAsPartDeep($y) ? 1 : 0 ),
        smartmatch => ( ( $x ~~ $y ) ? 1 : 0 ),
      );
    }
  }
  record( case => 'deref', first_is_e1 => ( $g->[0] eq $e1 ) ? 1 : 0, second_is_g23 => ( $g->[1] eq $g23 ) ? 1 : 0,
    elements => [ map { ref $_ } @$g ] );
  record( case => 'has_as_item scalar', r => $g->HasAsItem(1), deep => ( $g->HasAsPartDeep('x') ? 1 : 0 ) );
}

emit();
