# Oracle for SCategory/Sameness.pm, Ascending.pm, Descending.pm (and the
# SCategory::MetonymySpec::Metonyable role Sameness uses).
# Output: tests/golden/sequence_categories.json
#
# Seqsee::Object->create is replaced by a recorder (FakeBuilt) that keeps its
# arguments, so build() can be checked without the workspace. Instancer is fed
# fake groups whose CanBeSeenAs records the guess it got. Subobjects are real
# Seqsee::Element objects (the `ref eq 'Seqsee::Element'` check needs them).
# SMetonym->new and Seqsee::Anchored->create are replaced for the 'each'
# metonym finder.
use strict;
use Oracle;
use S;

package FakeBuilt;
sub new { my ($c, @items) = @_; bless { items => [@items], cats => [], reln => undef }, $c }
sub add_category { my ($s, $cat, $b) = @_; push @{ $s->{cats} }, [$cat, $b]; }
sub set_reln_scheme { $_[0]{reln} = $_[1] }

package FakeVal;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }

package FakeType;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub get_name { $_[0]{n} }

package FakeMeto;
sub new { my ($c, $t) = @_; bless { t => $t }, $c }
sub get_type { $_[0]{t} }

package FakeResult;
sub new { my ($c, %a) = @_; bless {%a}, $c }
sub GetPartsBlemished { $_[0]{parts} }
sub GetEntireBlemish  { $_[0]{entire} }

package FakeGroup;
sub new { my ($c, %a) = @_; bless { seen => [], %a }, $c }
sub get_items       { $_[0]{items} }
sub get_parts_count { $_[0]{parts_count} // scalar(@{ $_[0]{items} }) }
sub CanBeSeenAs { my ($s, $built) = @_; push @{ $s->{seen} }, $built; return $s->{result} }

package FakeElemGroup;
our @ISA = ('FakeGroup', 'Seqsee::Element');

# A subobject whose effective object is something else (an active metonym).
package FakeSub;
sub new { my ($c, $eo) = @_; bless { eo => $eo }, $c }
sub GetEffectiveObject { $_[0]{eo} }

# isa Seqsee::Element, but ref() is not 'Seqsee::Element'.
package FakeElemSub;
our @ISA = ('Seqsee::Element');
sub GetEffectiveObject { $_[0] }
sub get_mag { 9 }

package FakeCatObj;
sub new { my ($c, $b) = @_; bless { b => $b }, $c }
sub GetBindingForCategory { $_[0]{b} }

package FakeAnchObj;
our @ISA = ('FakeCatObj', 'Seqsee::Anchored');
sub get_edges { (2, 5) }

package FakeAnchored;
sub new { my ($c, $o) = @_; bless { o => $o, edges => undef }, $c }
sub set_edges { my ($s, @e) = @_; $s->{edges} = [@e] }

package FakeSMetonym;
sub new { my ($c, $a) = @_; bless { %$a }, $c }
sub get_starred { $_[0]{starred} }

package main;

{
  no warnings 'redefine';
  *Seqsee::Object::create   = sub { my ($pkg, @items) = @_; FakeBuilt->new(@items) };
  *Seqsee::Anchored::create = sub { my ($pkg, $o) = @_; FakeAnchored->new($o) };
  *SMetonym::new            = sub { my ($pkg, $a) = @_; FakeSMetonym->new($a) };
}

sub show { my ($v) = @_; defined($v) ? "$v" : undef }

# Values as text: SInt stringifies as "SInt(n)", elements as "E(mag)".
sub desc {
  my ($v) = @_;
  return undef unless defined $v;
  my $r = ref($v);
  return "$v" if !$r or $r eq 'SInt';
  return "V($v->{n})" if $r eq 'FakeVal';
  return "E(" . $v->get_mag . ")" if $r eq 'Seqsee::Element';
  return "Built(" . join(",", map { desc($_) } @{ $v->{items} }) . ")" if $r eq 'FakeBuilt';
  return "Anchored(" . desc($v->{o}) . ")" if $r eq 'FakeAnchored';
  return "OBJ:$r";
}
sub desc_hash { my ($h) = @_; return { map { $_ => desc($h->{$_}) } keys %$h } }
sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; return $e }
sub E { Seqsee::Element->create($_[0], 0) }

my %CATS = (sameness => $S::SAMENESS, ascending => $S::ASCENDING, descending => $S::DESCENDING);
my @CAT_KEYS = qw(sameness ascending descending);

# --- basics -------------------------------------------------------------------
for my $k (@CAT_KEYS) {
  my $cat  = $CATS{$k};
  my @meto = $cat->get_meto_types;
  my @deps = $cat->get_memory_dependencies;
  my $copy = ref($cat)->deserialize($cat->serialize);
  record(
    kind               => 'basics',
    cat                => $k,
    class              => ref($cat),
    get_name           => $cat->get_name,
    as_text            => $cat->as_text,
    serialize          => $cat->serialize,
    deserialized_class => ref($copy),
    deserialized_is_same => ($copy == $cat ? 1 : 0),
    is_pure            => $cat->is_pure,
    get_pure_is_self   => ($cat->get_pure == $cat ? 1 : 0),
    meto_types         => [ sort @meto ],
    memory_deps_count  => scalar(@deps),
    is_metonyable      => show(scalar $cat->is_metonyable),
    is_numeric         => ($cat->IsNumeric ? 1 : 0),
    smartmatch_self    => ($cat ~~ $cat ? 1 : 0),
  );
}

# --- AreAttributesSufficientToBuild --------------------------------------------
my @att_sets = ([], ['each'], ['length'], ['each', 'length'], ['length', 'each', 'x'],
                ['start'], ['start', 'end'], ['start', 'length'], ['end', 'length'],
                ['start', 'end', 'length'], ['start', 'start'], ['x', 'y'], ['end', 'x']);
for my $k (@CAT_KEYS) {
  for my $atts (@att_sets) {
    record(kind => 'sufficient', cat => $k, atts => $atts,
           result => show(scalar $CATS{$k}->AreAttributesSufficientToBuild(@$atts)));
  }
}

# --- build ----------------------------------------------------------------------
sub record_build {
  my ($k, $label, $args) = @_;
  my $cat = $CATS{$k};
  my $ret;
  my $died = dies(sub { no warnings; $ret = $cat->build($args) });
  my %out = (kind => 'build', cat => $k, label => $label, died => $died);
  if (!$died) {
    $out{defined} = defined($ret) ? 1 : 0;
    if (defined $ret) {
      my ($c, $b) = @{ $ret->{cats}[0] };
      $out{items}           = [ map { desc($_) } @{ $ret->{items} } ];
      $out{cats_count}      = scalar(@{ $ret->{cats} });
      $out{cat_is_self}     = ($c == $cat ? 1 : 0);
      $out{bindings_shared} = ($b->get_bindings_ref == $args ? 1 : 0);
      $out{slippages_count} = $b->slippages_count;
      $out{reln_is_chain}   = ($ret->{reln} == RELN_SCHEME::CHAIN() ? 1 : 0);
    }
  }
  $out{args_after} = desc_hash($args);
  record(%out);
}

my @sameness_builds = (
  ['plain', sub { { each => 7, length => 3 } }],
  ['sint length', sub { { each => 7, length => SInt->new(2) } }],
  ['sint each', sub { { each => SInt->new(4), length => 1 } }],
  ['object each', sub { { each => FakeVal->new(5), length => 2 } }],
  ['length 0', sub { { each => 7, length => 0 } }],
  ['length -1', sub { { each => 7, length => -1 } }],
  ['sint length 0', sub { { each => 7, length => SInt->new(0) } }],
  ['length 2.5', sub { { each => 7, length => 2.5 } }],
  ['length 0.5', sub { { each => 7, length => 0.5 } }],
  ['length "3"', sub { { each => 7, length => '3' } }],
  ['length "abc"', sub { { each => 7, length => 'abc' } }],
  ['each undef', sub { { each => undef, length => 2 } }],
  ['extra key', sub { { each => 'x', length => 2, extra => 1 } }],
  ['length undef', sub { { each => 7, length => undef } }],
  ['missing length', sub { { each => 7 } }],
  ['missing each', sub { { length => 3 } }],
  ['empty', sub { {} }],
);
record_build('sameness', $_->[0], $_->[1]->()) for @sameness_builds;

my @seq_builds = (
  ['start end', sub { { start => 1, end => 4 } }],
  ['start end reversed', sub { { start => 4, end => 1 } }],
  ['start end equal', sub { { start => 3, end => 3 } }],
  ['start length', sub { { start => 1, length => 3 } }],
  ['end length', sub { { end => 5, length => 3 } }],
  ['sint start end', sub { { start => SInt->new(2), end => SInt->new(5) } }],
  ['sint start, length', sub { { start => SInt->new(2), length => 3 } }],
  ['sint end, sint length', sub { { end => SInt->new(6), length => SInt->new(2) } }],
  ['plain end, sint length', sub { { end => 6, length => SInt->new(2) } }],
  ['all three, length kept', sub { { start => 2, end => 5, length => 99 } }],
  ['all three, length 0', sub { { start => 2, end => 5, length => 0 } }],
  ['start 0, length', sub { { start => 0, length => 3 } }],
  ['end 0, length', sub { { end => 0, length => 3 } }],
  ['negative', sub { { start => -2, end => 1 } }],
  ['floats', sub { { start => 1.5, end => 3.7 } }],
  ['strings', sub { { start => '2', length => '3' } }],
  ['length 0 only', sub { { start => 4, length => 0 } }],
  ['extra key', sub { { start => 1, end => 3, extra => 'x' } }],
  ['start only', sub { { start => 1 } }],
  ['length and x', sub { { length => 3, x => 1 } }],
  ['empty', sub { {} }],
);
for my $k (qw(ascending descending)) {
  record_build($k, $_->[0], $_->[1]->()) for @seq_builds;
}

# --- Instancer ------------------------------------------------------------------
my $m1 = FakeMeto->new(FakeType->new('t1'));
my $m2 = FakeMeto->new(FakeType->new('t2'));
my $ok = sub { FakeResult->new(parts => {}, entire => undef) };

sub record_instancer {
  my ($k, $label, $group) = @_;
  my $cat = $CATS{$k};
  my $b;
  my $died = dies(sub { no warnings; $b = $cat->Instancer($group) });
  my %out = (kind => 'instancer', cat => $k, label => $label, died => $died,
             seen => [ map { desc($_) } @{ $group->{seen} } ],
             seen_count => scalar(@{ $group->{seen} }));
  if (!$died) {
    $out{defined} = defined($b) ? 1 : 0;
    if (defined $b) {
      my $mode = $b->get_metonymy_mode;
      my $pos  = $b->get_position;
      $out{ref}             = ref($b);
      $out{bindings}        = desc_hash($b->get_bindings_ref);
      $out{slippage_positions} = [ sort $b->slippage_positions ];
      $out{slippages_count} = $b->slippages_count;
      $out{metonymy_mode}   = defined($mode) ? $mode->as_text : undef;
      $out{position}        = defined($pos) ? $pos->position : undef;
    }
  }
  record(%out);
}

my @sameness_instancer = (
  ['plain', sub { FakeGroup->new(items => [E(3), E(3), E(3)], result => $ok->()) }],
  ['not seen', sub { FakeGroup->new(items => [E(3), E(3)], result => undef) }],
  ['seen returns 0', sub { FakeGroup->new(items => [E(3), E(3)], result => 0) }],
  ['parts blemished', sub { FakeGroup->new(items => [E(3), E(4)], result => FakeResult->new(parts => { 1 => $m1 })) }],
  ['parts undef', sub { FakeGroup->new(items => [E(3)], result => FakeResult->new(parts => undef)) }],
  ['two blemishes', sub { FakeGroup->new(items => [E(3), E(4), E(4)], result => FakeResult->new(parts => { 1 => $m1, 2 => $m2 })) }],
  ['element group, entire blemish', sub { FakeElemGroup->new(items => [E(2)], result => FakeResult->new(parts => { 3 => $m1 }, entire => $m2)) }],
  ['element group, no entire', sub { FakeElemGroup->new(items => [E(2)], result => FakeResult->new(parts => { 2 => $m1 }, entire => undef)) }],
  ['non-element group, entire blemish', sub { FakeGroup->new(items => [E(2), E(2)], result => FakeResult->new(parts => {}, entire => $m2)) }],
  ['empty group', sub { FakeGroup->new(items => [], result => $ok->()) }],
  ['parts_count differs', sub { FakeGroup->new(items => [E(5), E(5)], parts_count => 4, result => $ok->()) }],
  ['first item undef', sub { FakeGroup->new(items => [undef, E(1)], result => $ok->()) }],
);
record_instancer('sameness', $_->[0], $_->[1]->()) for @sameness_instancer;

my @seq_instancer = (
  ['ascending items', sub { FakeGroup->new(items => [E(1), E(2), E(3)], result => $ok->()) }],
  ['descending items', sub { FakeGroup->new(items => [E(3), E(2), E(1)], result => $ok->()) }],
  ['single item', sub { FakeGroup->new(items => [E(4)], result => $ok->()) }],
  ['zero start', sub { FakeGroup->new(items => [E(0), E(2)], result => $ok->()) }],
  ['not seen', sub { FakeGroup->new(items => [E(1), E(3)], result => undef) }],
  ['first item undef', sub { FakeGroup->new(items => [undef, E(3)], result => $ok->()) }],
  ['last item undef', sub { FakeGroup->new(items => [E(1), undef], result => $ok->()) }],
  ['empty', sub { FakeGroup->new(items => [], result => $ok->()) }],
  ['metonym subobject', sub { FakeGroup->new(items => [FakeSub->new(E(5)), E(7)], result => $ok->()) }],
  ['effective object not an element', sub { FakeGroup->new(items => [FakeSub->new(FakeVal->new(1)), E(7)], result => $ok->()) }],
  ['element subclass', sub { FakeGroup->new(items => [E(1), bless({}, 'FakeElemSub')], result => $ok->()) }],
  ['parts blemished', sub { FakeGroup->new(items => [E(1), E(3)], result => FakeResult->new(parts => { 1 => $m1 })) }],
  ['element group, entire blemish', sub { FakeElemGroup->new(items => [E(2)], result => FakeResult->new(parts => {}, entire => $m1)) }],
  ['non-element group, entire blemish', sub { FakeGroup->new(items => [E(2), E(4)], result => FakeResult->new(parts => { 0 => $m2 }, entire => $m1)) }],
);
for my $k (qw(ascending descending)) {
  record_instancer($k, $_->[0], $_->[1]->()) for @seq_instancer;
}

# --- Metonyable: finders and unfinders --------------------------------------------
{
  my $cat = $S::SAMENESS;
  for my $k (@CAT_KEYS) {
    my $c = $CATS{$k};
    next unless $c->can('get_meto_finder');
    record(kind => 'meto_lookup', cat => $k,
           finder_each => (defined($c->get_meto_finder('each')) ? 1 : 0),
           finder_x    => (defined($c->get_meto_finder('x')) ? 1 : 0),
           unfinder_each => (defined($c->get_meto_unfinder('each')) ? 1 : 0),
           unfinder_x  => (defined($c->get_meto_unfinder('x')) ? 1 : 0));
  }

  my $bindings = SBindings->create({}, { each => 7, length => SInt->new(3) });
  my $obj = FakeCatObj->new($bindings);
  my $m = $cat->get_meto_finder('each')->($obj, $cat, 'each', $bindings);
  record(kind => 'finder', label => 'direct',
         ref => ref($m), category_is_self => ($m->{category} == $cat ? 1 : 0),
         name => $m->{name}, starred => desc($m->{starred}),
         unstarred_is_obj => ($m->{unstarred} == $obj ? 1 : 0),
         info_loss => desc_hash($m->{info_loss}),
         keys => [ sort keys %$m ]);

  my $m2 = $cat->find_metonym($obj, 'each');
  record(kind => 'finder', label => 'find_metonym', starred => desc($m2->{starred}),
         edges => $m2->{starred}{edges}, info_loss => desc_hash($m2->{info_loss}));

  my $anch = FakeAnchObj->new(SBindings->create({}, { each => SInt->new(4), length => 2 }));
  my $m3 = $cat->find_metonym($anch, 'each');
  record(kind => 'finder', label => 'find_metonym anchored', starred => desc($m3->{starred}),
         edges => $m3->{starred}{edges}, info_loss => desc_hash($m3->{info_loss}));

  record(kind => 'finder_dies', label => 'unknown name',
         dies => dies(sub { $cat->find_metonym($obj, 'x') }));
  record(kind => 'finder_dies', label => 'no binding',
         dies => dies(sub { $cat->find_metonym(FakeCatObj->new(undef), 'each') }));

  my $unfinder = $cat->get_meto_unfinder('each');
  for my $case (['length', { length => 3 }], ['length sint', { length => SInt->new(2) }],
                ['each overridden', { length => 2, each => 8 }], ['length 0', { length => 0 }],
                ['empty', {}], ['other key', { x => 1 }], ['two other keys', { x => 1, y => 2 }]) {
    my ($label, $info) = @{$case};
    my $ret;
    my $ok = eval { $ret = $unfinder->($cat, 'each', $info, FakeVal->new(1)); 1 };
    record(kind => 'unfinder', label => $label, died => ($ok ? 0 : 1),
           error => ($ok ? undef : err_text($@)),
           result => ($ok ? desc($ret) : undef));
  }
}

emit();
