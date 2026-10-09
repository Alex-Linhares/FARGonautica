# Oracle for SCategory/Mountain.pm and SCategory/Interlaced.pm.
# Output: tests/golden/sequence_categories2.json
#
# As in sequence_categories.pl, Seqsee::Object->create is replaced by a
# recorder (FakeBuilt), Instancer is fed fake groups whose CanBeSeenAs records
# the guess, and subobjects are real Seqsee::Elements.
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
sub get_items_array { @{ $_[0]{items} } }
sub get_parts_count { $_[0]{parts_count} // scalar(@{ $_[0]{items} }) }
sub CanBeSeenAs { my ($s, $built) = @_; push @{ $s->{seen} }, $built; return $s->{result} }

package FakeElemGroup;
our @ISA = ('FakeGroup', 'Seqsee::Element');

package FakeSub;
sub new { my ($c, $eo) = @_; bless { eo => $eo }, $c }
sub GetEffectiveObject { $_[0]{eo} }

package FakeElemSub;
our @ISA = ('Seqsee::Element');
sub GetEffectiveObject { $_[0] }
sub get_mag { 9 }

package main;

{
  no warnings 'redefine';
  *Seqsee::Object::create = sub { my ($pkg, @items) = @_; FakeBuilt->new(@items) };
}

sub show { my ($v) = @_; defined($v) ? "$v" : undef }

sub desc {
  my ($v) = @_;
  return undef unless defined $v;
  my $r = ref($v);
  return "$v" if !$r or $r eq 'SInt';
  return "V($v->{n})" if $r eq 'FakeVal';
  return "E(" . $v->get_mag . ")" if $r eq 'Seqsee::Element';
  return "Built(" . join(",", map { desc($_) // '' } @{ $v->{items} }) . ")" if $r eq 'FakeBuilt';
  return "OBJ:$r";
}
sub desc_hash { my ($h) = @_; return { map { $_ => desc($h->{$_}) } keys %$h } }
sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; return $e }
sub E { Seqsee::Element->create($_[0], 0) }

my %CATS = (
  mountain      => $S::MOUNTAIN,
  interlaced_2  => SCategory::Interlaced->Create(2),
  interlaced_3  => SCategory::Interlaced->Create(3),
  interlaced_0  => SCategory::Interlaced->Create(0),
);
my @CAT_KEYS = qw(mountain interlaced_2 interlaced_3 interlaced_0);

# --- basics -------------------------------------------------------------------
for my $k (@CAT_KEYS) {
  my $cat  = $CATS{$k};
  my @meto = $cat->get_meto_types;
  my @deps = $cat->get_memory_dependencies;
  my $ser;
  my $ser_ok = eval { $ser = $cat->serialize; 1 };
  my %out = (
    kind              => 'basics',
    cat               => $k,
    class             => ref($cat),
    get_name          => $cat->get_name,
    as_text           => $cat->as_text,
    serialize_died    => ($ser_ok ? 0 : 1),
    serialize_error   => ($ser_ok ? undef : err_text($@)),
    serialize         => $ser,
    is_pure           => $cat->is_pure,
    get_pure_is_self  => ($cat->get_pure == $cat ? 1 : 0),
    meto_types        => [ sort @meto ],
    memory_deps_count => scalar(@deps),
    is_metonyable     => show(scalar $cat->is_metonyable),
    is_numeric        => ($cat->IsNumeric ? 1 : 0),
    smartmatch_self   => ($cat ~~ $cat ? 1 : 0),
  );
  if ($ser_ok) {
    my $copy = ref($cat)->deserialize($ser);
    $out{deserialized_class}   = ref($copy);
    $out{deserialized_is_same} = ($copy == $cat ? 1 : 0);
  }
  if ($k =~ /interlaced/) {
    $out{parts_count}        = $cat->get_parts_count;
    $out{longer_description} = $cat->longer_description;
  }
  record(%out);
}

# --- AreAttributesSufficientToBuild --------------------------------------------
my @att_sets = ([], ['foot'], ['peak'], ['foot', 'peak'], ['peak', 'foot', 'x'],
                ['foot', 'foot'], ['x', 'y'], ['part_no_1'], ['part_no_1', 'part_no_2'],
                ['part_no_1', 'part_no_1'], ['part_no_1', 'part_no_2', 'part_no_3'],
                ['part_no_x', 'part_no_y'], ['x', 'part_no_1', 'part_no_2'],
                ['apart_no_1', 'part_no_2'], ['part_no_2', 'part_no_1', 'part_no_2']);
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
      $out{items}      = [ map { desc($_) } @{ $ret->{items} } ];
      $out{cats_count} = scalar(@{ $ret->{cats} });
      if (@{ $ret->{cats} }) {
        my ($c, $b) = @{ $ret->{cats}[0] };
        $out{cat_is_self}     = ($c == $cat ? 1 : 0);
        $out{bindings_shared} = ($b->get_bindings_ref == $args ? 1 : 0);
        $out{slippages_count} = $b->slippages_count;
      }
      $out{reln_is_chain} = (defined($ret->{reln}) && $ret->{reln} == RELN_SCHEME::CHAIN() ? 1 : 0);
    }
  }
  $out{args_after} = desc_hash($args);
  record(%out);
}

my @mountain_builds = (
  ['plain', sub { { foot => 1, peak => 4 } }],
  ['reversed', sub { { foot => 4, peak => 1 } }],
  ['equal', sub { { foot => 3, peak => 3 } }],
  ['sint', sub { { foot => SInt->new(2), peak => SInt->new(5) } }],
  ['sint equal', sub { { foot => SInt->new(2), peak => SInt->new(2) } }],
  ['zero foot', sub { { foot => 0, peak => 2 } }],
  ['negative', sub { { foot => -1, peak => 1 } }],
  ['floats', sub { { foot => 1.5, peak => 3.7 } }],
  ['float peak just above', sub { { foot => 1, peak => 1.5 } }],
  ['float equal', sub { { foot => 3.5, peak => 3.5 } }],
  ['strings', sub { { foot => '2', peak => '4' } }],
  ['strings equal', sub { { foot => '3', peak => '3.0' } }],
  ['foot undef', sub { { foot => undef, peak => 2 } }],
  ['peak undef', sub { { foot => 2, peak => undef } }],
  ['extra key', sub { { foot => 1, peak => 2, extra => 'x' } }],
  ['missing peak', sub { { foot => 1 } }],
  ['missing foot', sub { { peak => 1 } }],
  ['empty', sub { {} }],
);
record_build('mountain', $_->[0], $_->[1]->()) for @mountain_builds;

my @interlaced_builds = (
  ['all parts', sub { { part_no_1 => 1, part_no_2 => 2, part_no_3 => 3 } }],
  ['objects', sub { { part_no_1 => FakeVal->new(1), part_no_2 => SInt->new(2), part_no_3 => E(3) } }],
  ['missing part', sub { { part_no_1 => 1, part_no_3 => 3 } }],
  ['extra keys', sub { { part_no_1 => 1, part_no_2 => 2, part_no_3 => 3, part_no_4 => 4, x => 5 } }],
  ['empty', sub { {} }],
);
for my $k (qw(interlaced_2 interlaced_3 interlaced_0)) {
  record_build($k, $_->[0], $_->[1]->()) for @interlaced_builds;
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
      $out{ref}                = ref($b);
      $out{bindings}           = desc_hash($b->get_bindings_ref);
      $out{slippage_positions} = [ sort $b->slippage_positions ];
      $out{slippages_count}    = $b->slippages_count;
      $out{metonymy_mode}      = defined($mode) ? $mode->as_text : undef;
      $out{position}           = defined($pos) ? $pos->position : undef;
    }
  }
  record(%out);
}

my @mountain_instancer = (
  ['plain', sub { FakeGroup->new(items => [E(1), E(2), E(3), E(2), E(1)], result => $ok->()) }],
  ['even size', sub { FakeGroup->new(items => [E(1), E(2), E(2), E(1)], result => $ok->()) }],
  ['empty', sub { FakeGroup->new(items => [], result => $ok->()) }],
  ['single item', sub { FakeGroup->new(items => [E(4)], result => $ok->()) }],
  ['not seen', sub { FakeGroup->new(items => [E(1), E(3), E(1)], result => undef) }],
  ['seen returns 0', sub { FakeGroup->new(items => [E(1), E(3), E(1)], result => 0) }],
  ['zero foot', sub { FakeGroup->new(items => [E(0), E(1), E(0)], result => $ok->()) }],
  ['valley', sub { FakeGroup->new(items => [E(3), E(1), E(3)], result => $ok->()) }],
  ['first item undef', sub { FakeGroup->new(items => [undef, E(2), E(1)], result => $ok->()) }],
  ['middle item missing', sub { FakeGroup->new(items => [E(1)], parts_count => 3, result => $ok->()) }],
  ['parts_count even, items odd', sub { FakeGroup->new(items => [E(1), E(2), E(1)], parts_count => 2, result => $ok->()) }],
  ['metonym subobject', sub { FakeGroup->new(items => [FakeSub->new(E(1)), E(5), E(1)], result => $ok->()) }],
  ['effective object not an element', sub { FakeGroup->new(items => [FakeSub->new(FakeVal->new(1)), E(5), E(1)], result => $ok->()) }],
  ['element subclass peak', sub { FakeGroup->new(items => [E(1), bless({}, 'FakeElemSub'), E(1)], result => $ok->()) }],
  ['parts blemished', sub { FakeGroup->new(items => [E(1), E(2), E(1)], result => FakeResult->new(parts => { 1 => $m1 })) }],
  ['parts undef', sub { FakeGroup->new(items => [E(1), E(2), E(1)], result => FakeResult->new(parts => undef)) }],
  ['element group, entire blemish', sub { FakeElemGroup->new(items => [E(2)], result => FakeResult->new(parts => { 3 => $m1 }, entire => $m2)) }],
  ['element group, no entire', sub { FakeElemGroup->new(items => [E(2)], result => FakeResult->new(parts => { 2 => $m1 })) }],
  ['non-element group, entire blemish', sub { FakeGroup->new(items => [E(2), E(3), E(2)], result => FakeResult->new(parts => {}, entire => $m2)) }],
);
record_instancer('mountain', $_->[0], $_->[1]->()) for @mountain_instancer;

my @interlaced_instancer = (
  ['two items', sub { FakeGroup->new(items => [E(1), E(2)], result => $ok->()) }],
  ['three items', sub { FakeGroup->new(items => [E(1), E(2), E(3)], result => $ok->()) }],
  ['empty', sub { FakeGroup->new(items => [], result => $ok->()) }],
  ['undef item', sub { FakeGroup->new(items => [undef, E(2)], result => $ok->()) }],
  ['mixed', sub { FakeGroup->new(items => [FakeVal->new(1), SInt->new(2), 'x'], result => $ok->()) }],
);
for my $k (qw(interlaced_2 interlaced_3 interlaced_0)) {
  record_instancer($k, $_->[0], $_->[1]->()) for @interlaced_instancer;
}

# --- Interlaced: Create, memo, validation ----------------------------------------
{
  my $c2 = $CATS{interlaced_2};
  record(kind => 'create', label => 'memo',
         same_int        => (SCategory::Interlaced->Create(2) == $c2 ? 1 : 0),
         same_string     => (SCategory::Interlaced->Create('2') == $c2 ? 1 : 0),
         same_float      => (SCategory::Interlaced->Create(2.0) == $c2 ? 1 : 0),
         leading_zero_same => (SCategory::Interlaced->Create('02') == $c2 ? 1 : 0),
         leading_zero_name => SCategory::Interlaced->Create('02')->get_name,
         leading_zero_count => SCategory::Interlaced->Create('02')->get_parts_count,
         new_is_not_memo => (SCategory::Interlaced->new(parts_count => 2) == $c2 ? 1 : 0),
         deserialize_same => (SCategory::Interlaced->deserialize('2') == $c2 ? 1 : 0),
         deserialize_new_name => SCategory::Interlaced->deserialize('7')->get_name,
         negative_name   => SCategory::Interlaced->Create(-1)->get_name);

  for my $case (['2.5', 2.5], ['abc', 'abc'], ['undef', undef], ['empty string', ''], ['1e3', '1e3'], [' 3', ' 3']) {
    my ($label, $v) = @$case;
    record(kind => 'create_dies', label => $label,
           create => dies(sub { no warnings; SCategory::Interlaced->Create($v) }),
           new => dies(sub { no warnings; SCategory::Interlaced->new(parts_count => $v) }),
           create_again => dies(sub { no warnings; SCategory::Interlaced->Create($v) }));
  }
  record(kind => 'create_dies', label => 'missing',
         new => dies(sub { SCategory::Interlaced->new() }));

  # memoize('get_name'/'as_text'): the name is fixed by the first call.
  my $x = SCategory::Interlaced->new(parts_count => 5);
  my $name1 = $x->get_name;
  $x->set_parts_count(6);
  my $as_text_after = $x->as_text;
  my $name_after = $x->get_name;
  my $y = SCategory::Interlaced->new(parts_count => 5);
  $y->set_parts_count(8);
  record(kind => 'memoize', name_before => $name1, name_after => $name_after,
         as_text_after => $as_text_after, parts_count_after => $x->get_parts_count,
         longer_after => $x->longer_description,
         unnamed_set_name => $y->get_name, unnamed_set_as_text => $y->as_text,
         set_bad_dies => dies(sub { $y->set_parts_count(1.5) }),
         count_after_bad_set => $y->get_parts_count);
}

emit();
