# Oracle for SCategory/Alternating.pm.
# Output: tests/golden/alternating.json
#
# Pure objects are real SLTM::Platonic objects. They stringify as addresses
# ("SLTM::Platonic=SCALAR(0x...)"), so names and the Create sort order are
# recorded through `norm`, which replaces each such string by the platonic's
# as_text ("plat3"). Which platonic becomes object1 depends on addresses and is
# recorded only as a set.
#
# Replaced: Seqsee::Object->create (FakeBuilt), Mapping::Numeric->create
# (FakeMapping), Mapping::Structural->create (records its args), FindMapping
# (inside SCategory::Alternating, a table on FakeVal numbers) and main::message
# (records the text).
use strict;
use Oracle;
use S;

package FakeBuilt;
sub new { my ($c, @items) = @_; bless { items => [@items], described => [] }, $c }
sub describe_as { my ($s, $cat) = @_; push @{ $s->{described} }, $cat; return 'BINDINGS' }

package FakeMapping;
sub new { my ($c, $name, $cat) = @_; bless { name => $name, cat => $cat }, $c }

package FakeTransform;
sub new { my ($c, $name) = @_; bless { name => $name }, $c }
sub get_name { $_[0]{name} }
sub as_text { 'T<' . ($_[0]{name} // 'undef') . '>' }

package FakeBindings;
sub new { my ($c, $h) = @_; bless { h => $h }, $c }
sub get_bindings_ref { $_[0]{h} }

package FakeCat;
sub new { my ($c, %a) = @_; bless { numeric => 0, sufficient => 1, %a }, $c }
sub IsNumeric { $_[0]{numeric} }
sub AreAttributesSufficientToBuild { my ($s, @atts) = @_; push @{ $s->{asked} }, [sort @atts]; $s->{sufficient} }
sub get_name { $_[0]{name} }

# An object with a pure (a platonic), categories, and per-category bindings.
package FakeObj;
sub new { my ($c, %a) = @_; bless { cats => [], b => {}, described => [], %a }, $c }
sub get_pure { $_[0]{pure} }
sub as_text { $_[0]{text} }
sub describe_as { my ($s, $cat) = @_; push @{ $s->{described} }, $cat; return 1 }
sub is_of_category_p { my ($s, $cat) = @_; my $h = $s->{b}{$cat}; defined($h) ? FakeBindings->new($h) : undef }
sub get_common_categories {
  my ($self, @others) = @_;
  my @common;
  for my $cat (@{ $self->{cats} }) {
    push @common, $cat unless grep { !grep { $_ == $cat } @{ $_->{cats} } } @others;
  }
  return @common;
}

package main;

our @MESSAGES;
{
  no warnings 'redefine';
  *Seqsee::Object::create = sub { my ($pkg, @items) = @_; FakeBuilt->new(@items) };
  *Mapping::Numeric::create = sub { my ($pkg, $name, $cat) = @_; FakeMapping->new($name, $cat) };
  *Mapping::Structural::create = sub { my ($pkg, $opts) = @_; return { STRUCTURAL => 1, %$opts } };
  *main::message = sub { my ($text, $level) = @_; push @MESSAGES, "$text|$level" };
  # FindMapping on FakeVal numbers: same, succ, pred, else undef.
  *SCategory::Alternating::FindMapping = sub {
    my ($a, $b) = @_;
    return undef unless ref($a) eq 'FakeVal' and ref($b) eq 'FakeVal';
    my $d = $b->{n} - $a->{n};
    return $d == 0 ? 'same' : $d == 1 ? 'succ' : $d == -1 ? 'pred' : undef;
  };
}

package FakeVal;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub as_text { "V$_[0]{n}" }
sub get_pure { SLTM::Platonic->create("$_[0]{n}") }
sub describe_as { my ($s, $cat) = @_; push @{ $s->{described} }, $cat; return 1 }

package main;

sub P { SLTM::Platonic->create($_[0]) }

sub norm {
  my ($s) = @_;
  return undef unless defined $s;
  $s =~ s/(SLTM::Platonic=SCALAR\(0x[0-9a-f]+\))/plat_text($1)/ge;
  return $s;
}
my %PLAT_BY_STR;
sub plat_text { my $p = $PLAT_BY_STR{ $_[0] }; $p ? $p->as_text : $_[0] }
for my $s ('3', '5', '7', '[1,2]', '0', '1', '4') { my $p = P($s); $PLAT_BY_STR{"$p"} = $p }

sub alt_desc {
  my ($c) = @_;
  return 'Alt(' . join('|', sort map { $_->as_text } ($c->object1, $c->object2)) . ')';
}

sub desc {
  my ($v) = @_;
  return undef unless defined $v;
  my $r = ref($v);
  return "$v" if !$r or $r eq 'SInt';
  return '[' . join(',', map { desc($_) // 'undef' } @$v) . ']' if $r eq 'ARRAY';
  return 'Built(' . join(',', map { desc($_) // 'undef' } @{ $v->{items} }) . ')' if $r eq 'FakeBuilt';
  return $v->as_text if $r eq 'SLTM::Platonic' or $r eq 'FakeObj' or $r eq 'FakeVal';
  return 'Num(' . $v->{name} . ',' . alt_desc($v->{cat}) . ')' if $r eq 'FakeMapping';
  return alt_desc($v) if $r eq 'SCategory::Alternating';
  return 'Cat(' . $v->{name} . ')' if $r eq 'FakeCat';
  if ($r eq 'HASH' and $v->{STRUCTURAL}) {
    my $cb = $v->{changed_bindings};
    return 'Structural(cat=' . desc($v->{category}) . ',meto=' . $v->{meto_mode}->as_text
      . ',dir_same=' . ($v->{direction_reln} == $Mapping::Dir::Same ? 1 : 0)
      . ',slippages=' . scalar(keys %{ $v->{slippages} })
      . ',changed={' . join(',', map { "$_:" . desc($cb->{$_}) } sort keys %$cb) . '})';
  }
  return "OBJ:$r";
}
sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; return norm($e) }
sub show { my ($v) = @_; defined($v) ? "$v" : undef }

my ($p3, $p5, $p7, $p12) = map { P($_) } ('3', '5', '7', '[1,2]');
sub O { my ($p) = @_; FakeObj->new(pure => P($p), text => "O$p") }

my $alt   = SCategory::Alternating->Create(O('3'), O('5'));
my $alt12 = SCategory::Alternating->Create(O('[1,2]'), O('3'));
my $alt33 = SCategory::Alternating->Create(O('3'), O('3'));
my %CATS = (alt => $alt, alt12 => $alt12, alt33 => $alt33);

# --- basics -------------------------------------------------------------------
for my $k (sort keys %CATS) {
  my $c    = $CATS{$k};
  my @deps = $c->get_memory_dependencies;
  my @meto = $c->get_meto_types;
  record(kind => 'basics', cat => $k, class => ref($c),
         objects          => [ sort map { $_->as_text } ($c->object1, $c->object2) ],
         objects_are_platonic => [ map { ref($_) } ($c->object1, $c->object2) ],
         name             => norm($c->get_name),
         name_is_o1_or_o2 => ($c->get_name eq $c->object1 . ' or ' . $c->object2 ? 1 : 0),
         as_text_is_name  => ($c->as_text eq $c->get_name ? 1 : 0),
         sorted_by_string => ("" . $c->object1 le "" . $c->object2 ? 1 : 0),
         deps             => [ map { $_->as_text } @deps ],
         deps_are_objects => (@deps == 2 && $deps[0] == $c->object1 && $deps[1] == $c->object2 ? 1 : 0),
         is_pure          => $c->is_pure,
         get_pure_is_self => ($c->get_pure == $c ? 1 : 0),
         meto_types       => [@meto],
         is_metonyable    => show(scalar $c->is_metonyable),
         is_numeric       => ($c->IsNumeric ? 1 : 0));
}

# --- Create / new ----------------------------------------------------------------
{
  my $same_pure_other_obj = SCategory::Alternating->Create(FakeObj->new(pure => $p5, text => 'x'), $p3);
  record(kind => 'create', label => 'memo',
         reversed_same     => (SCategory::Alternating->Create(O('5'), O('3')) == $alt ? 1 : 0),
         platonics_same    => (SCategory::Alternating->Create($p3, $p5) == $alt ? 1 : 0),
         other_objs_same   => ($same_pure_other_obj == $alt ? 1 : 0),
         new_is_not_memo   => (SCategory::Alternating->new(object1 => $p3, object2 => $p5) == $alt ? 1 : 0),
         self_alt_objects  => [ map { $_->as_text } ($alt33->object1, $alt33->object2) ],
         self_alt_same_obj => ($alt33->object1 == $alt33->object2 ? 1 : 0),
         self_alt_name     => norm($alt33->get_name),
         different         => (SCategory::Alternating->Create($p3, $p7) == $alt ? 1 : 0));
  for my $case (['missing object1', sub { SCategory::Alternating->new(object2 => $p3) }],
                ['missing object2', sub { SCategory::Alternating->new(object1 => $p3) }],
                ['empty', sub { SCategory::Alternating->new() }]) {
    my $died = dies($case->[1]);
    record(kind => 'create_dies', label => $case->[0], died => $died, error => err_text($@));
  }

  # memoize('get_name'/'as_text'): fixed by the first call.
  my $x = SCategory::Alternating->new(object1 => $p3, object2 => $p5);
  my $name1 = $x->get_name;
  $x->set_object1($p7);
  my $y = SCategory::Alternating->new(object1 => $p3, object2 => $p5);
  $y->set_object2($p7);
  # Memoize keeps separate caches for scalar and list context. The port has one
  # cache, so these calls are in scalar context (plus one list-context call).
  my $name_after = $x->get_name;
  my ($name_after_list) = $x->get_name;
  my $as_text_after = $x->as_text;
  my $unnamed_name = $y->get_name;
  my $unnamed_as_text = $y->as_text;
  record(kind => 'memoize', name_before => norm($name1), name_after => norm($name_after),
         name_after_list_context => norm($name_after_list),
         as_text_after => norm($as_text_after), object1_after => $x->object1->as_text,
         deps_after => [ map { $_->as_text } $x->get_memory_dependencies ],
         unnamed_set_name => norm($unnamed_name), unnamed_set_as_text => norm($unnamed_as_text));
}

# --- AreAttributesSufficientToBuild --------------------------------------------
for my $atts ([], ['which'], ['x'], ['x', 'which'], ['which', 'which'], ['Which'], ['whic'], ['which ']) {
  record(kind => 'sufficient', atts => $atts,
         result => show(scalar $alt->AreAttributesSufficientToBuild(@$atts)));
}

# --- Instancer ------------------------------------------------------------------
for my $k (sort keys %CATS) {
  for my $p ('3', '5', '7', '[1,2]') {
    my $b = $CATS{$k}->Instancer(O($p));
    my %out = (kind => 'instancer', cat => $k, pure => $p, defined => (defined($b) ? 1 : 0));
    if (defined $b) {
      $out{ref}             = ref($b);
      $out{bindings}        = { map { $_ => desc($b->get_bindings_ref->{$_}) } keys %{ $b->get_bindings_ref } };
      $out{slippages_count} = $b->slippages_count;
      $out{metonymy_mode}   = $b->get_metonymy_mode->as_text;
    }
    record(%out);
  }
}

# --- FindMappingForCat ------------------------------------------------------------
for my $k (sort keys %CATS) {
  for my $pair (['3', '3'], ['5', '5'], ['3', '5'], ['5', '3'], ['7', '7'], ['3', '7'],
                ['7', '3'], ['7', '5'], ['[1,2]', '3'], ['[1,2]', '[1,2]']) {
    my $m = $CATS{$k}->FindMappingForCat(O($pair->[0]), O($pair->[1]));
    record(kind => 'find_mapping', cat => $k, a => $pair->[0], b => $pair->[1],
           result => desc($m), cat_is_self => (defined($m) && $m->{cat} == $CATS{$k} ? 1 : 0));
  }
}

# --- ApplyMappingForCat -----------------------------------------------------------
for my $k (qw(alt alt12)) {
  for my $tname ('flip', 'no_flip', '', undef, 'other') {
    for my $orig (['obj 3', sub { O('3') }], ['obj 5', sub { O('5') }], ['obj 7', sub { O('7') }],
                  ['obj [1,2]', sub { O('[1,2]') }], ['plain 3', sub { 3 }], ['plain 5', sub { '5' }],
                  ['plain 7', sub { 7 }], ['plain [1,2]', sub { '[1,2]' }], ['plain [1, 2]', sub { '[1, 2]' }]) {
      my $ret;
      my $died = dies(sub { $ret = $CATS{$k}->ApplyMappingForCat(FakeTransform->new($tname), $orig->[1]->()) });
      record(kind => 'apply', cat => $k, transform => $tname, original => $orig->[0], died => $died,
             error => ($died ? err_text($@) : undef), result => desc($ret),
             is_built => (ref($ret) eq 'FakeBuilt' ? 1 : 0));
    }
  }
}

# --- build ----------------------------------------------------------------------
for my $k (qw(alt alt12)) {
  for my $case (['sint 0', sub { { which => SInt->new(0) } }], ['sint 1', sub { { which => SInt->new(1) } }],
                ['sint 2', sub { { which => SInt->new(2) } }], ['sint -1', sub { { which => SInt->new(-1) } }],
                ['sint "0.0"', sub { { which => SInt->new('0.0') } }], ['sint "1.0"', sub { { which => SInt->new('1.0') } }],
                ['sint 0.5', sub { { which => SInt->new(0.5) } }], ['sint undef', sub { { which => SInt->new(undef) } }],
                ['sint "abc"', sub { { which => SInt->new('abc') } }], ['sint "1abc"', sub { { which => SInt->new('1abc') } }],
                ['sint ""', sub { { which => SInt->new('') } }],
                ['plain 0', sub { { which => 0 } }], ['plain 1', sub { { which => 1 } }],
                ['undef', sub { { which => undef } }], ['missing', sub { {} }],
                ['extra key', sub { { which => SInt->new(1), x => 5 } }]) {
    my $ret;
    my $died = dies(sub { no warnings; $ret = $CATS{$k}->build($case->[1]->()) });
    my %out = (kind => 'build', cat => $k, label => $case->[0], died => $died,
               error => ($died ? err_text($@) : undef));
    if (!$died) {
      $out{result}         = desc($ret);
      $out{described_self} = [ map { $_ == $CATS{$k} ? 1 : 0 } @{ $ret->{described} } ];
    }
    record(%out);
  }
}

# --- FlippingMapping ---------------------------------------------------------------
for my $k (sort keys %CATS) {
  my $m = $CATS{$k}->FlippingMapping;
  record(kind => 'flipping', cat => $k, result => desc($m), cat_is_self => ($m->{cat} == $CATS{$k} ? 1 : 0));
}

# --- CheckForAlternation ------------------------------------------------------------
sub record_check {
  my ($label, $objs, $extra) = @_;
  @MESSAGES = ();
  my $ret;
  my $died = dies(sub { $ret = SCategory::Alternating->CheckForAlternation(@$objs) });
  my %out = (kind => 'check', label => $label, died => $died, error => ($died ? err_text($@) : undef),
             result => desc($ret), messages => [ map { norm($_) } @MESSAGES ]);
  if (defined($ret) and ref($ret) eq 'FakeMapping') {
    $out{result_cat_is_memo} = (SCategory::Alternating->Create($ret->{cat}->object1, $ret->{cat}->object2) == $ret->{cat} ? 1 : 0);
  }
  $out{described} = [ map { my $o = $_; ref($o) eq 'SInt' ? undef : [ map { desc($_) } @{ $o->{described} || [] } ] } @$objs ];
  $out{sint_cats} = [ map { ref($_) eq 'SInt' ? [ map { ref($_) eq 'SCategory::Alternating' ? alt_desc($_) : $_->get_name } @{ $_->get_categories } ] : undef } @$objs ];
  $extra->(\%out) if $extra;
  record(%out);
}

sub F { my ($p, %a) = @_; FakeObj->new(pure => P($p), text => "O$p", %a) }

record_check('objects alternate', [ F('3'), F('5'), F('3') ]);
record_check('objects alternate, reversed', [ F('5'), F('3'), F('5') ]);
record_check('objects all same', [ F('3'), F('3'), F('3') ]);
record_check('sints alternate', [ SInt->new(3), SInt->new(5), SInt->new(3) ]);
record_check('sints all same', [ SInt->new(4), SInt->new(4), SInt->new(4) ]);
record_check('sints no alternation', [ SInt->new(3), SInt->new(5), SInt->new(7) ]);
record_check('sints first eq second', [ SInt->new(3), SInt->new(3), SInt->new(5) ]);
record_check('objects no common category', [ F('3'), F('5'), F('7') ]);

my $numcat = FakeCat->new(name => 'numcat', numeric => 1);
record_check('numeric common category',
             [ F('3', cats => [$numcat]), F('5', cats => [$numcat]), F('7', cats => [$numcat]) ]);

my $cat = FakeCat->new(name => 'cat');
my $other = FakeCat->new(name => 'other');
sub FC { my ($p, $b, %a) = @_; F($p, cats => [$cat], b => (defined($b) ? { $cat => $b } : {}), %a) }

record_check('not of category (second)',
             [ FC('3', { x => FakeVal->new(1) }), FC('5', undef), FC('7', { x => FakeVal->new(3) }) ]);
record_check('not of category (first)',
             [ FC('3', undef), FC('5', { x => FakeVal->new(2) }), FC('7', { x => FakeVal->new(3) }) ]);
record_check('common category only in some',
             [ F('3', cats => [$other, $cat], b => { $cat => { x => FakeVal->new(1) } }),
               F('5', cats => [$cat], b => { $cat => { x => FakeVal->new(2) } }),
               F('7', cats => [$cat, $other], b => { $cat => { x => FakeVal->new(3) } }) ]);
{
  my $insuff = FakeCat->new(name => 'insufficient', sufficient => 0);
  record_check('attributes insufficient',
               [ map { F($_->[0], cats => [$insuff], b => { $insuff => { x => FakeVal->new($_->[1]) } }) }
                 (['3', 1], ['5', 2], ['7', 3]) ],
               sub { $_[0]{asked} = $insuff->{asked} });
}
record_check('same mapping succ',
             [ FC('3', { x => FakeVal->new(1) }), FC('5', { x => FakeVal->new(2) }), FC('7', { x => FakeVal->new(3) }) ],
             sub { $_[0]{asked} = [ @{ $cat->{asked} } ]; $cat->{asked} = [] });
record_check('same mapping same',
             [ FC('3', { x => FakeVal->new(4) }), FC('5', { x => FakeVal->new(4) }), FC('7', { x => FakeVal->new(4) }) ]);
record_check('recurse into alternation',
             [ FC('3', { x => FakeVal->new(1) }), FC('5', { x => FakeVal->new(4) }), FC('7', { x => FakeVal->new(1) }) ]);
record_check('recurse, mappings differ (succ, pred)',
             [ FC('3', { x => FakeVal->new(1) }), FC('5', { x => FakeVal->new(2) }), FC('7', { x => FakeVal->new(1) }) ]);
record_check('recurse fails',
             [ FC('3', { x => FakeVal->new(1) }), FC('5', { x => FakeVal->new(4) }), FC('7', { x => FakeVal->new(9) }) ]);
record_check('recurse on sints',
             [ FC('3', { x => SInt->new(1) }), FC('5', { x => SInt->new(2) }), FC('7', { x => SInt->new(1) }) ]);
record_check('recurse on sints, numeric stop',
             [ FC('3', { x => SInt->new(1) }), FC('5', { x => SInt->new(2) }), FC('7', { x => SInt->new(6) }) ]);
record_check('two keys, one recursion',
             [ FC('3', { x => FakeVal->new(1), y => FakeVal->new(1) }),
               FC('5', { x => FakeVal->new(2), y => FakeVal->new(4) }),
               FC('7', { x => FakeVal->new(3), y => FakeVal->new(1) }) ]);
record_check('no keys',
             [ FC('3', {}), FC('5', {}), FC('7', {}) ]);
record_check('missing key in later bindings',
             [ FC('3', { x => FakeVal->new(1) }), FC('5', {}), FC('7', {}) ]);
$cat->{asked} = [];

emit();
