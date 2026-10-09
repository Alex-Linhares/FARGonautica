# Oracle for SCategory/Number.pm, Prime.pm, Odd.pm, Even.pm and Numeric.pm.
# Output: tests/golden/numeric_categories.json
#
# Mapping::Numeric->create is replaced by a recorder that returns "name/catname",
# so FindMappingForCat can be checked without SLTM. build() uses the real
# Seqsee::Element; only the bindings it attaches are recorded.
use strict;
use Oracle;
use S;

{
  no warnings 'redefine';
  *Mapping::Numeric::create = sub {
    my ($pkg, $name, $cat) = @_;
    return "$name/" . $cat->get_name;
  };
}

package FakeTransform;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub get_name { $_[0]{n} }

package FakeMagObj;
sub new { my ($c, $m) = @_; bless { m => $m }, $c }
sub get_mag { $_[0]{m} }

package main;

sub show { my ($v) = @_; defined($v) ? "$v" : undef }

my %CATS = (number => $S::NUMBER, prime => $S::PRIME, odd => $S::ODD, even => $S::EVEN);
my @CAT_KEYS = qw(number prime odd even);

# --- names, roles, LTMStorable::Independent, NotMetonyable ----------------------
for my $k (@CAT_KEYS) {
  my $cat = $CATS{$k};
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
    string_to_recreate => $cat->string_to_recreate,
    deserialized_class => ref($copy),
    deserialized_is_same => ($copy == $cat ? 1 : 0),
    is_pure            => $cat->is_pure,
    get_pure_is_self   => ($cat->get_pure == $cat ? 1 : 0),
    meto_types_count   => scalar(@meto),
    memory_deps_count  => scalar(@deps),
    is_metonyable      => show(scalar $cat->is_metonyable),
    is_numeric         => ($cat->IsNumeric ? 1 : 0),
    does_numeric       => ($cat->does('SCategory::Numeric') ? 1 : 0),
    smartmatch_self    => ($cat ~~ $cat ? 1 : 0),
  );
}

# AreAttributesSufficientToBuild: `my ($self, @atts) = 1;` empties @atts.
for my $k (@CAT_KEYS) {
  for my $atts ([], ['mag'], ['mag', 'x'], ['x']) {
    record(kind => 'sufficient', cat => $k, atts => $atts,
           result => $CATS{$k}->AreAttributesSufficientToBuild(@$atts));
  }
}

# --- Prime helpers --------------------------------------------------------------
my @prime_inputs = (-5, -1, 0, 1, 2, 3, 4, 5, 9, 10, 13, 50, 89, 96, 97, 98, 100, 1000,
                    '7', '2.0', 2.0, '07', ' 7', '', undef);
for my $n (@prime_inputs) {
  record(kind => 'is_prime', input => $n, result => SCategory::Prime::IsPrime($n));
}
for my $n (-5, -1, 0, 1, 2, 3, 4, 5, 13, 14, 88, 89, 90, 96, 97, 98, 1000, '5', undef, 2.0) {
  no warnings;
  record(kind => 'next_prime', input => $n, result => show(SCategory::Prime::NextPrime($n)));
}
for my $n (-5, 0, 1, 2, 3, 4, 5, 14, 89, 97, 98, 100, 1000, '5', undef, 7.0) {
  no warnings;
  record(kind => 'previous_prime', input => $n, result => show(SCategory::Prime::PreviousPrime($n)));
}

# --- NumericInstancer / Instancer / is_instance -------------------------------
my @mags = (-4, -3, -2.5, -1, 0, 1, 2, 2.5, 3, 3.5, 4, 7, 9, 97, 98, '5', '6', 'abc', '', undef);
for my $k (@CAT_KEYS) {
  my $cat = $CATS{$k};
  for my $m (@mags) {
    no warnings;
    my $b  = $cat->NumericInstancer($m);
    my $b2 = $cat->Instancer(FakeMagObj->new($m));
    record(
      kind      => 'instancer',
      cat       => $k,
      mag       => $m,
      defined   => (defined($b) ? 1 : 0),
      ref       => (defined($b) ? ref($b) : undef),
      bindings  => (defined($b) ? $b->get_bindings_ref : undef),
      slippages => (defined($b) ? $b->get_squinting_raw : undef),
      via_object_defined => (defined($b2) ? 1 : 0),
    );
  }
}

# --- FindMappingForCat ----------------------------------------------------------
my @pairs = ([3, 3], [3, 4], [4, 3], [3, 5], [5, 3], [3, 7], [7, 5], [5, 7], [4, 5], [4, 6],
             [2, 3], [3, 2], [2, 0], [97, 0], [97, 97], [96, 97], [97, 89], [1, 2], [0, 2],
             [-1, 1], ['3', '4'], ['3', 3.0], [10, 20], [2.5, 3.5], [2.5, 4.5]);
for my $k (@CAT_KEYS) {
  for my $p (@pairs) {
    no warnings;
    # PERL-QUIRK: NextPrime/PreviousPrime loop forever on non-integers.
    next if $k eq 'prime' and $p->[0] != int($p->[0]);
    record(kind => 'find_mapping', cat => $k, a => $p->[0], b => $p->[1],
           result => show($CATS{$k}->FindMappingForCat(@$p)));
  }
}

# --- ApplyMappingForCat ---------------------------------------------------------
for my $k (@CAT_KEYS) {
  for my $name (qw(same succ pred flip)) {
    for my $m (-1, 0, 1, 2, 3, 4, 5, 13, 96, 97, 98, '6') {
      no warnings;
      record(kind => 'apply_mapping', cat => $k, name => $name, mag => $m,
             result => show($CATS{$k}->ApplyMappingForCat(FakeTransform->new($name), $m)));
    }
  }
}

# --- build ----------------------------------------------------------------------
for my $k (@CAT_KEYS) {
  my $cat = $CATS{$k};
  for my $args ({ mag => 7 }, { mag => 4, extra => 'x' }, { mag => '5' }) {
    my $ret = $cat->build($args);
    my $b = $ret->GetBindingForCategory($cat);
    record(
      kind          => 'build',
      cat           => $k,
      args          => $args,
      class         => ref($ret),
      mag           => $ret->get_mag,
      has_cat       => ($ret->is_of_category_p($cat) ? 1 : 0),
      bindings      => $b->get_bindings_ref,
      bindings_shared => ($b->get_bindings_ref == $args ? 1 : 0),
      slippages     => $b->get_squinting_raw,
    );
  }
  record(kind => 'build_dies', cat => $k, args => {}, dies => dies(sub { $cat->build({}) }));
  record(kind => 'build_dies', cat => $k, args => { x => 1 }, dies => dies(sub { $cat->build({ x => 1 }) }));
  # Seqsee::Element->create rejects an undef mag (the role itself only checks `exists`).
  record(kind => 'build_dies', cat => $k, args => { mag => undef }, dies => dies(sub { $cat->build({ mag => undef }) }));
}

emit();
