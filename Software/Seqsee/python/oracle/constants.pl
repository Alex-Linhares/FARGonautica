# Oracle for the constant packages in S.pm (DIR, POS_MODE, METO_MODE,
# EXTENDIBILE, RELN_SCHEME, DISTANCE_MODE, DISTANCE).
# Output: tests/golden/constants.json
use strict;
use Oracle;
use S;

sub b { $_[0] ? 1 : 0 }

# DIR
my %dir = (
  LEFT => DIR::LEFT(), RIGHT => DIR::RIGHT(),
  UNKNOWN => DIR::UNKNOWN(), NEITHER => DIR::NEITHER(),
);
for my $name (sort keys %dir) {
  my $d = $dir{$name};
  my $flip = eval { $d->Flip->as_text };
  no strict 'refs';
  record(
    pkg => 'DIR', name => $name, as_text => $d->as_text,
    flip => $flip, flip_dies => dies(sub { $d->Flip }),
    potentially_extendible => b($d->PotentiallyExtendible),
    is_left_or_right => b($d->IsLeftOrRight),
    same_as_package_var => b($d eq ${"DIR::$name"}),
  );
}
for my $a (sort keys %dir) {
  for my $b (sort keys %dir) {
    record(pkg => 'DIR', op => 'eq', a => $a, b => $b,
           eq => b($dir{$a} eq $dir{$b}), smartmatch => b($dir{$a} ~~ $dir{$b}));
  }
}

# POS_MODE
for my $name (qw(FORWARD BACKWARD)) {
  my $m = POS_MODE->can($name)->();
  my $round = POS_MODE->deserialize($m->serialize);
  record(pkg => 'POS_MODE', name => $name, as_text => $m->as_text,
         serialize => $m->serialize, roundtrip_same => b($round eq $m),
         memory_dependencies => [$m->get_memory_dependencies]);
}
for my $str (qw(NONE xx forward)) {
  record(pkg => 'POS_MODE', op => 'deserialize', arg => $str,
         defined => b(defined POS_MODE->deserialize($str)));
}

# METO_MODE
for my $name (qw(NONE SINGLE ALLBUTONE ALL OTHER)) {
  my $m = METO_MODE->can($name)->();
  my $round = METO_MODE->deserialize($m->serialize);
  record(pkg => 'METO_MODE', name => $name, as_text => $m->as_text,
         serialize => $m->serialize, roundtrip_same => b($round eq $m),
         is_position_relevant => $m->is_position_relevant,
         is_metonymy_present => $m->is_metonymy_present,
         get_pure_same => b($m->get_pure eq $m),
         memory_dependencies => [$m->get_memory_dependencies]);
}
for my $str (qw(xx none)) {
  record(pkg => 'METO_MODE', op => 'deserialize', arg => $str,
         dies => dies(sub { METO_MODE->deserialize($str) }));
}

# EXTENDIBILE
for my $name (qw(NO PERHAPS UNKNOWN)) {
  my $e = EXTENDIBILE->can($name)->();
  record(pkg => 'EXTENDIBILE', name => $name, mode => $e->{mode}, bool => b($e));
}

# RELN_SCHEME
record(pkg => 'RELN_SCHEME', name => 'NONE', value => RELN_SCHEME::NONE(),
       bool => b(RELN_SCHEME::NONE()));
record(pkg => 'RELN_SCHEME', name => 'CHAIN', type => RELN_SCHEME::CHAIN()->{type},
       bool => b(RELN_SCHEME::CHAIN()),
       eq_none => b(RELN_SCHEME::CHAIN() == RELN_SCHEME::NONE()),
       eq_chain => b(RELN_SCHEME::CHAIN() == RELN_SCHEME::CHAIN()));

# DISTANCE_MODE
for my $name (qw(GROUP ELEMENT)) {
  my $m = DISTANCE_MODE->can($name)->();
  record(pkg => 'DISTANCE_MODE', name => $name, mode => $m->{mode},
         is_unit_groups => $m->IsUnitGroups);
}
srand(42);
record(pkg => 'DISTANCE_MODE', op => 'PickOne', seed => 42,
       picks => [map { DISTANCE_MODE::PickOne()->{mode} } 1 .. 40]);

# DISTANCE
my @distances = (
  [InElements => 3], [InElements => 0], [InElements => 1],
  [InGroups => 2], [InGroups => 0], [Zero => undef],
);
for my $d (@distances) {
  my ($ctor, $arg) = @$d;
  my $dist = DISTANCE->can($ctor)->(defined $arg ? ($arg) : ());
  record(pkg => 'DISTANCE', ctor => $ctor, arg => $arg,
         magnitude => $dist->GetMagnitude, is_non_zero => $dist->IsNonZero,
         is_unit_groups => $dist->IsUnitGroups, as_text => $dist->as_text);
}

emit();
