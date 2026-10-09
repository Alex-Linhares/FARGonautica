# Oracle for SChoose.pm and Set/Weighted.pm (weighted random choice).
# Output: tests/golden/schoose.json
# Every seeded case records the result of each call plus one rand() after the
# calls, so the tests also check how many draws were consumed.
use strict;
use warnings;
use Oracle;
use S;

# Describe a scalar-context result: undef -> null, else its string form.
sub v { defined $_[0] ? "$_[0]" : undef }

# Weighted objects for create(map => ...) and Set::Weighted keys.
package WObj;
sub new { my ($c, $n, $w) = @_; bless { n => $n, w => $w }, $c }
sub w { $_[0]{w} }
sub name { $_[0]{n} }
package main;

my @weight_sets = (
  [1],
  [0],
  [1, 1],
  [1, 2, 3, 4],
  [0, 0, 5],
  [5, 0, 0],
  [0, 3, 0, 1],
  [0, 0, 0],
  [0.1, 0.2, 0.7],
  [2.5, 0.25, 10, 1e-3],
  ['3', '1', '2abc'],
  [100, 1],
);

# --- choose / choose_if_non_zero --------------------------------------------
for my $fn (qw(choose choose_if_non_zero)) {
  for my $w (@weight_sets) {
    my @names = map { "n$_" } 0 .. $#$w;
    srand(42);
    my @res = map { v(SChoose->$fn($w, \@names)) } 1 .. 30;
    record(op => $fn, weights => $w, names => \@names, seed => 42,
           results => \@res, next_rand => rand());
    srand(42);
    my @res2 = map { v(SChoose->$fn($w)) } 1 .. 10;
    record(op => $fn, weights => $w, names => undef, seed => 42,
           results => \@res2, next_rand => rand());
  }
  srand(3);
  my $r = v(SChoose->$fn([], ['a']));
  record(op => $fn, weights => [], names => ['a'], seed => 3,
         results => [$r], next_rand => rand());
  # names shorter than weights
  srand(9);
  my @res = map { v(SChoose->$fn([1, 1, 1], ['a'])) } 1 .. 20;
  record(op => $fn, weights => [1, 1, 1], names => ['a'], seed => 9,
         results => \@res, next_rand => rand());
}

# --- choose_a_few_nonzero ---------------------------------------------------
my @few_cases = (
  [0, [1, 2, 3]],
  [1, [1, 2, 3]],
  [2, [1, 2, 3]],
  [3, [1, 2, 3]],
  [5, [1, 2, 3]],
  [2, [0, 0, 4, 0]],
  [3, [0, 0, 4, 0]],
  [2, [0, 0, 0]],
  [2, []],
  [3, [0.1, 0.2, 0.3, 0.4]],
  [4, [0.1, 0.7, 0.2]],
  [-1, [1, 2, 3]],
  [2, [5, 0, 1, 7, 2]],
);
for my $c (@few_cases) {
  my ($k, $w) = @$c;
  my @names = map { "n$_" } 0 .. $#$w;
  for my $seed (1, 42, 777) {
    srand($seed);
    my @runs = map { [map { v($_) } SChoose->choose_a_few_nonzero($k, $w, \@names)] } 1 .. 8;
    record(op => 'choose_a_few_nonzero', how_many => $k, weights => $w,
           names => \@names, seed => $seed, results => \@runs, next_rand => rand());
  }
  srand(5);
  my @plain = map { v($_) } SChoose->choose_a_few_nonzero($k, $w);
  record(op => 'choose_a_few_nonzero', how_many => $k, weights => $w,
         names => undef, seed => 5, results => [\@plain], next_rand => rand());
}

# --- uniform ----------------------------------------------------------------
for my $arr (['a'], ['a', 'b'], [qw(a b c d e f g)], []) {
  srand(11);
  my @res = map { v(SChoose->uniform($arr)) } 1 .. 25;
  record(op => 'uniform', items => $arr, seed => 11, results => \@res, next_rand => rand());
}

# --- create -----------------------------------------------------------------
# Objects are plain numbers (no map: the number is the likelihood) or WObj.
my %choosers = (
  plain     => [{}, 'num'],
  map       => [{ map => sub { $_[0]->w } }, 'obj'],
  map_str   => [{ map => q{$_->w * 2} }, 'obj'],
  grep      => [{ grep => sub { $_[0] > 1 } }, 'num'],
  grep_str  => [{ grep => q{$_ != 3} }, 'num'],
  map_grep  => [{ map => sub { $_[0]->w }, grep => sub { $_[0]->name =~ /^[ab]/ } }, 'obj'],
  map_grep_odd => [{ map => sub { $_[0]->w }, grep => sub { $_[0]->w != 2 } }, 'obj'],
);
my @num_lists = (
  [1], [0], [3], [1, 2, 3], [0, 0, 0], [0, 4, 0, 2], [2, 2], [1, 1, 1],
  [0.5, 3, 0.25], [3, 1, 3], [-1, -2], [5, -5], [2, -1, 3], [],
);
my @obj_lists = (
  [[a => 1]],
  [[a => 1], [b => 2], [c => 3]],
  [[a => 0], [b => 0], [c => 0]],
  [[c => 4], [a => 0], [b => 0]],
  [[a => 0], [c => 5], [b => 2], [d => 1]],
  [[x => 1], [y => 2]],
  [[a => 0.5], [b => 2], [z => 7]],
  [[a => -1], [b => -2]],
  [],
);
for my $name (sort keys %choosers) {
  my ($opts, $kind) = @{ $choosers{$name} };
  my $chooser = SChoose->create($opts);
  my @lists = $kind eq 'num' ? @num_lists : @obj_lists;
  for my $list (@lists) {
    my $objs = $kind eq 'num' ? $list : [map { WObj->new(@$_) } @$list];
    srand(42);
    my (@res, @ctx);
    for (1 .. 25) {
      my @r = $chooser->($objs);    # list context: () vs (x)
      push @ctx, scalar(@r);
      my $x = $r[0];
      push @res, ref $x ? $x->name : v($x);
    }
    record(op => 'create', chooser => $name, kind => $kind, items => $list, seed => 42,
           results => \@res, list_counts => \@ctx, next_rand => rand());
  }
}

# --- Set::Weighted ----------------------------------------------------------
{
  my $s = Set::Weighted->new();
  record(op => 'sw_empty', is_empty => $s->is_empty, is_not_empty => $s->is_not_empty,
         elements => [$s->get_elements]);
  srand(1);
  my $c = v($s->choose);
  record(op => 'sw_empty_choose', result => $c, few => [$s->choose_a_few_nonzero(2)],
         next_rand => rand());

  my @pairs = ([a => 1], [b => 0], [c => 2.5], [a => 2], [d => 0.5], [b => 3], [e => 0]);
  my $mk = sub { Set::Weighted->new(map { [@$_] } @pairs) };
  $s = $mk->();
  record(op => 'sw_basic', pairs => \@pairs, is_empty => $s->is_empty,
         is_not_empty => $s->is_not_empty,
         elements => [$s->get_elements], elements_1 => [$s->get_elements(1)],
         elements_2_5 => [$s->get_elements(2.5)], elements_neg => [$s->get_elements(-1)],
         elements_100 => [$s->get_elements(100)]);

  $s->insert([f => 4], [g => 1]);
  record(op => 'sw_insert', elements => [$s->get_elements], raw => [map { [@$_] } @$s]);

  $s = $mk->();
  $s->merge_keys;
  record(op => 'sw_merge_keys', pairs => \@pairs,
         merged => [sort { $a->[0] cmp $b->[0] } map { [@$_] } @$s]);

  for my $t (0, 1, 2.5, 3) {
    $s = $mk->();
    $s->delete_below_threshold($t);
    record(op => 'sw_delete_below_threshold', threshold => $t, raw => [map { [@$_] } @$s]);
  }

  for my $k (qw(a b e zz)) {
    $s = $mk->();
    $s->delete_key($k);
    record(op => 'sw_delete_key', key => $k,
           remaining => [sort { $a->[0] cmp $b->[0] } map { [@$_] } @$s]);
  }

  $s = $mk->();
  srand(42);
  my @ch = map { v($s->choose) } 1 .. 40;
  record(op => 'sw_choose', pairs => \@pairs, seed => 42, results => \@ch, next_rand => rand());
  srand(42);
  my @few = map { [$s->choose_a_few_nonzero($_)] } (1, 2, 3, 2, 10);
  record(op => 'sw_choose_a_few_nonzero', pairs => \@pairs, seed => 42,
         how_many => [1, 2, 3, 2, 10], results => \@few, next_rand => rand());

  # Object keys: merge_keys keeps the object; delete_key compares string forms.
  my ($o1, $o2) = (WObj->new(p => 1), WObj->new(q => 1));
  $s = Set::Weighted->new([$o1, 1], [$o2, 2], [$o1, 3]);
  $s->merge_keys;
  record(op => 'sw_object_keys',
         merged => [sort { $a->[0] cmp $b->[0] } map { [$_->[0]->name, $_->[1]] } @$s]);
  $s = Set::Weighted->new([$o1, 1], [$o2, 2], [$o1, 3]);
  $s->delete_key($o1);
  record(op => 'sw_object_delete_key', remaining => [map { [$_->[0]->name, $_->[1]] } @$s]);
}

emit();
