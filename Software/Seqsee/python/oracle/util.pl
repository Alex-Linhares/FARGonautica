# Oracle for SUtil.pm and Perl's rand/srand (drand48) and List::Util::shuffle.
# Output: tests/golden/util.json
use strict;
use warnings;
use Oracle;
use S;
use File::Path qw(make_path remove_tree);
use List::Util qw(shuffle);

sub b { $_[0] ? 1 : 0 }

# Small stand-ins for Seqsee objects.
package FlatObj;
sub new { my ($c, @f) = @_; bless [@f], $c }
sub flatten { @{ $_[0] } }
package TextObj;
sub new { my ($c, $t) = @_; bless { t => $t }, $c }
sub as_text { "T(" . $_[0]{t} . ")" }
package main;

# --- RNG: srand / rand ------------------------------------------------------
for my $seed (0, 1, 42, 12345, 4294967295, 4294967296 + 42) {
  my $ret = srand($seed);
  record(op => 'rand', seed => "$seed", srand_returns => "$ret",
         draws => [map { rand() } 1 .. 20]);
}
for my $seed ('-1', '3.7', '-42.9') {
  my $ret = srand($seed);
  record(op => 'rand', seed => $seed, srand_returns => "$ret",
         draws => [map { rand() } 1 .. 5]);
}
srand(42);
record(op => 'rand_n', seed => 42,
       args => [10, 0, -2, 0.5, 1000],
       draws => [rand(10), rand(0), rand(-2), rand(0.5), rand(1000)]);
srand(7);
record(op => 'int_rand', seed => 7, n => 13,
       draws => [map { int(rand(13)) } 1 .. 30]);
# srand resets the sequence
srand(99); my @a = map { rand() } 1 .. 3;
srand(99); my @b = map { rand() } 1 .. 3;
record(op => 'reseed_same', same => b("@a" eq "@b"));

# --- toss ------------------------------------------------------------------
for my $p (0, 0.25, 0.5, 0.9, 1) {
  srand(42);
  record(op => 'toss', seed => 42, prob => $p,
         draws => [map { SUtil::toss($p) } 1 .. 40]);
}
srand(5);
record(op => 'toss_mixed', seed => 5, probs => [0.1, 0.3, 0.5, 0.7, 0.9, 0.2, 0.8],
       draws => [map { SUtil::toss($_) } (0.1, 0.3, 0.5, 0.7, 0.9, 0.2, 0.8)]);
record(op => 'toss_undef', dies => dies(sub { SUtil::toss(undef) }));
# toss(1) never fails, toss(0) can only pass if rand() returns exactly 0
srand(1); my @neg = map { SUtil::toss(-0.5) } 1..10;
record(op => 'toss', seed => 1, prob => -0.5, draws => \@neg);

# --- shuffle (List::Util, uses the same drand48 state) ---------------------
for my $seed (1, 42) {
  srand($seed);
  record(op => 'shuffle', seed => $seed, input => [1 .. 10],
         output => [shuffle(1 .. 10)], output2 => [shuffle(qw(a b c d e))],
         after => rand());
}
srand(3);
record(op => 'shuffle', seed => 3, input => [], output => [shuffle()],
       output2 => [shuffle('x')], after => rand());

# --- uniq (hash order: record sorted) ---------------------------------------
for my $in ([1, 2, 2, 3, 1], ['a', 'b', 'a'], [1, '1', 1.0, '1.0'], [], [0.1, 0.1, 3, 1/3, 1/3]) {
  record(op => 'uniq', input => $in, sorted => [sort { "$a" cmp "$b" } SUtil::uniq(@$in)],
         count => scalar(my @x = SUtil::uniq(@$in)));
}

# --- compare_deep -------------------------------------------------------------
for my $c ([1, 1], [1, 2], [1, '1.0'], [[1, 2], [1, 2]], [[1, [2, 3]], [1, [2, 3]]],
           [[1, [2, 3]], [1, [2, 4]]], [[1, 2], [1, 2, 3]], [1, [1]], [[1], 1],
           [[], []]) {
  record(op => 'compare_deep', a => $c->[0], b => $c->[1],
         result => b(SUtil::compare_deep($c->[0], $c->[1])));
}
record(op => 'compare_deep_dies', one_arg => dies(sub { SUtil::compare_deep(1) }),
       hashref => dies(sub { SUtil::compare_deep({}, {}) }));

# --- equal_when_flattened ---------------------------------------------------
for my $c ([3, 3, 1], [3, 4, 1], [[1, 2], [1, 2], 0], [[1, 2], [1, 3], 0],
           [[1, 2], [1, 2, 3], 0], [3, [3], 0]) {
  my ($x, $y) = @$c[0, 1];
  $x = FlatObj->new(@$x) if ref $x;
  $y = FlatObj->new(@$y) if ref $y;
  record(op => 'equal_when_flattened', a => $c->[0], b => $c->[1],
         result => b(SUtil::equal_when_flattened($x, $y)));
}
record(op => 'equal_when_flattened', a => [5], b => 5,
       result => b(SUtil::equal_when_flattened(FlatObj->new(5), 5)));

# --- myall / all / significant / minmax -------------------------------------
for my $in ([1, 1, 1], [1, 0, 1], [], ['a', ''], ['0.0'], ['0'], [undef]) {
  record(op => 'myall', input => $in, result => b(SUtil::myall(@$in)));
  record(op => 'all_gt1', input => $in,
         result => b(SUtil::all(sub { $_[0] > 0 }, @$in)));
}
for my $x (0, 0.5, 0.7, 0.70001, 1, -3) {
  record(op => 'significant', x => $x, result => SUtil::significant($x));
}
for my $in ([5], [3, 1, 2], [2, 2], [-1, 10, 4, -7, 3], [1.5, 1.25]) {
  record(op => 'minmax', input => $in, result => [SUtil::minmax(@$in)]);
}
record(op => 'minmax_empty', dies => dies(sub { SUtil::minmax() }));

# --- odd_position -------------------------------------------------------------
for my $in ([1, 1, 0], [0, 1, 1], [1, 0, 1], [1, 1, 1], [1, 1, 1, 0, 1],
            [1, 1, 0, 0], [0, 1, 0, 1], ['a', 'a', 'b', 'a'], [1, 0, 2],
            [1, 1, 0, 1, 2]) {
  record(op => 'odd_position', input => $in, result => SUtil::odd_position(@$in));
}
record(op => 'odd_position_short', dies => dies(sub { SUtil::odd_position(1, 2) }));

# --- naive_brittle_chunking -------------------------------------------------
for my $in ([], [1], [1, 1], [1, 2], [1, 1, 2, 3, 3, 3, 4], [5, 5, 5], [1, 2, 2],
            [2, 2, 1], [1, 2, 1, 1]) {
  record(op => 'naive_brittle_chunking', input => $in,
         result => [SUtil::naive_brittle_chunking($in)]);
}

# --- next_available_file_number (relative dirs under a scratch cwd) ----------
{
  my $scratch = "/tmp/seqsee_oracle_util_scratch";
  remove_tree($scratch); make_path($scratch);
  chdir $scratch or die;
  my %layout = (
    empty => [],
    nodigits => ['abc', 'def.txt'],
    some => ['run7.log', 'run12', 'x', 'file003', 'zero0'],
    dir9 => ['a1', 'b'],
    dotted => ['.hidden99', 'p4'],
  );
  for my $d (sort keys %layout) {
    make_path($d);
    for my $f (@{ $layout{$d} }) { open my $fh, '>', "$d/$f" or die; close $fh }
    record(op => 'next_available_file_number', dir => $d, files => $layout{$d},
           result => SUtil::next_available_file_number($d));
  }
  record(op => 'next_available_file_number', dir => 'missing', files => [],
         result => SUtil::next_available_file_number('missing'));
  chdir '/'; remove_tree($scratch);
}

# --- StructureToString / StringifyDeepArray ---------------------------------
for my $s (5, 'a', [1, 2, 3], [1, [2, [3, 4]], 5], [], [[]], 0.1, 1e20, 1.0, -0.5,
           [0.1, 1e20, 1.0, 'x', -0.5, 1/3, 1e15, 2**53, 123456789012345678]) {
  record(op => 'structure_to_string', input => $s,
         result => SUtil::StructureToString($s),
         deep => SUtil::StringifyDeepArray($s));
}

# --- trim ---------------------------------------------------------------------
for my $s ('  a  ', "a   b   c", "\t x \n", '', 'abc', "  a  b  ") {
  my $t = $s; SUtil::trim($t);
  record(op => 'trim', input => $s, result => $t);
}

# --- StringifyForCarp -------------------------------------------------------
record(op => 'stringify_for_carp', what => 'scalar', result => SUtil::StringifyForCarp(17));
record(op => 'stringify_for_carp', what => 'string', result => SUtil::StringifyForCarp('hi'));
record(op => 'stringify_for_carp', what => 'undef', result => SUtil::StringifyForCarp(undef));
record(op => 'stringify_for_carp', what => 'as_text',
       result => SUtil::StringifyForCarp(TextObj->new('x')));
record(op => 'stringify_for_carp', what => 'array',
       result => SUtil::StringifyForCarp([1, TextObj->new('y'), 'z']));
record(op => 'stringify_for_carp', what => 'empty_array', result => SUtil::StringifyForCarp([]));
record(op => 'stringify_for_carp', what => 'hash_scalar',
       result => SUtil::StringifyForCarp({ k => 3 }));
record(op => 'stringify_for_carp', what => 'hash_undef',
       result => SUtil::StringifyForCarp({ k => undef }));
record(op => 'stringify_for_carp', what => 'hash_as_text',
       result => SUtil::StringifyForCarp({ k => TextObj->new('v') }));
record(op => 'stringify_for_carp', what => 'hash_array',
       result => SUtil::StringifyForCarp({ k => [1, TextObj->new('w')] }));
record(op => 'stringify_for_carp', what => 'empty_hash', result => SUtil::StringifyForCarp({}));
record(op => 'stringify_for_carp', what => 'code', result => SUtil::StringifyForCarp(sub { 1 }));
record(op => 'stringify_for_carp', what => 'scalar_ref', result => SUtil::StringifyForCarp(\1));
record(op => 'stringify_for_carp', what => 'dir', result => SUtil::StringifyForCarp($DIR::RIGHT));

# --- hash_sorted_as_array -----------------------------------------------------
for my $h ({}, { b => 2, a => 1, c => 3 }, { 10 => 'x', 9 => 'y', 100 => 'z' }) {
  record(op => 'hash_sorted_as_array', input => $h,
         result => [SUtil::hash_sorted_as_array(%$h)]);
}

# SUtil.pm contains raw latin-1 bytes \xab \xbb (« »); write them out as UTF-8.
binmode(STDOUT, ':encoding(UTF-8)');
emit();
