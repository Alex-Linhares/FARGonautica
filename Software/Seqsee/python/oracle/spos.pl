# Oracle for SPos.pm. Output: tests/golden/spos.json
use strict;
use Oracle;
use SErr;
use SPos;

# Minimal stand-in for a Seqsee object: find_range only calls these two methods.
package FakeObj;
sub new { my ( $c, $n, $s ) = @_; bless { n => $n, s => $s }, $c }
sub get_parts_count      { $_[0]{n} }
sub get_structure_string { $_[0]{s} }

package main;

# Positional (->new(3)) and named (->new(position => 3)) constructors.
for my $pos (2, 3, 1, -1, 100, '7', '-1') {
  my $p = SPos->new($pos);
  record(op => 'new', arg => "$pos", position => $p->position);
  my $q = SPos->new(position => $pos);
  record(op => 'new_named', arg => "$pos", position => $q->position);
}
# Rejected: BUILD (<= 0 and not -1) and the Moose Int constraint.
for my $pos (0, -3, -2, 2.5, '3.0', ' 3', 'abc', '') {
  record(op => 'new_dies', arg => "$pos", dies => dies(sub { SPos->new($pos) }));
  record(op => 'new_named_dies', arg => "$pos",
         dies => dies(sub { SPos->new(position => $pos) }));
}
record(op => 'new_dies', arg => undef, dies => dies(sub { SPos->new(position => undef) }));
record(op => 'new_no_args_dies', dies => dies(sub { SPos->new() }));

# eq / ne / ~~ overloads compare positions numerically.
for my $pair ([2, 2], [2, 3], [3, 2], [-1, -1], [-1, 1], [1, 1]) {
  my ($a, $b) = @$pair;
  my ($pa, $pb) = (SPos->new($a), SPos->new($b));
  record(op => 'eq', a => $a, b => $b,
         eq => ($pa eq $pb) ? 1 : 0,
         ne => ($pa ne $pb) ? 1 : 0,
         smartmatch => ($pa ~~ $pb) ? 1 : 0);
}
{
  my $p = SPos->new(2);
  record(op => 'eq_same_object', eq => ($p eq $p) ? 1 : 0, ne => ($p ne $p) ? 1 : 0,
         smartmatch => ($p ~~ $p) ? 1 : 0);
  record(op => 'eq_non_spos_dies', dies => dies(sub { my $x = ($p eq "2") }));
}

# The rw accessor checks Int but BUILD is not re-run, so 0 is accepted.
{
  my $p = SPos->new(2);
  $p->position(5);
  record(op => 'set', value => 5, position => $p->position);
  $p->position(0);
  record(op => 'set', value => 0, position => $p->position);
  $p->position(-7);
  record(op => 'set', value => -7, position => $p->position);
  record(op => 'set_dies', value => '2.5', dies => dies(sub { $p->position(2.5) }));
}

# find_range: returns [index] (0-based) or throws SErr with a message.
for my $pos (1, 2, 3, 4, -1) {
  for my $size (0, 1, 3) {
    my $p = SPos->new($pos);
    my $obj = FakeObj->new($size, "[1, 2, 3]");
    my $r = eval { $p->find_range($obj) };
    my $e = $@;
    record(op => 'find_range', position => $pos, size => $size,
           result => $r,
           error_class => (ref $e) || undef,
           error => ref($e) ? $e->message : undef);
  }
}
# find_range with a position set to 0 / -7 through the accessor.
for my $pos (0, -7) {
  my $p = SPos->new(1);
  $p->position($pos);
  my $obj = FakeObj->new(3, "[4, 5, 6]");
  my $r = eval { $p->find_range($obj) };
  my $e = $@;
  record(op => 'find_range', position => $pos, size => 3,
         result => $r,
         error_class => (ref $e) || undef,
         error => ref($e) ? $e->message : undef);
}
emit();
