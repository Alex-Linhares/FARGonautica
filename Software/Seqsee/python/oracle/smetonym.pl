# Oracle for SMetonymType.pm and SMetonym.pm.
# Output: tests/golden/smetonym.json
#
# Categories are FakeCats (an unfinder table that records its calls, and get_pure);
# objects are FakeObjs (describe_as is recorded). SLTM::encode/decode are replaced
# by recorders. Opts are written as specs so the Python test can rebuild them:
#   category: 'A'/'B' (FakeCats) or 's:<string>';
#   name: 's:<string>', or 'u' (undef);
#   values: 's:<string>', 'n:<number>', 'i:<SInt mag>', 'o:<FakeObj name>', 'u' (undef).
use strict;
use Oracle;
use S;

package FakeObj;
our @LOG;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub describe_as { push @LOG, "$_[0]{n}.describe_as(" . $_[1]{n} . ")"; 1 }
sub get_pure { 'pure:' . $_[0]{n} }

package FakeCat;
our @LOG;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub get_pure { 'pure:' . $_[0]{n} }
sub get_meto_unfinder {
  my ($s, $name) = @_;
  return undef unless $name eq 'each';
  return sub {
    my ($cat, $nm, $info_loss, $obj) = @_;
    push @LOG, join(',', $cat->{n}, $nm, join('/', map { "$_=" . main::desc($info_loss->{$_}) } sort keys %$info_loss),
                    $obj->{n});
    return FakeObj->new('B' . $obj->{n});
  };
}

package main;

our @ENCODED;
our %DECODE;
{
  no warnings 'redefine';
  *SLTM::encode = sub { push @ENCODED, [ map { desc($_) } @_ ]; return 'ENC' };
  *SLTM::decode = sub { my ($s) = @_; return @{ $DECODE{$s} } };
}

sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; $e =~ s/\n.*//s; return $e }
sub desc {
  my ($v) = @_;
  return 'undef' unless defined $v;
  my $r = ref($v);
  return "$v" if !$r;
  return 'SInt(' . $v->[0] . ')' if $r eq 'SInt';
  return 'obj:' . $v->{n} if $r eq 'FakeObj';
  return 'cat:' . $v->{n} if $r eq 'FakeCat';
  return '{' . join(',', map { "$_=" . desc($v->{$_}) } sort keys %$v) . '}' if $r eq 'HASH';
  return "OBJ:$r";
}

my %CAT = (A => FakeCat->new('A'), B => FakeCat->new('B'));
my %OBJ = (x => FakeObj->new('x'), y => FakeObj->new('y'));
sub val {
  my ($spec) = @_;
  return undef if $spec eq 'u';
  return "$1" if $spec =~ /^s:(.*)$/s;
  return 0 + $1 if $spec =~ /^n:(.*)$/;
  return SInt->new($1) if $spec =~ /^i:(.*)$/;
  return $OBJ{$1} if $spec =~ /^o:(.*)$/;
  die "bad spec $spec";
}
sub cat { my ($spec) = @_; $spec =~ /^s:(.*)$/ ? "$1" : $CAT{$spec} }
# opts(cat, name, [k, v, k, v ...] or 'u'/'s:..'/'array')
sub opts {
  my ($c, $n, $il) = @_;
  my $info = ref($il) ? {} : $il eq 'array' ? [] : val($il);
  if (ref $il) { my @kv = @$il; while (@kv) { my ($k, $v) = splice(@kv, 0, 2); $info->{$k} = val($v) } }
  return { category => cat($c), name => val($n), info_loss => $info };
}

# --- constructor (new) ----------------------------------------------------------------
{
  my $t = SMetonymType->new(opts('A', 's:each', ['length', 'n:2']));
  my ($c, $n) = $t->GetCatAndName;
  record(kind => 'new', category => desc($t->get_category), name => $t->get_name,
         info_loss => desc($t->get_info_loss), cat_and_name => [desc($c), $n],
         as_text => $t->as_text, get_pure_is_self => ($t->get_pure == $t ? 1 : 0),
         new_not_memoized => (SMetonymType->new(opts('A', 's:each', ['length', 'n:2'])) != $t ? 1 : 0));

  my @cases = (
    [ 'A', 's:each', [] ], [ 'A', 's:each', 'n:0' ], [ 'A', 's:each', 's:' ], [ 'A', 's:each', 'u' ],
    [ 'A', 's:each', 's:abc' ], [ 'A', 's:each', 'array' ],
    [ 'A', 's:0', [] ], [ 'A', 's:', [] ], [ 'A', 'u', [] ], [ 'A', 's:0.0', [] ],
    [ 's:0', 's:each', [] ], [ 's:', 's:each', [] ], [ 's:cat', 's:each', [] ],
  );
  for my $cs (@cases) {
    my $t;
    my $ok = eval { no warnings; $t = SMetonymType->new(opts(@$cs)); 1 };
    record(kind => 'new_check', spec => [@$cs], dies => ($ok ? 0 : 1), error => ($ok ? undef : err_text($@)),
           category => ($ok ? desc($t->get_category) : undef),
           info_loss => ($ok ? desc($t->get_info_loss) : undef));
  }
}

# --- create (memoized by a joined key) ------------------------------------------------
{
  # Pairs of specs: is the second create the same object as the first?
  my @pairs = (
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 'n:2'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 'n:3'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'B', 's:each', ['length', 'n:2'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:other', ['length', 'n:2'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 'i:2'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 's:2'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 'n:2.0'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 's:2.0'] ] ],
    [ [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['len', 'n:2'] ] ],
    [ [ 'A', 's:each', ['length', 'i:5'] ], [ 'A', 's:each', ['length', 'i:5'] ] ],
    [ [ 'A', 's:each', ['length', 'o:x'] ], [ 'A', 's:each', ['length', 'o:x'] ] ],
    [ [ 'A', 's:each', ['length', 'o:x'] ], [ 'A', 's:each', ['length', 'o:y'] ] ],
    [ [ 'A', 's:each', ['length', 'u'] ], [ 'A', 's:each', ['length', 's:'] ] ],
    [ [ 'A', 's:each', ['a', 'n:1', 'b', 'n:2'] ], [ 'A', 's:each', ['b', 'n:2', 'a', 'n:1'] ] ],
    [ [ 'A', 's:each', [] ], [ 'A', 's:each', [] ] ],
    # The key is a plain join on ';', so these collide.
    [ [ 'A', 's:a', ['b', 's:c'] ], [ 'A', 's:a;b;c', [] ] ],
    [ [ 'A', 's:a', ['b', 's:c;d'] ], [ 'A', 's:a', ['b;c', 's:d'] ] ],
    [ [ 's:cat', 's:each', [] ], [ 's:cat', 's:each', [] ] ],
    [ [ 'A', 's:each', ['length', 'i:7'] ], [ 'A', 's:each', ['length', 'n:7'] ] ],
  );
  for my $p (@pairs) {
    my $o1 = opts(@{ $p->[0] });
    my $t1 = SMetonymType->create($o1);
    my $t2 = SMetonymType->create(opts(@{ $p->[1] }));
    record(kind => 'create_pair', first => $p->[0], second => $p->[1], same => ($t1 == $t2 ? 1 : 0),
           info_loss_is_first_hash => ($t2->get_info_loss == $o1->{info_loss} ? 1 : 0),
           info_loss => desc($t2->get_info_loss));
  }

  my @errs = (
    [ 'A', 's:each', 'u' ], [ 'A', 's:each', 'array' ], [ 'A', 's:each', 's:abc' ],
    [ 'A', 'u', ['zz', 'n:1'] ], [ 's:', 's:each', ['zz', 'n:2'] ], [ 'A', 's:0', ['zz', 'n:3'] ],
  );
  for my $e (@errs) {
    my $ok = eval { no warnings; no strict 'refs'; SMetonymType->create(opts(@$e)); 1 };
    # A failed new must not poison the memo: a valid create with the same key still dies.
    my $again = eval { no warnings; SMetonymType->create(opts(@$e)); 1 };
    record(kind => 'create_error', spec => [@$e], dies => ($ok ? 0 : 1),
           error => ($ok ? undef : err_text($@)), dies_again => ($again ? 0 : 1));
  }

  # S.pm's $DOUBLE comes from new, so create gives a different object.
  my $d = $S::DOUBLE;
  my $c = SMetonymType->create({ category => $S::SAMENESS, name => 'each', info_loss => { length => 2 } });
  record(kind => 'double', category => $d->get_category->get_name, name => $d->get_name,
         info_loss => desc($d->get_info_loss), category_is_sameness => ($d->get_category == $S::SAMENESS ? 1 : 0),
         create_is_double => ($c == $d ? 1 : 0));
}

# --- blemish --------------------------------------------------------------------------
{
  for my $spec ([ 'A', 's:each', ['length', 'n:2'] ], [ 'B', 's:each', ['length', 'i:3', 'x', 'o:y'] ],
                [ 'A', 's:each', [] ]) {
    my $t = SMetonymType->create(opts(@$spec));
    @FakeCat::LOG = (); @FakeObj::LOG = ();
    my $r = $t->blemish($OBJ{x});
    record(kind => 'blemish', spec => $spec, result => desc($r),
           unfinder_calls => [@FakeCat::LOG], describe_calls => [@FakeObj::LOG]);
  }
  my $t = SMetonymType->create(opts('A', 's:double', ['length', 'n:2']));
  @FakeObj::LOG = ();
  my $ok = eval { $t->blemish($OBJ{x}); 1 };
  record(kind => 'blemish_no_unfinder', dies => ($ok ? 0 : 1), describe_calls => [@FakeObj::LOG]);
}

# --- memory dependencies, serialize, deserialize --------------------------------------
{
  my @specs = (
    [ 'A', 's:each', ['length', 'n:2'] ], [ 'A', 's:each', ['length', 'i:2'] ],
    [ 'A', 's:each', ['length', 'o:x'] ], [ 's:cat', 's:each', ['length', 'o:y'] ],
    [ 's:cat', 's:each', ['length', 's:2'] ], [ 'B', 's:each', [] ],
    [ 'A', 's:each', ['a', 'o:x', 'b', 'o:y', 'c', 'i:1', 'd', 'u'] ],
  );
  for my $spec (@specs) {
    my $t = SMetonymType->create(opts(@$spec));
    my @deps = $t->get_memory_dependencies;
    @ENCODED = ();
    my $s = $t->serialize;
    record(kind => 'deps', spec => $spec, deps_sorted => [ sort map { desc($_) } @deps ],
           deps_count => scalar(@deps), serialized => $s, encoded => [@ENCODED]);
  }
  my $o = opts('A', 's:each', ['length', 'n:4']);
  my $t = SMetonymType->create($o);
  $DECODE{X} = [ $CAT{A}, 'each', { length => 4 } ];
  $DECODE{Y} = [ $CAT{B}, 'each', { length => 4 } ];
  my $dx = SMetonymType->deserialize('X');
  my $dy = SMetonymType->deserialize('Y');
  my $dy2 = SMetonymType->deserialize('Y');
  record(kind => 'deserialize', x_is_existing => ($dx == $t ? 1 : 0), y_is_new => ($dy != $t ? 1 : 0),
         y_memo => ($dy == $dy2 ? 1 : 0), y_category => desc($dy->get_category), y_name => $dy->get_name,
         y_info_loss => desc($dy->get_info_loss));
  $DECODE{Z} = [ $CAT{A}, undef, { length => 4 } ];
  my $ok = eval { no warnings; SMetonymType->deserialize('Z'); 1 };
  record(kind => 'deserialize_error', dies => ($ok ? 0 : 1), error => ($ok ? undef : err_text($@)));
}

# --- SMetonym -------------------------------------------------------------------------
{
  my $starred = FakeObj->new('s');
  my $unstarred = FakeObj->new('u');
  my $o = opts('A', 's:each', ['length', 'n:6']);
  my $m = SMetonym->new({ %$o, starred => $starred, unstarred => $unstarred });
  my $t = SMetonymType->create(opts('A', 's:each', ['length', 'n:6']));
  my $m2 = SMetonym->new({ %{ opts('A', 's:each', ['length', 'n:6']) }, starred => $starred, unstarred => $unstarred });
  my $m3 = SMetonym->new({ %{ opts('A', 's:each', ['length', 'n:7']) }, starred => $starred, unstarred => $unstarred });
  record(kind => 'metonym', type_is_create => ($m->get_type == $t ? 1 : 0),
         category => desc($m->get_category), name => $m->get_name, info_loss => desc($m->get_info_loss),
         info_loss_is_opts_hash => ($m->get_info_loss == $o->{info_loss} ? 1 : 0),
         starred => desc($m->get_starred), unstarred => desc($m->get_unstarred),
         same_type => ($m2->get_type == $m->get_type ? 1 : 0),
         intersection_same => (SMetonym->intersection($m, $m2) == $t ? 1 : 0),
         intersection_one => (SMetonym->intersection($m3) == $m3->get_type ? 1 : 0),
         intersection_differ => (defined(SMetonym->intersection($m, $m2, $m3)) ? 1 : 0),
         intersection_empty_dies => dies(sub { SMetonym->intersection() }));

  # unstarred is weakened: once the last strong ref goes, get_unstarred is undef.
  my $weak;
  {
    my $u = FakeObj->new('tmp');
    $weak = SMetonym->new({ %$o, starred => $starred, unstarred => $u });
    record(kind => 'weak_before', unstarred => desc($weak->get_unstarred));
  }
  # starred is a strong ref.
  my $strong;
  { my $s = FakeObj->new('tmp2'); $strong = SMetonym->new({ %$o, starred => $s, unstarred => $unstarred }); }
  record(kind => 'weak_after', unstarred => desc($weak->get_unstarred), starred => desc($strong->get_starred));

  my %errs = (
    no_starred => { %$o, unstarred => $unstarred },
    no_unstarred => { %$o, starred => $starred },
    zero_starred => { %$o, starred => 0, unstarred => $unstarred },
    empty_unstarred => { %$o, starred => $starred, unstarred => '' },
    string_starred => { %$o, starred => 'abc', unstarred => $unstarred },
    string_unstarred => { %$o, starred => $starred, unstarred => 'abc' },
    no_category => { %{ opts('s:', 's:each', ['length', 'n:6']) }, starred => $starred, unstarred => $unstarred },
    no_name => { %{ opts('A', 'u', ['length', 'n:6']) }, unstarred => $unstarred },
    no_info_loss => { %{ opts('A', 's:each', 'u') }, unstarred => $unstarred },
  );
  for my $k (sort keys %errs) {
    my $m;
    my $ok = eval { no warnings; $m = SMetonym->new($errs{$k}); 1 };
    record(kind => 'metonym_error', label => $k, dies => ($ok ? 0 : 1), error => ($ok ? undef : err_text($@)),
           starred => ($ok ? desc($m->get_starred) : undef), unstarred => ($ok ? desc($m->get_unstarred) : undef));
  }
}

emit();
