# Oracle for Mapping.pm, Mapping/Dir.pm and Mapping/Position.pm, plus the
# Class::Multimethods dispatch they rely on. Output: tests/golden/mapping_base.json
#
# Replaced: SLTM::SpikeAndChoose (records its args and picks via $PICK),
# Mapping::Numeric->create (plain new: the real memo collides across categories),
# Seqsee::Element->create (records its args, returns a FakeElem) and main::message
# (recorder). Objects are fakes whose packages inherit from the real ones, so the
# dispatch walks real @ISA chains.
use strict;
use Oracle;
use S;
use Class::Multimethods;
multimethod 'FindMapping';
multimethod 'ApplyMapping';
multimethod 'Probe';

# --- the dispatch engine on its own (multimethod Probe) --------------------------------
package PA; sub new { bless {}, $_[0] }
package PB; our @ISA = ('PA');
package PC; our @ISA = ('PB');
package PX; sub new { bless {}, $_[0] }
package PD; our @ISA = ('PA', 'PX');
package PY; sub new { bless {}, $_[0] }

package main;
for my $sig ([qw(PA PA)], [qw(PB PA)], [qw(PA PB)], ['#', '#'], ['$', '$'],
             ['ARRAY', '*'], ['*', 'PX'], ['*', 'PY'], ['PY', '*'], ['HASH', 'HASH']) {
  my $label = join ',', @$sig;
  multimethod Probe => @$sig => sub { $label };
}

sub first_line {
  my ($e) = @_;
  my ($line) = split /\n/, $e;
  $line =~ s/ at \S+ line \d+\.?$//;
  return $line;
}

my %MAKE = (PA => sub { PA->new }, PB => sub { PB->new }, PC => sub { PC->new },
            PD => sub { PD->new }, PX => sub { PX->new }, PY => sub { PY->new },
            ARRAY => sub { [] }, HASH => sub { {} }, num => sub { 3 }, float => sub { 2.5 },
            neg => sub { -1 }, str => sub { "abc" }, numstr => sub { "3" },
            empty => sub { "" }, undef => sub { undef });
for my $args ([qw(PA PA)], [qw(PB PA)], [qw(PA PB)], [qw(PB PB)], [qw(PC PA)],
              [qw(PC PC)], [qw(PD PA)], [qw(PD PX)], [qw(PD PD)], [qw(PY PY)],
              [qw(PA PY)], [qw(PY PA)], [qw(num num)], [qw(float neg)], [qw(undef undef)],
              [qw(num str)], [qw(str num)], [qw(numstr numstr)], [qw(empty num)],
              [qw(ARRAY num)], [qw(ARRAY PA)], [qw(HASH HASH)], [qw(HASH PA)],
              [qw(PA str)], [qw(num num num)], [qw(PA)]) {
  my @vals = map { $MAKE{$_}->() } @$args;
  my $r = eval { Probe(@vals) };
  record(kind => 'probe', args => $args, result => $r,
         error => ($@ ? first_line($@) : undef));
}

# --- fakes -------------------------------------------------------------------------------
package FakeCat;
our @LOG;
sub new { my ($c, $name, %a) = @_; bless { name => $name, %a }, $c }
sub as_text { $_[0]{name} }
sub IsNumeric { 0 }
sub FindMappingForCat {
  my ($s, $a, $b) = @_;
  push @LOG, "find:" . ref($a) . "," . ref($b);
  return "found-by-$s->{name}";
}
sub ApplyMappingForCat {
  my ($s, $t, $o) = @_;
  push @LOG, "apply:" . ref($t) . "," . ref($o);
  return $s->{apply};
}
sub AreAttributesSufficientToBuild {
  my ($s, @atts) = @_;
  push @LOG, "suff:" . join(',', sort @atts);
  return $s->{sufficient};
}

package FakeElem;
our @ISA = ('Seqsee::Element');
sub new { my ($c, $mag, @cats) = @_; bless { fmag => $mag, fcats => [@cats] }, $c }
sub get_mag { $_[0]{fmag} }
sub get_common_categories {
  my ($s, $o) = @_;
  my %theirs = map { ("$_" => 1) } @{ $o->{fcats} };
  return grep { $theirs{"$_"} } @{ $s->{fcats} };
}

package FakeAnch;
our @ISA = ('Seqsee::Anchored');
sub new { my ($c, @cats) = @_; bless { fcats => [@cats] }, $c }
*get_common_categories = \&FakeElem::get_common_categories;

package FakeObj;
our @ISA = ('Seqsee::Object');
sub new { bless {}, $_[0] }

package FakeStruct;
our @ISA = ('Mapping::Structural');
sub new { my ($c, %a) = @_; bless {%a}, $c }
sub get_category { $_[0]{cat} }
sub get_changed_bindings { $_[0]{changed} }

package main;
our @SPIKE;
our $PICK = sub { $_[0] };
{
  no warnings 'redefine';
  *SLTM::SpikeAndChoose = sub {
    my ($amount, @concepts) = @_;
    push @SPIKE, [$amount, map { $_->as_text } @concepts];
    return $PICK->(@concepts);
  };
  # Mapping::Numeric->create memoizes on SLTM::encode(name, category), which is the same
  # string for every category not yet in the LTM ("succ" for even, odd, ...), so the
  # first category wins (item 017's concern). Use plain `new` here.
  *Mapping::Numeric::create = sub {
    my ($package, $name, $category) = @_;
    $package->new({ name => $name, category => $category });
  };
  *Seqsee::Element::create = sub {
    my ($package, $mag, $pos) = @_;
    my $e = FakeElem->new($mag);
    $e->{created} = [$package, $mag, $pos];
    return $e;
  };
}
our @MESSAGES;
{ no warnings 'redefine'; *main::message = sub { push @MESSAGES, $_[0] }; }

sub describe {
  my ($x) = @_;
  return undef unless defined $x;
  return "$x" unless ref $x;
  return 'Mapping::Numeric:' . $x->get_name . '/' . $x->get_category->as_text
    if ref($x) eq 'Mapping::Numeric';
  return 'Mapping::Dir:' . $$x if ref($x) eq 'Mapping::Dir';
  return 'Mapping::Position:' . $x->get_text if ref($x) eq 'Mapping::Position';
  return 'SInt:' . $x->get_mag if ref($x) eq 'SInt';
  return 'SPos:' . $x->position if ref($x) eq 'SPos';
  return 'DIR:' . $x->as_text if ref($x) eq 'DIR';
  return 'FakeElem:' . (defined $x->{fmag} ? $x->{fmag} : 'undef') if ref($x) eq 'FakeElem';
  return ref($x);
}

sub run {
  my ($code) = @_;
  @SPIKE = (); @FakeCat::LOG = (); @MESSAGES = ();
  my @r = eval { $code->() };
  my $err = $@;
  return (
    count  => scalar(@r),
    result => describe($r[0]),
    error  => ($err ? first_line($err) : undef),
    spike  => [@SPIKE],
    log    => [@FakeCat::LOG],
    messages => [@MESSAGES],
  );
}

# --- FindMapping ---------------------------------------------------------------------------
# On numbers (#,#) → $S::NUMBER; strings ($) have no variant.
for my $pair ([3, 4], [4, 3], [3, 3], [3, 7], [undef, 1], [undef, undef], [1.5, 2.5],
              [0, -1], ["3", "4"], ["a", "b"], [3, "4"], ["4", 3], ["", 1]) {
  my ($a, $b) = @$pair;
  # JSON::PP writes numbers unquoted and strings quoted, so the golden keeps the
  # number/string distinction the dispatch depends on.
  record(kind => 'find_num', a => $a, b => $b, run(sub { FindMapping($a, $b) }));
}

# Three args → $cat->FindMappingForCat($a, $b).
{
  my $fc = FakeCat->new('fc');
  record(kind => 'find_cat', label => 'number', run(sub { FindMapping(3, 4, $S::NUMBER) }));
  record(kind => 'find_cat', label => 'number_strings', run(sub { FindMapping("5", "6", $S::NUMBER) }));
  record(kind => 'find_cat', label => 'fake_sints', run(sub { FindMapping(SInt->new(1), SInt->new(2), $fc) }));
  record(kind => 'find_cat', label => 'fake_undef', run(sub { FindMapping(undef, undef, $fc) }));
  record(kind => 'find_cat', label => 'undef_cat', run(sub { FindMapping(1, 2, undef) }));
}

# SInt,SInt and Seqsee::Element,Seqsee::Element: common categories → SpikeAndChoose(0).
{
  my $fc = FakeCat->new('fc');
  my $s = sub { my $x = SInt->new($_[0]); $x->add_category($_) for @_[1..$#_]; $x };
  record(kind => 'find_sint', label => 'succ', run(sub { FindMapping(SInt->new(3), SInt->new(4)) }));
  record(kind => 'find_sint', label => 'none', run(sub { FindMapping(SInt->new(3), SInt->new(9)) }));
  record(kind => 'find_sint', label => 'pick_undef',
         run(sub { local $PICK = sub { undef }; FindMapping(SInt->new(5), SInt->new(4)) }));
  record(kind => 'find_sint', label => 'fake_cat',
         run(sub { local $PICK = sub { (grep { ref($_) eq 'FakeCat' } @_)[0] };
                   FindMapping($s->(5, $fc), $s->(4, $fc)) }));
  record(kind => 'find_sint', label => 'no_common',
         run(sub { my $a = SInt->new(1); $a->[1] = []; FindMapping($a, SInt->new(2)) }));
  record(kind => 'find_sint', label => 'undef_cat',
         run(sub { my ($a, $b) = (SInt->new(1), SInt->new(2));
                   push @{ $a->[1] }, undef; push @{ $b->[1] }, undef; FindMapping($a, $b) }));

  my $e = sub { FakeElem->new(@_) };
  record(kind => 'find_elem', label => 'succ',
         run(sub { FindMapping($e->(3, $S::NUMBER), $e->(4, $S::NUMBER)) }));
  record(kind => 'find_elem', label => 'same_two_cats',
         run(sub { FindMapping($e->(7, $S::NUMBER, $fc), $e->(7, $fc, $S::NUMBER)) }));
  record(kind => 'find_elem', label => 'fake_cat',
         run(sub { local $PICK = sub { (grep { ref($_) eq 'FakeCat' } @_)[0] };
                   FindMapping($e->(7, $S::NUMBER, $fc), $e->(9, $fc, $S::NUMBER)) }));
  record(kind => 'find_elem', label => 'no_common',
         run(sub { FindMapping($e->(3, $S::NUMBER), $e->(4, $fc)) }));
  record(kind => 'find_elem', label => 'pick_undef',
         run(sub { local $PICK = sub { undef }; FindMapping($e->(3, $fc), $e->(2, $fc)) }));

  # Seqsee::Anchored,Seqsee::Anchored: SpikeAndChoose(10), `or return` twice.
  my $an = sub { FakeAnch->new(@_) };
  record(kind => 'find_anch', label => 'fake_cat', run(sub { FindMapping($an->($fc), $an->($fc)) }));
  record(kind => 'find_anch', label => 'no_common', run(sub { FindMapping($an->($fc), $an->()) }));
  record(kind => 'find_anch', label => 'pick_undef',
         run(sub { local $PICK = sub { undef }; FindMapping($an->($fc), $an->($fc)) }));
  record(kind => 'find_anch', label => 'elem_anch',
         run(sub { FindMapping($e->(1, $fc), $an->($fc)) }));
  record(kind => 'find_anch', label => 'anch_elem',
         run(sub { FindMapping($an->($fc), $e->(1, $fc)) }));

  # $Fail variants and pairs with no variant.
  record(kind => 'find_mixed', label => 'sint_elem', run(sub { FindMapping(SInt->new(1), $e->(1, $fc)) }));
  record(kind => 'find_mixed', label => 'elem_sint', run(sub { FindMapping($e->(1, $fc), SInt->new(1)) }));
  record(kind => 'find_mixed', label => 'anch_sint', run(sub { FindMapping($an->($fc), SInt->new(1)) }));
  record(kind => 'find_mixed', label => 'sint_anch', run(sub { FindMapping(SInt->new(1), $an->($fc)) }));
  record(kind => 'find_mixed', label => 'obj_obj', run(sub { FindMapping(FakeObj->new, FakeObj->new) }));
  record(kind => 'find_mixed', label => 'sint_num', run(sub { FindMapping(SInt->new(1), 2) }));
  record(kind => 'find_mixed', label => 'num_sint', run(sub { FindMapping(1, SInt->new(2)) }));
  record(kind => 'find_mixed', label => 'elem_num', run(sub { FindMapping($e->(1, $fc), 2) }));
  record(kind => 'find_mixed', label => 'one_arg', run(sub { FindMapping(1) }));
  record(kind => 'find_mixed', label => 'dir_spos', run(sub { FindMapping($DIR::LEFT, SPos->new(1)) }));
}

# DIR,DIR.
my %DIRS = (left => $DIR::LEFT, right => $DIR::RIGHT, unknown => $DIR::UNKNOWN, neither => $DIR::NEITHER);
for my $da (sort keys %DIRS) {
  for my $db (sort keys %DIRS) {
    my $r = FindMapping($DIRS{$da}, $DIRS{$db});
    record(kind => 'find_dir', a => $da, b => $db, result => describe($r),
           is_memo => ($r == Mapping::Dir->create($$r)) ? 1 : 0);
  }
}

# SPos,SPos.
for my $pair ([1, 2], [2, 1], [2, 2], [1, 3], [3, 1], [-1, 1], [-1, -1], [1, -1], [5, 6]) {
  my ($a, $b) = @$pair;
  record(kind => 'find_pos', a => $a, b => $b,
         run(sub { FindMapping(SPos->new($a), SPos->new($b)) }));
}

# --- ApplyMapping --------------------------------------------------------------------------
my %CATS = (number => $S::NUMBER, prime => $S::PRIME, odd => $S::ODD, even => $S::EVEN);
for my $cat (sort keys %CATS) {
  for my $name (qw(same succ pred flip)) {
    my $t = Mapping::Numeric->create($name, $CATS{$cat});
    # Prime's NextPrime/PreviousPrime loop forever on non-integers (iteration 11).
    for my $num (3, 0, -1, ($cat eq 'prime' ? () : 2.5), 97, undef) {
      record(kind => 'apply_num', cat => $cat, name => $name, num => $num,
             run(sub { ApplyMapping($t, $num) }));
      record(kind => 'apply_sint', cat => $cat, name => $name, num => $num,
             run(sub { ApplyMapping($t, SInt->new($num)) }));
      record(kind => 'apply_elem', cat => $cat, name => $name, num => $num,
             created => do { my $r = eval { ApplyMapping($t, FakeElem->new($num)) };
                             ($r && $r->{created}) ? [$r->{created}[0], describe($r->{created}[1]), $r->{created}[2]] : undef },
             run(sub { ApplyMapping($t, FakeElem->new($num)) }));
    }
  }
}
{
  my $t = Mapping::Numeric->create('succ', $S::NUMBER);
  record(kind => 'apply_misc', label => 'numeric_str', run(sub { ApplyMapping($t, "3") }));
  record(kind => 'apply_misc', label => 'numeric_anch', run(sub { ApplyMapping($t, FakeAnch->new) }));
  record(kind => 'apply_misc', label => 'numeric_obj', run(sub { ApplyMapping($t, FakeObj->new) }));
  record(kind => 'apply_misc', label => 'numeric_spos', run(sub { ApplyMapping($t, SPos->new(1)) }));
  my $fc = FakeCat->new('fc', apply => 'applied');
  my $st = FakeStruct->new(cat => $fc);
  record(kind => 'apply_misc', label => 'struct_obj', run(sub { ApplyMapping($st, FakeObj->new) }));
  record(kind => 'apply_misc', label => 'struct_elem', run(sub { ApplyMapping($st, FakeElem->new(1)) }));
  record(kind => 'apply_misc', label => 'struct_anch', run(sub { ApplyMapping($st, FakeAnch->new) }));
  record(kind => 'apply_misc', label => 'struct_sint', run(sub { ApplyMapping($st, SInt->new(1)) }));
  record(kind => 'apply_misc', label => 'struct_num', run(sub { ApplyMapping($st, 1) }));
  record(kind => 'apply_misc', label => 'struct_undef_result',
         run(sub { ApplyMapping(FakeStruct->new(cat => FakeCat->new('u')), FakeObj->new) }));
  record(kind => 'apply_misc', label => 'dir_spos', run(sub { ApplyMapping($Mapping::Dir::Same, SPos->new(1)) }));
  record(kind => 'apply_misc', label => 'pos_dir', run(sub { ApplyMapping(Mapping::Position->create('same'), $DIR::LEFT) }));
  record(kind => 'apply_misc', label => 'undef_undef', run(sub { ApplyMapping(undef, undef) }));
}

# Mapping::Dir × DIR.
{
  my %MAPS = (Same => $Mapping::Dir::Same, Different => $Mapping::Dir::Different,
              Unknown => $Mapping::Dir::Unknown, NewSame => Mapping::Dir->new('Same'));
  for my $m (sort keys %MAPS) {
    for my $d (sort keys %DIRS) {
      my %r = run(sub { ApplyMapping($MAPS{$m}, $DIRS{$d}) });
      my $r = eval { ApplyMapping($MAPS{$m}, $DIRS{$d}) };
      record(kind => 'apply_dir', map => $m, dir => $d, %r,
             is_input => (defined $r && $r == $DIRS{$d}) ? 1 : 0);
    }
  }
}

# Mapping::Position × SPos.
{
  my %MAPS = (succ => Mapping::Position->create('succ'), pred => Mapping::Position->create('pred'),
              same => Mapping::Position->create('same'), foo => Mapping::Position->create('foo'),
              new_succ => Mapping::Position->new({ text => 'succ' }));
  for my $m (sort keys %MAPS) {
    for my $p (1, 2, -1) {
      my $pos = SPos->new($p);
      my $r = eval { ApplyMapping($MAPS{$m}, $pos) };
      record(kind => 'apply_pos', map => $m, pos => $p, run(sub { ApplyMapping($MAPS{$m}, $pos) }),
             is_input => (ref($r) && $r == $pos) ? 1 : 0);
    }
  }
}

# --- Mapping::Dir methods ------------------------------------------------------------------
{
  my ($S, $D, $U) = ($Mapping::Dir::Same, $Mapping::Dir::Different, $Mapping::Dir::Unknown);
  record(kind => 'dir_basics',
         refs => [ref($S), ref($D), ref($U)],
         strings => [$$S, $$D, $$U],
         serialize => [$S->serialize, $D->serialize, $U->serialize],
         create_same_is_memo => (Mapping::Dir->create('Same') == $S) ? 1 : 0,
         new_same_is_memo => (Mapping::Dir->new('Same') == $S) ? 1 : 0,
         deserialize_is_memo => (Mapping::Dir->deserialize('Different') == $D) ? 1 : 0,
         sameness => [map { $_->IsEffectivelyASamenessRelation } $S, $D, $U, Mapping::Dir->new('Same')],
         flipped_is_self => [map { ($_->FlippedVersion == $_) ? 1 : 0 } $S, $D, $U],
         pure_is_self => [map { ($_->get_pure == $_) ? 1 : 0 } $S, $D, $U],
         deps => [map { scalar(my @x = $_->get_memory_dependencies) } $S, $D, $U],
         isa_mapping => $S->isa('Mapping') ? 1 : 0,
         can_check_sanity => $S->can('CheckSanity') ? 1 : 0,
         can_as_text => $S->can('as_text') ? 1 : 0,
         can_get_name => $S->can('get_name') ? 1 : 0);
  my $other = Mapping::Dir->create('Other');
  record(kind => 'dir_other', string => $$other,
         again_is_memo => (Mapping::Dir->create('Other') == $other) ? 1 : 0,
         deserialize_is_memo => (Mapping::Dir->deserialize('Other') == $other) ? 1 : 0,
         sameness => $other->IsEffectivelyASamenessRelation);
}

# --- Mapping::Position methods -------------------------------------------------------------
{
  my %P = map { ($_ => Mapping::Position->create($_)) } qw(succ pred same);
  for my $t (qw(succ pred same)) {
    my $p = $P{$t};
    my $f = $p->FlippedVersion;
    record(kind => 'pos_basics', text => $t, ref => ref($p), get_text => $p->get_text,
           as_text => $p->as_text, serialize => $p->serialize,
           deserialize_is_memo => (Mapping::Position->deserialize($t) == $p) ? 1 : 0,
           create_is_memo => (Mapping::Position->create($t) == $p) ? 1 : 0,
           new_is_memo => (Mapping::Position->new({ text => $t }) == $p) ? 1 : 0,
           sameness => $p->IsEffectivelyASamenessRelation,
           new_sameness => Mapping::Position->new({ text => $t })->IsEffectivelyASamenessRelation,
           flipped => $f->get_text,
           flipped_is_memo => ($f == Mapping::Position->create($f->get_text)) ? 1 : 0,
           pure_is_self => ($p->get_pure == $p) ? 1 : 0,
           deps => scalar(my @x = $p->get_memory_dependencies),
           isa_mapping => $p->isa('Mapping') ? 1 : 0,
           can_check_sanity => $p->can('CheckSanity') ? 1 : 0);
  }
  my $foo = Mapping::Position->create('foo');
  record(kind => 'pos_edge', label => 'flip_foo_before_empty', run(sub { $foo->FlippedVersion }));
  record(kind => 'pos_edge', label => 'create_undef', run(sub { Mapping::Position->create(undef) }));
  record(kind => 'pos_edge', label => 'create_num', run(sub { Mapping::Position->create(5)->get_text }));
  record(kind => 'pos_edge', label => 'create_num_str_same',
         run(sub { (Mapping::Position->create("5") == Mapping::Position->create(5)) ? 1 : 0 }));
  record(kind => 'pos_edge', label => 'new_array', run(sub { Mapping::Position->new({ text => [] }) }));
  record(kind => 'pos_edge', label => 'new_missing', run(sub { Mapping::Position->new({}) }));
  my $empty = Mapping::Position->create('');
  record(kind => 'pos_edge', label => 'create_empty', run(sub { $empty->get_text }));
  record(kind => 'pos_edge', label => 'flip_foo_after_empty',
         run(sub { my $f = $foo->FlippedVersion; ($f == $empty) ? 'is_empty' : $f->get_text }));
  record(kind => 'pos_edge', label => 'create_undef_after_empty',
         run(sub { (Mapping::Position->create(undef) == $empty) ? 'is_empty' : 'other' }));
}

# --- CheckSanity ---------------------------------------------------------------------------
{
  my $t = Mapping::Numeric->create('succ', $S::NUMBER);
  record(kind => 'sanity', label => 'numeric', run(sub { $t->CheckSanity }));
  for my $case (['ok_two', {a => 1, b => 2}, 1], ['bad_one', {a => 1}, 0],
                ['bad_none', {}, ''], ['ok_none', {}, 1], ['bad_undef', {x => 1}, undef]) {
    my ($label, $changed, $suff) = @$case;
    my $st = FakeStruct->new(cat => FakeCat->new('sc', sufficient => $suff), changed => $changed);
    record(kind => 'sanity', label => $label, run(sub { $st->CheckSanity }));
  }
  record(kind => 'sanity', label => 'plain_mapping', run(sub { (bless {}, 'Mapping')->CheckSanity }));
}

emit();
