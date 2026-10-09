# Oracle for Categorizable.pm and SCategory.pm. Output: tests/golden/scategory.json
#
# Everything SCategory talks to is faked: objects, bindings, relations and the
# values inside bindings. FindMapping/ApplyMapping get extra multimethod
# variants for the fake value/relation types, and Mapping::Structural->create
# is replaced by a recorder, so the role's own logic is all that runs.
use strict;
use Oracle;
use S;

# --- fake values and relations ------------------------------------------------
# FindMapping(FakeVal, FakeVal) is a FakeRel(diff) when |diff| <= 2, else undef.
# ApplyMapping(FakeRel, FakeVal) is FakeVal(n + diff) unless that exceeds 20.
package FakeVal;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub n { $_[0]{n} }

package FakeRel;
sub new { my ($c, $d) = @_; bless { d => $d }, $c }
sub d { $_[0]{d} }

package main;
use Class::Multimethods;
multimethod FindMapping => qw(FakeVal FakeVal) => sub {
  my ($a, $b) = @_;
  my $d = $b->n - $a->n;
  return abs($d) <= 2 ? FakeRel->new($d) : undef;
};
multimethod ApplyMapping => qw(FakeRel FakeVal) => sub {
  my ($r, $v) = @_;
  my $n = $v->n + $r->d;
  return $n > 20 ? undef : FakeVal->new($n);
};

my $structural_opts;
{
  no warnings 'redefine';
  *Mapping::Structural::create = sub { my ($pkg, $opts) = @_; $structural_opts = $opts; return 'STRUCTURAL' };
}

# --- fake objects ----------------------------------------------------------------
package FakeObj;
use Moose;
with 'Categorizable';
has hist => (is => 'rw', default => sub { [] });
has descr => (is => 'rw', default => sub { {} });    # category name => bindings
has calls => (is => 'rw', default => sub { [] });
sub AddHistory { push @{ $_[0]->hist }, $_[1] }
sub describe_as {
  my ($self, $cat) = @_;
  my $name = defined($cat) ? $cat->get_name : 'undef';
  push @{ $self->calls }, "describe_as $name";
  return $self->descr->{$name};
}

# A Seqsee::Object as far as UNIVERSAL::isa is concerned.
package FakeSObj;
our @ISA = ('FakeObj', 'Seqsee::Object');
sub new {
  my ($c, %a) = @_;
  bless { categories => {}, hist => [], descr => {}, calls => [], %a }, $c;
}
sub GetEffectiveObject { $_[0]{effective} // $_[0] }
sub apply_blemish_everywhere {
  my ($self, $t) = @_;
  return FakeSObj->new(built => $self->{built}, blemish => 'everywhere ' . $t->n);
}
sub apply_blemish_at {
  my ($self, $t, $p) = @_;
  die "bad position" if $p->n > 3;
  return FakeSObj->new(built => $self->{built}, blemish => 'at ' . $p->n . ' ' . $t->n);
}
sub set_group_p { $_[0]{group_p} = $_[1] }

# --- fake categories ---------------------------------------------------------------
package FakeCat;
use Moose;
has name => (is => 'ro');
has inst => (is => 'rw', default => sub { {} });     # object id => bindings
has sufficient => (is => 'rw', default => 1);
has build_fails => (is => 'rw', default => 0);
has suff_calls => (is => 'rw', default => sub { [] });
sub Instancer { my ($self, $o) = @_; return $self->inst->{ $o->{id} } }
sub build {
  my ($self, $b) = @_;
  return if $self->build_fails;
  return FakeSObj->new(built => { map { $_ => $b->{$_}->n } keys %$b });
}
sub get_name { $_[0]->name }
sub as_text { 'fake ' . $_[0]->name }
sub AreAttributesSufficientToBuild {
  my ($self, @atts) = @_;
  push @{ $self->suff_calls }, [@atts];
  return $self->sufficient;
}
sub get_meto_types { () }
sub get_pure { $_[0] }
sub get_memory_dependencies { () }
sub serialize { $_[0]->name }
sub deserialize { }
with 'SCategory';

package FakeInterlacedCat;
use Moose;
extends 'FakeCat';

package FakeNumCat;
use Moose;
has name => (is => 'ro');
sub NumericInstancer { }
sub get_name { $_[0]->name }
sub as_text { $_[0]->name }
sub get_meto_types { () }
sub get_pure { $_[0] }
sub get_memory_dependencies { () }
sub serialize { }
sub deserialize { }
with 'SCategory::Numeric';
with 'SCategory';

# Not an SCategory and never registered.
package FakeUnreg;
sub new { bless { n => $_[1] }, $_[0] }
sub get_name { $_[0]{n} }

package FakeB;
sub new { my ($c, %a) = @_; bless {%a}, $c }
sub get_metonymy_mode { $_[0]{mode} }
sub get_bindings_ref { $_[0]{b} }
sub get_position { $_[0]{pos} }
sub get_metonymy_type { $_[0]{type} }

package FakeReln;
sub new { my ($c, %a) = @_; bless {%a}, $c }
sub get_category { $_[0]{cat} }
sub get_changed_bindings { $_[0]{changed} }
sub get_slippages { $_[0]{slips} }
sub get_meto_mode { $_[0]{meto_mode} }
sub get_metonymy_reln { $_[0]{meto_reln} }
sub get_position_reln { $_[0]{pos_reln} }
sub get_direction_reln { $_[0]{dir_reln} }

package main;

sub prefix { my ($s) = @_; $s =~ s/=HASH\(0x[0-9a-f]+\)\z//; $s }
sub vals { my ($h) = @_; return { map { $_ => $h->{$_}->n } keys %$h } }
sub rels { my ($h) = @_; return { map { $_ => $h->{$_}->d } keys %$h } }

my $cat_a = FakeCat->new(name => 'catA');
my $cat_b = FakeCat->new(name => 'catB');
my $cat_c = FakeCat->new(name => 'catC');
my $cat_i = FakeInterlacedCat->new(name => 'catI');
my $unreg = FakeUnreg->new('unreg');
my $num   = FakeNumCat->new(name => 'num');

# --- Categorizable ---------------------------------------------------------------
{
  my $o = FakeObj->new;
  my @ret = $o->add_category($cat_a, 'bA');
  my $sret = $o->add_category($cat_b, 'bB');
  record(op => 'add_category', ret_list => \@ret, ret_scalar => $sret, hist => [ @{ $o->hist } ],
         cats => [ sort map { $_->get_name } @{ $o->get_categories } ],
         strings => [ sort map { prefix($_) } $o->category_list_as_strings ],
         as_string_parts => [ sort map { prefix($_) } split /, /, $o->get_categories_as_string ],
         hash_size => scalar(keys %{ $o->get_cats_hash }));

  $o->add_category($cat_a, 'bA2');
  record(op => 'add_again', hist => [ @{ $o->hist } ], binding => $o->is_of_category_p($cat_a),
         n => scalar(@{ $o->get_categories }));

  record(op => 'lookup', present => $o->GetBindingForCategory($cat_b), absent => $o->is_of_category_p($cat_c),
         absent_defined => defined($o->is_of_category_p($cat_c)) ? 1 : 0);

  my @r1 = $o->remove_category($cat_b);
  my @r2 = $o->remove_category($cat_c);
  record(op => 'remove_category', ret_present => \@r1, ret_absent => [ map { $_ // 'undef' } @r2 ],
         hist => [ @{ $o->hist } ], cats => [ sort map { $_->get_name } @{ $o->get_categories } ]);

  my $d = dies(sub { $o->add_category($cat_c) });
  record(op => 'add_one_arg', dies => $d, hist => [ @{ $o->hist } ],
         n => scalar(@{ $o->get_categories }));

  my $o2 = FakeObj->new;
  $o2->add_category($unreg, 'bU');
  $o2->add_category($cat_a, 'bA');
  my @cats = @{ $o2->get_categories };
  record(op => 'unregistered', n => scalar(@cats), undefs => scalar(grep { !defined } @cats),
         hist => [ @{ $o2->hist } ], strings => [ sort map { prefix($_) } $o2->category_list_as_strings ]);
}

{
  my $mk = sub { my $o = FakeObj->new; $o->add_category($_, 1) for @_; $o };
  my @cases = (
    ['none', []],
    ['one object', [ [$cat_a, $cat_b] ]],
    ['two overlap', [ [$cat_a, $cat_b], [$cat_b, $cat_c] ]],
    ['three all share', [ [$cat_a, $cat_b, $cat_c], [$cat_c, $cat_a], [$cat_a, $cat_c, $cat_i] ]],
    ['disjoint', [ [$cat_a], [$cat_b] ]],
    ['empty object', [ [$cat_a], [] ]],
    ['common unregistered', [ [$unreg, $cat_a], [$unreg] ]],
  );
  for my $c (@cases) {
    my ($label, $spec) = @$c;
    my @objs = map { $mk->(@$_) } @$spec;
    my @r;
    my $d = dies(sub { @r = Categorizable::get_common_categories(@objs) });
    record(op => 'get_common_categories', label => $label, dies => $d, names => [ sort map { $_->get_name } @r ]);
  }
  my $d = dies(sub { Categorizable::get_common_categories($mk->($cat_a), 3) });
  record(op => 'get_common_categories', label => 'non-ref arg', dies => $d);
  $d = dies(sub { Categorizable::get_common_categories(undef) });
  record(op => 'get_common_categories', label => 'undef arg', dies => $d);
}

{
  my @cases = (['none', []], ['interlaced only', [$cat_i]], ['plain only', [$cat_a]],
               ['interlaced and plain', [$cat_i, $cat_a]], ['unregistered', [$unreg]]);
  for my $c (@cases) {
    my $o = FakeObj->new;
    $o->add_category($_, 1) for @{ $c->[1] };
    record(op => 'HasNonAdHocCategory', label => $c->[0], result => $o->HasNonAdHocCategory);
  }
}

{
  my @cases = (
    ['no categories', [], {}],
    ['all describable', [$cat_a, $cat_b], { catA => 'x', catB => 'y' }],
    ['one fails', [$cat_a, $cat_b], { catA => 'x' }],
    ['false bindings', [$cat_a], { catA => 0 }],
  );
  for my $c (@cases) {
    my ($label, $cats, $descr) = @$c;
    my $from = FakeObj->new;
    $from->add_category($_, 1) for @$cats;
    my $to = FakeObj->new(descr => $descr);
    my $r = $from->CopyCategoriesTo($to);
    record(op => 'CopyCategoriesTo', label => $label, result => $r, calls => [ sort @{ $to->calls } ],
           to_cats => scalar(@{ $to->get_categories }));
  }
}

# --- SCategory ------------------------------------------------------------------------
{
  my $o = FakeSObj->new(id => 'o1');
  $cat_a->inst({ o1 => 'BIND' });
  my $r = $cat_a->is_instance($o);
  my $r2 = $cat_b->is_instance($o);
  $cat_c->inst({ o1 => 0 });
  my $r3 = $cat_c->is_instance($o);
  record(op => 'is_instance', yes => $r, no_defined => defined($r2) ? 1 : 0, false_defined => defined($r3) ? 1 : 0,
         cats => [ sort map { $_->get_name } @{ $o->get_categories } ], hist => [ @{ $o->hist } ],
         binding => $o->is_of_category_p($cat_a));
}

{
  my $fresh = FakeCat->new(name => 'fresh');
  my $o = FakeObj->new;
  $o->add_category($fresh, 1);
  record(op => 'registration', registered => scalar(grep { defined } @{ $o->get_categories }),
         is_numeric_fake => $cat_a->IsNumeric ? 1 : 0, is_numeric_num => $num->IsNumeric ? 1 : 0,
         smartmatch_self => ($cat_a ~~ $cat_a) ? 1 : 0, smartmatch_other => ($cat_a ~~ $cat_b) ? 1 : 0,
         eq_self => ($cat_a eq $cat_a) ? 1 : 0, eq_other => ($cat_a eq $cat_b) ? 1 : 0,
         numeq_self => ($cat_a == $cat_a) ? 1 : 0, numeq_other => ($cat_a == $cat_b) ? 1 : 0,
         smartmatch_list => ($cat_b ~~ [$cat_a, $cat_b]) ? 1 : 0);
}

# --- FindMappingForCat -----------------------------------------------------------------
sub mode_name { my ($m) = @_; defined($m) ? $m->as_text : undef }
sub describe_opts {
  my ($o) = @_;
  my %r = (keys => [ sort keys %$o ]);
  $r{category} = $o->{category}->get_name;
  $r{meto_mode} = mode_name($o->{meto_mode});
  $r{changed_bindings} = rels($o->{changed_bindings});
  $r{unchanged_bindings} = [ sort keys %{ $o->{unchanged_bindings} } ];
  $r{slippages} = $o->{slippages};
  for my $k (qw(position_reln metonymy_reln)) {
    $r{$k} = !exists $o->{$k} ? 'missing' : ref($o->{$k}) ? 'rel ' . $o->{$k}->d : $o->{$k};
  }
  $r{direction_is_same} = ($o->{direction_reln} eq $Mapping::Dir::Same) ? 1 : 0;
  $r{first} = $o->{first}{id};
  $r{second} = $o->{second}{id};
  return %r;
}

sub fv { my %h = @_; return { map { $_ => FakeVal->new($h{$_}) } keys %h } }

my $NONE = METO_MODE::NONE();
my $SINGLE = METO_MODE::SINGLE();
my $ALL = METO_MODE::ALL();
my $ABO = METO_MODE::ALLBUTONE();

my @find_cases = (
  # label, b1 spec, b2 spec, sufficient
  ['no_slips none', { mode => $NONE, b => { a => 1, b => 5 } }, { mode => $NONE, b => { a => 2, b => 3 } }, 1],
  ['no_slips empty bindings', { mode => $NONE, b => {} }, { mode => $NONE, b => {} }, 1],
  ['meto modes differ', { mode => $NONE, b => { a => 1 } }, { mode => $ALL, b => { a => 1 } }, 1],
  ['no mapping anywhere', { mode => $NONE, b => { a => 1 } }, { mode => $NONE, b => { a => 9 } }, 1],
  ['slips swap', { mode => $NONE, b => { a => 1, b => 9 } }, { mode => $NONE, b => { a => 9, b => 1 } }, 1],
  ['slips swap insufficient', { mode => $NONE, b => { a => 1, b => 9 } }, { mode => $NONE, b => { a => 9, b => 1 } }, 0],
  ['slips reverse fails', { mode => $NONE, b => { a => 1, b => 10 } }, { mode => $NONE, b => { a => 2, b => 3 } }, 1],
  ['ALL mode', { mode => $ALL, b => { a => 1 } }, { mode => $ALL, b => { a => 3 } }, 1],
  ['SINGLE ok', { mode => $SINGLE, b => { a => 1 }, pos => 1, type => 4 },
                { mode => $SINGLE, b => { a => 2 }, pos => 2, type => 5 }, 1],
  ['ALLBUTONE ok', { mode => $ABO, b => { a => 1 }, pos => 3, type => 4 },
                   { mode => $ABO, b => { a => 0 }, pos => 1, type => 4 }, 1],
  ['SINGLE position fails', { mode => $SINGLE, b => { a => 1 }, pos => 1, type => 4 },
                            { mode => $SINGLE, b => { a => 2 }, pos => 8, type => 5 }, 1],
  ['SINGLE type fails', { mode => $SINGLE, b => { a => 1 }, pos => 1, type => 4 },
                        { mode => $SINGLE, b => { a => 2 }, pos => 2, type => 15 }, 1],
);
for my $c (@find_cases) {
  my ($label, $s1, $s2, $suff) = @$c;
  my @b = map {
    my $s = $_;
    FakeB->new(mode => $s->{mode}, b => fv(%{ $s->{b} }),
               pos => defined $s->{pos} ? FakeVal->new($s->{pos}) : undef,
               type => defined $s->{type} ? FakeVal->new($s->{type}) : undef)
  } ($s1, $s2);
  my $cat = FakeCat->new(name => 'mapcat', sufficient => $suff);
  my $o1 = FakeSObj->new(id => 'o1');
  my $o2 = FakeSObj->new(id => 'o2');
  $o1->{categories}{$cat} = $b[0];
  $o2->{categories}{$cat} = $b[1];
  undef $structural_opts;
  my $r;
  my $d = dies(sub { $r = $cat->FindMappingForCat($o1, $o2) });
  record(op => 'FindMappingForCat', label => $label,
         b1 => { %$s1, mode => mode_name($s1->{mode}) }, b2 => { %$s2, mode => mode_name($s2->{mode}) },
         sufficient => $suff, dies => $d, result => $r, suff_calls => $cat->suff_calls,
         ($structural_opts ? (opts => { describe_opts($structural_opts) }) : ()));
}

{
  my $cat = FakeCat->new(name => 'mapcat');
  my $o1 = FakeSObj->new(id => 'o1');
  my $o2 = FakeSObj->new(id => 'o2');
  my $b = FakeB->new(mode => $NONE, b => fv(a => 1));
  record(op => 'FindMappingForCat', label => 'two args', dies => dies(sub { $cat->FindMappingForCat($o1) }));
  record(op => 'FindMappingForCat', label => 'four args', dies => dies(sub { $cat->FindMappingForCat($o1, $o2, $o2) }));
  record(op => 'FindMappingForCat', label => 'not Seqsee::Object',
         dies => dies(sub { $cat->FindMappingForCat($o1, FakeObj->new) }));
  my $r1 = $cat->FindMappingForCat($o1, $o2);
  $o1->{categories}{$cat} = $b;
  my $r2 = $cat->FindMappingForCat($o1, $o2);
  my $r3 = $cat->FindMappingForCat($o2, $o1);
  record(op => 'FindMappingForCat', label => 'not of category', dies => 0,
         neither => defined($r1) ? 1 : 0, second_missing => defined($r2) ? 1 : 0, first_missing => defined($r3) ? 1 : 0);
  my $o3 = FakeSObj->new(id => 'o3');
  $o3->{categories}{$cat} = FakeB->new(mode => $NONE, b => fv(b => 1));
  my $err = '';
  my $d = dies(sub { eval { $cat->FindMappingForCat($o1, $o3); 1 } or do { $err = "$@"; die $@ } });
  $err =~ s/\n.*//s;
  record(op => 'FindMappingForCat', label => 'missing key in second', dies => $d, error => $err);
}

# --- ApplyMappingForCat ----------------------------------------------------------------
# Object bindings for 'mapcat': a=1, b=5 (optionally position 2 / type 4).
my @apply_cases = (
  # label, reln spec, object bindings spec, options
  ['plain change', { changed => { a => 1 }, slips => {}, meto_mode => $NONE }, { mode => $NONE, b => { a => 1, b => 5 } }, {}],
  ['no change at all', { changed => {}, slips => {}, meto_mode => $NONE }, { mode => $NONE, b => { a => 1, b => 5 } }, {}],
  ['change overflows', { changed => { b => 16 }, slips => {}, meto_mode => $NONE }, { mode => $NONE, b => { a => 1, b => 5 } }, {}],
  ['slippages', { changed => { a => 2 }, slips => { a => 'b', b => 'a', c => '' }, meto_mode => $NONE },
                { mode => $NONE, b => { a => 1, b => 5 } }, {}],
  ['slippage change overflows', { changed => { a => 16 }, slips => { a => 'b' }, meto_mode => $NONE },
                                { mode => $NONE, b => { a => 1, b => 5 } }, {}],
  ['slippage to missing attr', { changed => {}, slips => { a => 'zz' }, meto_mode => $NONE },
                               { mode => $NONE, b => { a => 1, b => 5 } }, {}],
  ['not describable', { changed => {}, slips => {}, meto_mode => $NONE }, undef, {}],
  ['build fails', { changed => {}, slips => {}, meto_mode => $NONE }, { mode => $NONE, b => { a => 1 } }, { build_fails => 1 }],
  ['meto modes differ', { changed => {}, slips => {}, meto_mode => $ALL }, { mode => $NONE, b => { a => 1 } }, {}],
  ['ALL blemish', { changed => {}, slips => {}, meto_mode => $ALL, meto_reln => 1 },
                  { mode => $ALL, b => { a => 1 }, type => 4 }, {}],
  ['ALL meto reln overflows', { changed => {}, slips => {}, meto_mode => $ALL, meto_reln => 17 },
                              { mode => $ALL, b => { a => 1 }, type => 4 }, {}],
  ['SINGLE blemish', { changed => {}, slips => {}, meto_mode => $SINGLE, meto_reln => 1, pos_reln => 1 },
                     { mode => $SINGLE, b => { a => 1 }, type => 4, pos => 2 }, {}],
  ['SINGLE blemish dies', { changed => {}, slips => {}, meto_mode => $SINGLE, meto_reln => 1, pos_reln => 2 },
                          { mode => $SINGLE, b => { a => 1 }, type => 4, pos => 2 }, {}],
  ['SINGLE position overflows', { changed => {}, slips => {}, meto_mode => $SINGLE, meto_reln => 1, pos_reln => 19 },
                                { mode => $SINGLE, b => { a => 1 }, type => 4, pos => 2 }, {}],
  ['effective object', { changed => { a => 1 }, slips => {}, meto_mode => $NONE },
                       { mode => $NONE, b => { a => 3 } }, { effective => 1 }],
);
for my $c (@apply_cases) {
  my ($label, $rs, $bs, $opt) = @$c;
  my $cat = FakeCat->new(name => 'mapcat', build_fails => $opt->{build_fails} // 0);
  my $reln = FakeReln->new(
    cat => $cat, changed => { map { $_ => FakeRel->new($rs->{changed}{$_}) } keys %{ $rs->{changed} } },
    slips => $rs->{slips}, meto_mode => $rs->{meto_mode},
    meto_reln => defined $rs->{meto_reln} ? FakeRel->new($rs->{meto_reln}) : undef,
    pos_reln => defined $rs->{pos_reln} ? FakeRel->new($rs->{pos_reln}) : undef,
  );
  my $bind = defined $bs ? FakeB->new(mode => $bs->{mode}, b => fv(%{ $bs->{b} }),
                                      pos => defined $bs->{pos} ? FakeVal->new($bs->{pos}) : undef,
                                      type => defined $bs->{type} ? FakeVal->new($bs->{type}) : undef) : undef;
  my $target = FakeSObj->new(id => 'target', descr => { mapcat => $bind });
  my $orig = $opt->{effective} ? FakeSObj->new(id => 'orig', effective => $target) : $target;
  my $r;
  my $d = dies(sub { $r = $cat->ApplyMappingForCat($reln, $orig) });
  my $rel_spec = { %$rs, meto_mode => mode_name($rs->{meto_mode}) };
  my $b_spec = defined $bs ? { %$bs, mode => mode_name($bs->{mode}) } : undef;
  record(op => 'ApplyMappingForCat', label => $label, reln => $rel_spec, bindings => $b_spec,
         build_fails => $opt->{build_fails} // 0, effective => $opt->{effective} // 0,
         dies => $d, defined => defined($r) ? 1 : 0, target_calls => $target->{calls},
         ($r ? (built => $r->{built}, blemish => $r->{blemish}, group_p => $r->{group_p},
                result_calls => $r->{calls}) : ()));
}

{
  my $cat = FakeCat->new(name => 'mapcat');
  my $other = FakeCat->new(name => 'other');
  my $reln = FakeReln->new(cat => $other, changed => {}, slips => {}, meto_mode => $NONE);
  my $o = FakeSObj->new(id => 'o');
  record(op => 'ApplyMappingForCat', label => 'category mismatch', dies => dies(sub { $cat->ApplyMappingForCat($reln, $o) }));
  record(op => 'ApplyMappingForCat', label => 'undef object', dies => dies(sub { $cat->ApplyMappingForCat($reln, undef) }));
}

emit();
