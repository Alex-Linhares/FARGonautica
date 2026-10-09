# Oracle for SCategory/MappingBased.pm (and the MetonymySpec roles it consumes).
# Output: tests/golden/mapping_based.json
#
# Transforms are FakeMaps (@ISA = Mapping, so Moose's `isa => 'Mapping'` accepts
# them). Replaced: ApplyMapping (inside SCategory::MappingBased: a table on FakeVal
# numbers), Seqsee::Object->create (FakeBuilt, a blessed array like the real
# object's @{} overload), SLTM::encode and SLTM::decode (recorders).
use strict;
use Oracle;
use S;

package FakeMap;
our @ISA = ('Mapping');
sub new { my ($c, %a) = @_; bless {%a}, $c }
sub as_text { 'M<' . $_[0]{name} . '>' }
sub get_name { $_[0]{name} }
sub get_category { $_[0]{cat} }

package FakeRel;
our @ISA = ('SRelation');
sub new { my ($c, $type) = @_; bless { type => $type }, $c }
sub get_type { $_[0]{type} }

package NotAMapping;
sub new { bless {}, $_[0] }

# The category inside a transform: is_instance gives a configured value.
package FakeCat;
sub new { my ($c, $ret) = @_; bless { ret => $ret, asked => 0 }, $c }
sub is_instance { $_[0]{asked}++; $_[0]{ret} }

# A value ApplyMapping works on; its structure is its number.
package FakeVal;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub get_structure { $_[0]{n} }

# A part of a group: CanBeSeenAs(structure) is true when the structure is its n
# (or always, with any => 1).
package FakePart;
our @ISA = ('FakeVal');
our @LOG;
sub new { my ($c, $n, %a) = @_; bless { n => $n, %a }, $c }
sub GetEffectiveObject { push @LOG, "eff$_[0]{n}"; $_[0] }
sub CanBeSeenAs {
  my ($s, $structure) = @_;
  push @LOG, "see$s->{n}?" . (defined $structure ? $structure : 'undef');
  return 1 if $s->{any};
  return (defined $structure and $structure == $s->{n}) ? 1 : 0;
}

package FakeGroup;
sub new { my ($c, @parts) = @_; bless { parts => [@parts], slippages => {} }, $c }
sub get_items_array { @{ $_[0]{parts} } }
sub GetEffectiveSlippages { $_[0]{slippages} }

package FakeBuilt;
sub new { my ($c, @items) = @_; my $s = bless [@items], $c; $FakeBuilt::EXTRA{$s} = { cats => [] }; $s }
sub add_category { my ($s, $cat, $b) = @_; push @{ $FakeBuilt::EXTRA{$s}{cats} }, [$cat, $b]; 1 }
sub set_reln_scheme { $FakeBuilt::EXTRA{ $_[0] }{scheme} = $_[1] }

package main;

our @APPLY;
our @ENCODED;
our %DECODE;
{
  no warnings 'redefine';
  *Seqsee::Object::create = sub { my ($pkg, @items) = @_; FakeBuilt->new(@items) };
  # ApplyMapping: succ/pred/same on FakeVals (results above 5 are undef), on plain
  # numbers n+1 / n-1 / n; 'none' always gives undef.
  *SCategory::MappingBased::ApplyMapping = sub {
    my ($t, $o) = @_;
    my $name = $t->get_name;
    push @APPLY, $name . '(' . (ref($o) ? 'V' . $o->{n} : $o // 'undef') . ')';
    my $n = ref($o) ? $o->{n} : $o;
    my $r = $name eq 'succ' ? $n + 1 : $name eq 'pred' ? $n - 1 : $name eq 'same' ? $n : undef;
    return undef unless defined $r;
    return undef if $r > 5;
    return ref($o) ? FakeVal->new($r) : $r;
  };
  *SLTM::encode = sub { push @ENCODED, [ map { $_->as_text } @_ ]; return 'ENC' };
  *SLTM::decode = sub { my ($s) = @_; return ($DECODE{$s}) };
}

sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; $e =~ s/\n.*//s; return $e }
sub show { my ($v) = @_; defined($v) ? "$v" : undef }
sub desc {
  my ($v) = @_;
  return undef unless defined $v;
  my $r = ref($v);
  return "$v" if !$r or $r eq 'SInt';
  return 'V' . $v->{n} if $r eq 'FakeVal';
  return 'P' . $v->{n} if $r eq 'FakePart';
  return 'G(' . join(',', map { desc($_) } @{ $v->{parts} }) . ')' if $r eq 'FakeGroup';
  return 'Built(' . join(',', map { desc($_) // 'undef' } @$v) . ')' if $r eq 'FakeBuilt';
  return '[' . join(',', map { desc($_) // 'undef' } @$v) . ']' if $r eq 'ARRAY';
  return "OBJ:$r";
}

my %CAT_RET = (bindings => SBindings->new({ raw_slippages => {}, bindings => {} }),
               undef => undef, zero => 0, empty => '');
my %CATS = map { $_ => FakeCat->new($CAT_RET{$_}) } keys %CAT_RET;
sub T { my ($name, $catkey) = @_; FakeMap->new(name => $name, cat => $CATS{ $catkey // 'undef' }) }

# --- basics, Create, constructor checks -----------------------------------------------
{
  my $t  = T('succ');
  my $c  = SCategory::MappingBased->Create($t);
  my @meto = $c->get_meto_types;
  my @deps = $c->get_memory_dependencies;
  record(kind => 'basics', class => ref($c), name => $c->get_name, as_text => $c->as_text,
         transform_is_t => ($c->get_transform == $t ? 1 : 0),
         is_pure => $c->is_pure, get_pure_is_self => ($c->get_pure == $c ? 1 : 0),
         deps_is_transform => (@deps == 1 && $deps[0] == $t ? 1 : 0),
         meto_types => [@meto], meto_types_scalar => show(scalar $c->get_meto_types),
         is_metonyable => show(scalar $c->is_metonyable),
         does_metonymy_spec => ($c->does('SCategory::MetonymySpec') ? 1 : 0),
         does_not_metonyable => ($c->does('SCategory::MetonymySpec::NotMetonyable') ? 1 : 0),
         does_metonyable => ($c->does('SCategory::MetonymySpec::Metonyable') ? 1 : 0),
         is_numeric => ($c->IsNumeric ? 1 : 0));

  my $c2 = SCategory::MappingBased->Create($t);
  my $c3 = SCategory::MappingBased->Create(T('succ'));
  my $c4 = SCategory::MappingBased->Create(FakeRel->new($t));
  my $c5 = SCategory::MappingBased->new({ transform => $t });
  record(kind => 'create', memo_same => ($c == $c2 ? 1 : 0),
         other_transform_new => ($c3 != $c ? 1 : 0),
         relation_uses_type => ($c4 == $c ? 1 : 0),
         new_is_fresh => ($c5 != $c ? 1 : 0),
         new_name => $c5->get_name);

  # Memoized names: set_transform does not change them. Memoize keeps separate
  # scalar- and list-context caches; the port has only the scalar one, so the names
  # are read in scalar context and the list-context value is recorded apart.
  my $m = SCategory::MappingBased->new({ transform => T('same') });
  my $before = scalar $m->get_name;
  my $as_text_before = scalar $m->as_text;
  $m->set_transform(T('pred'));
  record(kind => 'memo_name', before => $before, after => scalar($m->get_name),
         as_text_after => scalar($m->as_text), transform_after => $m->get_transform->as_text,
         name_after_list_context => [ $m->get_name ]->[0]);
  my $m2 = SCategory::MappingBased->new({ transform => T('same') });
  $m2->set_transform(T('pred'));
  record(kind => 'memo_name_unset', name => $m2->get_name);

  my %errs = (
    create_undef => sub { SCategory::MappingBased->Create(undef) },
    create_string => sub { SCategory::MappingBased->Create('abc') },
    create_not_mapping => sub { SCategory::MappingBased->Create(NotAMapping->new) },
    create_rel_of_undef => sub { SCategory::MappingBased->Create(FakeRel->new(undef)) },
    new_missing => sub { SCategory::MappingBased->new({}) },
    new_undef => sub { SCategory::MappingBased->new({ transform => undef }) },
    set_not_mapping => sub { SCategory::MappingBased->new({ transform => $t })->set_transform(NotAMapping->new) },
    set_undef => sub { SCategory::MappingBased->new({ transform => $t })->set_transform(undef) },
  );
  for my $k (sort keys %errs) {
    my $died = dies($errs{$k});
    record(kind => 'error', label => $k, dies => $died);
  }
}

# --- serialize / deserialize --------------------------------------------------------
{
  my $t = T('succ');
  my $c = SCategory::MappingBased->Create($t);
  @ENCODED = ();
  my $s = $c->serialize;
  my $t2 = T('pred');
  $DECODE{X} = $t2;
  my $d = SCategory::MappingBased->deserialize('X');
  my $d2 = SCategory::MappingBased->deserialize('X');
  $DECODE{Y} = $t;
  my $d3 = SCategory::MappingBased->deserialize('Y');
  record(kind => 'serialize', serialized => $s, encoded => [@ENCODED],
         deserialized_transform_is_t2 => ($d->get_transform == $t2 ? 1 : 0),
         deserialize_memo => ($d == $d2 ? 1 : 0), deserialize_existing => ($d3 == $c ? 1 : 0));
}

# --- AreAttributesSufficientToBuild --------------------------------------------------
{
  my $c = SCategory::MappingBased->Create(T('succ'));
  # Each attribute is tagged: s:<string>, n:<number>, u (undef).
  my @sets = ([], ['s:first'], ['s:length'], ['s:first', 's:length'], ['s:length', 's:x', 's:first'],
              ['n:0', 's:length'], ['s:0', 's:length'], ['s:0.0', 's:length'], ['n:0.0', 's:first'],
              ['u', 's:length'], ['s:First', 's:length'], ['s:first ', 's:length'],
              ['s:first', 's:length', 's:first'], ['n:1', 's:length'], ['n:0']);
  for my $set (@sets) {
    my @atts = map { /^s:(.*)/s ? "$1" : /^n:(.*)/ ? 0 + $1 : undef } @$set;
    no warnings;
    record(kind => 'sufficient', atts => [@$set],
           result => show(scalar $c->AreAttributesSufficientToBuild(@atts)));
  }
}

# --- Instancer ------------------------------------------------------------------------
{
  my @groups = (
    [ empty      => 'succ', 'bindings', [] ],
    [ one        => 'succ', 'bindings', [1] ],
    [ asc3       => 'succ', 'undef',    [1, 2, 3] ],
    [ asc5       => 'succ', 'undef',    [1, 2, 3, 4, 5] ],
    [ same3      => 'same', 'undef',    [4, 4, 4] ],
    [ desc3      => 'pred', 'undef',    [3, 2, 1] ],
    [ gap_b      => 'succ', 'bindings', [1, 2, 4] ],
    [ gap_undef  => 'succ', 'undef',    [1, 2, 4] ],
    [ gap_zero   => 'succ', 'zero',     [1, 2, 4] ],
    [ gap_empty  => 'succ', 'empty',    [1, 2, 4] ],
    [ first_bad  => 'succ', 'undef',    [1, 3, 4] ],
    [ past5      => 'succ', 'bindings', [4, 5, 6] ],
    [ none2      => 'none', 'undef',    [1, 2] ],
    [ none1      => 'none', 'undef',    [1] ],
    [ any_parts  => 'succ', 'undef',    [1, 'any:9', 'any:7'] ],
  );
  for my $g (@groups) {
    my ($label, $tname, $catkey, $ns) = @$g;
    my $t = T($tname, $catkey);
    my $c = SCategory::MappingBased->new({ transform => $t });
    my @parts = map { /^any:(\d+)/ ? FakePart->new($1, any => 1) : FakePart->new($_) } @$ns;
    my $group = FakeGroup->new(@parts);
    $group->{slippages} = { 0 => 'NOT_USED' } if 0;
    @APPLY = (); @FakePart::LOG = ();
    my $asked_before = $CATS{$catkey}{asked};
    my $res = $c->Instancer($group);
    my %rec = (kind => 'instancer', label => $label, transform => $tname, cat => $catkey,
               parts => [@$ns], apply => [@APPLY], log => [@FakePart::LOG],
               cat_asked => $CATS{$catkey}{asked} - $asked_before,
               result_ref => ref($res));
    if (ref($res) eq 'SBindings') {
      my $b = $res->get_bindings_ref;
      $rec{bindings} = { map { $_ => desc($b->{$_}) } sort keys %$b };
      $rec{first_is_group} = ($b->{first} == $group ? 1 : 0);
      $rec{slippages_shared} = ($res->get_squinting_raw == $group->{slippages} ? 1 : 0);
    } else {
      $rec{result} = show($res);
      $rec{result_defined} = defined($res) ? 1 : 0;
    }
    record(%rec);
  }
}

# --- build ------------------------------------------------------------------------------
{
  my @args = (
    [ v1_len3     => 'succ', { first => 'V1', length => 3 } ],
    [ v1_len1     => 'succ', { first => 'V1', length => 1 } ],
    [ v1_len0     => 'succ', { first => 'V1', length => 0 } ],
    [ v1_len_neg  => 'succ', { first => 'V1', length => -1 } ],
    [ v1_len_str  => 'succ', { first => 'V1', length => '3' } ],
    [ v1_len_abc  => 'succ', { first => 'V1', length => 'abc' } ],
    [ v1_len_2_5  => 'succ', { first => 'V1', length => 2.5 } ],
    [ v1_len_0_5  => 'succ', { first => 'V1', length => 0.5 } ],
    [ v1_sint3    => 'succ', { first => 'V1', length => 'S3' } ],
    [ v1_sint0    => 'succ', { first => 'V1', length => 'S0' } ],
    [ v1_array2   => 'succ', { first => 'V1', length => 'A2' } ],
    [ v1_nolen    => 'succ', { first => 'V1' } ],
    [ nofirst     => 'succ', { length => 3 } ],
    [ v3_len5     => 'succ', { first => 'V3', length => 5 } ],
    [ v1_none     => 'none', { first => 'V1', length => 2 } ],
    [ v1_none1    => 'none', { first => 'V1', length => 1 } ],
    [ n2_pred     => 'pred', { first => 2, length => 3 } ],
    [ n3_pred     => 'pred', { first => 3, length => 3 } ],
    [ n0_first    => 'succ', { first => 0, length => 3 } ],
    [ n_str_first => 'succ', { first => '2', length => 2 } ],
    [ v4_same     => 'same', { first => 'V4', length => 3, extra => 'x' } ],
  );
  for my $a (@args) {
    my ($label, $tname, $spec) = @$a;
    my %opts;
    for my $k (keys %$spec) {
      my $v = $spec->{$k};
      $v = FakeVal->new($1) if $v =~ /^V(\d+)$/;
      $v = SInt->new($1) if !ref($v) and $v =~ /^S(\d+)$/;
      $v = [$1, 'x'] if !ref($v) and $v =~ /^A(\d+)$/;
      $opts{$k} = $v;
    }
    my $c = SCategory::MappingBased->new({ transform => T($tname) });
    @APPLY = ();
    my $ret;
    my $died = dies(sub { $ret = $c->build(\%opts) });
    my %rec = (kind => 'build', label => $label, transform => $tname,
               spec => { map { $_ => "$spec->{$_}" } keys %$spec },
               died => $died, apply => [@APPLY], ret_ref => ref($ret));
    if (ref($ret) eq 'FakeBuilt') {
      my $x = $FakeBuilt::EXTRA{$ret};
      my ($cat, $b) = @{ $x->{cats}[0] };
      my $bh = $b->get_bindings_ref;
      $rec{items} = desc($ret);
      $rec{cats_added} = scalar @{ $x->{cats} };
      $rec{cat_is_self} = ($cat == $c ? 1 : 0);
      $rec{bindings} = { map { $_ => desc($bh->{$_}) } sort keys %$bh };
      $rec{length_is_arg} = ($bh->{length} eq $opts{length} ? 1 : 0);
      $rec{first_is_item0} = ($bh->{first} eq $ret->[0] ? 1 : 0);
      $rec{first_is_arg} = ($bh->{first} eq $opts{first} ? 1 : 0);
      $rec{slippages} = scalar keys %{ $b->get_squinting_raw };
      $rec{scheme_is_chain} = ($x->{scheme} == RELN_SCHEME::CHAIN() ? 1 : 0);
    } else {
      $rec{ret} = show($ret);
      $rec{ret_defined} = defined($ret) ? 1 : 0;
    }
    record(%rec);
  }
}

emit();
