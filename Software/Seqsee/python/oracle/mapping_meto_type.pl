# Oracle for Mapping/MetoType.pm. Output: tests/golden/mapping_meto_type.json
#
# Specs shared with tests/test_mapping_meto_type.py:
#   categories: NUMBER/EVEN/ODD (the real ones), 's:<string>', 'u' (undef)
#   change values (mappings): 'N:<name>' (Mapping::Numeric->create(name, $S::NUMBER)),
#     'MD:<string>' (Mapping::Dir->create), 'MP:<text>' (Mapping::Position->create),
#     's:<string>', 'i:<SInt mag>', 'u'
#   info-loss values: 'n:<number>', 'D:<left|right|unknown|neither>', 'P:<pos>' (SPos),
#     'i:<SInt mag>', 's:<string>', 'u'
# Hashes are given as lists of [key, spec] pairs, so the Python test can build its dicts
# in the same order. Multi-key hashes are only used where order doesn't matter, or where
# the case records Perl's key order (the each-iterator quirk).
use strict;
use Oracle;
use S;

package main;
use Class::Multimethods;
multimethod 'FindMapping';
multimethod 'ApplyMapping';

sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; $e =~ s/\n.*//s; return vis($e) }
sub vis {
  my ($s) = @_;
  return undef unless defined $s;
  my @tok = qw(<S1> <S2> <C1> <C2> <C3>);
  $s =~ s/([\x{81}-\x{85}])/$tok[ord($1) - 129]/g;
  return $s;
}

my %CATS = (NUMBER => $S::NUMBER, EVEN => $S::EVEN, ODD => $S::ODD);
my %DIRS = (left => $DIR::LEFT, right => $DIR::RIGHT, unknown => $DIR::UNKNOWN,
            neither => $DIR::NEITHER);
sub cat {
  my ($spec) = @_;
  return $CATS{$spec} if exists $CATS{$spec};
  return undef if $spec eq 'u';
  return $1 if $spec =~ /^s:(.*)$/s;
  die "bad cat $spec";
}
sub catdesc {
  my ($c) = @_;
  return 'u' unless defined $c;
  return "s:$c" unless ref($c);
  for (keys %CATS) { return $_ if Scalar::Util::refaddr($c) == Scalar::Util::refaddr($CATS{$_}) }
  return 'OBJ:' . ref($c);
}
sub val {    # change values and info-loss values
  my ($spec) = @_;
  return undef if $spec eq 'u';
  return Mapping::Numeric->create($1, $S::NUMBER) if $spec =~ /^N:(.*)$/;
  return Mapping::Dir->create($1) if $spec =~ /^MD:(.*)$/;
  return Mapping::Position->create($1) if $spec =~ /^MP:(.*)$/;
  return $1 if $spec =~ /^s:(.*)$/s;
  return SInt->new($1) if $spec =~ /^i:(.*)$/;
  return $1 + 0 if $spec =~ /^n:(.*)$/;
  return $DIRS{$1} if $spec =~ /^D:(.*)$/;
  return SPos->new($1) if $spec =~ /^P:(.*)$/;
  die "bad val $spec";
}
sub valdesc {
  my ($v) = @_;
  return 'u' unless defined $v;
  my $r = ref($v);
  unless ($r) { return (Scalar::Util::looks_like_number($v) ? "n:$v" : "s:$v") }
  return 'N:' . $v->get_name . '/' . catdesc($v->get_category) if $r eq 'Mapping::Numeric';
  return 'MD:' . $$v if $r eq 'Mapping::Dir';
  return 'MP:' . $v->get_text if $r eq 'Mapping::Position';
  return 'i:' . $v->[0] if $r eq 'SInt';
  return 'D:' . $v->{text} if $r eq 'DIR';
  return 'P:' . $v->position if $r eq 'SPos';
  return 'MT' if $r eq 'Mapping::MetoType';
  return "OBJ:$r";
}
sub hash_of {
  my ($pairs) = @_;
  return undef unless defined $pairs;
  return { map { ($_->[0] => val($_->[1])) } @$pairs };
}
sub hashdesc {   # sorted [key, desc] pairs
  my ($h) = @_;
  return undef unless defined $h;
  return 'NOTHASH' unless ref($h) eq 'HASH';
  return [ map { [ $_, valdesc($h->{$_}) ] } sort keys %$h ];
}

# Distinct Mapping::MetoType objects get labels in first-seen order, so the test can
# check memo identity.
my @MT;
sub label {
  my ($m) = @_;
  return undef unless defined $m;
  for my $i (0 .. $#MT) { return $i if $MT[$i] == $m }
  push @MT, $m;
  return $#MT;
}
sub mtdesc {
  my ($m) = @_;
  return { result => undef } unless defined $m;
  return { result => 'NOT_MT:' . ref($m) } unless ref($m) eq 'Mapping::MetoType';
  return { label => label($m), name => $m->get_name, category => catdesc($m->get_category),
           change => hashdesc($m->get_change_ref) };
}
sub smtdesc {   # SMetonymType results of ApplyMapping
  my ($t) = @_;
  return { result => undef } unless defined $t;
  return { name => $t->get_name, category => catdesc($t->get_category),
           info_loss => hashdesc($t->get_info_loss) };
}
sub smt {   # SMetonymType->new from [cat, name, pairs]
  my ($c, $n, $pairs) = @_;
  return SMetonymType->new({ category => cat($c), name => $n, info_loss => hash_of($pairs) });
}

# --- new ------------------------------------------------------------------------------
for my $args (
  { category => 'NUMBER', name => 'x', change => [] },
  { category => 'NUMBER', change => [ [ a => 'N:succ' ] ] },
  { category => 'u', name => 'x', change => [] },
  { category => 's:foo', name => 'x', change => [] },
  { name => 'x', change => [] },
  { category => 'NUMBER', name => 'x' },
  { category => 'NUMBER', name => 'x', change => 'u' },
  { category => 'NUMBER', name => 'u', change => [] },
  { category => 'NUMBER', name => 'ARRAY', change => [] },
  { category => 'NUMBER', name => 7, change => [] },
  ) {
  my %real;
  $real{category} = cat($args->{category}) if exists $args->{category};
  if (exists $args->{name}) {
    $real{name} = $args->{name} eq 'u' ? undef : $args->{name} eq 'ARRAY' ? [] : $args->{name};
  }
  if (exists $args->{change}) {
    $real{change_ref} = ref($args->{change}) ? hash_of($args->{change}) : undef;
  }
  my $m = eval { Mapping::MetoType->new(\%real) };
  my %rec = (kind => 'new', args => $args, error => ($@ ? err_text($@) : undef));
  if ($m) {
    %rec = (%rec, name => $m->get_name, category => catdesc($m->get_category),
            change => hashdesc($m->get_change_ref),
            isa_mapping => ($m->isa('Mapping') ? 1 : 0), pure_is_self => ($m->get_pure == $m ? 1 : 0));
  }
  record(%rec);
}

# --- setters -------------------------------------------------------------------------
{
  my $m = Mapping::MetoType->new({ category => $S::NUMBER, name => 'x', change_ref => {} });
  $m->set_name('y');
  $m->set_category($S::EVEN);
  $m->set_change_ref({ a => val('N:same') });
  my $e = eval { $m->set_name(undef); 1 } ? undef : err_text($@);
  record(kind => 'setters', name => $m->get_name, category => catdesc($m->get_category),
         change => hashdesc($m->get_change_ref), set_name_undef_error => $e);
}

# --- the scripted memo / SLTM sequence -----------------------------------------------
# Ops: [create => cat, name, pairs|'u'] [flip => step] [serialize => step]
#      [deserialize => step] [deserialize_str => string] [insert => cat|val spec]
#      [insert_mt => step] [find => [cat,name,pairs], [cat,name,pairs]]
#      [apply => step, [cat,name,pairs]] [sameness => step] [deps => step] [as_text => step]
my @script = (
  [ create => 'NUMBER', 'x', [ [ a => 'N:succ' ] ] ],
  [ create => 'NUMBER', 'x', [ [ a => 'N:succ' ] ] ],
  [ create => 'NUMBER', 'x', [ [ a => 'N:pred' ] ] ],
  [ create => 'EVEN', 'x', [ [ a => 'N:succ' ] ] ],
  [ create => 'NUMBER', 'y', [ [ a => 'N:succ' ] ] ],
  [ create => 'NUMBER', 'x', [ [ b => 'N:succ' ] ] ],
  [ create => 'NUMBER', 'x', [ [ a => 'N:succ' ], [ b => 'MD:Same' ] ] ],
  [ create => 'NUMBER', 'x', [ [ b => 'MD:Same' ], [ a => 'N:succ' ] ] ],
  [ create => 'NUMBER', 'x', [] ],
  [ create => 'NUMBER', 'u', [] ],
  [ create => 'NUMBER', '', [] ],
  [ create => 'NUMBER', 'x', 'u' ],
  [ create => 's:foo', 'x', [] ],
  [ create => 's:foo', 'x', [] ],
  [ create => 'u', 'x', [] ],
  [ create => 's:', 'x', [] ],
  [ create => 'NUMBER', 'x;a', [] ],
  [ create => 'NUMBER', 'x', [ [ a => 's:q' ] ] ],
  [ create => 'NUMBER', 'x', [ [ a => 'i:4' ] ] ],
  [ create => 'NUMBER', 'x', [ [ a => 'i:4' ] ] ],
  # FlippedVersion
  [ flip => 0 ],
  [ flip => 20 ],
  [ flip => 2 ],
  [ flip => 8 ],
  [ flip => 9 ],
  [ flip => 24 ],
  [ flip => 6 ],
  [ create => 'NUMBER', 'flipped_', [ [ p => 'MP:succ' ] ] ],
  [ flip => 27 ],
  [ flip => 28 ],
  [ flip => 17 ],
  [ create => 'NUMBER', 'flippedx', [] ],
  [ flip => 31 ],
  [ create => 'NUMBER', 'z', [ [ a => 'MD:Different' ] ] ],
  [ flip => 33 ],
  [ flip => 34 ],
  # FindMapping(SMetonymType, SMetonymType)
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ], [ 'NUMBER', 'each', [ [ length => 'n:3' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ], [ 'NUMBER', 'each', [ [ length => 'n:3' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:3' ] ] ], [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:3' ] ] ], [ 'NUMBER', 'each', [ [ length => 'n:3' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ], [ 'EVEN', 'each', [ [ length => 'n:3' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ], [ 'NUMBER', 'other', [ [ length => 'n:3' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ], [ 'NUMBER', 'each', [ [ len => 'n:3' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ length => 'n:2' ] ] ],
            [ 'NUMBER', 'each', [ [ length => 'n:3' ], [ x => 'n:1' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [] ], [ 'NUMBER', 'each', [] ] ],
  [ find => [ 'NUMBER', 'each', [ [ d => 'D:left' ] ] ], [ 'NUMBER', 'each', [ [ d => 'D:right' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ d => 'D:left' ] ] ], [ 'NUMBER', 'each', [ [ d => 'D:left' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ d => 'D:left' ] ] ], [ 'NUMBER', 'each', [ [ d => 'D:unknown' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ p => 'P:1' ] ] ], [ 'NUMBER', 'each', [ [ p => 'P:2' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ p => 'P:-1' ] ] ], [ 'NUMBER', 'each', [ [ p => 'P:1' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ l => 'n:2' ], [ d => 'D:left' ] ] ],
            [ 'NUMBER', 'each', [ [ d => 'D:right' ], [ l => 'n:1' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ l => 's:a' ] ] ], [ 'NUMBER', 'each', [ [ l => 's:b' ] ] ] ],
  [ find => [ 'NUMBER', 'each', [ [ l => 'i:2' ] ] ], [ 'NUMBER', 'each', [ [ l => 'n:3' ] ] ] ],
  [ find => [ 's:foo', 'each', [] ], [ 's:foo', 'each', [] ] ],
  [ find => [ 's:foo', 'each', [] ], [ 's:bar', 'each', [] ] ],
  [ find => [ 'NUMBER', '2', [] ], [ 'NUMBER', '2.0', [] ] ],
  # ApplyMapping(Mapping::MetoType, SMetonymType)
  [ apply => 36, [ 'NUMBER', 'each', [ [ length => 'n:5' ] ] ] ],
  [ apply => 36, [ 'NUMBER', 'each', [ [ length => 'n:5' ] ] ] ],
  [ apply => 36, [ 'EVEN', 'other', [ [ length => 'n:5' ], [ q => 's:keep' ] ] ] ],
  [ apply => 36, [ 'NUMBER', 'each', [ [ other => 'n:5' ] ] ] ],
  [ apply => 36, [ 'NUMBER', 'each', [] ] ],
  [ apply => 36, [ 'NUMBER', 'each', [ [ length => 'i:7' ] ] ] ],
  [ apply => 36, [ 'NUMBER', 'each', [ [ length => 's:x' ] ] ] ],
  [ apply => 45, [ 'NUMBER', 'each', [ [ d => 'D:right' ] ] ] ],
  [ apply => 45, [ 'NUMBER', 'each', [ [ d => 'D:unknown' ] ] ] ],
  [ apply => 46, [ 'NUMBER', 'each', [ [ d => 'D:unknown' ] ] ] ],
  [ apply => 48, [ 'NUMBER', 'each', [ [ p => 'P:3' ] ] ] ],
  [ apply => 48, [ 'NUMBER', 'each', [ [ p => 'P:-1' ] ] ] ],
  [ apply => 17, [ 'NUMBER', 'each', [ [ a => 'n:1' ] ] ] ],
  # IsEffectivelyASamenessRelation, get_memory_dependencies, as_text
  [ sameness => 8 ],
  [ sameness => 0 ],
  [ sameness => 39 ],
  [ sameness => 46 ],
  [ sameness => 33 ],
  [ create => 'NUMBER', 'w', [ [ a => 'MP:same' ] ] ],
  [ sameness => 74 ],
  [ deps => 0 ],
  [ deps => 6 ],
  [ deps => 8 ],
  [ deps => 12 ],
  [ deps => 14 ],
  [ deps => 17 ],
  [ deps => 18 ],
  [ as_text => 0 ],
  [ as_text => 8 ],
  [ as_text => 12 ],
  [ as_text => 74 ],
  [ as_text => 14 ],
  # serialize / deserialize with the real SLTM
  [ serialize => 0 ],
  [ deserialize => 0 ],
  [ insert => 'NUMBER' ],
  [ serialize => 0 ],
  [ deserialize => 0 ],
  [ insert => 'N:succ' ],
  [ serialize => 0 ],
  [ deserialize => 0 ],
  [ serialize => 8 ],
  [ deserialize => 8 ],
  [ serialize => 12 ],
  [ deserialize => 12 ],
  [ serialize => 17 ],
  [ deserialize => 17 ],
  [ insert_mt => 33 ],
  [ serialize => 33 ],
  [ deserialize => 33 ],
  [ deserialize_str => 'x' ],
  [ deserialize_str => '' ],
  [ deserialize_str => '<C1>1<S1>x<S1><C2>a<S2><C1>2' ],
  [ deserialize_str => '<C1>1<S1>x<S1><C2>a<S2><C1>9' ],
  [ deserialize_str => '<C1>1<S1>x<S1>' ],
);
my %tokchar = ('<S1>' => chr(129), '<S2>' => chr(130), '<C1>' => chr(131), '<C2>' => chr(132),
               '<C3>' => chr(133));
sub unvis { my ($s) = @_; $s =~ s/(<S1>|<S2>|<C1>|<C2>|<C3>)/$tokchar{$1}/g; $s }

my @RES;
for my $step (0 .. $#script) {
  my ($what, @a) = @{ $script[$step] };
  undef $@;
  my %rec = (kind => 'script', step => $step, op => $script[$step]);
  my $obj;
  if ($what =~ /^(flip|serialize|deserialize|insert_mt|apply|sameness|deps|as_text)$/) {
    $obj = $RES[ $a[0] ];
    unless ($obj) { $RES[$step] = undef; record(%rec, missing => 1); next }
  }
  my $r;
  if ($what eq 'create') {
    my ($c, $n, $pairs) = @a;
    $r = eval {
      Mapping::MetoType->create({ category => cat($c), name => ($n eq 'u' ? undef : $n),
                                  change_ref => (ref($pairs) ? hash_of($pairs) : undef) });
    };
    %rec = (%rec, %{ mtdesc($r) });
  } elsif ($what eq 'flip') {
    $r = eval { $obj->FlippedVersion };
    %rec = (%rec, %{ mtdesc($r) });
  } elsif ($what eq 'find') {
    my ($t1, $t2) = (smt(@{ $a[0] }), smt(@{ $a[1] }));
    $r = eval { FindMapping($t1, $t2) };
    %rec = (%rec, %{ mtdesc($r) });
  } elsif ($what eq 'apply') {
    my $t = smt(@{ $a[1] });
    my $res = eval { ApplyMapping($obj, $t) };
    %rec = (%rec, %{ smtdesc($res) }) unless $@;
    $rec{is_smt} = ref($res) eq 'SMetonymType' ? 1 : 0 if $res;
    if ($res) {   # not memoized: a second identical call gives a new object
      my $again = ApplyMapping($obj, $t);
      $rec{fresh} = ($again != $res) ? 1 : 0;
      $rec{info_loss_shared} = ($again->get_info_loss == $res->get_info_loss) ? 1 : 0;
    }
  } elsif ($what eq 'sameness') {
    $rec{result} = $obj->IsEffectivelyASamenessRelation;
  } elsif ($what eq 'deps') {
    $rec{result} = [ map { my $c = catdesc($_); $c =~ /^OBJ/ ? valdesc($_) : $c }
                     $obj->get_memory_dependencies ];
  } elsif ($what eq 'as_text') {
    # StringifyForCarp's « » are Latin-1 bytes; shown as << >> to keep the JSON UTF-8.
    ($rec{result} = $obj->as_text) =~ tr/\x{ab}\x{bb}/<>/;
    $rec{result} =~ s/([<>])/$1$1/g;
  } elsif ($what eq 'serialize') {
    $rec{result} = eval { vis($obj->serialize) };
  } elsif ($what eq 'deserialize' or $what eq 'deserialize_str') {
    $r = eval {
      my $str = $what eq 'deserialize' ? $obj->serialize : unvis($a[0]);
      Mapping::MetoType->deserialize($str);
    };
    %rec = (%rec, %{ mtdesc($r) });
  } elsif ($what eq 'insert') {
    my $x = exists $CATS{ $a[0] } ? cat($a[0]) : val($a[0]);
    $rec{index} = SLTM::InsertUnlessPresent($x);
  } elsif ($what eq 'insert_mt') {
    $rec{index} = SLTM::InsertUnlessPresent($obj);
    $rec{node_count} = SLTM::GetNodeCount();
  }
  $rec{error} = err_text($@) if $@;
  $RES[$step] = $r;
  record(%rec);
}

# --- the each-iterator quirk ------------------------------------------------------------
# IsEffectivelyASamenessRelation returns from inside `while (each ...)`, leaving the
# hash's iterator part-way, so the next call (or FlippedVersion, which also uses each)
# resumes from there. `keys` resets it (FindMapping calls `keys` on info_loss1 first).
{
  my $m = Mapping::MetoType->new({ category => $S::NUMBER, name => 'x',
    change_ref => hash_of([ [ a => 'N:same' ], [ b => 'N:succ' ], [ c => 'N:same' ], [ d => 'N:succ' ] ]) });
  my @order = keys %{ $m->get_change_ref };
  my @seq = map { $m->IsEffectivelyASamenessRelation ? 1 : 0 } 1 .. 7;
  my $f = $m->FlippedVersion;   # resumes after the 7th call's early return
  my @flipped = sort keys %{ $f->get_change_ref };
  my $f2 = $m->FlippedVersion;  # the iterator was reset at the end of the last loop
  record(kind => 'each_quirk', order => \@order, sameness_seq => \@seq, flipped_keys => \@flipped,
         flipped_again_keys => [ sort keys %{ $f2->get_change_ref } ]);
}
{
  # keys() resets: after an early return, a keys call makes the next call start over.
  my $m = Mapping::MetoType->new({ category => $S::NUMBER, name => 'x',
    change_ref => hash_of([ [ a => 'N:same' ], [ b => 'N:succ' ], [ c => 'N:same' ], [ d => 'N:same' ] ]) });
  my @order = keys %{ $m->get_change_ref };
  my $r0 = $m->IsEffectivelyASamenessRelation;
  my $r1 = $m->IsEffectivelyASamenessRelation;
  $m->IsEffectivelyASamenessRelation;
  my @k = keys %{ $m->get_change_ref };
  my $r2 = $m->IsEffectivelyASamenessRelation;
  record(kind => 'each_reset', order => \@order, first => ($r0 ? 1 : 0),
         without_reset => ($r1 ? 1 : 0), after_keys => ($r2 ? 1 : 0));
}
{
  # FindMapping returns early inside `each %$info_loss1`; a later ApplyMapping over the
  # same info_loss hash resumes from there and so skips the keys before it.
  my $pairs = [ [ a => 'P:1' ], [ b => 'P:-1' ], [ c => 'P:1' ], [ d => 'P:1' ] ];
  my $t1 = smt('NUMBER', 'each', $pairs);
  my @order = keys %{ $t1->get_info_loss };
  my $t2 = smt('NUMBER', 'each', [ map { [ $_->[0], 'P:2' ] } @$pairs ]);
  my $found = FindMapping($t1, $t2);
  my $mt = Mapping::MetoType->create({ category => $S::NUMBER, name => 'each', change_ref => {} });
  my $applied = ApplyMapping($mt, $t1);
  my $applied2 = ApplyMapping($mt, $t1);
  record(kind => 'each_find_apply', order => \@order, found => (defined($found) ? 1 : 0),
         applied_keys => [ sort keys %{ $applied->get_info_loss } ],
         applied_again_keys => [ sort keys %{ $applied2->get_info_loss } ]);
}

# --- multimethod dispatch ---------------------------------------------------------------
{
  my $mt = Mapping::MetoType->create({ category => $S::NUMBER, name => 'x', change_ref => {} });
  my $t = smt('NUMBER', 'x', []);
  my $e1 = eval { FindMapping($mt, $mt); 1 } ? undef : err_text($@);
  my $e2 = eval { ApplyMapping($t, $mt); 1 } ? undef : err_text($@);
  my $e3 = eval { ApplyMapping($mt, 3); 1 } ? undef : err_text($@);
  record(kind => 'dispatch', find_mt_mt => $e1, apply_smt_mt => $e2, apply_mt_num => $e3);
}

emit();
