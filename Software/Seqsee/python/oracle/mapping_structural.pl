# Oracle for Mapping/Structural.pm. Output: tests/golden/mapping_structural.json
#
# Specs shared with tests/test_mapping_structural.py:
#   categories: ASC/DESC/SAME/NUMBER/PRIME/MOUNTAIN (the S singletons), IL2/IL3
#     (SCategory::Interlaced->Create(n)), 's:<string>', 'u' (undef)
#   meto modes: NONE/SINGLE/ALLBUTONE/ALL, 's:<string>', 'u'
#   relations / binding values: 'N:<name>' (Mapping::Numeric->create(name, $S::NUMBER)),
#     'MD:<Same|Different>' (Mapping::Dir->create), 'MP:<text>' (Mapping::Position->create),
#     'MT:<name>:<key>:<numeric name>' (Mapping::MetoType->create over $S::NUMBER),
#     'ST:<step>' (the Mapping::Structural made at that script step), 's:<string>', 'u'
#   A create/new spec is a hash: category, meto_mode, position_reln, metonymy_reln,
#   direction_reln (absent keys stay absent), cb / sl (lists of [key, spec] /
#   [new, old] pairs, or 'u', or 's:<string>').
# Hashes are given as lists of pairs, so the Python test can build its dicts in the same
# order. Where Perl's hash order would show, the case records Perl's key order or uses a
# single key.
use strict;
use Oracle;
use S;

package main;
use Class::Multimethods;
multimethod 'FindMapping';
multimethod 'ApplyMapping';

our @MSG;
no warnings 'redefine';
sub message { push @MSG, norm($_[0]) }
use warnings;

sub norm {   # addresses differ between Perl and Python
  my ($s) = @_;
  return undef unless defined $s;
  $s =~ s/=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)/=REF/g;
  return $s;
}
sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; $e =~ s/\n.*//s; return norm(vis($e)) }
sub vis {
  my ($s) = @_;
  return undef unless defined $s;
  my @tok = qw(<S1> <S2> <C1> <C2> <C3>);
  $s =~ s/([\x{81}-\x{85}])/$tok[ord($1) - 129]/g;
  return $s;
}
my %tokchar = ('<S1>' => chr(129), '<S2>' => chr(130), '<C1>' => chr(131), '<C2>' => chr(132),
               '<C3>' => chr(133));
sub unvis { my ($s) = @_; $s =~ s/(<S1>|<S2>|<C1>|<C2>|<C3>)/$tokchar{$1}/g; $s }

my %CATS = (ASC => $S::ASCENDING, DESC => $S::DESCENDING, SAME => $S::SAMENESS,
            NUMBER => $S::NUMBER, PRIME => $S::PRIME, MOUNTAIN => $S::MOUNTAIN,
            IL2 => SCategory::Interlaced->Create(2), IL3 => SCategory::Interlaced->Create(3));
my %MODES = (NONE => $METO_MODE::NONE, SINGLE => $METO_MODE::SINGLE,
             ALLBUTONE => $METO_MODE::ALLBUTONE, ALL => $METO_MODE::ALL);

my @RES;    # script step -> result
my @ST;     # distinct Mapping::Structural objects, labelled in first-seen order
sub label {
  my ($m) = @_;
  for my $i (0 .. $#ST) { return $i if $ST[$i] == $m }
  push @ST, $m;
  return $#ST;
}

sub thing {   # any spec -> value
  my ($spec) = @_;
  return undef if $spec eq 'u';
  return $CATS{$spec} if exists $CATS{$spec};
  return $MODES{$spec} if exists $MODES{$spec};
  return $1 if $spec =~ /^s:(.*)$/s;
  return Mapping::Numeric->create($1, $S::NUMBER) if $spec =~ /^N:(.*)$/;
  return Mapping::Dir->create($1) if $spec =~ /^MD:(.*)$/;
  return Mapping::Position->create($1) if $spec =~ /^MP:(.*)$/;
  if ($spec =~ /^MT:(.*?):(.*?):(.*)$/) {
    return Mapping::MetoType->create({ category => $S::NUMBER, name => $1,
                                       change_ref => { $2 => Mapping::Numeric->create($3, $S::NUMBER) } });
  }
  return $RES[$1] if $spec =~ /^ST:(\d+)$/;
  die "bad spec $spec";
}
sub desc {
  my ($v) = @_;
  return 'u' unless defined $v;
  my $r = ref($v);
  return "s:$v" unless $r;
  for (keys %CATS) { return $_ if $v == $CATS{$_} }
  for (keys %MODES) { return $_ if $v == $MODES{$_} }
  return 'N:' . $v->get_name . '/' . desc($v->get_category) if $r eq 'Mapping::Numeric';
  return 'MD:' . $$v if $r eq 'Mapping::Dir';
  return 'MP:' . $v->get_text if $r eq 'Mapping::Position';
  return 'MT:' . $v->get_name if $r eq 'Mapping::MetoType';
  return 'ST#' . label($v) if $r eq 'Mapping::Structural';
  return 'HASH' if $r eq 'HASH';
  return "OBJ:$r";
}
sub hashdesc {   # sorted [key, desc] pairs
  my ($h) = @_;
  return 'u' unless defined $h;
  return desc($h) unless ref($h) eq 'HASH';
  return [ map { [ $_, desc($h->{$_}) ] } sort keys %$h ];
}
sub hash_of {   # $plain: the values are the strings themselves (slippages)
  my ($pairs, $plain) = @_;
  return thing($pairs) unless ref($pairs);
  return { map { ($_->[0] => ($plain ? $_->[1] : thing($_->[1]))) } @$pairs };
}
sub opts_of {
  my ($spec) = @_;
  my %o;
  for (qw(category meto_mode position_reln metonymy_reln direction_reln)) {
    $o{$_} = thing($spec->{$_}) if exists $spec->{$_};
  }
  $o{changed_bindings} = hash_of($spec->{cb}) if exists $spec->{cb};
  $o{slippages} = hash_of($spec->{sl}, 1) if exists $spec->{sl};
  return \%o;
}
sub stdesc {
  my ($m) = @_;
  return { result => undef } unless defined $m;
  return { result => 'NOT_ST:' . ref($m) } unless ref($m) eq 'Mapping::Structural';
  return { label => label($m), category => desc($m->get_category), meto_mode => desc($m->get_meto_mode),
           position_reln => desc($m->get_position_reln), metonymy_reln => desc($m->get_metonymy_reln),
           direction_reln => desc($m->get_direction_reln), cb => hashdesc($m->get_changed_bindings),
           sl => hashdesc($m->get_slippages) };
}

my @script = (
  # 0-: create memo, the 'x' replacement, autovivification
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ], sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ], sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ], sl => [],
                metonymy_reln => 's:', position_reln => 's:' } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:pred' ] ], sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ end => 'N:succ' ] ], sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Different', cb => [ [ start => 'N:succ' ] ], sl => [] } ],
  [ create => { category => 'DESC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ], sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same' } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => 'u', sl => 'u' } ],
  # 10-
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [], sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => 's:x', sl => [] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [], sl => 's:x' } ],
  [ create => { category => 'ASC', meto_mode => 'u', direction_reln => 'MD:Same' } ],
  [ create => { category => 'ASC', direction_reln => 'MD:Same' } ],
  [ create => { category => 'ASC', meto_mode => 's:', direction_reln => 'MD:Same' } ],
  [ create => { category => 'ASC', meto_mode => 's:NONE', direction_reln => 'MD:Same' } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', cb => [] } ],
  [ create => { meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [] } ],
  [ create => { category => 'u', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [] } ],
  # 20-
  [ create => { category => 's:', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [] } ],
  [ create => { category => 's:foo', meto_mode => 'NONE', direction_reln => 'u', cb => [] } ],
  [ create => { category => 'SAME', meto_mode => 'SINGLE', direction_reln => 'MD:Same', position_reln => 'MP:same',
                metonymy_reln => 'MT:each:length:same', cb => [ [ each => 'N:same' ] ] } ],
  [ create => { category => 'SAME', meto_mode => 'SINGLE', direction_reln => 'MD:Same', position_reln => 'MP:succ',
                metonymy_reln => 'MT:each:length:succ', cb => [ [ each => 'N:succ' ] ] } ],
  [ create => { category => 'SAME', meto_mode => 'ALL', direction_reln => 'MD:Same', position_reln => 'MP:succ',
                metonymy_reln => 'MT:each:length:succ', cb => [ [ each => 'N:succ' ] ] } ],
  [ create => { category => 'SAME', meto_mode => 'ALL', direction_reln => 'MD:Same', position_reln => 's:',
                cb => [ [ each => 'N:succ' ] ] } ],
  [ create => { category => 'SAME', meto_mode => 'ALLBUTONE', direction_reln => 'MD:Different', position_reln => 'MP:pred',
                metonymy_reln => 'MT:each:length:pred', cb => [ [ each => 'N:pred' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ], [ end => 'N:succ' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ end => 'N:succ' ], [ start => 'N:succ' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ],
                sl => [ [ start => 'end' ] ] } ],
  # 30-
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ end => 'N:succ' ] ],
                sl => [ [ start => 'end' ], [ end => 'start' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:same' ] ],
                sl => [ [ start => 'start' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:same' ] ],
                sl => [ [ start => 'end' ], [ end => 'end' ] ] } ],
  [ create => { category => 'MOUNTAIN', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ foot => 'N:succ' ], [ peak => 'N:succ' ] ] } ],
  [ create => { category => 'IL2', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ] ] } ],
  [ create => { category => 'IL3', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [] } ],
  [ create => { category => 'PRIME', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [] } ],
  [ create => { category => 'NUMBER', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ mag => 'N:succ' ] ] } ],
  [ create => { category => 'SAME', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ each => 'ST:0' ] ] } ],
  [ create => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', cb => [ [ start => 'N:succ' ], [ end => 'N:succ' ] ],
                sl => [ [ start => 'end' ], [ end => 'start' ] ] } ],
  # 40-: new
  [ new => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', position_reln => 's:x', metonymy_reln => 's:x' } ],
  [ new => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', position_reln => 's:x' } ],
  [ new => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', metonymy_reln => 's:x' } ],
  [ new => { category => 'ASC', meto_mode => 'NONE', position_reln => 's:x', metonymy_reln => 's:x' } ],
  [ new => { category => 'ASC', direction_reln => 'MD:Same', position_reln => 's:x', metonymy_reln => 's:x' } ],
  [ new => { meto_mode => 'NONE', direction_reln => 'MD:Same', position_reln => 's:x', metonymy_reln => 's:x' } ],
  [ new => { category => 'u', meto_mode => 'u', direction_reln => 'u', position_reln => 'u', metonymy_reln => 'u' } ],
  [ new => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', position_reln => 's:x', metonymy_reln => 's:x', cb => 'u' } ],
  [ new => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', position_reln => 's:x', metonymy_reln => 's:x', sl => 's:q' } ],
  [ new => { category => 'ASC', meto_mode => 'NONE', direction_reln => 'MD:Same', position_reln => 's:x', metonymy_reln => 's:x',
             cb => [ [ start => 'N:succ' ] ], sl => [ [ a => 'b' ] ] } ],
  # 50-: FlippedVersion
  [ flip => 0 ],
  [ flip => 0 ],
  [ flip => 50 ],
  [ flip => 4 ],
  [ flip => 8 ],
  [ flip => 23 ],
  [ flip => 24 ],
  [ flip => 26 ],
  [ flip => 29 ],
  [ flip => 30 ],
  # 60-
  [ flip => 32 ],
  [ flip => 32 ],
  [ flip => 33 ],
  [ flip => 38 ],
  [ flip => 39 ],
  [ flip => 21 ],
  [ flip => 37 ],
  [ flip => 27 ],
  [ flip => 25 ],
  [ flip => 22 ],
  # 70-: CheckSanity
  [ sanity => 0 ],
  [ sanity => 8 ],
  [ sanity => 27 ],
  [ sanity => 33 ],
  [ sanity => 37 ],
  [ sanity => 22 ],
  # 76-: IsEffectivelyASamenessRelation
  [ sameness => 0 ],
  [ sameness => 8 ],
  [ sameness => 22 ],
  [ sameness => 23 ],
  [ sameness => 25 ],
  [ sameness => 31 ],
  [ sameness => 29 ],
  [ sameness => 38 ],
  [ sameness => 52 ],
  [ sameness => 26 ],
  # 86-: get_memory_dependencies
  [ deps => 0 ],
  [ deps => 8 ],
  [ deps => 21 ],
  [ deps => 22 ],
  [ deps => 25 ],
  [ deps => 27 ],
  [ deps => 38 ],
  [ deps => 19 ],
  # 94-: as_text
  [ as_text => 0 ],
  [ as_text => 8 ],
  [ as_text => 22 ],
  [ as_text => 23 ],
  [ as_text => 27 ],
  [ as_text => 29 ],
  [ as_text => 30 ],
  [ as_text => 31 ],
  [ as_text => 32 ],
  [ as_text => 38 ],
  [ as_text => 39 ],
  [ as_text => 21 ],
  [ as_text => 19 ],
  [ as_text => 33 ],
  # 108-: get_complexity
  [ complexity => 0 ],
  [ complexity => 8 ],
  [ complexity => 7 ],
  [ complexity => 22 ],
  [ complexity => 23 ],
  [ complexity => 24 ],
  [ complexity => 27 ],
  [ complexity => 29 ],
  [ complexity => 30 ],
  [ complexity => 32 ],
  [ complexity => 33 ],
  [ complexity => 34 ],
  [ complexity => 35 ],
  [ complexity => 36 ],
  [ complexity => 37 ],
  [ complexity => 38 ],
  [ complexity => 39 ],
  [ complexity => 21 ],
  [ complexity => 20 ],
  [ complexity => 19 ],
  # 128-: serialize / deserialize with the real SLTM
  [ serialize => 0 ],
  [ deserialize => 0 ],
  [ insert => 'N:succ' ],
  [ serialize => 0 ],
  [ deserialize => 0 ],
  [ insert_st => 0 ],
  [ serialize => 0 ],
  [ deserialize => 0 ],
  [ serialize => 8 ],
  [ deserialize => 8 ],
  # 138-
  [ insert_st => 29 ],
  [ serialize => 29 ],
  [ deserialize => 29 ],
  [ insert_st => 22 ],
  [ serialize => 22 ],
  [ deserialize => 22 ],
  [ insert_st => 38 ],
  [ serialize => 38 ],
  [ deserialize => 38 ],
  [ serialize => 21 ],
  # 148-
  [ deserialize => 21 ],
  [ deserialize_str => '' ],
  [ deserialize_str => '<C1>1' ],
  [ deserialize_str => '<C1>1<S1><C1>2<S1>x<S1><C1>3<S1>x<S1><C2><S1><C2>' ],
  [ deserialize_str => '<C1>1<S1><C1>2<S1>x<S1><C1>3<S1>x' ],
  [ deserialize_str => '<C1>1<S1><C1>2<S1>x<S1><C1>3<S1>x<S1><C2>a<S2><C1>4<S1><C2>' ],
  [ deserialize_str => '<C1>1<S1><C1>2<S1>x<S1><C1>3<S1>x<S1><C2>a<S2><C1>4<S1><C2>a<S2>b' ],
);

for my $step (0 .. $#script) {
  my ($what, $a) = @{ $script[$step] };
  undef $@;
  @MSG = ();
  my %rec = (kind => 'script', step => $step, op => $script[$step]);
  my $obj;
  if ($what !~ /^(create|new|insert|deserialize_str)$/) {
    $obj = $RES[$a];
    unless ($obj) { $RES[$step] = undef; record(%rec, missing => 1); next }
  }
  my $r;
  if ($what eq 'create' or $what eq 'new') {
    my $opts = opts_of($a);
    $r = eval { $what eq 'create' ? Mapping::Structural->create($opts) : Mapping::Structural->new($opts) };
    %rec = (%rec, %{ stdesc($r) }) if $r;
    $rec{opts_after} = { map { ($_ => hashdesc($opts->{$_})) } sort keys %$opts };
    if ($r) {
      $rec{isa_mapping} = $r->isa('Mapping') ? 1 : 0;
      $rec{pure_is_self} = $r->get_pure == $r ? 1 : 0;
      $rec{cb_is_given} = (exists $opts->{changed_bindings} and ref($opts->{changed_bindings})
                           and $r->get_changed_bindings == $opts->{changed_bindings}) ? 1 : 0;
    }
  } elsif ($what eq 'flip') {
    $r = eval { $obj->FlippedVersion };
    %rec = (%rec, %{ stdesc($r) }) unless $@;
  } elsif ($what eq 'sanity') {
    $rec{result} = $obj->CheckSanity;
  } elsif ($what eq 'sameness') {
    $rec{result} = eval { $obj->IsEffectivelyASamenessRelation };
  } elsif ($what eq 'deps') {
    $rec{result} = [ map { desc($_) } $obj->get_memory_dependencies ];
  } elsif ($what eq 'as_text') {
    $rec{result} = eval { $obj->as_text };
  } elsif ($what eq 'complexity') {
    $rec{result} = eval { $obj->get_complexity };
  } elsif ($what eq 'serialize') {
    $rec{result} = eval { vis($obj->serialize) };
  } elsif ($what eq 'deserialize' or $what eq 'deserialize_str') {
    $r = eval {
      my $str = $what eq 'deserialize' ? $obj->serialize : unvis($a);
      Mapping::Structural->deserialize($str);
    };
    %rec = (%rec, %{ stdesc($r) }) unless $@;
  } elsif ($what eq 'insert') {
    $rec{index} = SLTM::InsertUnlessPresent(thing($a));
  } elsif ($what eq 'insert_st') {
    $rec{index} = eval { SLTM::InsertUnlessPresent($obj) };
    $rec{node_count} = SLTM::GetNodeCount();
  }
  $rec{error} = err_text($@) if $@;
  $rec{messages} = [@MSG] if @MSG;
  $RES[$step] = $r;
  record(%rec);
}

# --- FlippedVersion is memoized per object (setters don't refresh it) ---------------------
{
  my $m = Mapping::Structural->new({ category => $S::ASCENDING, meto_mode => $METO_MODE::NONE,
    direction_reln => Mapping::Dir->create('Same'), position_reln => 'x', metonymy_reln => 'x',
    changed_bindings => { start => Mapping::Numeric->create('succ', $S::NUMBER) } });
  my $f1 = $m->FlippedVersion;
  $m->get_changed_bindings->{start} = Mapping::Numeric->create('same', $S::NUMBER);
  my $f2 = $m->FlippedVersion;
  $m->set_category($S::DESCENDING);
  record(kind => 'flip_memo', same_object => ($f1 == $f2 ? 1 : 0), flip => stdesc($f2),
         category_after_set => desc($m->get_category));
}

# --- the each-iterator quirk ----------------------------------------------------------------
# IsEffectivelyASamenessRelation returns from inside `while (each %{slippages})`. The next
# call resumes from there. as_text/get_complexity/FlippedVersion copy the hash (%h = %$ref),
# which resets the iterator.
{
  my $m = Mapping::Structural->new({ category => $S::ASCENDING, meto_mode => $METO_MODE::NONE,
    direction_reln => Mapping::Dir->create('Same'), position_reln => 'x', metonymy_reln => 'x',
    changed_bindings => {}, slippages => { a => 'a', b => 'x', c => 'c', d => 'y' } });
  my @order = keys %{ $m->get_slippages };
  my @seq = map { $m->IsEffectivelyASamenessRelation ? 1 : 0 } 1 .. 7;
  $m->IsEffectivelyASamenessRelation;
  $m->get_complexity;
  my $after_copy = $m->IsEffectivelyASamenessRelation ? 1 : 0;
  record(kind => 'each_quirk_slippages', order => \@order, sameness_seq => \@seq, after_copy => $after_copy);
}
{
  my $n = sub { Mapping::Numeric->create($_[0], $S::NUMBER) };
  my $m = Mapping::Structural->new({ category => $S::ASCENDING, meto_mode => $METO_MODE::NONE,
    direction_reln => Mapping::Dir->create('Same'), position_reln => 'x', metonymy_reln => 'x',
    changed_bindings => { a => $n->('same'), b => $n->('succ'), c => $n->('same'), d => $n->('pred') },
    slippages => {} });
  my @order = keys %{ $m->get_changed_bindings };
  my @seq = map { $m->IsEffectivelyASamenessRelation ? 1 : 0 } 1 .. 7;
  $m->IsEffectivelyASamenessRelation;
  my @deps = $m->get_memory_dependencies;   # values %h resets
  my $after_values = $m->IsEffectivelyASamenessRelation ? 1 : 0;
  record(kind => 'each_quirk_bindings', order => \@order, sameness_seq => \@seq, after_values => $after_values);
}

# --- ApplyMapping dispatch on a structural mapping with a non-object ------------------------
{
  my $m = $RES[0];
  my $e1 = eval { ApplyMapping($m, 3); 1 } ? undef : err_text($@);
  my $e2 = eval { FindMapping($m, $m); 1 } ? undef : err_text($@);
  record(kind => 'dispatch', apply_num => $e1, find_st_st => $e2);
}

emit();
