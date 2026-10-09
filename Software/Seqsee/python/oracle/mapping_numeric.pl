# Oracle for Mapping/Numeric.pm. Output: tests/golden/mapping_numeric.json
#
# Uses the real SLTM (encode/decode, InsertUnlessPresent), because create memoizes on
# SLTM::encode($name, $category): while a category isn't in the LTM its encoding is
# empty, so names collide across categories. The "script" section is a sequence of
# operations that the Python test replays in order against a fresh memo and LTM.
#
# Category specs: NUMBER/EVEN/ODD/PRIME (the real ones), ALT (a FakeAlt, isa
# SCategory::Alternating), 's:<string>', 'i:<SInt mag>', 'h:<key>=<value>' (a
# one-pair hash), 'u' (undef). SLTM's separator characters chr(129..133) are shown
# as <S1> <S2> <C1> <C2> <C3>.
use strict;
use Oracle;
use S;

package FakeAlt;
our @ISA = ('SCategory::Alternating');
sub new { bless {}, $_[0] }
sub as_text { 'fakealt' }
sub get_pure { $_[0] }
sub get_memory_dependencies { () }

package main;

sub err_text { my ($e) = @_; $e =~ s/ at \S+ line \d+.*//s; $e =~ s/\n.*//s; return $e }
sub vis {
  my ($s) = @_;
  return undef unless defined $s;
  my @tok = qw(<S1> <S2> <C1> <C2> <C3>);
  $s =~ s/([\x{81}-\x{85}])/$tok[ord($1) - 129]/g;
  return $s;
}

my $ALT = FakeAlt->new;
my %NAMED = (NUMBER => $S::NUMBER, EVEN => $S::EVEN, ODD => $S::ODD, PRIME => $S::PRIME,
             ALT => $ALT);
sub cat {
  my ($spec) = @_;
  return $NAMED{$spec} if exists $NAMED{$spec};
  return undef if $spec eq 'u';
  return $1 if $spec =~ /^s:(.*)$/;
  return SInt->new($1) if $spec =~ /^i:(.*)$/;
  return { $1 => $2 } if $spec =~ /^h:(.*)=(.*)$/;
  die "bad spec $spec";
}
sub catdesc {
  my ($c) = @_;
  return 'undef' unless defined $c;
  my $r = ref($c);
  return "s:$c" unless $r;
  return 'i:' . $c->[0] if $r eq 'SInt';
  for (keys %NAMED) { return $_ if $c == $NAMED{$_} }
  return 'HASH' if $r eq 'HASH';
  return "OBJ:$r";
}

# --- constructor (new) ---------------------------------------------------------------
for my $args (
  [ name => 'succ', category => 'NUMBER' ],
  [ name => '', category => 'EVEN' ],
  [ name => 'pred', category => 'u' ],
  [ name => 'pred', category => 's:x' ],
  [ category => 'NUMBER' ],
  [ name => 'succ' ],
  [ name => undef, category => 'NUMBER' ],
  [ name => [], category => 'NUMBER' ],
  [ name => 7, category => 'ODD' ],
  ) {
  my %a = @$args;
  my %real = %a;
  $real{category} = cat($a{category}) if exists $a{category};
  my %spec = %a;
  $spec{name} = 'ARRAY' if ref($a{name});
  my $m = eval { Mapping::Numeric->new( \%real ) };
  record(kind => 'new', args => \%spec, error => ($@ ? err_text($@) : undef),
         name => ($m ? $m->get_name : undef), category => ($m ? catdesc($m->get_category) : undef),
         isa_mapping => ($m ? ($m->isa('Mapping') ? 1 : 0) : undef),
         check_sanity => ($m ? $m->CheckSanity : undef));
}

# --- create errors (no memo entry is made) -----------------------------------------
for my $name ('', '0', undef) {
  eval { Mapping::Numeric->create($name, $S::NUMBER) };
  record(kind => 'create_error', name => $name, error => err_text($@));
}

# --- the scripted memo / LTM sequence ------------------------------------------------
my @script = (
  [ create => 'succ', 'EVEN' ],
  [ create => 'succ', 'NUMBER' ],
  [ create => 'pred', 'NUMBER' ],
  [ create => 'pred', 'PRIME' ],
  [ create => 'same', 'ODD' ],
  [ serialize => 0 ],
  [ insert => 'ODD' ],
  [ create => 'succ', 'ODD' ],
  [ create => 'succ', 'ODD' ],
  [ create => 'same', 'ODD' ],
  [ serialize => 7 ],
  [ insert => 'NUMBER' ],
  [ insert => 'ODD' ],
  [ create => 'succ', 'NUMBER' ],
  [ create => 'flip', 'NUMBER' ],
  [ create => 'succ', 's:x' ],
  [ create => 'succ', 's:x' ],
  [ create => 'succ', 's:' ],
  [ create => 'succ', 'u' ],
  [ create => 'succ', 'i:5' ],
  [ create => 'succ', 's:<C3>5' ],
  [ create => 'succ', 'h:a=1' ],
  [ insert_mapping => 14 ],
  [ serialize => 14 ],
  [ serialize => 2 ],
  [ deserialize => 14 ],
  [ deserialize => 0 ],
  [ deserialize => 17 ],
  [ deserialize_str => 'pred<S1><C1>2' ],
  [ deserialize_str => 'pred<S1><C1>7' ],
  [ deserialize_str => 'succ' ],
  [ deserialize_str => '' ],
  [ flip => 0 ],
  [ flip => 14 ],
  [ flip => 15 ],
  [ create => 'no_flip', 'PRIME' ],
  [ flip => 35 ],
  [ create => 'weird', 'PRIME' ],
  [ flip => 37 ],
  [ insert => 'PRIME' ],
  [ create => 'pred', 'PRIME' ],
  [ create => 'succ', 'PRIME' ],
  [ serialize => 19 ],
  [ deserialize => 19 ],
  [ serialize => 21 ],
  [ deserialize => 21 ],
  [ serialize => 18 ],
);
my %tokchar = ('<S1>' => chr(129), '<S2>' => chr(130), '<C1>' => chr(131), '<C2>' => chr(132),
               '<C3>' => chr(133));
sub unvis { my ($s) = @_; $s =~ s/(<S1>|<S2>|<C1>|<C2>|<C3>)/$tokchar{$1}/g; $s }

# Object arguments are script step numbers (0-based): the mapping that step returned.
my @RES;     # per step: the mapping it returned
my @OBJS;    # distinct mappings in first-seen order; the label is the index
sub label {
  my ($m) = @_;
  return undef unless defined $m;
  for my $i (0 .. $#OBJS) { return $i if $OBJS[$i] == $m }
  push @OBJS, $m;
  return $#OBJS;
}
sub mdesc {
  my ($m) = @_;
  return {} unless defined $m;
  return { label => label($m), name => $m->get_name, category => catdesc($m->get_category) };
}
for my $step (0 .. $#script) {
  my ($what, @a) = @{ $script[$step] };
  my %rec = (kind => 'script', step => $step, op => $script[$step]);
  my ($obj, $r);
  if ($what =~ /^(serialize|deserialize|flip|insert_mapping)$/) {
    $obj = $RES[ $a[0] ] or die "step $step: no object";
  }
  if ($what eq 'create') {
    $r = eval { Mapping::Numeric->create($a[0], cat(unvis($a[1]))) };
    %rec = (%rec, %{ mdesc($r) });
    $rec{error} = err_text($@) if $@;
  } elsif ($what eq 'insert') {
    $rec{index} = SLTM::InsertUnlessPresent(cat($a[0]));
  } elsif ($what eq 'insert_mapping') {
    $rec{index} = SLTM::InsertUnlessPresent($obj);
    $rec{node_count} = SLTM::GetNodeCount();
  } elsif ($what eq 'serialize') {
    $rec{result} = vis($obj->serialize);
  } elsif ($what eq 'deserialize' or $what eq 'deserialize_str') {
    my $str = $what eq 'deserialize' ? $obj->serialize : unvis($a[0]);
    $r = eval { Mapping::Numeric->deserialize($str) };
    %rec = (%rec, %{ mdesc($r) });
    $rec{error} = err_text($@) if $@;
  } elsif ($what eq 'flip') {
    $r = eval { $obj->FlippedVersion };
    %rec = (%rec, %{ mdesc($r) });
    $rec{error} = err_text($@) if $@;
  }
  $RES[$step] = $r;
  record(%rec);
}

# --- methods on fresh (new) objects --------------------------------------------------
# Memoize keys as_text/get_complexity on the object's address, so every object is kept
# alive: a freed object's address gets reused, and the new object would then read the
# old one's cached values (an artifact of Perl memory reuse, not ported).
my @KEEP;
for my $catspec (qw(NUMBER EVEN ODD PRIME ALT s:x u)) {
  for my $name (qw(same succ pred flip no_flip other)) {
    my $m = Mapping::Numeric->new({ name => $name, category => cat($catspec) });
    push @KEEP, $m;
    my %rec = (kind => 'methods', name => $name, category => $catspec);
    $rec{is_sameness} = $m->IsEffectivelyASamenessRelation;
    $rec{pure_is_self} = ($m->get_pure == $m) ? 1 : 0;
    my @deps = $m->get_memory_dependencies;
    $rec{deps} = [ map { catdesc($_) } @deps ];
    for my $meth (qw(as_text get_complexity)) {
      my $v = eval { $m->$meth() };
      $rec{$meth} = $v;
      $rec{"${meth}_error"} = err_text($@) if $@;
    }
    my $rbc = eval { $m->GetRelationBasedCategory };
    if ($@) {
      $rec{rbc_error} = err_text($@);
    } else {
      $rec{rbc} = ref($rbc);
      $rec{rbc_is} = (grep { $rbc == $_ } $S::ASCENDING, $S::SAMENESS, $S::DESCENDING) ? 'singleton'
        : ($rbc->get_transform == $m ? 'mapping_based_of_self' : 'other');
      $rec{rbc_memo} = ($m->GetRelationBasedCategory == $rbc) ? 1 : 0;
    }
    record(%rec);
  }
}

# --- memoized as_text / get_complexity go stale --------------------------------------
{
  my $m = Mapping::Numeric->new({ name => 'succ', category => $S::EVEN });
  my @before = ($m->as_text, $m->get_complexity);
  $m->set_name('same');
  $m->set_category($S::NUMBER);
  record(kind => 'stale', before => \@before,
         after => [ $m->as_text, $m->get_complexity ],
         is_sameness_after => $m->IsEffectivelyASamenessRelation,
         rbc_after => ref($m->GetRelationBasedCategory));
  push @KEEP, $m;
  my $n = Mapping::Numeric->new({ name => 'succ', category => $S::EVEN });
  push @KEEP, $n;
  $n->set_name('pred');
  record(kind => 'stale_fresh', as_text => $n->as_text, get_complexity => $n->get_complexity);
  eval { $n->set_name(undef) };
  record(kind => 'set_name_undef', error => err_text($@), name => $n->get_name);
}

emit();
