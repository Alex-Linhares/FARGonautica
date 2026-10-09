# Oracle for SStream2.pm (item 039).
# Output: tests/golden/sstream2.json
#
# Each case is a scenario: a stream built with CreateNew(name, opts), a set of thoughts and
# a list of ops with one result per op; tests/test_sstream2.py replays them.
#
# Most thoughts are fakes (packages FT and FTB, which differ only in class name, for
# thoughtTypeMatch). A fake has a core, a fringe and a list of actions:
#   fringe components: "s:x" (the string x), "n:5" (the number 5), "o:X" (a blessed object
#   named X, shared by every thought that names it);
#   actions: [codelet => FAMILY, URGENCY, TAG], [action => URGENCY, TAG] (an SAction of the
#   probe family OracleProbe, which records TAG when it runs), [undef], [zero], [str => S].
# Real thoughts: [scat => CAT] (SThought->create, scalar context), [scat_list => CAT]
# (list context), [scat_new => CAT] (SThought::SCat->new).
#
# After each op the stream's state is recorded: current thought, older thoughts, counts,
# ThoughtsSet, ComponentOwnership_of, vivify, hit_intensity, thought_hit_intensity, the
# coderack, the probes that ran, $Global::CurrentCodeletFamily, and one rand() draw (to check
# the number of draws).
#
# Hash order: when more than one older thought is hit, SChoose->choose gets them in hash
# order, so the chosen thought (the codelet's "a") is hash-order dependent. Such ops record
# hit_count > 1 and the test only checks that "a" is one of the hit thoughts.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine', 'once';
use Oracle;
use S;

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

package FT;
sub new { my ( $c, %a ) = @_; bless {%a}, $c }
sub core { $_[0]{core} }
sub get_fringe { $_[0]{fringe} }
sub stored_fringe { my $s = shift; $s->{sf} = shift if @_; $s->{sf} }
sub get_actions { @{ $_[0]{actions} } }

package FTB;
our @ISA = ('FT');

package FakeComp;
sub new { bless { name => $_[1] }, $_[0] }

package Seqsee::SCF::OracleProbe;
sub run { my ( $self, $args ) = @_; push @main::RAN, $args->{tag}; }

package main;

our @RAN;
my ( %obj, %name_of, %comp );

sub reg { my ( $n, $o ) = @_; $obj{$n} = $o; $name_of{$o} = $n if ref $o; }

sub nm {
  my ($o) = @_;
  return undef unless defined $o;
  return $name_of{$o} // ( ref($o) ? '?' . ref($o) : $o );
}

sub comp {
  my ($spec) = @_;
  my ( $k, $v ) = split /:/, $spec, 2;
  return $v       if $k eq 's';
  return 0 + $v   if $k eq 'n';
  if ( $k eq 'o' ) {
    $comp{$v} //= FakeComp->new($v);
    $name_of{ $comp{$v} } = "o:$v";
    return $comp{$v};
  }
  die "bad comp $spec";
}

# Name of a stringified hash key (thought or component).
sub knm {
  my ($k) = @_;
  return $name_of{$k} if exists $name_of{$k};
  return "s:$k";
}

sub action {
  my ($spec) = @_;
  my ( $kind, @a ) = @$spec;
  return SCodelet->new( $a[0], $a[1], { tag => $a[2] } ) if $kind eq 'codelet';
  return SAction->new( { family => 'OracleProbe', urgency => $a[0], arguments => { tag => $a[1] } } )
  if $kind eq 'action';
  return undef if $kind eq 'undef';
  return 0     if $kind eq 'zero';
  return $a[0] if $kind eq 'str';
  die "bad action $kind";
}

sub cat {
  my %c = ( ascending => $S::ASCENDING, descending => $S::DESCENDING, sameness => $S::SAMENESS );
  return $c{ $_[0] } // die "cat $_[0]";
}

sub make_thought {
  my ( $name, $spec ) = @_;
  my $t;
  my $kind = $spec->{kind} // 'FT';
  if ( $kind eq 'scat' ) { $t = SThought->create( cat( $spec->{cat} ) ) }
  elsif ( $kind eq 'scat_list' ) { ($t) = SThought->create( cat( $spec->{cat} ) ) }
  elsif ( $kind eq 'scat_new' ) { $t = SThought::SCat->new( { core => cat( $spec->{cat} ) } ) }
  else {
    $t = $kind->new(
      core    => $spec->{core} // 1,
      fringe  => [ map { [ comp( $_->[0] ), $_->[1] ] } @{ $spec->{fringe} // [] } ],
      actions => [ map { action($_) } @{ $spec->{actions} // [] } ],
    );
  }
  reg( $name, $t );
  $name_of{ cat( $spec->{cat} ) } = "cat:$spec->{cat}" if $spec->{cat};
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/=HASH\(0x[0-9a-f]+\)/=HASH/g;
  return $e;
}

sub fl { defined $_[0] ? 0 + sprintf( '%.12g', $_[0] ) : undef }

sub state {
  my ($s) = @_;
  my %own;
  for my $c ( keys %{ $s->{ComponentOwnership_of} } ) {
    my $h = $s->{ComponentOwnership_of}{$c};
    $own{ knm($c) } = { map { ( knm($_) => fl( $h->{$_} ) ) } keys %$h };
  }
  my @rack = map {
    my $args = $_->[3];
    [ $_->[0], 0 + $_->[1], { map { ( $_ => nm( $args->{$_} ) ) } keys %$args } ]
  } @SCoderack::CODELETS;
  return {
    current => ( $s->{CurrentThought} eq '' ? '' : nm( $s->{CurrentThought} ) ),
    older   => [ map { nm($_) } @{ $s->{OlderThoughts} } ],
    count   => 0 + $s->{OlderThoughtCount},
    set     => [ sort map { knm($_) } keys %{ $s->{ThoughtsSet} } ],
    set_ok  => ( ( grep { nm( $s->{ThoughtsSet}{$_} ) ne knm($_) } keys %{ $s->{ThoughtsSet} } ) ? 0 : 1 ),
    own     => \%own,
    vivify  => [ sort map { knm($_) } keys %{ $s->{vivify} } ],
    hit     => { map { ( knm($_) => fl( $s->{hit_intensity}{$_} ) ) } keys %{ $s->{hit_intensity} } },
    thit    => { map { ( knm($_) => fl( $s->{thought_hit_intensity}{$_} ) ) } keys %{ $s->{thought_hit_intensity} } },
    rack    => \@rack,
    ran     => [@RAN],
    family  => $Global::CurrentCodeletFamily,
  };
}

my $S;
my %OPS = (
  add => sub {
    my $t = $obj{ $_[0] };
    $S->add_thought($t);
    return { sf => defined( $t->stored_fringe ) ? scalar( @{ $t->stored_fringe } ) : undef };
  },
  add_args   => sub { $S->add_thought(@_); return undef },
  antiquate  => sub { $S->antiquate_current_thought; return undef },
  clear      => sub { $S->clear; return undef },
  init       => sub { $S->init; return undef },
  type_match => sub { return 0 + $S->thoughtTypeMatch( $obj{ $_[0] }, $obj{ $_[1] } ) },
  fringe_len => sub { my $f = $obj{ $_[0] }->stored_fringe; return defined $f ? scalar(@$f) : undef },
);

sub scenario {
  my (%sc) = @_;
  %obj = %name_of = %comp = ();
  @RAN = ();
  SCoderack->clear;
  $Global::CurrentCodeletFamily = undef;
  %Global::Feature = ();
  my $sname = "S_$sc{name}";
  $S = SStream2->CreateNew( $sname, $sc{opts} );
  make_thought( $_, $sc{thoughts}{$_} ) for sort keys %{ $sc{thoughts} // {} };
  srand( $sc{seed} // 1 );
  my @results;
  for my $op ( @{ $sc{ops} } ) {
    my ( $kind, @args ) = @$op;
    my $value;
    my $ok = eval { $value = $OPS{$kind}->(@args); 1 };
    push @results,
    { value => $ok ? $value : undef, error => $ok ? undef : err($@), state => state($S), rand => rand() };
  }
  record( %sc, results => \@results, discount => 0 + $S->{DiscountFactor}, max => 0 + $S->{MaxOlderThoughts} );
}

my %plain = map { ( "t$_" => { fringe => [ [ "s:x$_", 100 ] ] } ) } 1 .. 6;

scenario( name => 'opts_default', ops => [] );
scenario( name => 'opts_given', opts => { DiscountFactor => 0.5, MaxOlderThoughts => 3 }, ops => [] );
scenario( name => 'opts_zero',  opts => { DiscountFactor => 0,   MaxOlderThoughts => 0 }, ops => [] );

scenario(
  name     => 'sequence_no_hits',
  thoughts => {%plain},
  ops      => [ [ add => 't1' ], [ add => 't2' ], [ add => 't3' ], [ add => 't3' ], [ fringe_len => 't1' ] ],
);

scenario(
  name     => 'false_core',
  thoughts => {
    z => { core => 0,  fringe => [ [ 's:a', 1 ] ] },
    e => { core => '', fringe => [ [ 's:a', 1 ] ] },
    t1 => $plain{t1},
  },
  ops => [ [ add => 'z' ], [ add => 'e' ], [ add => 't1' ], [ add => 'z' ] ],
);

scenario(
  name     => 'arity',
  thoughts => {%plain},
  ops      => [ ['add_args'], [ 'add_args', 't1', 't2' ], [ add => 't1' ] ],
);

scenario(
  name     => 'simple_hit',
  thoughts => {
    a => { fringe => [ [ 's:x', 100 ], [ 's:y', 50 ] ] },
    b => { fringe => [ [ 's:y', 40 ], [ 's:z', 10 ] ] },
  },
  ops => [ [ add => 'a' ], [ add => 'b' ] ],
);

scenario(
  name     => 'object_and_number_components',
  thoughts => {
    a => { fringe => [ [ 'o:P', 100 ], [ 'n:5', 30 ] ] },
    b => { fringe => [ [ 'o:Q', 100 ], [ 's:5', 20 ] ] },
    c => { fringe => [ [ 'o:P', 70 ] ] },
  },
  ops => [ [ add => 'a' ], [ add => 'b' ], [ add => 'c' ] ],
);

scenario(
  name     => 'dampening_single',
  opts     => { DiscountFactor => 0.5 },
  thoughts => {
    a => { fringe => [ [ 's:k', 100 ] ] },
    b => { fringe => [ [ 's:m', 100 ] ] },
    c => { fringe => [ [ 's:n', 100 ] ] },
    d => { fringe => [ [ 's:k', 60 ] ] },
  },
  ops => [ [ add => 'a' ], [ add => 'b' ], [ add => 'c' ], [ add => 'd' ] ],
);

for my $seed ( 1 .. 4 ) {
  scenario(
    name     => "dampening_multi_$seed",
    seed     => $seed,
    opts     => { DiscountFactor => 0.5 },
    thoughts => {
      a => { fringe => [ [ 's:k', 100 ], [ 's:j', 10 ] ] },
      b => { fringe => [ [ 's:k', 50 ] ] },
      c => { fringe => [ [ 's:j', 100 ] ] },
      d => { fringe => [ [ 's:k', 60 ], [ 's:j', 20 ] ] },
    },
    ops => [ [ add => 'a' ], [ add => 'b' ], [ add => 'c' ], [ add => 'd' ] ],
  );
}

scenario(
  name     => 'type_mismatch',
  thoughts => {
    a => { fringe => [ [ 's:k', 100 ] ] },
    b => { kind => 'FTB', fringe => [ [ 's:k', 100 ] ] },
    c => { kind => 'FTB', fringe => [ [ 's:k', 100 ] ] },
  },
  ops => [ [ add => 'a' ], [ add => 'b' ], [ add => 'c' ], [ type_match => 'a', 'b' ], [ type_match => 'b', 'c' ] ],
);

scenario(
  name     => 'revisit',
  thoughts => {
    a => { fringe => [ [ 's:x', 100 ] ] },
    b => { fringe => [ [ 's:y', 100 ] ] },
    c => { fringe => [ [ 's:z', 100 ] ] },
    d => { fringe => [ [ 's:y', 50 ] ] },
  },
  ops => [ [ add => 'a' ], [ add => 'b' ], [ add => 'c' ], [ add => 'a' ], [ add => 'd' ], [ add => 'b' ] ],
);

scenario(
  name     => 'expel',
  opts     => { MaxOlderThoughts => 2 },
  thoughts => { %plain, h => { fringe => [ [ 's:x1', 100 ], [ 's:x4', 100 ] ] } },
  ops      => [ map( { [ add => "t$_" ] } 1 .. 5 ), [ add => 'h' ], [ add => 't1' ], [ add => 't4' ] ],
);

scenario(
  name     => 'antiquate_clear_init',
  thoughts => {%plain},
  ops      => [
    [ add => 't1' ], ['antiquate'], [ add => 't2' ], ['antiquate'], ['antiquate'], ['init'], ['clear'], [ add => 't1' ]
  ],
);

scenario(
  name     => 'actions_few',
  thoughts => {
    a => { actions => [ [ codelet => 'FamA', 30, 'c1' ], [ codelet => 'FamB', 60, 'c2' ] ] },
  },
  ops => [ [ add => 'a' ] ],
);

for my $seed ( 1 .. 3 ) {
  scenario(
    name     => "actions_many_$seed",
    seed     => $seed,
    thoughts => {
      a => {
        fringe  => [ [ 's:q', 10 ] ],
        actions => [
          [ codelet => 'FamA', 30, 'c1' ],
          [ action  => 100, 'p1' ],
          [ codelet => 'FamB', 60, 'c2' ],
          [ codelet => 'FamC', 0,  'c3' ],
          [ action  => 50,  'p2' ],
          [ codelet => 'FamD', 10, 'c4' ],
        ],
      },
    },
    ops => [ [ add => 'a' ] ],
  );
}

scenario(
  name     => 'actions_zero_urgency',
  thoughts => {
    a => { actions => [ [ codelet => 'FamA', 0, 'c1' ], [ codelet => 'FamB', 0, 'c2' ], [ codelet => 'FamC', 0, 'c3' ] ] },
  },
  ops => [ [ add => 'a' ] ],
);

for my $bad ( [ 'undef' ], [ 'zero' ], [ str => 'junk' ] ) {
  scenario(
    name     => "actions_bad_$bad->[0]",
    thoughts => {
      a => { actions => [ [ action => 100, 'p1' ], [ codelet => 'FamA', 10, 'c1' ], $bad, [ action => 100, 'p2' ] ] },
    },
    ops => [ [ add => 'a' ], [ add => 'a' ] ],
  );
}

scenario(
  name     => 'scat',
  thoughts => {
    sa => { kind => 'scat',      cat => 'ascending' },
    sl => { kind => 'scat_list', cat => 'ascending' },
    sn => { kind => 'scat_new',  cat => 'ascending' },
    sd => { kind => 'scat',      cat => 'descending' },
    f  => { fringe => [ [ 's:x', 100 ] ] },
  },
  ops => [ [ add => 'sa' ], [ add => 'sd' ], [ add => 'sl' ], [ add => 'f' ], [ add => 'sn' ], [ type_match => 'sa', 'f' ] ],
);

emit();
