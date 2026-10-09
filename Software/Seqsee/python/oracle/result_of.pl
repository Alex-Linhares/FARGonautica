# Oracle for the Seqsee/ResultOf*.pm result objects (item 026):
# ResultOfAttributeCopy, ResultOfPlonk, ResultOfGetConflicts, ResultOfGetSomethingLike.
# (ResultOfCanBeSeenAs is covered by seqsee_object2.pl; ResultOfTestRun belongs to item 049.)
# Output: tests/golden/result_of.json
#
# Objects are real Seqsee::Elements and Seqsee::Anchored groups.
# SWorkspace->FightUntoDeath (item 034) and SWorkspace::__CheckLiveness (item 032), used
# by ResultOfGetConflicts->Resolve, are replaced by scripted recorders.
use strict;
use Oracle;
use S;
use Scalar::Util qw(refaddr);
use Seqsee::ResultOfAttributeCopy;
use Seqsee::ResultOfPlonk;
use Seqsee::ResultOfGetConflicts;
use Seqsee::ResultOfGetSomethingLike;

our @LOG;
our %WINS;     # incumbent name => what FightUntoDeath returns
our %DEAD;     # names that __CheckLiveness says are not live

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    my $m = $e->can('message') ? $e->message : "$e";
    $m =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
    return { class => ref($e), message => $m };
  }
  my $s = "$e";
  $s =~ s/ at (\S+ line \d+|accessor |constructor |reader |native delegation ).*//s;
  $s =~ s/=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)/=REF/g;
  return { class => 'DIE', message => $s };
}

sub E { Seqsee::Element->create(@_) }
sub G { Seqsee::Anchored->create(@_) }
sub log_take { my @l = @LOG; @LOG = (); return \@l }

# Named objects. The Python test builds the same ones.
my %OBJ;
$OBJ{e0} = E( 5, 0 );
$OBJ{e1} = E( 6, 1 );
$OBJ{e2} = E( 7, 2 );
$OBJ{e3} = E( 8, 3 );
$OBJ{e4} = E( 9, 4 );
$OBJ{gA} = G( $OBJ{e0}, $OBJ{e1} );
$OBJ{gB} = G( $OBJ{e1}, $OBJ{e2} );
$OBJ{gC} = G( $OBJ{e2}, $OBJ{e3} );
$OBJ{gD} = G( $OBJ{e3}, $OBJ{e4} );
my %NAME = map { refaddr( $OBJ{$_} ) => $_ } keys %OBJ;

sub name_of {    # an object's name, or the scalar itself
  my ($v) = @_;
  return undef unless defined $v;
  return $v unless ref $v;
  return $NAME{ refaddr($v) } // ( 'unnamed ' . ref($v) );
}

sub V {          # a named value
  my ($v) = @_;
  return $v unless defined $v;
  return $OBJ{$v} if exists $OBJ{$v};
  return [] if $v eq 'ARRAY';
  return {} if $v eq 'HASH';
  return $DIR::RIGHT if $v eq 'DIR';
  return Seqsee::ResultOfAttributeCopy->new() if $v eq 'COPY';
  return SBindings->new() if $v eq 'SBindings';
  return $v;
}

sub VL { [ map { V($_) } @{ $_[0] } ] }

{
  no warnings 'redefine';
  *SWorkspace::FightUntoDeath = sub {
    my ( $p, $o ) = @_;
    my ( $c, $i ) = map { name_of( $o->{$_} ) } qw(challenger incumbent);
    push @LOG, [ 'fight', $c, $i ];
    return exists $WINS{$i} ? $WINS{$i} : 1;
  };
  *SWorkspace::__CheckLiveness = sub {
    push @LOG, [ 'live', map { name_of($_) } @_ ];
    return !grep { $DEAD{ name_of($_) } } @_;
  };
}

# ---------------------------------------------------------------- ResultOfAttributeCopy
{
  my @news = (
    [ 'none', [] ],
    [ 's1', [ success => 1 ] ],
    [ 's0', [ success => 0 ] ],
    [ 'sundef', [ success => undef ] ],
    [ 'sempty', [ success => '' ] ],
    [ 's5', [ success => 5 ] ],
    [ 'sx', [ success => 'x' ] ],
    [ 'sarray', [ success => [] ] ],
    [ 'hashref0', [ { success => 0 } ] ],
    [ 'extra', [ success => 0, foo => 3 ] ],
  );
  for my $n (@news) {
    my ( $name, $args ) = @$n;
    my @a = map { ref $_ eq 'ARRAY' ? [] : $_ } @$args;
    my $obj;
    my $e = err( eval { $obj = Seqsee::ResultOfAttributeCopy->new(@a); 1 } ? undef : $@ );
    record(
      kind    => 'copy_new',
      name    => $name,
      error   => $e,
      success => ( defined $obj ? $obj->success : undef ),
      defined => ( defined $obj ? ( defined $obj->success ? 1 : 0 ) : undef ),
      bool    => ( defined $obj ? ( $obj ? 1 : 0 ) : undef ),
    );
  }

  my ( $s1, $s2, $f1, $f2 ) = map { Seqsee::ResultOfAttributeCopy->$_ } qw(Success Success Failed Failed);
  record(
    kind      => 'copy_ctors',
    success   => [ map { $_->success } $s1, $s2, $f1, $f2 ],
    same_s    => ( refaddr($s1) == refaddr($s2) ? 1 : 0 ),
    same_f    => ( refaddr($f1) == refaddr($f2) ? 1 : 0 ),
    bool_fail => ( $f1 ? 1 : 0 ),
    ref       => ref($f1),
  );

  # The rw accessor.
  for my $v ( 0, 1, '', undef, 5, 'x' ) {
    my $obj = Seqsee::ResultOfAttributeCopy->new();
    my $ret;
    my $e = err( eval { $ret = $obj->success($v); 1 } ? undef : $@ );
    record(
      kind    => 'copy_set',
      value   => $v,
      error   => $e,
      ret     => $ret,
      ret_def => ( defined $ret ? 1 : 0 ),
      after   => $obj->success,
      after_def => ( defined $obj->success ? 1 : 0 ),
    );
  }

  # UpdateWith over a matrix of success values.
  my @vals = ( 1, 0, undef, '' );
  for my $mine (@vals) {
    for my $theirs ( @vals, 'UNDEF_OBJECT' ) {
      my $obj = Seqsee::ResultOfAttributeCopy->new( success => $mine );
      my $other =
        $theirs && $theirs eq 'UNDEF_OBJECT'
        ? undef
        : Seqsee::ResultOfAttributeCopy->new( success => $theirs );
      my $ret;
      my $e = err( eval { $ret = $obj->UpdateWith($other); 1 } ? undef : $@ );
      record(
        kind      => 'copy_update',
        mine      => $mine,
        theirs    => $theirs,
        error     => $e,
        ret       => $ret,
        ret_def   => ( defined $ret ? 1 : 0 ),
        after     => $obj->success,
        after_def => ( defined $obj->success ? 1 : 0 ),
        other     => ( $other ? $other->success : undef ),
      );
    }
  }
}

# ---------------------------------------------------------------- ResultOfPlonk
sub describe_plonk {
  my ($p) = @_;
  return {
    object    => name_of( $p->object_being_plonked ),
    resultant => name_of( $p->resultant_object ),
    has       => $p->has_resultant_object,
    success   => $p->PlonkWasSuccessful,
    bool      => ( $p ? 1 : 0 ),
    str       => "$p",
    copy_ok   => $p->AttributeCopyWasSuccessful,
    copy_ref  => ref( $p->attribute_copy_result ),
  };
}

{
  my @news = (
    [ 'full',     { object_being_plonked => 'gA', resultant_object => 'gB', attribute_copy_result => 'COPY' } ],
    [ 'no_res',   { object_being_plonked => 'e0', attribute_copy_result => 'COPY' } ],
    [ 'element',  { object_being_plonked => 'e0', resultant_object => 'e1', attribute_copy_result => 'COPY' } ],
    [ 'no_obj',   { attribute_copy_result => 'COPY' } ],
    [ 'no_copy',  { object_being_plonked => 'gA' } ],
    [ 'nothing',  {} ],
    [ 'obj_5',    { object_being_plonked => 5, attribute_copy_result => 'COPY' } ],
    [ 'obj_x',    { object_being_plonked => 'x', attribute_copy_result => 'COPY' } ],
    [ 'obj_undef', { object_being_plonked => undef, attribute_copy_result => 'COPY' } ],
    [ 'obj_dir',  { object_being_plonked => 'DIR', attribute_copy_result => 'COPY' } ],
    [ 'res_undef', { object_being_plonked => 'gA', resultant_object => undef, attribute_copy_result => 'COPY' } ],
    [ 'res_5',    { object_being_plonked => 'gA', resultant_object => 5, attribute_copy_result => 'COPY' } ],
    [ 'copy_5',   { object_being_plonked => 'gA', attribute_copy_result => 5 } ],
    [ 'copy_obj', { object_being_plonked => 'gA', attribute_copy_result => 'gB' } ],
    [ 'copy_undef', { object_being_plonked => 'gA', attribute_copy_result => undef } ],
    [ 'all_bad',  { object_being_plonked => 5, resultant_object => 5, attribute_copy_result => 5 } ],
    [ 'obj_bad_res_bad', { object_being_plonked => 5, resultant_object => 5, attribute_copy_result => 'COPY' } ],
  );
  for my $form ( 'list', 'hashref' ) {
    for my $n (@news) {
      my ( $name, $h ) = @$n;
      my %args = map { $_ => V( $h->{$_} ) } keys %$h;
      my $p;
      my $e = err(
        eval {
          $p = $form eq 'list'
            ? Seqsee::ResultOfPlonk->new(%args)
            : Seqsee::ResultOfPlonk->new( \%args );
          1;
        } ? undef : $@
      );
      record(
        kind  => 'plonk_new',
        form  => $form,
        name  => $name,
        args  => $h,
        error => $e,
        ( defined $p ? ( result => describe_plonk($p) ) : () ),
      );
    }
  }

  for my $what ( 'e0', 'gA', undef, 5, 'DIR' ) {
    my $p;
    my $e = err( eval { $p = Seqsee::ResultOfPlonk->Failed( V($what) ); 1 } ? undef : $@ );
    record(
      kind  => 'plonk_failed',
      what  => $what,
      error => $e,
      ( defined $p ? ( result => describe_plonk($p), copy_success => $p->attribute_copy_result->success ) : () ),
    );
  }

  # Writers.
  {
    my @steps;
    my $p = Seqsee::ResultOfPlonk->Failed( $OBJ{gA} );
    my $do = sub {
      my ( $label, $code ) = @_;
      my $ret;
      my $e = err( eval { $ret = $code->(); 1 } ? undef : $@ );
      push @steps, { label => $label, error => $e, ret => ( ref $ret ? name_of($ret) : $ret ),
                     state => describe_plonk($p) };
    };
    $do->( 'set_res_gB',  sub { $p->resultant_object( $OBJ{gB} ) } );
    $do->( 'set_res_5',   sub { $p->resultant_object(5) } );
    $do->( 'set_res_undef', sub { $p->resultant_object(undef) } );
    $do->( 'set_obj_gC',  sub { $p->object_being_plonked( $OBJ{gC} ) } );
    $do->( 'set_obj_x',   sub { $p->object_being_plonked('x') } );
    $do->( 'copy_ok_set_1', sub { $p->AttributeCopyWasSuccessful(1) } );
    $do->( 'copy_ok_set_5', sub { $p->AttributeCopyWasSuccessful(5) } );
    $do->( 'set_copy_failed', sub { $p->attribute_copy_result( Seqsee::ResultOfAttributeCopy->Failed ) } );
    $do->( 'set_copy_gA', sub { $p->attribute_copy_result( $OBJ{gA} ) } );
    $do->( 'set_copy_undef', sub { $p->attribute_copy_result(undef) } );
    record( kind => 'plonk_writers', steps => \@steps );
  }

  # Weak references: a group nobody else holds goes away.
  {
    my $p = Seqsee::ResultOfPlonk->new(
      object_being_plonked  => G( E( 1, 7 ), E( 2, 8 ) ),
      resultant_object      => G( E( 1, 7 ), E( 2, 8 ) ),
      attribute_copy_result => Seqsee::ResultOfAttributeCopy->Failed(),
    );
    record(
      kind      => 'plonk_weak',
      obj_def   => ( defined $p->object_being_plonked ? 1 : 0 ),
      res_def   => ( defined $p->resultant_object ? 1 : 0 ),
      has       => $p->has_resultant_object,
      bool      => ( $p ? 1 : 0 ),
      copy_def  => ( defined $p->attribute_copy_result ? 1 : 0 ),
    );
  }
}

# ---------------------------------------------------------------- ResultOfGetConflicts
sub describe_conflicts {
  my ($c) = @_;
  my $exact = $c->exact_conflict;
  return {
    challenger => name_of( $c->challenger ),
    exact      => name_of($exact),
    exact_def  => ( defined $exact ? 1 : 0 ),
    has        => $c->has_overlapping_conflicts,
    count      => $c->overlapping_conflict_count,
    all        => [ map { name_of($_) } $c->all_overlapping_conflicts ],
    bool       => ( $c ? 1 : 0 ),
    ( ref $exact ? () : ( str => "$c" ) ),
  };
}

{
  my @news = (
    [ 'min',        { challenger => 'gA' } ],
    [ 'exact_g',    { challenger => 'gA', exact_conflict => 'gB' } ],
    [ 'exact_e',    { challenger => 'e0', exact_conflict => 'e1' } ],
    [ 'exact_empty', { challenger => 'gA', exact_conflict => '' } ],
    [ 'exact_0',    { challenger => 'gA', exact_conflict => 0 } ],
    [ 'exact_str',  { challenger => 'gA', exact_conflict => 'abc' } ],
    [ 'exact_undef', { challenger => 'gA', exact_conflict => undef } ],
    [ 'exact_7',    { challenger => 'gA', exact_conflict => 7 } ],
    [ 'over_1',     { challenger => 'gA', overlapping_conflicts => ['gB'] } ],
    [ 'over_2',     { challenger => 'gA', overlapping_conflicts => [ 'gB', 'gC' ] } ],
    [ 'over_empty_exact', { challenger => 'gA', exact_conflict => '', overlapping_conflicts => [ 'gC', 'e3' ] } ],
    [ 'both',       { challenger => 'gA', exact_conflict => 'gB', overlapping_conflicts => [ 'gC', 'gD' ] } ],
    [ 'exact_str_over', { challenger => 'gA', exact_conflict => 'abc', overlapping_conflicts => ['gC'] } ],
    [ 'no_chal',    { exact_conflict => 'gB' } ],
    [ 'chal_5',     { challenger => 5 } ],
    [ 'chal_undef', { challenger => undef } ],
    [ 'chal_dir',   { challenger => 'DIR' } ],
    [ 'over_str',   { challenger => 'gA', overlapping_conflicts => 'x' } ],
    [ 'over_hash',  { challenger => 'gA', overlapping_conflicts => 'HASH' } ],
    [ 'over_undef', { challenger => 'gA', overlapping_conflicts => undef } ],
    [ 'over_bad_item', { challenger => 'gA', overlapping_conflicts => [ 'gB', 5 ] } ],
    [ 'over_dir_item', { challenger => 'gA', overlapping_conflicts => ['DIR'] } ],
    [ 'chal_bad_over_bad', { challenger => 5, overlapping_conflicts => 'x' } ],
  );
  for my $form ( 'list', 'hashref' ) {
    for my $n (@news) {
      my ( $name, $h ) = @$n;
      my %args = map { $_ => ( ref $h->{$_} eq 'ARRAY' ? VL( $h->{$_} ) : V( $h->{$_} ) ) } keys %$h;
      my $c;
      my $e = err(
        eval {
          $c = $form eq 'list'
            ? Seqsee::ResultOfGetConflicts->new(%args)
            : Seqsee::ResultOfGetConflicts->new( \%args );
          1;
        } ? undef : $@
      );
      record(
        kind  => 'conflicts_new',
        form  => $form,
        name  => $name,
        args  => $h,
        error => $e,
        ( defined $c ? ( result => describe_conflicts($c) ) : () ),
      );
    }
  }

  # Read-only accessors and the native delegations' argument checks.
  {
    my $c = Seqsee::ResultOfGetConflicts->new( challenger => $OBJ{gA}, overlapping_conflicts => [ $OBJ{gB} ] );
    for my $m (qw(challenger exact_conflict overlapping_conflicts has_overlapping_conflicts
                  overlapping_conflict_count all_overlapping_conflicts)) {
      my $e = err( eval { $c->$m( $OBJ{gC} ); 1 } ? undef : $@ );
      record( kind => 'conflicts_ro', method => $m, error => $e );
    }
    # The list behind overlapping_conflicts is the object's own (a reference).
    push @{ $c->overlapping_conflicts }, $OBJ{gC};
    record( kind => 'conflicts_list_ref', count => $c->overlapping_conflict_count,
            all => [ map { name_of($_) } $c->all_overlapping_conflicts ] );
  }

  # Weak references.
  {
    my $c = Seqsee::ResultOfGetConflicts->new(
      challenger     => G( E( 1, 7 ), E( 2, 8 ) ),
      exact_conflict => [1],
      overlapping_conflicts => [ G( E( 1, 7 ), E( 2, 8 ) ) ],
    );
    record(
      kind     => 'conflicts_weak',
      chal_def => ( defined $c->challenger ? 1 : 0 ),
      exact_def => ( defined $c->exact_conflict ? 1 : 0 ),
      count    => $c->overlapping_conflict_count,
      bool     => ( $c ? 1 : 0 ),
    );
    my $c2 = Seqsee::ResultOfGetConflicts->new( challenger => $OBJ{gA}, exact_conflict => G( E( 1, 7 ), E( 2, 8 ) ) );
    record( kind => 'conflicts_weak2', exact_def => ( defined $c2->exact_conflict ? 1 : 0 ), bool => ( $c2 ? 1 : 0 ) );
  }

  # Resolve.
  my @scenarios = (
    [ 'nothing', {}, undef, {}, {} ],
    [ 'nothing_empty_opts', {}, {}, {}, {} ],
    [ 'exact_wins', { exact => 'gB' }, undef, {}, {} ],
    [ 'exact_loses', { exact => 'gB' }, undef, { gB => 0 }, {} ],
    [ 'exact_loses_undef', { exact => 'gB' }, {}, { gB => undef }, {} ],
    [ 'exact_loses_empty', { exact => 'gB' }, {}, { gB => '' }, {} ],
    [ 'exact_wins_str', { exact => 'gB' }, {}, { gB => 'yes' }, {} ],
    [ 'exact_ignored', { exact => 'gB' }, { IgnoreConflictWith => 'gB' }, { gB => 0 }, {} ],
    [ 'exact_ignore_other', { exact => 'gB' }, { IgnoreConflictWith => 'gC' }, { gB => 0 }, {} ],
    [ 'exact_ignore_undef', { exact => 'gB' }, { IgnoreConflictWith => undef }, {}, {} ],
    [ 'exact_fail', { exact => 'gB' }, { FailIfExact => 1 }, {}, {} ],
    [ 'exact_fail0', { exact => 'gB' }, { FailIfExact => 0 }, {}, {} ],
    [ 'exact_fail_ignored', { exact => 'gB' }, { FailIfExact => 1, IgnoreConflictWith => 'gB' }, {}, {} ],
    [ 'exact_dead', { exact => 'gB' }, {}, {}, { gB => 1 } ],
    [ 'exact_empty_str', { exact => '' }, { FailIfExact => 1 }, {}, {} ],
    [ 'exact_str', { exact => 'abc' }, {}, {}, {} ],
    [ 'exact_str_ignored', { exact => 'abc' }, { IgnoreConflictWith => 'abc' }, {}, {} ],
    [ 'exact_str_fail', { exact => 'abc' }, { FailIfExact => 'x' }, {}, {} ],
    [ 'over_all_win', { over => [ 'gB', 'gC' ] }, {}, {}, {} ],
    [ 'over_first_loses', { over => [ 'gB', 'gC' ] }, {}, { gB => 0 }, {} ],
    [ 'over_second_loses', { over => [ 'gB', 'gC' ] }, {}, { gC => 0 }, {} ],
    [ 'over_dead', { over => [ 'gB', 'gC' ] }, {}, { gB => 0 }, { gB => 1 } ],
    [ 'over_ignored', { over => [ 'gB', 'gC' ] }, { IgnoreConflictWith => 'gB' }, { gB => 0 }, {} ],
    [ 'over_fail_exact', { over => [ 'gB', 'gC' ] }, { FailIfExact => 1 }, {}, {} ],
    [ 'over_ignore_undef', { over => ['gB'] }, { IgnoreConflictWith => undef }, {}, {} ],
    [ 'both_win', { exact => 'gB', over => [ 'gC', 'gD' ] }, undef, {}, {} ],
    [ 'both_exact_loses', { exact => 'gB', over => [ 'gC', 'gD' ] }, undef, { gB => 0 }, {} ],
    [ 'both_exact_fail', { exact => 'gB', over => [ 'gC', 'gD' ] }, { FailIfExact => 1 }, {}, {} ],
    [ 'both_ignore_exact', { exact => 'gB', over => [ 'gC', 'gD' ] }, { IgnoreConflictWith => 'gB' }, { gB => 0, gD => 0 }, {} ],
    [ 'both_exact_dead', { exact => 'gB', over => [ 'gC', 'gD' ] }, {}, {}, { gB => 1, gC => 1 } ],
    [ 'same_in_both', { exact => 'gB', over => [ 'gB', 'gC' ] }, {}, {}, {} ],
    [ 'self_ignored', { exact => 'gA', over => [ 'gA' ] }, { IgnoreConflictWith => 'gA' }, {}, {} ],
  );
  for my $sc (@scenarios) {
    my ( $name, $spec, $opts, $wins, $dead ) = @$sc;
    my $c = Seqsee::ResultOfGetConflicts->new(
      challenger => $OBJ{gA},
      ( exists $spec->{exact} ? ( exact_conflict => V( $spec->{exact} ) ) : () ),
      ( $spec->{over} ? ( overlapping_conflicts => VL( $spec->{over} ) ) : () ),
    );
    my $o = defined $opts ? { map { $_ => V( $opts->{$_} ) } keys %$opts } : undef;
    local %WINS = %$wins;
    local %DEAD = %$dead;
    @LOG = ();
    my $ret;
    my $e = err( eval { $ret = $c->Resolve($o); 1 } ? undef : $@ );
    record(
      kind  => 'resolve',
      name  => $name,
      spec  => $spec,
      opts  => $opts,
      wins  => $wins,
      dead  => $dead,
      error => $e,
      ret   => $ret,
      ret_def => ( defined $ret ? 1 : 0 ),
      log   => log_take(),
    );
  }
}

# ---------------------------------------------------------------- ResultOfGetSomethingLike
{
  my @attrs = qw(to_ask literally_present probable_matches potential_matches);
  my @news = (
    [ 'full', [ { to_ask => 'a', literally_present => 'b', probable_matches => 'c', potential_matches => 'd' } ] ],
    [ 'undefs', [ { to_ask => undef, literally_present => undef, probable_matches => undef, potential_matches => undef } ] ],
    [ 'extra', [ { to_ask => 1, literally_present => 2, probable_matches => 3, potential_matches => 4, foo => 5 } ] ],
    [ 'nested', [ { 'Seqsee::ResultOfGetSomethingLike' => { to_ask => 1, literally_present => 2, probable_matches => 3, potential_matches => 4 } } ] ],
    [ 'nested_override', [ { to_ask => 1, literally_present => 2, probable_matches => 3, potential_matches => 4,
                              'Seqsee::ResultOfGetSomethingLike' => { to_ask => 9 } } ] ],
    [ 'one_missing', [ { to_ask => 1, literally_present => 2, probable_matches => 3 } ] ],
    [ 'three_missing', [ { to_ask => 1 } ] ],
    [ 'two_missing_two_keys', [ { to_ask => 1, potential_matches => 4 } ] ],
    [ 'all_missing_one_key', [ { foo => 1 } ] ],
    [ 'empty', [ {} ] ],
    [ 'no_args', [] ],
    [ 'undef_arg', [undef] ],
    [ 'number', [5] ],
    [ 'array', [ [] ] ],
  );
  for my $n (@news) {
    my ( $name, $args ) = @$n;
    my $s;
    my $e = err( eval { $s = Seqsee::ResultOfGetSomethingLike->new(@$args); 1 } ? undef : $@ );
    record(
      kind  => 'gsl_new',
      name  => $name,
      error => $e,
      ( defined $s ? ( values => [ map { my $m = "get_$_"; $s->$m } @attrs ] ) : () ),
    );
  }

  # The order of the "mislabel" names follows Perl's hash order, so record them sorted too.
  {
    my $e = err( eval { Seqsee::ResultOfGetSomethingLike->new( { foo => 1, bar => 2 } ); 1 } ? undef : $@ );
    my ($names) = $e->{message} =~ /passed: (.*)\?\)/;
    record( kind => 'gsl_mislabel_two_keys', missing_lines => [ $e->{message} =~ /^(Missing .*)$/mg ],
            names => [ sort $names =~ /'(\w+)'/g ], tail => ( $e->{message} =~ /\n(Fatal.*)$/ )[0] );
  }

  # Setters.
  {
    my $s = Seqsee::ResultOfGetSomethingLike->new(
      { to_ask => 'old', literally_present => 'b', probable_matches => 'c', potential_matches => 'd' } );
    my @steps;
    for my $a (@attrs) {
      my ( $set, $get ) = ( "set_$a", "get_$a" );
      my $ret = $s->$set("new_$a");
      push @steps, { attr => $a, ret_is_self => ( ref $ret && refaddr($ret) == refaddr($s) ? 1 : 0 ),
                     ret => ( ref $ret ? undef : $ret ), after => $s->$get };
    }
    my $e1 = err( eval { $s->set_to_ask(); 1 } ? undef : $@ );
    my $r2 = $s->set_to_ask(undef);
    my $e3 = err( eval { $s->get_to_ask(5); 1 } ? undef : $@ );
    record( kind => 'gsl_setters', steps => \@steps, set_no_value => $e1,
            set_undef_after => $s->get_to_ask, get_with_arg => $e3 );
  }
}

emit();
