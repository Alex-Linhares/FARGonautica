# Oracle for item 046 (Global wiring): the S.pm singletons and Sanity.pm's SanityCheck
# variants. Output: tests/golden/sanity.json
#
# Each case is a named scenario built directly below; tests/test_sanity.py builds the same
# workspace in Python. Errors are recorded with Perl's " at FILE line N." stripped and
# with ref addresses masked (=HASH).
use strict;
use warnings;
no warnings 'uninitialized', 'numeric';
use Oracle;
use S;
use Sanity;
use Class::Multimethods;
multimethod 'SanityCheck';

# Test::Seqsee runs INITIALIZE_for_testing at load, which prints "View: 1!".
BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

my @messages;
{
  no warnings 'redefine';
  *main::message = sub { push @messages, $_[0] };
}

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    my $m = '' . ( $e->can('message') ? $e->message : $e );
    $m =~ s/=(HASH|ARRAY)\(0x[0-9a-f]+\)/=$1/g;
    return { class => ref($e), message => $m };
  }
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/=(HASH|ARRAY)\(0x[0-9a-f]+\)/=$1/g;
  return $e;
}

sub check {
  my (@args) = @_;
  @messages = ();
  my $ok = eval { SanityCheck(@args); 1 };
  my $e = $ok ? undef : err($@);
  my @m = map { my $x = $_; $x =~ s/=(HASH|ARRAY)\(0x[0-9a-f]+\)/=$1/g; $x } @messages;
  return { error => $e, messages => \@m };
}

sub init {
  my (@seq) = @_;
  %Global::Feature               = ();
  $Global::Steps_Finished        = 0;
  $Global::CurrentRunnableString = '';
  $Global::CurrentCodelet        = undef;
  SLTM->Clear();
  SWorkspace->init( { seq => [@seq] } );
  SWorkspace::__ClearBarLines();
  return SWorkspace::GetElements();
}

sub gp {
  my (@items) = @_;
  my $g = Seqsee::Anchored->create(@items);
  return $g;
}

sub reln {
  my ( $f, $s, $type ) = @_;
  return SRelation->new(
    { first => $f, second => $s, type => Mapping::Numeric->create( $type, $S::NUMBER ) } );
}

sub with_codelet { $Global::CurrentCodelet = SCodelet->new( 'FocusOn', 50, {} ) }

# --- S.pm singletons --------------------------------------------------------------------
{
  my @names = qw(ASCENDING DESCENDING MOUNTAIN SAMENESS NUMBER PRIME ODD EVEN);
  my %s;
  for (@names) { no strict 'refs'; $s{$_} = ${"S::$_"}; }
  record(
    case       => 'singletons',
    classes    => [ map { ref $s{$_} } @names ],
    names      => [ map { $s{$_}->get_name } @names ],
    texts      => [ map { $s{$_}->as_text } @names ],
    ad_hoc     => ( defined $S::AD_HOC ? 1 : 0 ),
    double     => [ ref($S::DOUBLE), $S::DOUBLE->get_name, ( $S::DOUBLE->get_category eq $S::SAMENESS ? 1 : 0 ),
                    $S::DOUBLE->get_info_loss->{length}, $S::DOUBLE->as_text ],
    new_is_new => ( SCategory::Ascending->new eq $S::ASCENDING ? 0 : 1 ),
  );
}

# --- What `use S` loads: every codelet family / script package with a run sub -----------
{
  no strict 'refs';
  my @fams = sort grep { s/::$// and defined &{"Seqsee::SCF::${_}::run"} } keys %Seqsee::SCF::;
  record( case => 'families', families => \@fams );
}

# --- SanityCheck() on a consistent workspace --------------------------------------------
{
  my @e = init( 1, 2, 3, 4, 5, 6 );
  my $g = gp( @e[ 0 .. 2 ] );
  $g->describe_as($S::ASCENDING);
  SWorkspace->add_group($g);
  my $r = reln( $e[3], $e[4], 'succ' );
  $r->insert;
  record( case => 'all_clean', groups => scalar( my @gps = SWorkspace::GetGroups() ), %{ check() } );
  record( case => 'group_clean', %{ check($g) } );
  record( case => 'relation_clean', %{ check($r) } );
  record( case => 'element_clean', %{ check( $e[0] ) } );

  # Element variant: a non-ref binding (an element's categories come from init).
  $e[1]->describe_as($S::ASCENDING);
  my $b = $e[1]->GetBindingForCategory($S::ASCENDING);
  $b->get_bindings_ref()->{start} = 7;
  delete $b->get_bindings_ref()->{$_} for grep { $_ ne 'start' } keys %{ $b->get_bindings_ref };
  record( case => 'element_nonref', %{ check( $e[1] ) } );
  with_codelet();
  record( case => 'element_nonref_codelet', %{ check( $e[1] ) } );
  $Global::CurrentRunnableString = 'Seqsee::SCF::FocusOn';
  $Global::Steps_Finished        = 17;
  record( case => 'element_nonref_steps', %{ check( $e[1] ) } );
}

# --- Anchored variant failures ----------------------------------------------------------
{
  my @e = init( 1, 2, 3, 4, 5, 6 );
  with_codelet();
  my $g = gp( @e[ 0 .. 2 ] );
  record( case => 'group_no_category_count', cats => scalar( @{ $g->get_categories } ) );
  # Categories via create? record whatever create gave.
  $g->remove_category($_) for @{ $g->get_categories };
  record( case => 'group_no_category', %{ check($g) } );

  $g->describe_as($S::ASCENDING);
  my $b = $g->GetBindingForCategory($S::ASCENDING)->get_bindings_ref;
  record( case => 'group_binding_keys', keys => [ sort keys %$b ] );
  $b->{$_} = 3 for keys %$b;
  delete $b->{$_} for grep { $_ ne 'start' } keys %$b;
  record( case => 'group_nonref', %{ check($g) } );
  $g->remove_category($S::ASCENDING);
  $g->describe_as($S::ASCENDING);

  $g->set_edges( 0, 6 );
  record( case => 'edge_right', %{ check($g) } );
  $g->set_edges( 2, 1 );
  record( case => 'edge_order', %{ check($g) } );
  $g->set_edges( -1, 2 );
  record( case => 'edge_left', %{ check($g) } );
  $g->set_edges( 0, 2 );
  record( case => 'edges_restored', %{ check($g) } );

  # Holes: move a subgroup.
  my $a  = gp( @e[ 0, 1 ] );
  my $bb = gp( @e[ 2, 3 ] );
  my $gg = gp( $a, $bb );
  $gg->describe_as($S::SAMENESS) unless @{ $gg->get_categories };
  record( case => 'gg_cats', cats => [ map { $_->get_name } @{ $gg->get_categories } ] );
  record( case => 'gg_clean', %{ check($gg) } );
  $bb->set_edges( 3, 3 );
  record( case => 'holes', %{ check($gg) } );
  $bb->set_edges( 2, 3 );

  # A metonym as a part.
  $a->set_is_a_metonym($a);
  record( case => 'metonym_part_self', %{ check($gg) } );
  $a->set_is_a_metonym(undef);

  # An unanchored part: the holes check dies first.
  push @{ $gg->get_parts_ref }, 5;
  record( case => 'unanchored_scalar', %{ check($gg) } );
  pop @{ $gg->get_parts_ref };
  push @{ $gg->get_parts_ref }, Seqsee::Object->create( 7, 8 );
  record( case => 'unanchored_object', %{ check($gg) } );
  pop @{ $gg->get_parts_ref };

  # Underlying rule app out of sync, reached through the Anchored variant.
  my $r = reln( $e[0], $e[1], 'succ' );
  $r->insert;
  my $g2 = gp( @e[ 0 .. 2 ] );
  $g2->describe_as($S::ASCENDING);
  $g2->set_underlying_ruleapp($r);
  my $ra = $g2->get_underlying_reln;
  record( case => 'ruleapp_items', count => scalar( @{ $ra->get_items } ) );
  record( case => 'ruleapp_clean', %{ check($g2) } );
  pop @{ $ra->get_items };
  record( case => 'ruleapp_out_of_sync', %{ check($g2) } );
}

# --- SRelation variant ------------------------------------------------------------------
{
  my @e = init( 1, 2, 3, 4, 5, 6 );
  with_codelet();
  my $r = reln( $e[2], $e[1], 'pred' );
  record( case => 'relation_leftward', %{ check($r) } );
  my $r2 = reln( $e[1], $e[2], 'succ' );
  $e[2]->set_is_a_metonym( $e[1] );
  record( case => 'relation_metonymed_end', %{ check($r2) } );
  $e[2]->set_is_a_metonym( $e[2] );
  record( case => 'relation_self_metonym', %{ check($r2) } );
  $e[2]->set_is_a_metonym(undef);

  # SanityCheck() reaches relations through %SWorkspace::relations.
  my $r3 = reln( $e[3], $e[4], 'succ' );
  $r3->insert;
  $e[4]->set_is_a_metonym( $e[3] );
  record( case => 'all_relation_bad', %{ check() } );
  $e[4]->set_is_a_metonym(undef);

  # ... and groups through GetGroups.
  my $g = gp( @e[ 0 .. 2 ] );
  $g->describe_as($S::ASCENDING);
  SWorkspace->add_group($g);
  record( case => 'all_clean2', %{ check() } );
  $g->set_edges( 0, 9 );
  record( case => 'all_group_bad', %{ check() } );
}

# --- Dispatch -------------------------------------------------------------------------
{
  init( 1, 2, 3 );
  for my $arg ( [ 'number', 3 ], [ 'string', 'x' ], [ 'undef', undef ] ) {
    record( case => "dispatch_$arg->[0]", %{ check( $arg->[1] ) } );
  }
}

emit();
