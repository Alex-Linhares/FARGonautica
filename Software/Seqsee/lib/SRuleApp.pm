package SRuleApp;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Carp;
use Smart::Comments;

use Class::Multimethods;
use Try::Tiny;

multimethod 'ApplyMapping';
multimethod 'FindMapping';

has rule => (
    is         => 'rw',
    reader     => 'get_rule',
    writer     => 'set_rule',
    init_arg   => 'rule',
    required   => 1,
);

has item => (
  traits => ['Array'],
  is        => 'ro',
  isa       => 'ArrayRef',
  default   => sub { [] },
  init_arg => 'items',
    writer => 'set_items',
    reader => 'get_items',
  handles => {
    'get_all_items' => 'elements',
    'push_item' => 'push',
    'unshift_item' => 'unshift'
  }
);

has direction => (
    is         => 'rw',
    reader     => 'get_direction',
    writer     => 'set_direction',
    init_arg   => 'direction',
    required   => 1,
    weak_ref   => 0,
);

sub BUILD {
  my $self = shift;
  if ($self->get_direction() ne $DIR::RIGHT) {
    confess "Expected direction to be right!";
  }
}
sub CheckConsitencyOfGroup {       #CHECK THIS CODE
  my ( $self, $group ) = @_;

  # If the ruleapp does not fully cover group, it is still consistent
  my ( $left,    $right )    = $self->get_edges();
  my ( $gp_left, $gp_right ) = $group->get_edges();
  return 1 unless ( $gp_left >= $left and $gp_right <= $right );
  return 1 if $group->get_underlying_reln() eq $self;

  # Reasons for consistency:
  # a: is an item.
  # b: is an item of the item etc.
  for my $item ( $self->get_all_items ) {
    return 1 if $group eq $item;    #Reason a
    return 1 if $item->HasAsPartDeep($group);    # Reason b
  }

  return 0;
}

sub FindExtension {
  my ( $self, $opts_ref ) = @_;

  my $rule      = $self->get_rule;
  my @items = $self->get_all_items;

  my $direction_to_extend_in = $opts_ref->{direction_to_extend_in}
  or confess "need direction_to_extend_in";
  my $skip_this_many_elements = $opts_ref->{skip_this_many_elements} || 0;

  return if $skip_this_many_elements >= scalar(@items);
  my ( $last_object, $relation_to_use );
  if ( $direction_to_extend_in eq $DIR::RIGHT ) {
    $last_object     = $items[ -1 - $skip_this_many_elements ];
    $relation_to_use = $rule->get_transform;
  }
  else {
    $last_object     = $items[$skip_this_many_elements];
    $relation_to_use = $rule->get_flipped_transform;
  }

  $relation_to_use // return;
  confess "Strange transform: $relation_to_use"
  unless UNIVERSAL::isa( $relation_to_use, 'Mapping' );

  my $next_pos = $last_object->get_next_pos_in_dir($direction_to_extend_in)
  // return;
  my $expected_next_object =
  ApplyMapping( $relation_to_use, $last_object->GetEffectiveObject() )
  or return;
  return unless @$expected_next_object;

  # XXX
  return SWorkspace->GetSomethingLike(    # This is crazy! Fix workflow.
    {
      object      => $expected_next_object,
      start       => $next_pos,
      direction   => $direction_to_extend_in,
      trust_level => 50 * $self->get_span() / ( $SWorkspace::ElementCount + 1 )
      ,                                   # !!
      reason => '',    # 'Extension attempted for: ' . $rule->as_text(),
      hilit_set => [@items],
    }
  );

}

sub _ExtendOneStep {
  my ($opts_ref) = @_;

  my $items_ref = $opts_ref->{items_ref} or confess "need items_ref";
  my $direction_to_extend_in = $opts_ref->{direction_to_extend_in}
  or confess "need direction_to_extend_in";
  my $object_at_end = $opts_ref->{object_at_end}
  or confess "need object_at_end";
  my $transform = $opts_ref->{transform} or confess "need transform";
  my $extend_at_start_or_end = $opts_ref->{extend_at_start_or_end}
  or confess "need extend_at_start_or_end";

  my $next_pos = $object_at_end->get_next_pos_in_dir($direction_to_extend_in)
  // return;
  my $next_object =
  ApplyMapping( $transform, $object_at_end->GetEffectiveObject() );

  my $is_this_what_is_present = SWorkspace->check_at_location(
    {
      start     => $next_pos,
      direction => $direction_to_extend_in,
      what      => $next_object
    }
  ) or return;

  my $plonk_result =
  __PlonkIntoPlace( $next_pos, $direction_to_extend_in, $next_object );
  confess "__PlonkIntoPlace failed. Shouldn't have, I think"
  unless $plonk_result->PlonkWasSuccessful();
  my $wso = $plonk_result->resultant_object();
  my $reln;
  given ($extend_at_start_or_end) {
    when ('end') {
      push @{$items_ref}, $wso;
      my $transform = FindMapping( $object_at_end, $wso ) or return;
      $reln = SRelation->new(
        { first => $object_at_end, second => $wso, type => $transform } );
    }
    when ('start') {
      unshift @{$items_ref}, $wso;
      my $transform = FindMapping( $wso, $object_at_end ) or return;
      $reln = SRelation->new(
        { first => $wso, second => $object_at_end, type => $transform } );
    }
    default {
      confess "Huh?";
    }
  }

  return unless $reln;
  $reln->insert();
  SLTM::SpikeBy( 200, $reln );
  return 1;
}

sub _ExtendSeveralSteps {
  my ( $self, $extend_at_start_or_end, $steps ) = @_;
  $steps //= 1;

  my $index_of_end = ( $extend_at_start_or_end eq 'end' ) ? -1 :0;

  my $direction_to_extend_in = $self->get_direction;
  my @items                  = $self->get_all_items;
  my $count                  = scalar(@items);
  my $rule                   = $self->get_rule;
  my $transform              = $rule->get_transform();

  for ( 1 .. $steps ) {
    my $current_end = $items[$index_of_end];
    my $success;

    eval { 
      $success = _ExtendOneStep(
        {
          items_ref              => \@items,
          direction_to_extend_in => $direction_to_extend_in,
          object_at_end          => $current_end,
          transform              => $transform,
          extend_at_start_or_end => $extend_at_start_or_end,
        }
      );
    };
    
    my $e;
    if ( $e = Exception::Class->caught('SErr::ElementsBeyondKnownSought') ) {
      my $trust_level =
      0.5 *
      List::Util::sum( map { $_->get_span() } @items ) /
      $SWorkspace::ElementCount;
      return unless SUtil::toss($trust_level);
      SCoderack->add_codelet(
        SCodelet->new(
          'MaybeAskTheseTerms',
          10000,
          {
            core      => $self,
            exception => $_,
          }
        )
      );
      return;   
    }
    elsif($e = Exception::Class->caught()) {
      ref $e ? $e->rethrow : die $e;
    }
    return unless $success;
  }

  $self->set_items([@items]);
  Global::UpdateGroupStrengthByConsistency();
  return 1;
}

sub ExtendForward {
  my ( $self, $steps ) = @_;
  $self->_ExtendSeveralSteps( 'end', $steps );
}

sub ExtendBackward {
  my ( $self, $steps ) = @_;
  $self->_ExtendSeveralSteps( 'start', $steps );
}

sub ExtendRight {
  my ( $self, $steps ) = @_;
  $self->ExtendForward($steps);
}

sub ExtendLeft {
  my ( $self, $steps ) = @_;
  $self->ExtendBackward($steps);
}

sub ExtendLeftMaximally {
  my ($self) = @_;
  while ( $self->ExtendLeft(1) ) {
  }
}

sub as_text {
  my ($self) = @_;
  return "SRuleApp $self";
}

sub get_span {
  my ($self) = @_;
  my ( $l, $r ) = $self->get_edges();
  return $r - $l + 1;
}

sub get_edges {
  my ($self) = @_;
  my @items  = $self->get_all_items;
  return ( $items[0]->get_left_edge(), $items[-1]->get_right_edge() );
}


__PACKAGE__->meta->make_immutable;
1;
