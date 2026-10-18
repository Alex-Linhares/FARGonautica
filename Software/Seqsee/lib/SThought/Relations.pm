package SThought::SRelation;
use Moose;
extends 'SThought';
use Carp;
use Smart::Comments;
use English qw(-no_match_vars);
use Class::Multimethods;
use List::Util qw{min max};

our @actions_ret;
our $NAME = 'Focusing on an Analogy';

multimethod 'createRule';

sub get_fringe {
  my ($self) = @_;
  my $core = $self->core() or confess "Core is empty!";
  my @ret;
  push @ret, [ $core->get_type(),   100 ];
  push @ret, [ $core->get_first(),  50 ];
  push @ret, [ $core->get_second(), 50 ];
  return \@ret;
}

sub get_actions {
  my ($self) = @_;
  my $core = $self->core();
  our @actions_ret = ();
  my ( $end1,        $end2 )         = $core->get_ends;
  my ( $extent_left, $extent_right ) = $core->get_extent;
  my $relntype                = $core->get_type();
  my $relationtype_activation = SLTM::SpikeBy( 5, $relntype );
  my $are_ends_contiguous     = $core->are_ends_contiguous();

  if (  $are_ends_contiguous
    and $relntype->IsEffectivelyASamenessRelation() )
  {
    push @actions_ret,
    SCodelet->new(
      "AreTheseGroupable",
      100,
      {
        items => [ $end1, $end2 ],
        reln  => $core,
      }
    );
  }
  elsif ( $are_ends_contiguous
    and
    not SWorkspace::__GetObjectsWithEndsBeyond( $extent_left, $extent_right ) )
  {
    push @actions_ret,
    SCodelet->new(
      "AreTheseGroupable",
      80,
      {
        items => [ $end1, $end2 ],
        reln  => $core,
      }
    );
  }

  push @actions_ret,
  SCodelet->new(
    "AttemptExtensionOfRelation",
    100,
    {
      core      => $core,
      direction => DIR::RIGHT()
    }
  );
  push @actions_ret,
  SCodelet->new(
    "AttemptExtensionOfRelation",
    100,
    {
      core      => $core,
      direction => DIR::LEFT()
    }
  );

# SLTM::InsertFollowsLink( $core->get_ends(), $core )->Spike(5) if $Global::Feature{LTM};

  my @ends = SWorkspace::__SortLtoRByLeftEdge( $end1, $end2 );
  my @intervening_objects =
  SWorkspace->get_intervening_objects( $ends[0]->get_right_edge + 1,
    $ends[1]->get_left_edge - 1 );
  my $distance_magnitude = scalar(@intervening_objects);
  if ($distance_magnitude) {
    my $possible_ad_hoc_cat =
    SCategory::Interlaced->Create( $distance_magnitude + 1 );
    my $ad_hoc_activation =
    SLTM::SpikeBy( 20 / $distance_magnitude, $possible_ad_hoc_cat );
    if (  SUtil::significant($ad_hoc_activation)
      and SUtil::toss($ad_hoc_activation)
      and!$Global::Feature{NoInterlaced} )
    {
      my @new_object_parts =
      SUtil::toss(0.5)
      ? ( $ends[0], @intervening_objects )
      :( @intervening_objects, $ends[1] );
      if (
        not SWorkspace::__GetObjectsWithEndsExactly(
          $new_object_parts[0]->get_left_edge(),
          $new_object_parts[-1]->get_right_edge()
        )
      )
      {
        my $new_obj = Seqsee::Anchored->create(@new_object_parts);
        SWorkspace->add_group($new_obj);
        $new_obj->describe_as($possible_ad_hoc_cat);
      }
    }
  }

  return @actions_ret;
}

sub as_text {
  my $self = shift;
  return $self->core()->as_text;
}

1;
