package SThought::Seqsee::Anchored;
use Moose;
extends 'SThought';
use Carp;
use Smart::Comments;
use English qw(-no_match_vars);
use Class::Multimethods;
use List::Util qw{min max};

our @actions_ret;
our $NAME = 'Focusing on a Group';

multimethod get_fringe_for => ('Seqsee::Anchored') => sub {
  my ($core) = @_;
  my @ret;

  my $structure = $core->get_structure();
  push @ret, [ $core->get_pure(), 100 ];

  if ( my $rel = $core->get_underlying_reln() ) {
    push @ret, [ $rel->get_rule()->get_transform, 50 ];
  }

  for my $category ( @{ $core->get_categories() } ) {

    # next if $category eq $S::RELN_BASED;
    push @ret, [ $category, 100 ];
    SLTM::SpikeBy( 5, $category );

    my $bindings  = $core->GetBindingForCategory($category);
    my $meto_mode = $bindings->get_metonymy_mode();
    if ( $meto_mode ne $METO_MODE::NONE ) {
      push @ret, [ $bindings->get_position(), 100 ];
      push @ret, [ $meto_mode, 100 ];
      push @ret, [ $bindings->get_metonymy_type(), 100 ];
    }
  }

  return \@ret;
};

sub StrengthenLink {
  my ( $o1, $o2 ) = @_;
  my $relation = $o1->get_relation($o2) || return;
  my $category = $relation->get_type()->get_category();
  SLTM::InsertISALink( $o1, $category )->Spike(10);
  SLTM::InsertISALink( $o2, $category )->Spike(10);
  SLTM::InsertFollowsLink( $category, $relation )->Spike(15);
}

sub ExtendFromMemory {
  my ($core) = @_;
  my @actions_ret;
  my $flush_right = $core->IsFlushRight();
  my $flush_left  = $core->IsFlushLeft();

  my $weighted_set = SLTM::FindActiveFollowers($core);
  return unless $weighted_set->is_not_empty();
  my $chosen_follower = $weighted_set->choose();

  if ( $flush_right and ( $flush_left or $SWorkspace::ElementCount <= 3 ) ) {
    my $exception =
    SErr::ElementsBeyondKnownSought->new(
      next_elements => $chosen_follower->get_flattened() );
    $exception->Ask();
    return;
  }
  elsif ( $Global::Feature{LTM_expt} ) {
    my $next = SWorkspace->GetSomethingLike(
      {
        object      => $chosen_follower,
        start       => $core->get_right_edge() + 1,
        direction   => $DIR::RIGHT,
        trust_level => 50,
      }
    ) || return;
    push @actions_ret,
    SCodelet->new( "FindIfRelated", 1000, { a => $core, b => $next } );
    return @actions_ret;
  }
}

sub AddCategoriesFromMemory {
  my ($core) = @_;
  my @actions_ret;
  my $weighted_set = SLTM::FindActiveCategories($core);
  $weighted_set->delete_below_threshold(0.3);
  if ( $weighted_set->is_not_empty() ) {
    my $category = $weighted_set->choose();
    push @actions_ret,
    SCodelet->new( "CheckIfInstance", 100, { obj => $core, cat => $category } );
  }
  return @actions_ret;
}

sub IsThisAMountainUpslope {
  my ($core) = @_;
  return 0 unless $core->is_of_category_p($S::ASCENDING);
  my @parts = @$core;
  return 0 if scalar(@parts) == 1;
  my $last_part             = $parts[-1];
  my $left_end_of_last_part = $last_part->get_left_edge();
  my @groups_starting_here =
  SWorkspace::__GetObjectsWithEndsExactly( $left_end_of_last_part, undef );
  my @possible_downslopes = grep {
        $_->is_of_category_p($S::DESCENDING)
    and scalar(@$_) == scalar(@parts)
    and $_->[0] eq $last_part
  } @groups_starting_here;
  return 0 unless @possible_downslopes;
  return $possible_downslopes[0];    # There can only be 1.
}

sub get_fringe {
  my ($self) = @_;
  return get_fringe_for( $self->core()->GetEffectiveObject() );
}

sub get_actions {
  my ($self) = @_;
  my $core = $self->core();
  our @actions_ret = ();
  SLTM::SpikeBy( 10, $core ) if $Global::Feature{LTM};

  my $metonym            = $core->get_metonym();
  my $metonym_activeness = $core->get_metonym_activeness();
  my $strength           = $core->get_strength();
  my $flush_right        = $core->IsFlushRight();
  my $flush_left         = $core->IsFlushLeft();
  my $span_fraction      = $core->get_span() / $SWorkspace::ElementCount;
  my $underlying_reln    = $core->get_underlying_reln();
  my $parts_count        = scalar(@$core);

  # extendibility checking...
  #if ( $flush_right and not($flush_left) ) {
  #    next unless SUtil::toss(0.15);
  #}

  if ( $flush_left or SUtil::toss(0.8) ) {
    push @actions_ret,
    SCodelet->new(
      "AttemptExtensionOfGroup",
      80,
      {
        object    => $core,
        direction => DIR::RIGHT(),
      }
    );
  }

  if ( !$flush_left ) {
    push @actions_ret,
    SCodelet->new(
      "AttemptExtensionOfGroup",
      150,
      {
        object    => $core,
        direction => DIR::LEFT(),
      }
    );
  }

  if ( scalar(@$core) > 1 and SUtil::toss(0.8) ) {

    if ( SUtil::toss(0.5) ) {
      my $urgency =
      ( $Global::Feature{AllowSquinting}
        and not $core->[-1]->get_metonym_activeness ) ? 200 :10;

      #main::message("Will launch ConvulseEnd");
      push @actions_ret,
      SCodelet->new(
        "ConvulseEnd",
        $urgency,
        {
          object    => $core,
          direction => $DIR::RIGHT,
        }
      );
    }
    else {
      my $urgency =
      ( $Global::Feature{AllowSquinting}
        and not $core->[0]->get_metonym_activeness ) ? 200 :10;

      #main::message("Will launch ConvulseEnd");
      push @actions_ret,
      SCodelet->new(
        "ConvulseEnd",
        $urgency,
        {
          object    => $core,
          direction => $DIR::LEFT,
        }
      );
    }
  }

  if ( $Global::Feature{LTM} ) {

    # Spread activation from corresponding node:
    SLTM::SpreadActivationFrom( SLTM::GetMemoryIndex($core) );
    push @actions_ret, ExtendFromMemory($core);

    push @actions_ret, AddCategoriesFromMemory($core);
  }

  my $poss_cat;
  my $first_reln = $core->[0]->get_relation( $core->[1] );
  $poss_cat = $first_reln->SuggestCategory() if $first_reln;
  if ($poss_cat) {
    my $is_inst = $core->is_of_category_p($poss_cat);

    # main::message("$core is of $poss_cat? '$is_inst'");
    unless ($is_inst) {    #XXX if it already known, skip!
      push @actions_ret,
      SCodelet->new(
        "CheckIfInstance",
        500,
        {
          obj => $core,
          cat => $poss_cat
        }
      );
    }

  }

  my $possible_category_for_ends;
  $possible_category_for_ends = $first_reln->SuggestCategoryForEnds()
  if $first_reln;
  if ($possible_category_for_ends) {
    for ( @{ $core->get_underlying_reln()->get_items() } ) {
      unless ( UNIVERSAL::isa( $_, "Seqsee::Anchored" ) ) {
        print "An item of an Seqsee::Anchored object($core) is not anchored!\n";
        print "The anchored object is ", $core->get_structure_string(), "\n";
        print "Its items are: ", join( "; ", @$core );
        print "Items of the underlying ruleapp are: ",
        join( "; ", @{ $core->get_underlying_reln()->get_items() } );
        confess "$_ is not anchored!";
      }
      my $is_inst = $_->is_of_category_p($possible_category_for_ends);
      unless ($is_inst) {
        push @actions_ret,
        SCodelet->new(
          "CheckIfInstance",
          100,
          {
            obj => $_,
            cat => $possible_category_for_ends
          }
        );
      }
    }
  }

  if ( $span_fraction > 0.5 ) {
    push @actions_ret, SCodelet->new( "LargeGroup", 100, { group => $core } );
  }

  if ( $Global::Feature{LTM} ) {
    if ( $parts_count >= 3 ) {
      for ( 0 .. $parts_count - 2 ) {
        StrengthenLink( $core->[$_], $core->[ $_ + 1 ] );
      }
    }
  }

  my @categories = @{ $core->get_categories() };
  my $category = SLTM::SpikeAndChoose( 0, @categories );
  if ( $category and SUtil::toss( 0.5 * SLTM::SpikeBy( 5, $category ) ) ) {
    push @actions_ret, SCodelet->new( "FocusOn", 100, { what => $category } );
  }

  push @actions_ret,
  SCodelet->new( "LookForSimilarGroups", 20, { group => $core } );
  push @actions_ret, SCodelet->new( "CleanUpGroup", 20, { group => $core } );

  if ($underlying_reln) {
    my $transform = $underlying_reln->get_rule()->get_transform();
    if ( $transform->isa('Mapping::Structural')
      and not( $transform->get_category()->isa('SCategory::Interlaced') ) )
    {
      my $urgency = 30 * SLTM::GetRealActivationsForOneConcept($transform);
      push @actions_ret,
      SCodelet->new( "DoTheSameThing", 100, { transform => $transform } );
    }
  }

  if ( my $downslope = IsThisAMountainUpslope($core) ) {
    IFUPSLOPE: {
      my @upslope   = @$core;
      my @downslope = @$downslope;
      shift(@downslope);
      my $mountain_activation = SLTM::SpikeBy( 20, $S::MOUNTAIN );
      last IFUPSLOPE
      unless ( SUtil::significant($mountain_activation)
        and SUtil::toss($mountain_activation) );
      push @actions_ret,
      SCodelet->new(
        "CreateGroup",
        100,
        {
          items    => [ @upslope, @downslope ],
          category => $S::MOUNTAIN,
        }
      );
    }
  }

  if ( $Global::Feature{AllowSquinting} and $core->IsThisAMetonymedObject() ) {

  }

  if ( $Global::Feature{Alternating} ) {

# Look for adjacent objects. If all 3 belong to the some common category, check for
# alternatingness.
    my $left_neighbour = SWorkspace::__ChooseByStrength(
      SWorkspace::__GetObjectsWithEndsExactly(
        undef, $core->get_left_edge() - 1
      )
    );
    my $right_neighbour = SWorkspace::__ChooseByStrength(
      SWorkspace::__GetObjectsWithEndsExactly(
        $core->get_right_edge() + 1, undef
      )
    );
    if ( $left_neighbour and $right_neighbour ) {
      if ( my ($cat) =
        $core->get_common_categories( $left_neighbour, $right_neighbour ) )
      {
        push @actions_ret,
        SCodelet->new(
          "CheckIfAlternating",
          100,
          {
            first  => $left_neighbour,
            second => $core,
            third  => $right_neighbour,
          }
        );
      }
    }
  }
  return @actions_ret;
}

sub as_text {
  my $self = shift;
  return "Group " . $self->core()->as_text;
}

package SThought::Seqsee::Element;
use Moose;
extends 'SThought';
use Carp;
use Smart::Comments;
use English qw(-no_match_vars);
use Class::Multimethods;
use List::Util qw{min max};

our @actions_ret;
our $NAME = 'Focusing on a Single Element';

has magnitude => (
  is         => 'rw',
  lazy_build => 1,
);

sub _build_magnitude() {
  $_[0]->core->get_mag();
}

multimethod get_fringe_for => ('Seqsee::Element') => sub {
  my ($core) = @_;
  my $mag = $core->get_mag();
  my @ret;

  for ( @{ $core->get_categories() } ) {
    next if $_ eq $S::NUMBER;
    SLTM::SpikeBy( 10, $_ );
    push @ret, [ $_, 80 ];

    if ( $_ eq $S::PRIME ) {
      my $next = SCategory::Prime::NextPrime($mag);
      my $prev = SCategory::Prime::PreviousPrime($mag);
      if ($next) {
        my $plat = SLTM::Platonic->create($next);
        push @ret, [ $plat, 100 ];
      }
      if ($prev) {
        my $plat = SLTM::Platonic->create($prev);
        push @ret, [ $plat, 100 ];
      }
    }
  }

  my @literal_cats = map { SLTM::Platonic->create( $mag + $_ ) } ( 0, 1, -1 );
  push @ret, [ $literal_cats[0],  100 ];
  push @ret, [ $literal_cats[1],  30 ];
  push @ret, [ $literal_cats[-1], 30 ];

  my $pos     = $core->get_left_edge();
  my $abs_pos = "absolute_position_" . $pos;
  push @ret, [ $abs_pos, 80 ];
  my $prev_abs_pos = "absolute_position_" . ( $pos - 1 );
  my $next_abs_pos = "absolute_position_" . ( $pos + 1 );
  push @ret, [ $prev_abs_pos, 20 ];
  push @ret, [ $next_abs_pos, 20 ];
  return \@ret;
};

sub get_fringe {
  my ($self) = @_;
  return get_fringe_for( $self->core );
}

sub get_actions {
  my ($self)    = @_;
  my $core      = $self->core();
  my $magnitude = $self->magnitude();
  our @actions_ret = ();

  SLTM::SpikeBy( 10, $core ) if $Global::Feature{LTM};

  if ( $Global::Feature{LTM} ) {

    # Spread activation from corresponding node:
    SLTM::SpreadActivationFrom( SLTM::GetMemoryIndex($core) );
    SThought::Seqsee::Anchored::ExtendFromMemory($core);

    SThought::Seqsee::Anchored::AddCategoriesFromMemory($core);
  }

  return @actions_ret;
}

sub as_text {
  my $self = shift;
  return "Element " . $self->core()->as_text();
}

1;
