package Seqsee::SCF::LookForSimilarGroups;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [ group => { required => 1 } ],
  body       => sub {
    my ($group) = @_;
    my $wset = SWorkspace::__GetObjectsBelongingToSimilarCategories($group);
    return if $wset->is_empty();

    for ( $wset->choose_a_few_nonzero(3) ) {
      SCodelet->new( 'FocusOn', 50, { what => $_ } )->schedule();
    }

  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::MergeGroups;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [ a => { required => 1 }, b => { required => 1 } ],
  body       => sub {
    my ( $a, $b ) = @_;
    return if $a eq $b;
    SWorkspace::__CheckLiveness( $a, $b ) or return;
    my @items = SUtil::uniq( @$a, @$b );
    @items = SWorkspace::__SortLtoRByLeftEdge(@items);

    return if SWorkspace::__AreThereHolesOrOverlap(@items);
    my $new_group;
    
       eval { 
      my @unstarred_items = map { $_->GetConcreteObject() } @items;
      ### require: SWorkspace::__CheckLivenessAtSomePoint(@unstarred_items)
      SWorkspace::__CheckLiveness(@unstarred_items) or return;   # dead objects.
      $new_group = Seqsee::Anchored->create(@unstarred_items);
      if ( $new_group and $a->get_underlying_reln() ) {
        $new_group->set_underlying_ruleapp(
          $a->get_underlying_reln()->get_rule() );
        $a->CopyCategoriesTo($new_group);
        SWorkspace->add_group($new_group);
      }
     };
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::CleanUpGroup;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [ group => { required => 1 } ],
  body       => sub {
    my ($group) = @_;
    return unless SWorkspace::__CheckLiveness($group);
    my @edges           = $group->get_edges();
    my @potential_cruft = SWorkspace::__GetObjectsWithEndsNotBeyond(@edges);
    SWorkspace::__DeleteNonSubgroupsOfFrom(
      {
        of   => [$group],
        from => \@potential_cruft,
      }
    );
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::DoTheSameThing;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
Codelet_Family(
  attributes => [
    group     => { default  => 0 },
    category  => { default  => 0 },
    direction => { default  => 0 },
    transform => { required => 1 }
  ],
  body => sub {
    my ( $group, $category, $direction, $transform ) = @_;
    unless ( $group or $category ) {
      $category = $transform->get_category();
    }
    if ( $group and $category ) {
      confess "Need exactly one of group and category: got both.";
    }
    $direction ||= SChoose->choose( [ 1, 1 ], [ $DIR::LEFT, $DIR::RIGHT ] );
    unless ($group) {
      my @groups_of_cat = SWorkspace::__GetObjectsBelongingToCategory($category)
      or return;
      $group = SWorkspace::__ChooseByStrength(@groups_of_cat);
    }

#main::message("DoTheSameThing: group=" . $group->as_text()." transform=".$transform->as_text());
    my $effective_transform =
    $direction eq $DIR::RIGHT ? $transform :$transform->FlippedVersion();
    $effective_transform or return;
    $effective_transform->CheckSanity() or confess "Mapping insane!";

    my $expected_next_object;

    # BandAid: The following occasionally crashes.
    eval {
      $expected_next_object = ApplyMapping( $effective_transform, $group );
    } or return;
    @$expected_next_object or return;

    my $next_pos = SWorkspace::__GetPositionInDirectionAtDistance(
      {
        from_object => $group,
        direction   => $direction,
        distance    => DISTANCE::Zero(),
      }
    );
    return if ( !defined($next_pos) or $next_pos > $SWorkspace::ElementCount );

    my $is_this_what_is_present;
    eval {       $is_this_what_is_present = SWorkspace->check_at_location(
        {
          start     => $next_pos,
          direction => $direction,
          what      => $expected_next_object,
        }
      );
    };
    
    my $e;
    if ( $e = Exception::Class->caught('SErr::ElementsBeyondKnownSought') ) {
      # Ignore.
    }
    elsif($e = Exception::Class->caught()) {
      ref $e ? $e->rethrow : die $e;
    }

    if ($is_this_what_is_present) {
      my $plonk_result =
      __PlonkIntoPlace( $next_pos, $direction, $expected_next_object );
      return unless $plonk_result->PlonkWasSuccessful();
      my $wso = $plonk_result->resultant_object() or return;

      $wso->describe_as( $effective_transform->get_category() );
      my @ends =
      ( $direction eq $DIR::RIGHT ) ? ( $group, $wso ) :( $wso, $group );
      SRelation->new(
        { first => $ends[0], second => $ends[1], type => $transform } )
      ->insert();

      #main::message("yeah, that was present!");
    }
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::CreateGroup;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [
    items     => { required => 1 },
    category  => { default  => 0 },
    transform => { default  => 0 }
  ],
  body => sub {
    my ( $items, $category, $transform ) = @_;
    my ( @left_edges, @right_edges );
    for (@$items) {
      push @left_edges,  $_->get_left_edge;
      push @right_edges, $_->get_right_edge;
    }
    my $left_edge  = List::Util::min(@left_edges);
    my $right_edge = List::Util::max(@right_edges);
    my $is_covering =
    scalar( SWorkspace::__GetObjectsWithEndsBeyond( $left_edge, $right_edge ) );
    return if $is_covering;

    unless ( $category or $transform ) {
      confess "At least one of category or transform needed. Got neither.";
    }
    if ( $category and $transform ) {
      confess "Exactly one of  category or transform needed. Got both.";
    }

    unless ($category) {

      # Generate from transform.
      confess "transform should be a Mapping!"
      unless $transform->isa('Mapping');
      if ( $transform->isa('Mapping::Numeric') ) {
        $category = $transform->GetRelationBasedCategory();
      }
      else {
        $category = SCategory::MappingBased->Create($transform);
      }
    }

    my @unstarred_items = map { $_->GetConcreteObject() } @$items;
    ### require: SWorkspace::__CheckLivenessAtSomePoint(@unstarred_items)
    SWorkspace::__CheckLiveness(@unstarred_items) or return;    # dead objects.
    my $new_group;
    
    $new_group = Seqsee::Anchored->create(@unstarred_items);
    return unless $new_group;
    $new_group->describe_as($category) or return;
    if ($transform) {
      $new_group->set_underlying_ruleapp($transform);
    }
    SWorkspace->add_group($new_group);
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::FindIfRelatedRelations;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
multimethod 'FindMapping';
multimethod 'ApplyMapping';

Codelet_Family(
  attributes => [ a => { required => 1 }, b => { required => 1 } ],
  body       => sub {
    my ( $a, $b ) = @_;
    my ( $af, $as, $bf, $bs ) = ( $a->get_ends(), $b->get_ends() );
    if ( $bs eq $af ) {

      # Switch the two...
      ( $af, $as, $a, $bf, $bs, $b ) = ( $bf, $bs, $b, $af, $as, $a );
    }

    return unless $as eq $bf;

    my ( $a_transform, $b_transform ) = ( $a->get_type(), $b->get_type() );
    if ( $a_transform eq $b_transform ) {
      SCodelet->new(
        'CreateGroup',
        100,
        {
          items     => [ $af, $as, $bs ],
          transform => $a_transform,
        }
      )->schedule();
    }
    elsif ( $Global::Feature{Alternating}
      and $a_transform->get_category() eq $b_transform->get_category() )
    {

      # There is a chance that these are somehow alternating...
      my $new_transform = SCategory::Alternating->CheckForAlternation(

        # $a_transform->get_category(),
        $af, $as, $bs
      );
      if ($new_transform) {
        SCodelet->new(
          'CreateGroup',
          100,
          {
            items     => [ $af, $as, $bs ],
            transform => $new_transform
          }
        )->schedule();
      }
    }

  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::CheckIfAlternating;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
multimethod 'FindMapping';
multimethod 'ApplyMapping';

Codelet_Family(
  attributes => [
    first  => { required => 1 },
    second => { required => 1 },
    third  => { required => 1 }
  ],
  body => sub {
    my ( $first, $second, $third ) = @_;
    my $transform_to_consider;

    my $t1 = FindMapping( $first,  $second );
    my $t2 = FindMapping( $second, $third );
    if ( $t1 and $t1 eq $t2 ) {
      $transform_to_consider = $t1;
    }
    else {
      $transform_to_consider =
      SCategory::Alternating->CheckForAlternation( $first, $second, $third )
      or return;
    }
    SCodelet->new(
      'CreateGroup',
      100,
      {
        items     => [ $first, $second, $third ],
        transform => $transform_to_consider
      }
    )->schedule();
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::FindIfRelated;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
multimethod 'FindMapping';
multimethod 'ApplyMapping';

Codelet_Family(
  attributes => [ a => { required => 1 }, b => { required => 1 } ],
  body       => sub {
    my ( $a, $b ) = @_;
    return unless SWorkspace::__CheckLiveness( $a, $b );
    ( $a, $b ) = SWorkspace::__SortLtoRByLeftEdge( $a, $b );
    if ( $a->overlaps($b) ) {
      my ( $ul_a, $ul_b ) =
      ( $a->get_underlying_reln(), $b->get_underlying_reln() );
      return unless ( $ul_a and $ul_b );
      return unless $ul_a->get_rule() eq $ul_b->get_rule();
      return unless ( $a->[-1] ~~ @$b );    #i.e., actual subgroups overlap.
      SCodelet->new( 'MergeGroups', 200, { a => $a, b => $b } )->schedule();
      return;
    }

    if ( my $relation = $a->get_relation($b) ) {
      SLTM::SpikeBy( 10, $relation->get_type() );
      SCodelet->new( 'FocusOn', 100, { what => $relation } )->schedule();
      return;
    }

    my $reln_type = FindMapping( $a, $b ) || return;
    SLTM::SpikeBy( 10, $reln_type );

    # insert relation with certain probability:
    my $transform_complexity = $reln_type->get_complexity();
    my $transform_activation =
    SLTM::GetRealActivationsForOneConcept($reln_type);
    my $distance =
    SWorkspace::__FindDistance( $a, $b, $DISTANCE_MODE::ELEMENT )
    ->GetMagnitude();
    my $sense_in_continuing =
    ShouldIContinue( $transform_complexity, $transform_activation, $distance );
    main::message("Sense in continuing=$sense_in_continuing")
    if $Global::debugMAX;
    return unless SUtil::toss($sense_in_continuing);

    SLTM::SpikeBy( 10, $reln_type );
    my $relation = SRelation->new(
      {
        first  => $a,
        second => $b,
        type   => $reln_type
      }
    );
    $relation->insert();
    SCodelet->new( 'FocusOn', 200, { what => $relation } )->schedule();
  }
);

sub ShouldIContinue {
  my ( $transform_complexity, $transform_activation, $distance ) = @_;

  # transform_activation and transform_complexity are between 0 and 1

  my $not_continue =
  $transform_complexity * ( 1 - $transform_activation ) * sqrt($distance);
  return 1 - $not_continue;
}

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::AttemptExtensionOfRelation;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
multimethod '__PlonkIntoPlace';
multimethod 'SanityCheck';
multimethod 'FindMapping';
multimethod 'ApplyMapping';

Codelet_Family(
  attributes => [ core => { required => 1 }, direction => { required => 1 } ],
  body       => sub {
    my ( $core, $direction ) = @_;
    ## Codelet started:
    my $transform = $core->get_type();
    my ( $end1, $end2 ) = $core->get_ends();
    ## ends: $end1->as_text, $end2->as_text
    my ( $effective_transform, $object_at_end );
    if ( $direction eq $DIR::RIGHT ) {
      ( $effective_transform, $object_at_end ) = ( $transform, $end2 );
      ## Thought it was right:
    }
    else {
      $effective_transform = $transform->FlippedVersion() or return;
      $object_at_end = $end1;
    }

    my $distance = SWorkspace::__FindDistance( $end1, $end2 );
    ## oae_l: $object_at_end->get_left_edge(), $distance, $direction
    my $next_pos = SWorkspace::__GetPositionInDirectionAtDistance(
      {
        from_object => $object_at_end,
        direction   => $direction,
        distance    => $distance,
      }
    );
    ## next_pos: $next_pos
    return unless defined($next_pos);
    ## distance, next_pos: $distance, $next_pos
    return if ( !defined($next_pos) or $next_pos > $SWorkspace::ElementCount );

    my $what_next =
    ApplyMapping( $effective_transform, $object_at_end->GetEffectiveObject() );
    return unless $what_next;
    return unless @$what_next;    # 0 elts also not okay

    my $is_this_what_is_present;
    
       eval { 
      $is_this_what_is_present = SWorkspace->check_at_location(
        {
          start     => $next_pos,
          direction => $direction,
          what      => $what_next
        }
      );
     };
       if (my $err = $EVAL_ERROR) {
          CATCH_BLOCK: { if (UNIVERSAL::isa($err, 'SErr::ElementsBeyondKnownSought')) { 
        return unless EstimateAskability( $core, $transform, $end1, $end2 );
        SCodelet->new(
          'AskIfThisIsTheContinuation',
          100,
          {
            relation         => $core,
            exception        => $err,
            expected_object  => $what_next,
            start_position   => $next_pos,
            known_term_count => $SWorkspace::ElementCount,
          }
        )->schedule();
      ; last CATCH_BLOCK; }die $err }
       }
    ;

    ## is_this_what_is_present:
    if ($is_this_what_is_present) {
      SLTM::SpikeBy( 10, $transform );

      my $plonk_result = __PlonkIntoPlace( $next_pos, $direction, $what_next );
      return unless $plonk_result->PlonkWasSuccessful();
      my $wso = $plonk_result->resultant_object();

      my $cat = $transform->get_category();
      SLTM::SpikeBy( 10, $cat );
      $wso->describe_as($cat) or return;

      my $reln_to_add;
      if ( $direction eq $DIR::RIGHT ) {
        $reln_to_add = SRelation->new(
          {
            first  => $end2,
            second => $wso,
            type   => $transform,
          }
        );
      }
      else {
        $reln_to_add = SRelation->new(
          {
            first  => $wso,
            second => $end1,
            type   => $transform,
          }
        );

      }
      $reln_to_add->insert() if $reln_to_add;
      ## HERE1:
      # SanityCheck($reln_to_add);
      ## Here2:
    }
  }
);

sub EstimateAskability {
  my ( $relation, $transform, $end1, $end2 ) = @_;
  if ( SWorkspace->AreThereAnySuperSuperGroups($end1)
    or SWorkspace->AreThereAnySuperSuperGroups($end2) )
  {
    return 0;
  }

  my $supergroup_penalty = 0;
  if ( SWorkspace->GetSuperGroups($end1) or SWorkspace->GetSuperGroups($end2) )
  {
    $supergroup_penalty = 0.6;
  }

  my $transform_activation = SLTM::GetRealActivationsForOneConcept($transform);
  return SUtil::toss( $transform_activation * ( 1 - $supergroup_penalty ) );
}

__PACKAGE__->meta->make_immutable;

1;
