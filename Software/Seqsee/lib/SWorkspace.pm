#####################################################
#
#    Package: SWorkspace
#
#####################################################
#   manages the workspace
#####################################################

package SWorkspace;
use 5.10.0;
use strict;
use warnings;
use Carp;
use Carp::Source qw{source_cluck};
use Class::Std;
use Class::Multimethods;
use List::Util;
use SUtil;

use Smart::Comments '###';
use English qw(-no_match_vars);

use Sort::Key qw{rikeysort ikeysort};

# Seqsee packages. Fix.
use Seqsee::ResultOfGetConflicts;
use Seqsee::ResultOfAttributeCopy;
use Seqsee::ResultOfPlonk;
use Seqsee::ResultOfGetSomethingLike;

#=============================================
#=============================================
# NEW CODE
#=============================================

our $ElementCount = 0;
my @Elements;
my @ElementMagnitudes;
my %Objects;          # List of all objects.
my %NonEltObjects;    # Only groups (of size 2 or more)

my %LeftEdge_of;      # Maintains left edges of "registered" groups.
my %RightEdge_of;     # Likewise, right edges.
my %SuperGroups_of;   # Groups whose direct element is this group.
my %Span_of;          # Span.
my %LiveAtSomePoint;  # List of all live objects ever.

my @BarLines;
my $BarLineCount = 0;

our %relations;
our %relations_by_ends;    # keys: end1;end2 value:1 if a relation present.

sub GetElements {
  my ($package) = @_;
  return @Elements;
}

sub GetGroups {
  my ($package) = @_;
  return rikeysort { $Span_of{$_} } values %NonEltObjects;
}

sub GetSuperGroups {
  my ( $package, $group ) = @_;
  return values %{ $SuperGroups_of{$group} };
}

sub AreThereAnySuperSuperGroups {
  my ( $package, $group ) = @_;
  for ( values %{ $SuperGroups_of{$group} } ) {
    return 1 if %{ $SuperGroups_of{$_} };
  }
  return 0;
}

sub __Clear {
  $ElementCount   = 0;
  @Elements       = @ElementMagnitudes = %Objects = %NonEltObjects = ();
  %LeftEdge_of    = %RightEdge_of = ();
  %SuperGroups_of = %Span_of = ();
}

sub __CheckLiveness {
  return SUtil::all { exists $Objects{$_} } @_;
}

sub __CheckLivenessAtSomePoint {
  return SUtil::all { exists $LiveAtSomePoint{$_} } @_;
}

sub __CheckLivenessAndDiagnose {
  my $problems_so_far = 0;
  my $msg;
  for my $object (@_) {
    next if exists( $Objects{$object} );

    # So a dead object!
    $msg .= "NON_LIVE OBJECT: >>" . $object->as_text() . "<<\n";

    if ( not( defined $object ) ) {
      $msg .= "In fact, IT IS UNDEF!!\n";
    }
    else {
      if ( exists $LiveAtSomePoint{$object} ) {
        $msg .= "But was live once!\n";
      }
      my $unstarred = $object->GetConcreteObject();
      if ( $unstarred ne $object ) {
        $msg .= "A METONYM IS BEING CHECKED FOR LIVENESS!\n";
        if ( exists $Objects{$unstarred} ) {
          $msg .= "\tIts unstarred *is* live.\n";
        }
        else {
          $msg .= "\tEven its unstarred is non-live.\n";
        }
      }
    }
    $problems_so_far++;
  }
  if ($problems_so_far) {
    confess("Dying because of liveness issues!\n$msg");
  }
  return 1;
}

sub __GrepLiveness {
  grep { exists( $Objects{$_} ) } @_;
}

sub __GetObjectsWithEndsExactly {
  my ( $left, $right ) = @_;
  my @objects = values %Objects;
  if ( defined $left ) {
    @objects = grep { $LeftEdge_of{$_} == $left } @objects;
  }
  if ( defined $right ) {
    @objects = grep { $RightEdge_of{$_} == $right } @objects;
  }
  return @objects;
}

sub __GetObjectsWithEndsBeyond {
  my ( $left, $right ) = @_;
  my @objects = values %Objects;
  if ( defined $left ) {
    @objects = grep { $LeftEdge_of{$_} <= $left } @objects;
  }
  if ( defined $right ) {
    @objects = grep { $RightEdge_of{$_} >= $right } @objects;
  }
  return @objects;
}

sub __GetObjectsWithEndsNotBeyond {
  my ( $left, $right ) = @_;
  my @objects = values %Objects;
  if ( defined $left ) {
    @objects = grep { $LeftEdge_of{$_} >= $left } @objects;
  }
  if ( defined $right ) {
    @objects = grep { $RightEdge_of{$_} <= $right } @objects;
  }
  return @objects;
}

sub __GetExactObjectIfPresent {
  my ($object) = @_;
  my ( $left, $right ) = $object->get_edges();

  my $structure_string = $object->get_structure_string();
  my @matching =
  grep { $_->get_structure_string() eq $structure_string }
  __GetObjectsWithEndsExactly( $left, $right );
  return unless @matching;
  return $matching[0];    # There can be only one.
}

sub __GetGroupsThatPartiallyOverlap
{                         # i.e., the set difference is non empty both ways
  my ($object) = @_;
  my ( $left, $right ) = $object->get_edges();

  # main::message("__GetGroupsThatOverlap called!");
  return grep {
    my $obj_left  = $LeftEdge_of{$_};
    my $obj_right = $RightEdge_of{$_};
    ( $obj_left < $left and $left <= $obj_right and $obj_right < $right )
    or ( $left < $obj_left and $obj_left <= $right and $right < $obj_right )
  } values %NonEltObjects;
}

sub __SortLtoRByLeftEdge {
  ### require: __CheckLivenessAndDiagnose(@_)
  return ikeysort { $LeftEdge_of{$_} } @_;
}

sub __SortRtoLByLeftEdge {
  ### require: __CheckLivenessAndDiagnose(@_)
  return rikeysort { $LeftEdge_of{$_} } @_;
}

sub __SortLtoRByRightEdge {
  ### require: __CheckLivenessAndDiagnose(@_)
  return ikeysort { $RightEdge_of{$_} } @_;
}

sub __SortRtoLByRightEdge {
  ### require: __CheckLivenessAndDiagnose(@_)
  return rikeysort { $RightEdge_of{$_} } @_;
}

sub __DeleteGroup {
  my ($group) = @_;

# main::message("Deleting group while running a [$Global::Steps_Finished] $Global::CurrentCodeletFamily! ".$group->as_text());
  my @super_groups = values %{ $SuperGroups_of{$group} };

  for my $super_group (@super_groups) {
    next unless __CheckLiveness($super_group);
    __DeleteGroup($super_group);
  }

  for my $part ( $group->get_items_array() ) {
    delete $SuperGroups_of{$part}{$group};
  }

  # __RemoveRelations_of($group);
  $group->RemoveAllRelations();
  delete $LeftEdge_of{$group};
  delete $RightEdge_of{$group};
  delete $Span_of{$group};
  delete $SuperGroups_of{$group};
  delete $Objects{$group};
  delete $NonEltObjects{$group};
}

multimethod __InsertElement => ('Seqsee::Element') => sub {
  my ($element) = @_;
  my $magnitude = $element->get_mag();

  $element->set_edges( $ElementCount, $ElementCount );
  push @Elements,          $element;
  push @ElementMagnitudes, $magnitude;
  Global::UpdateExtensionsRejectedByUser($magnitude);

  $LeftEdge_of{$element}     = $ElementCount;
  $RightEdge_of{$element}    = $ElementCount;
  $Span_of{$element}         = 1;
  $Objects{$element}         = $element;
  $LiveAtSomePoint{$element} = 1;
  $SuperGroups_of{$element}  = {};

  SLTM::InsertUnlessPresent($element);

  $ElementCount++;
};

sub __CheckTwoGroupsForConflict {    # Second must be live.
  my ( $A, $B ) = @_;

  return 1 if $A eq $B;
  if ( !__CheckLiveness($B) ) {
    confess "This method only works when the second object is live";
  }

  my ( $smaller, $bigger ) = ikeysort { $_->get_span() } ( $A, $B );
  return 0 if $smaller->isa('Seqsee::Element');
  return 0 unless $bigger->spans($smaller);

  my $smaller_left_edge = $smaller->get_left_edge();
  my $current_index     = -1;
  for my $piece_of_bigger (@$bigger) {
    $current_index++;
    next if $RightEdge_of{$piece_of_bigger} < $smaller_left_edge;

    # No "backtracking" beyond here. If @$smaller is a subset of @$bigger,
    # it must start here! So no "next" here on.

    # If @$smaller is indeed a subset of @$bigger, the pieces must match...
    for my $piece_of_smaller (@$smaller) {
      return 0 unless $piece_of_smaller eq $bigger->[$current_index];
      $current_index++;
    }

# If we reach here, then all smaller pieces match a bigger piece.
# smaller and bigger may also have exactly the same elements (i.e., not "proper" subset)
# but the conflict exists!
    return 1;
  }
  confess "Should never reach here.";
}

sub __GroupAddSanityCheck {
  my (@parts) = @_;
  ### require: __CheckLivenessAndDiagnose(@_)
  return 0 if __AreHolesPresent(@parts);
}

sub __DoGroupAddBookkeeping {
  my ($group) = @_;

  # Assuming sanity checks passed.
  $Objects{$group}         = $group;
  $NonEltObjects{$group}   = $group;
  $SuperGroups_of{$group}  = {};
  $LiveAtSomePoint{$group} = 1;
  SLTM::InsertUnlessPresent($group);
  __UpdateGroup($group);
}

# When a group shortens, I am going to have trouble with false super_group data...
# So carefully call __RemoveFromSupergroups_of
sub __UpdateGroup {
  my ($group) = @_;

  # Assume that if $group has changed, $SuperGroups_of{$group} was empty.
  ### require: not(%{$SuperGroups_of{$group}})
  my @parts = $group->get_items_array();
  for my $part (@parts) {
    $SuperGroups_of{$part}{$group} = $group;
  }

  # I can assume that all parts are live...
  my $left_edge  = List::Util::min( @LeftEdge_of{@parts} );
  my $right_edge = List::Util::max( @RightEdge_of{@parts} );
  $LeftEdge_of{$group}  = $left_edge;
  $RightEdge_of{$group} = $right_edge;
  $Span_of{$group}      = $right_edge - $left_edge + 1;

}

sub __RemoveFromSupergroups_of {
  my ( $subgroup, $supergroup ) = @_;
  delete $SuperGroups_of{$subgroup}{$supergroup};
}

sub __FindObjectSetDirection {
  my (@objects) = @_;
  ### require: __CheckLivenessAndDiagnose(@_)

  # Assumption: @objects are live.
  my @left_edges = map { $LeftEdge_of{$_} } @objects;
  my $how_many = scalar(@objects);
  confess "Need at least 2" if $how_many <= 1;

  my ( $leftward, $rightward );
  for ( 0 .. $how_many - 2 ) {
    my $diff = $left_edges[ $_ + 1 ] - $left_edges[$_];
    if ( $diff > 0 ) {
      $rightward++;
    }
    elsif ( $diff < 0 ) {
      $leftward++;
    }
    else {
      return $DIR::UNKNOWN;
    }
  }

  return $DIR::NEITHER if ( $leftward and $rightward );
  return $DIR::LEFT    if $leftward;
  return $DIR::RIGHT   if $rightward;
  confess "huh?";
}

sub __AreThereHolesOrOverlap {
  my (@parts) = @_;
  ### require: __CheckLivenessAndDiagnose(@_)

  # Assumption: All @parts live
  my $direction = __FindObjectSetDirection(@parts);
  if ( $direction eq $DIR::RIGHT ) {
    for my $idx ( 0 .. scalar(@parts) - 2 ) {
      $RightEdge_of{ $parts[$idx] } + 1 == $LeftEdge_of{ $parts[ $idx + 1 ] }
      or return 1;    # Hole/overlap present
    }
    return 0;         # No holes, no overlap.
  }
  elsif ( $direction eq $DIR::LEFT ) {
    for my $idx ( 0 .. scalar(@parts) - 2 ) {
      $LeftEdge_of{ $parts[$idx] } - 1 == $RightEdge_of{ $parts[ $idx + 1 ] }
      or return 1;    # Hole/overlap present
    }
    return 0;         # No holes, no overlap.
  }
  else {
    return
    1
    ; # funny direction, so question of holes is moot. This cannot make a good object.
  }
}

sub __CheckMagnitudesRightwards {
  my ( $start_position, $expected_magnitudes_ref ) = @_;
  my $next_position_to_check = $start_position;
  my @expected_magnitudes    = @{$expected_magnitudes_ref};
  my @already_validated;

  while (@expected_magnitudes) {
    my $next_magnitude_expected = shift(@expected_magnitudes);
    if ( $next_position_to_check >= $ElementCount ) {

      # throw!
      SErr::ElementsBeyondKnownSought->throw(
        next_elements => \@expected_magnitudes );
    }
    elsif (
      $ElementMagnitudes[$next_position_to_check] == $next_magnitude_expected )
    {
      $next_position_to_check++;
      push @already_validated, $next_magnitude_expected;
    }
    else {
      return 0;    # Failed! Those are not the next elements.
    }
  }

  return 1;        # Yes, right elements.
}

sub __FindGroupsConflictingWith {
  my ($object) = @_;
  my ( $l, $r ) = $object->get_edges();

  my @exact_span = __GetObjectsWithEndsExactly( $l, $r );
  my $structure_string = $object->get_structure_string();
  my ($exact_conflict) =    # Can only ever be one.
  grep { $_->get_structure_string() eq $structure_string } @exact_span;
  $exact_conflict ||= '';

  my @conflicting =
  grep { $_ ne $exact_conflict }
  grep { __CheckTwoGroupsForConflict( $object, $_ ) } (
    __GetObjectsWithEndsBeyond( $l, $r ),
    __GetObjectsWithEndsNotBeyond( $l, $r )
  );
  if ( $Global::Feature{NoGpOverlap} ) {
    push @conflicting, __GetGroupsThatPartiallyOverlap($object);
  }

  ## @conflicting: @conflicting
  return Seqsee::ResultOfGetConflicts->new(
    {
      challenger            => $object,
      exact_conflict        => $exact_conflict,
      overlapping_conflicts => \@conflicting,
    }
  );
}

sub __CopyAttributes {
  my ($opts_ref) = @_;
  my ( $from, $to ) = ( $opts_ref->{from}, $opts_ref->{to} );
  if ( !$from or!$to ) {
    die "Missing from or to";
  }

  my $any_failure_so_far = 0;

  # Copy Metonym:
  if ( my $metonym = $from->get_metonym() ) {
    my ( $cat, $name ) = ( $metonym->get_category(), $metonym->get_name() );
    $to->AnnotateWithMetonym( $cat, $name );
    $to->SetMetonymActiveness( $from->get_metonym_activeness() );
  }

  # Relation Scheme:
  if ( my $rel_scheme = $from->get_reln_scheme() ) {
    $to->apply_reln_scheme($rel_scheme);
  }

  # Copy groupness:
  $to->set_group_p(1) if $from->get_group_p();

  # Copy Categories:
  for my $category ( @{ $from->get_categories() } ) {
    my $bindings;
    unless ( $bindings = $to->describe_as($category) ) {
      $any_failure_so_far++;
      next;
    }
    my $bindings_for_from      = $from->describe_as($category);
    my $position_mode_for_from = $bindings_for_from->get_position_mode();
    if ( defined $position_mode_for_from ) {
      $bindings->TellDirectedStory( $to, $position_mode_for_from );
    }
  }
  if ($any_failure_so_far) {
    return Seqsee::ResultOfAttributeCopy->Failed();
  }
  else {
    return Seqsee::ResultOfAttributeCopy->Success();
  }
}

multimethod __PlonkIntoPlace => ( '#', 'DIR', 'Seqsee::Element' ) => sub {
  my ( $start, $direction, $element ) = @_;
  *__ANON__ = '__ANON__PlonkIntoPlace_el';
  my $magnitude = $element->get_mag();

  unless ( $magnitude == $ElementMagnitudes[$start] ) {
    return Seqsee::ResultOfPlonk->Failed($element);
  }

  my $attribute_copy_result = __CopyAttributes(
    {
      from => $element,
      to   => $Elements[$start],
    }
  );
  return Seqsee::ResultOfPlonk->new(
    {
      object_being_plonked  => $element,
      resultant_object      => $Elements[$start],
      attribute_copy_result => $attribute_copy_result,
    }
  );
};

multimethod __PlonkIntoPlace => ( '#', 'DIR', 'Seqsee::Object' ) => sub {
  my ( $start, $direction, $object ) = @_;
  *__ANON__ = '__ANON__PlonkIntoPlace_obj';

  my $span = $object->get_span() or return Seqsee::ResultOfPlonk->Failed($object);

  if ( $direction eq $DIR::LEFT ) {
    return Seqsee::ResultOfPlonk->Failed($object) if $start < $span - 1;
    return __PlonkIntoPlace( $start - $span + 1, $DIR::RIGHT, $object );
  }

  my @to_insert = $object->get_items_array;
  my @new_parts;
  my $plonk_cursor                 = $start;
  my $attribute_copy_status_so_far = Seqsee::ResultOfAttributeCopy->Success();

  for my $subobject (@to_insert) {
    my $subobjectspan = $subobject->get_span;
    my $plonk_result =
    __PlonkIntoPlace( $plonk_cursor, $DIR::RIGHT, $subobject );
    if ( $plonk_result->PlonkWasSuccessful() ) {
      push @new_parts, $plonk_result->resultant_object();
      $plonk_cursor += $subobjectspan;
      $attribute_copy_status_so_far->UpdateWith(
        $plonk_result->attribute_copy_result );
    }
    else {
      return Seqsee::ResultOfPlonk->Failed($object);
    }
  }

  my $new_obj = Seqsee::Anchored->create(@new_parts);
  if ( my $existing_object = __GetExactObjectIfPresent($new_obj) ) {
    $new_obj = $existing_object;
  }
  else {
    if ( !SWorkspace->add_group($new_obj) ) {
      return Seqsee::ResultOfPlonk->Failed($object);
    }
  }

  my $attribute_copy_result = __CopyAttributes(
    {
      from => $object,
      to   => $new_obj,
    }
  );
  $attribute_copy_status_so_far->UpdateWith($attribute_copy_result);
  return Seqsee::ResultOfPlonk->new(
    {
      object_being_plonked  => $object,
      resultant_object      => $new_obj,
      attribute_copy_result => $attribute_copy_status_so_far,
    }
  );

};

sub __GetLongestNonAdHocWithEndsExactly {
  my ( $left, $right ) = @_;
  if ( defined($left) and not( defined($right) ) ) {
    for my $gp (
      __SortRtoLByRightEdge( __GetObjectsWithEndsExactly( $left, undef ) ) )
    {
      return $gp if $gp->HasNonAdHocCategory();
    }
    return $Elements[$left];
  }
  elsif ( defined($right) and not( defined($left) ) ) {
    for my $gp (
      __SortLtoRByLeftEdge( __GetObjectsWithEndsExactly( undef, $right ) ) )
    {
      return $gp if $gp->HasNonAdHocCategory();
    }
    return $Elements[$right];
  }
  else {
    confess
    "__GetLongestNonAdHocWithEndsExactly needs exactly one defined argument. Got '$left' and '$right'";
  }
}

# Non-ad-hoc, with left=$left, right<=$right
sub __GetLongestNonAdHocWithLeftExactRightBelow {
  my ( $left, $right ) = @_;

  # main::message("__GetLongestNonAdHocWithLeftExactRightBelow($left, $right)");
  for my $gp (
    __SortRtoLByRightEdge( __GetObjectsWithEndsExactly( $left, undef ) ) )
  {
    next if $RightEdge_of{$gp} > $right;
    return $gp if $gp->HasNonAdHocCategory();
  }
  return $Elements[$left];
}

# Distance, where each non-ad-hoc counts as 1
sub __FindDistance {
  my ( $object1, $object2, $requested_mode ) = @_;
  ### require: __CheckLivenessAndDiagnose($object1, $object2)

  my $mode = $requested_mode;
  $mode ||= DISTANCE_MODE::PickOne();

  my $min_right =
  List::Util::min( $RightEdge_of{$object1}, $RightEdge_of{$object2} );
  my $max_left =
  List::Util::max( $LeftEdge_of{$object1}, $LeftEdge_of{$object2} );

#main::message("Finding distance: " . $object1->as_text() . ' to ' . $object2->as_text(),1 );
  if ( $max_left <= $min_right + 1 ) {    # Adjacent or overlapping.
    return DISTANCE::Zero();
  }

  # Find distance now.
  my $distance = __FindDistanceHelper_( $min_right + 1, $max_left - 1, $mode );
  if ( $mode->IsUnitGroups() and!$requested_mode ) {

    # Is what we got really elements? If so, change unit.
    my $magnitude = $distance->GetMagnitude;
    if ( $max_left - $min_right - 1 == $magnitude ) {
      $distance = DISTANCE::InElements($magnitude);    # change units.
    }
  }
  return $distance;
}

sub __FindDistanceHelper_ {
  my ( $left_end_of_gap, $right_end_of_gap, $mode ) = @_;
  if ( !$mode->IsUnitGroups() ) {

    # main::message("Distance in element units!");
    return DISTANCE::InElements( 1 + $right_end_of_gap - $left_end_of_gap );
  }

#main::message("__FindDistanceHelper_: Filling gap $left_end_of_gap to $right_end_of_gap", 1);

  # A Simple Greedy Algo.
  my $intermediate_groups_seen = 0;
  while ( $left_end_of_gap <= $right_end_of_gap ) {
    if ( $left_end_of_gap == $right_end_of_gap ) {
      $intermediate_groups_seen++;
      last;
    }
    my $longest_first_group =
    __GetLongestNonAdHocWithLeftExactRightBelow( $left_end_of_gap,
      $right_end_of_gap );
    $left_end_of_gap = $LeftEdge_of{$longest_first_group} + 1;
    $intermediate_groups_seen++;

#main::message("__FindDistanceHelper_: Group seen ". $longest_first_group->as_text(), 1);
#main::message("Gap now: $left_end_of_gap -> $right_end_of_gap", 1);
  }

  #main::message("Returning $intermediate_groups_seen as the answer");
  return DISTANCE::InGroups($intermediate_groups_seen);
}

sub __GetPositionInDirectionAtDistance {
  my ($opts_ref) = @_;
  my $from_object = $opts_ref->{from_object} or confess "Need from_object";
  my $direction   = $opts_ref->{direction}   or confess "Need direction";
  my $distance    = $opts_ref->{distance}    or confess "Need distance";
  ### require: $distance->isa("DISTANCE");

  if ( $direction eq $DIR::RIGHT ) {
    my $end = $RightEdge_of{$from_object} + 1;
    if ( !$distance->IsUnitGroups() ) {
      return $end + $distance;
    }
    $distance = $distance->GetMagnitude();
    for ( 1 .. $distance ) {
      my $next_object = __GetLongestNonAdHocWithEndsExactly( $end, undef );
      return unless $next_object;
      $end = 1 + $RightEdge_of{$next_object};
    }
    return $end;
  }
  elsif ( $direction eq $DIR::LEFT ) {
    my $end = $LeftEdge_of{$from_object} - 1;
    if ( !$distance->IsUnitGroups() ) {
      if ( $end >= $distance ) {
        return $end - $distance;
      }
      else {
        return;
      }
    }
    $distance = $distance->GetMagnitude();
    for ( 1 .. $distance ) {
      if ( $end < 0 ) {
        return;
      }
      my $next_object = __GetLongestNonAdHocWithEndsExactly( undef, $end );
      return unless $next_object;
      $end = $LeftEdge_of{$next_object} - 1;
    }
    return if $end < 0;
    return $end;
  }
  else {
    confess "HUH?";
  }
}

sub __GetSamenessAround {
  my ($pos) = @_;
  my $magnitude = $ElementMagnitudes[$pos];
  my ( $left_margin, $right_margin ) = ( $pos, $pos );
  while ( $left_margin > 0 ) {
    last unless $ElementMagnitudes[ $left_margin - 1 ] == $magnitude;
    $left_margin--;
  }
  while ( $right_margin < $ElementCount - 1 ) {
    last unless $ElementMagnitudes[ $right_margin + 1 ] == $magnitude;
    $right_margin++;
  }
  return ( $left_margin, $right_margin );
}

sub __CreateSamenessGroupAround {
  my ($pos) = @_;
  my ( $left_margin, $right_margin ) = __GetSamenessAround($pos);
  my $span = $right_margin - $left_margin + 1;
  return if $span < 2;

  my @covering = __GetObjectsWithEndsBeyond( $left_margin, $right_margin );
  return if ( @covering and SUtil::toss(0.5) );

  my @items = @Elements[ $left_margin .. $right_margin ];
  for (@items) {
    return if $_->get_metonym_activeness();
  }

  my $new_group = Seqsee::Anchored->create(@items);
  $new_group->describe_as($S::SAMENESS);
  return __AddGroup($new_group);
}

sub __AddGroup {
  my ($gp) = @_;
  my $conflicts = __FindGroupsConflictingWith($gp);
  if ($conflicts) {
    $conflicts->Resolve( { FailIfExact => 1 } ) or return;
  }

  # $groups{$gp} = $gp;
  $Global::TimeOfNewStructure = $Global::Steps_Finished;
  __DoGroupAddBookkeeping($gp);
  return 1;

}

sub __ScanRightwardForElements {

  # Returns the position of the *leftmost* element of matching magnitude set.
  my ( $start_position, $magnitudes_ref ) = @_;

# A simple worst case n square algorithm. n will be small in general, and expected time is much better.
  my @magnitudes_to_hunt                 = @$magnitudes_ref;
  my $how_many_to_hunt                   = scalar(@magnitudes_to_hunt);
  my $largest_possible_leftmost_position = $ElementCount - $how_many_to_hunt;

  OUTER:
  for my $possible_leftmost_position (
    $start_position .. $largest_possible_leftmost_position )
  {
    for ( 0 .. $how_many_to_hunt - 1 ) {
      next OUTER
      if $ElementMagnitudes[ $possible_leftmost_position + $_ ] !=
        $magnitudes_to_hunt[$_];
    }

    # We have our matching elements!
    return $possible_leftmost_position;
  }

  return;
}

sub __ScanLeftwardForElements {

  # Returns the position of the *leftmost* element of matching magnitude set.
  # Returns undef if none match
  my ( $start_position, $magnitudes_ref ) = @_;

# A simple worst case n square algorithm. n will be small in general, and expected time is much better.
  my @magnitudes_to_hunt = @$magnitudes_ref;
  my $how_many_to_hunt   = scalar(@magnitudes_to_hunt);
  my $largest_possible_leftmost_position =
  $start_position - $how_many_to_hunt + 1;

  my $possible_leftmost_position = $largest_possible_leftmost_position;
  OUTER: while ( $possible_leftmost_position >= 0 ) {
    for ( 0 .. $how_many_to_hunt - 1 ) {
      if ( $ElementMagnitudes[ $possible_leftmost_position + $_ ] !=
        $magnitudes_to_hunt[$_] )
      {
        $possible_leftmost_position--;
        next OUTER;
      }
    }

    # We have our matching elements!
    return $possible_leftmost_position;
  }

  return;
}

sub __FindSetsOfObjectsWithOverlappingSubgroups {
  my (@objects) = @_;
  my %subgroup_to_objects;
  for my $obj (@objects) {
    for my $subobj (@$obj) {
      push @{ $subgroup_to_objects{$subobj} }, $obj;
    }
  }
  return grep { scalar(@$_) >= 2 } ( values %subgroup_to_objects );
}

#############################################
# BAR LINES
#############################################
sub __ClearBarLines {
  @BarLines     = ();
  $BarLineCount = 0;
}

sub __AddBarLines {
  my (@indices) = @_;
  @BarLines = sort { $a <=> $b } ( @BarLines, @indices );
  $BarLineCount = scalar(@BarLines);
}

sub GetBarLines {
  return @BarLines;
}

sub __ClosestBarLineToLeftGivenIndex {
  my ($index) = @_;
  return unless $BarLineCount;
  return if $BarLines[0] > $index;
  my $count = 1;
  while ( $count < $BarLineCount and $BarLines[$count] <= $index ) {
    $count++;
  }
  return $BarLines[ $count - 1 ];
}

sub __ClosestBarLineToRightGivenIndex {
  my ($index) = @_;
  return unless $BarLineCount;
  return if $BarLines[-1] <= $index;
  my $count = $BarLineCount - 2;
  while ( $count > -1 and $BarLines[$count] > $index ) {
    $count--;
  }
  return $BarLines[ $count + 1 ];
}

sub __RemoveGroupsCrossingBarLines {
  my @groups = GetGroups();
  for my $group (@groups) {
    next unless __CheckLiveness($group);
    if ( __CheckIfCrossesBarLinesInappropriately($group) ) {
      __DeleteGroup($group);
    }
  }
}

sub __CheckIfCrossesBarLinesInappropriately {
  my ($group) = @_;
  my ( $l, $r ) = ( $LeftEdge_of{$group}, $RightEdge_of{$group} );
  my $closest_bar_line_before_end = __ClosestBarLineToLeftGivenIndex($r)
  // return;
  return if $closest_bar_line_before_end <= $l;    # No crossing!

  # Crossing exists. It is appropriate if there are bar lines at either ends.
  return 1 unless __ClosestBarLineToLeftGivenIndex($l) == $l;
  my $rightward_closest = __ClosestBarLineToRightGivenIndex($r) // return;
  return 1 unless $rightward_closest == $r + 1;
  return;
}

##########

sub __UpdateObjectStrengths {
  for ( values(%relations), values(%Objects) ) {
    $_->UpdateStrength();
  }
}

#=============================================
#=============================================

# Next 2 lines: should be my!
our @elements = ();

# variable: %groups
#    All groups
# our %groups;

# variable: $ReadHead
#    Points just beyond the last object read.
#
#    If never called before any reads, points to 0.
our $ReadHead = 0;

# variable: %relations

my $strength_chooser = SChoose->create( { map => \&SFasc::get_strength } );

# method: clear
#  starts workspace off as new

sub clear {
  $ElementCount = 0;
  @elements     = ();

  # %groups         = ();
  %relations = ();
  $ReadHead  = 0;

  __Clear();
}

# method: init
#   Given the options ref, initializes the workspace
#
# exceptions:
#   none

sub init {
  my ( $package, $OPTIONS_ref ) = @_;
  $package->clear();
  my @seq = @{ $OPTIONS_ref->{seq} };
  for (@seq) {

    # print "Inserting '$_'\n";
    _insert_element($_);
  }
  @Global::RealSequence     = @seq;
  $Global::InitialTermCount = scalar(@seq);
}

sub insert_elements {
  shift;
  for (@_) {
    _insert_element($_);
  }
  $Global::TimeOfLastNewElement = $Global::Steps_Finished;
  $Global::TimeOfNewStructure   = $Global::Steps_Finished;
}

# section: _insert_element

# method: _insert_element(#)

# method: _insert_element($)

# method: _insert_element(Seqsee::Element)

multimethod _insert_element => ('#') => sub {

  # using bogues edges, since they'd be corrected soon anyway
  my $mag = shift;
  _insert_element( Seqsee::Element->create( $mag, 0 ) );
};

multimethod _insert_element => ('$') => sub {
  use Scalar::Util qw(looks_like_number);
  my $what = shift;
  if ( looks_like_number($what) ) {

    # using bogus edges; these will get fixed immediately...
    _insert_element( Seqsee::Element->create( int($what), 0 ) );
  }
  else {
    die "Huh? Trying to insert '$what' into the workspace";
  }
};

multimethod _insert_element => ('Seqsee::Element') => sub {
  my $elt = shift;
  %Global::ExtensionRejectedByUser = ();

  __InsertElement($elt);
};

sub __ReadObjectOrRelation {
  my ( $dist_likelihoods_ref, $dist_objects ) =
  __GetObjectOrRelationChoiceProbabilityDistribution();
  my $chosen = SChoose->choose( $dist_likelihoods_ref, $dist_objects );
  if ( UNIVERSAL::isa( $chosen, 'Seqsee::Anchored' ) ) {
    my $right_edge = $RightEdge_of{$chosen};
    if ( $right_edge == $ElementCount - 1 ) {
      _saccade();
    }
    else {
      $ReadHead = $right_edge + 1;
    }
  }

  return $chosen;
}

sub __GetObjectOrRelationChoiceProbabilityDistribution {

  # Returns two array_refs: the first with likelihoods, the second with objects.
  # Likelihoods sum to 1 if arrays non-empty.
  my ( $obj_likelihood, $obj_list ) =
  __GetObjectChoiceProbabilityDistribution();
  my ( $rel_likelihood, $rel_list ) =
  __GetRelationChoiceProbabilityDistribution();
  for ( @$obj_likelihood, @$rel_likelihood ) {
    $_ *= 0.5;    # normalize.
  }
  return ( [ @$obj_likelihood, @$rel_likelihood ], [ @$obj_list, @$rel_list ] );
}

sub __GetObjectChoiceProbabilityDistribution {
  my @choose_from = __GetObjectsWithEndsBeyond( $ReadHead, $ReadHead );
  my @distribution_values;
  my @distribution_objects;
  my $sum_of_numbers = 0;
  for my $object (@choose_from) {
    my $strength = $object->get_strength();
    $strength *= 2 if $Span_of{$object} > 4;

    $strength *= 0.3 if %{ $SuperGroups_of{$object} };

    #$strength *= 100 if $LeftEdge_of{$object} eq $ReadHead;

    next unless $strength;
    push @distribution_objects, $object;
    push @distribution_values,  $strength;
    $sum_of_numbers += $strength;
  }

  if ($sum_of_numbers) {
    $_ /= $sum_of_numbers for @distribution_values;
  }

  # These could be empty:
  return ( \@distribution_values, \@distribution_objects );
}

sub __GetRelationChoiceProbabilityDistribution {
  my @choose_from = values %relations;
  my @distribution_values;
  my @distribution_objects;
  my $sum_of_numbers = 0;
  for my $object (@choose_from) {
    my $strength = $object->get_strength();

    my ( $end1, $end2 ) = $object->get_ends();
    $strength *= 0.8
    if ( %{ $SuperGroups_of{$end1} } or %{ $SuperGroups_of{$end2} } );

    next unless $strength;
    push @distribution_objects, $object;
    push @distribution_values,  $strength;
    $sum_of_numbers += $strength;
  }

  if ($sum_of_numbers) {
    $_ /= $sum_of_numbers for @distribution_values;
  }

  # These could be empty:
  return ( \@distribution_values, \@distribution_objects );
}

sub __GetPositionStructure {
  my ($group) = @_;
  given ( ref($group) ) {
    when ('Seqsee::Element') {
      return $LeftEdge_of{$group};
    }
    when ('Seqsee::Anchored') {
      return [ map { __GetPositionStructure($_) } @{$group} ];
    }
  }
}

sub __GetPositionStructureAsString {
  my ($group) = @_;
  SUtil::StringifyDeepArray( __GetPositionStructure($group) );
}

sub __GetObjectsBelongingToCategory {
  my ($cat) = @_;
  return grep { $_->is_of_category_p($cat) } ( values %Objects );
}

sub __GetObjectsBelongingToSimilarCategories {
  my ($object) = @_;
  my @cats = @{ $object->get_categories() } or return;

  my $wset = Set::Weighted->new();
  for my $cat (@cats) {
    my $activation_level = SLTM::GetRealActivationsForOneConcept($cat);
    my @objects          = __GetObjectsBelongingToCategory($cat);
    $wset->insert( map { [ $_, $activation_level ] } @objects );
  }
  $wset->delete_key($object);
  return $wset;
}

sub __ChooseByStrength {
  $strength_chooser->( [@_] );
}

{

  sub read_relation {
    my ($ws) = @_;
    return $strength_chooser->( [ values %relations ] );
  }

  # method: _get_some_object_at
  # returns some object spanning that index.
  #

  sub _get_some_object_at {
    my ($idx) = @_;
    my @matching_objects =
    grep { $_->get_left_edge() <= $idx and $_->get_right_edge() >= $idx }
    ( values %Objects );

    return $strength_chooser->( \@matching_objects );
  }

}

# method: _saccade
# unthought through method to saccade
#
#    Jumps to a random valid position
sub _saccade {
  if ( SUtil::toss(0.5) ) {
    return $ReadHead = 0;
  }
  my $random_pos = int( rand() * $ElementCount );
  $ReadHead = $random_pos;
}

# method: AddRelation
#
#
sub AddRelation {
  my ( $package, $reln ) = @_;
  my ( $f,       $s )    = $reln->get_ends();
  for ( $f, $s ) {
    confess "Metonym'd end of relation" if $_->IsThisAMetonymedObject();
  }

  my $key = join( ';', $f, $s );
  return if exists $relations_by_ends{$key};

  #my $key_r = join(';', $s, $f);
  #confess 'reverse relation exists!' if exists $relations_by_ends{$key_r};

  $relations_by_ends{$key} = 1;
  $relations{$reln}        = $reln;
}

sub RemoveRelation {
  my ( $package, $reln ) = @_;

  my $key = join( ';', $reln->get_ends() );
  delete $relations_by_ends{$key};

  delete $relations{$reln};
}

sub get_longest_non_adhoc_object_starting_at {
  my ( $self, $left ) = @_;
  for my $gp (
    __SortRtoLByRightEdge( __GetObjectsWithEndsExactly( $left, undef ) ) )
  {

    # That gives us longest first.
    INNER: for my $cat ( @{ $gp->get_categories() } ) {
      if ( $cat->get_name() !~ m#Interlaced# ) {
        return $gp;
      }
    }
  }

  if ( $left >= $ElementCount ) {
    ### left: $left
    ### ElementCount: $ElementCount
    confess "Why am I being asked this?";
  }

  # Getting here means no group. Return the element.
  return $Elements[$left];
}

sub get_longest_non_adhoc_object_ending_at {
  my ( $self, $right ) = @_;
  for my $gp (
    __SortLtoRByLeftEdge( __GetObjectsWithEndsExactly( undef, $right ) ) )
  {

    # That gives us longest first.
    INNER: for my $cat ( @{ $gp->get_categories() } ) {
      if ( $cat->get_name() !~ m#^Interlaced# ) {
        return $gp;
      }
    }
  }

  # Getting here means no group. Return the element.
  return $Elements[$right];
}

sub AreGroupsInConflict {
  my ( $package, $A, $B ) = @_;
  return __CheckTwoGroupsForConflict( $A, $B ) if __CheckLiveness($B);
}

sub FindGroupsConflictingWith {
  my ( $package, $object ) = @_;
  my ( $l,       $r )      = $object->get_edges();

  my @exact_span = __GetObjectsWithEndsExactly( $l, $r );
  my $structure_string = $object->get_structure_string();
  my ($exact_conflict) =    # Can only ever be one.
  grep { $_->get_structure_string() eq $structure_string } @exact_span;

  my @conflicting = grep { __CheckTwoGroupsForConflict( $object, $_ ) } (
    __GetObjectsWithEndsBeyond( $l, $r ),
    __GetObjectsWithEndsNotBeyond( $l, $r )
  );
  ## @conflicting: @conflicting

  # @conflicting will also contain $exact_conflict, but that is fine.
  return ( $exact_conflict, @conflicting );
}

# XXX(Board-it-up): [2006/09/27] Need tests. Really.
sub get_intervening_objects {
  my ( $self, $l, $r ) = @_;
  my @ret;
  my $left = $l;
  ##  $left, $r
  if ( $r >= $ElementCount ) {
    confess
    "get_intervening_objects called with right end of gap beyond known elements";
  }
  while ( $left <= $r ) {
    my $o = SWorkspace->get_longest_non_adhoc_object_starting_at($left);
    push @ret, $o;
    ##  $o
    $left = $o->get_right_edge() + 1;
    ## $left
  }
  return @ret if ( $left == $r + 1 );    # Not overshot
  return;                                # overshot
}

sub add_group {
  my ( $self, $gp ) = @_;
  my $conflicts = __FindGroupsConflictingWith($gp);
  if ($conflicts) {
    $conflicts->Resolve( { FailIfExact => 1 } ) or return;
  }

  # $groups{$gp} = $gp;
  $Global::TimeOfNewStructure = $Global::Steps_Finished;
  __DoGroupAddBookkeeping($gp);
  return 1;
}

sub remove_gp {
  my ( $self, $gp ) = @_;
  __DeleteGroup($gp);
}

# method: check_at_location
# checks if this is the object present at a location
#
#    Arguments are start, direction(+1 or -1) and what the object to look for is.
sub check_at_location {
  my ( $self, $opts_ref ) = @_;
  my $direction = $opts_ref->{direction} || die "need direction";
  confess "Need start" unless defined $opts_ref->{start};
  my $start     = $opts_ref->{start};
  my $what      = $opts_ref->{what};
  my @flattened = @{ $what->get_flattened };
  my $span      = scalar(@flattened);

  ## $direction, $start, $what
  ## @flattened
  if ( $direction eq DIR::RIGHT() ) {    # rightward
    CheckElementsRightwardFromLocation( $start, \@flattened, $what, $start,
      $direction );
  }
  elsif ( $direction eq $DIR::LEFT ) {

    if ( $span > $start + 1 ) {          # would extend beyond left edge
      return;
    }

    my $left_end_of_potential_match = $start - $span + 1;
    return CheckElementsRightwardFromLocation( $left_end_of_potential_match,
      \@flattened, $what, $start, $direction );
  }
  else {
    confess "Huh?";
  }

}

sub CheckElementsRightwardFromLocation {
  my ( $start, $elements_ref, $object_being_looked_for,
    $position_it_is_being_looked_from,
    $direction_to_look_in )
  = @_;
  my @flattened   = @$elements_ref;
  my $current_pos = $start - 1;
  my @already_validated;
  while (@flattened) {
    $current_pos++;
    if ( $current_pos >= $ElementCount ) {

      # already out of range!
      SErr::ElementsBeyondKnownSought->throw( next_elements => [@flattened] );
    }
    else {
      ## expecting: $flattened[0]
      ## got: $elements[$current_pos]->get_mag()
      if ( $Elements[$current_pos]->get_mag() == $flattened[0] ) {
        push @already_validated, shift(@flattened);
      }
      else {
        return;
      }
    }
  }
  return 1;
}

sub rapid_create_gp {
  my ( $self, $cats, @items ) = @_;
  @items = map {
    if ( ref($_) eq "ARRAY" )
    {
      $self->rapid_create_gp(@$_);
    }
    else {
      $_;
    }
  } @items;

  my $object = Seqsee::Anchored->create(@items);
  SWorkspace->add_group($object);

  while (@$cats) {
    my $next = shift(@$cats);
    if ( $next eq "metonym" ) {
      my $cat  = shift(@$cats);
      my $name = shift(@$cats);
      $object->describe_as($cat);
      $object->AnnotateWithMetonym( $cat, $name );
      $object->SetMetonymActiveness(1);
    }
    else {
      $object->describe_as($next);
    }
  }
  return $object;
}

sub are_there_holes_here {
  my ( $self, @items ) = @_;
  return 0 unless @items;
  my %slots_taken;
  for my $item (@items) {
    SErr->throw(
      "SWorkspace are_there_holes_here called with a non anchored object $item")
    unless UNIVERSAL::isa( $item, "Seqsee::Anchored" );
    my ( $left, $right ) = $item->get_edges();
    @slots_taken{ $left .. $right } = ( $left .. $right );
  }

  my @keys = values %slots_taken;
  ## @keys
  my ( $left, $right ) = SUtil::minmax(@keys);
  ## $left, $right
  my $span = $right - $left + 1;

  unless ( scalar(@keys) == $span ) {
    return 1;
  }
  return 0;
}

sub FightUntoDeath {
  my ( $package, $opts_ref ) = @_;
  my ( $challenger, $incumbent ) =
  ( $opts_ref->{challenger}, $opts_ref->{incumbent} );
  if ( !__CheckLiveness($incumbent) ) {

    # Hah. $challenger a No contest winner.
    return 1;
  }

  if ( $incumbent->get_is_locked_against_deletion() ) {
    return 0;    # Locked, so cannot be deleted.
  }

  my (@strengths) = map { $_->get_strength() } ( $challenger, $incumbent );
  confess "Both strengths 0" unless $strengths[0] + $strengths[1];
  if ( SUtil::toss( $strengths[0] / ( $strengths[0] + 1.5 * $strengths[1] ) ) )
  {

    # challenger won!
    __DeleteGroup($incumbent);
    return 1;
  }
  else {

    # incumbent won!
    return 0;
  }
}

sub GetSomethingLike {
  my ( $package, $opts_ref ) = @_;
  my $object    = $opts_ref->{object}    // confess;
  my $start_pos = $opts_ref->{start}     // confess;
  my $direction = $opts_ref->{direction} // confess;
  my $reason = $opts_ref->{reason} || '';    # used for message for ask_user
  my $trust_level = $opts_ref->{trust_level} // confess;    # used if ask_user
  my $hilit_set = $opts_ref->{hilit_set};

  #print "GetSomethingLike: object ", $object->as_text;
  #print " Start: ", $start_pos;
  #print " dir ", $direction->as_text, "\n";
  my @objects_at_that_location;
  if ( $direction eq $DIR::RIGHT ) {
    @objects_at_that_location =
    __GetObjectsWithEndsExactly( $start_pos, undef );
  }
  elsif ( $direction eq $DIR::LEFT ) {
    @objects_at_that_location =
    __GetObjectsWithEndsExactly( undef, $start_pos );
  }

# print "\tOBJECTS THERE: ", join(", ", map { $_->as_text } @objects_at_that_location), "\n";
  my $expected_structure_string = $object->get_structure_string();

  my ( @matching_objects, @potentially_matching_objects );
  for (@objects_at_that_location) {
    if ( $_->GetEffectiveObject()->get_structure_string() eq
      $expected_structure_string )
    {
      push @matching_objects, $_;
    }
    else {
      push @potentially_matching_objects, $_;
    }
  }

#print "\tMATCHING: ", join(", ", map { $_->as_text } @matching_objects), "\n";
#print "\tPotentially: ", join(", ", map { $_->as_text } @potentially_matching_objects), "\n";

  my $is_object_literally_present;

  eval {
    $is_object_literally_present = SWorkspace->check_at_location(
      { direction => $direction, start => $start_pos, what => $object } );
  };
  if ( my $err = $EVAL_ERROR ) {
    CATCH_BLOCK: {
      if ( UNIVERSAL::isa( $err, 'SErr::ElementsBeyondKnownSought' ) ) {
        $trust_level *= 0.02;    # had multiplied by 50 for toss...
        if ( SUtil::toss($trust_level) ) {    # Kludge.
          Global::Hilit( 1, @$hilit_set );
          $err->Ask( "$reason. ", '' ) or return;
          Global::ClearHilit();
          eval {
            $is_object_literally_present = SWorkspace->check_at_location(
              { direction => $direction, start => $start_pos, what => $object }
            );
          };
        }
        last CATCH_BLOCK;
      }
      die $err;
    }
  }

  if ($is_object_literally_present) {
    my $plonk_result = __PlonkIntoPlace( $start_pos, $direction, $object )
    or return;
    my $present_object = $plonk_result->resultant_object();
    if ( SUtil::toss(0.5) ) {
      return $present_object;
    }
    else {
      push @matching_objects, $present_object;
    }
  }

  if ( $Global::Feature{AllowSquinting} ) {
    for (@potentially_matching_objects) {
      SCoderack->add_codelet(
        SCodelet->new(
          'TryToSquint',
          200,
          {
            actual   => $_,
            intended => $object,
          }
        )
      );
    }
  }

  return $strength_chooser->( \@matching_objects );
}

sub SErr::AskUser::WorthAsking {
  my ( $self, $trust_level ) = @_;
  ### trust_level: $trust_level
  my ( $match_size, $ask_size ) = (
    scalar( @{ $self->already_matched() } ),
    scalar( @{ $self->next_elements() } )
  );
  my $fraction_already_matched = $match_size / ( $match_size + $ask_size );
  $trust_level += ( 1 - $trust_level ) * $fraction_already_matched;
  ### trust_level: $trust_level
  ### acceptable: $Global::AcceptableTrustLevel
  return 0 if ( $trust_level < $Global::AcceptableTrustLevel );
  return SUtil::toss($trust_level) ? $trust_level :0;
}

sub SErr::AskUser::Ask {
  my ( $self, $msg ) = @_;
  my $already_matched = $self->already_matched();
  my $next_elements   = $self->next_elements();

  my $object_being_looked_for          = $self->object();
  my $position_it_is_being_looked_from = $self->from_position();
  my $direction_to_look_in             = $self->direction();

  if (@$already_matched) {
    $msg .= "I found the expected terms " . join( ', ', @$already_matched );
  }

  my $answer = main::ask_user_extension( $next_elements, $msg );
  if ($answer) {
    SWorkspace->insert_elements(@$next_elements);
    $Global::AcceptableTrustLevel = 0.5;
    main::update_display();
    $Global::Break_Loop = 1;

    if ( defined $object_being_looked_for ) {
      __PlonkIntoPlace( $position_it_is_being_looked_from,
        $direction_to_look_in, $object_being_looked_for );
    }

  }
  else {
    my $seq = join( ', ', @$next_elements );
    $Global::ExtensionRejectedByUser{$seq} = 1;
  }
  return $answer;
}

sub DeleteObjectsInconsistentWith {
  my ($ruleapp) = @_;

  for my $rel ( values %relations ) {
    ## TODO: We don't always need to delete!
    $rel->uninsert();
  }

  my @groups = values %NonEltObjects;
  for my $gp (@groups) {
    next unless __CheckLiveness($gp);
    my $is_consistent = $ruleapp->CheckConsitencyOfGroup($gp);
    __DeleteGroup($gp) unless $is_consistent;
  }
}

sub __DeleteNonSubgroupsOfFrom {
  my ($opts_ref) = @_;
  my $of   = $opts_ref->{of}   or confess "need of";
  my $from = $opts_ref->{from} or confess "need from";

  my %groups_to_keep;

  # Use BFT to mark groups to keep.
  my @queue = @$of;
  while (@queue) {
    my $front = shift(@queue);
    next if $front->isa('Seqsee::Element');
    $groups_to_keep{$front} = 1;
    push @queue, @$front;
  }

  for my $potential_delete (@$from) {
    next if $potential_delete ~~ %groups_to_keep;
    next if $potential_delete->isa('Seqsee::Element');
    next unless __CheckLiveness($potential_delete);

    # main::message("Deleting cruft: " . $potential_delete->as_text());
    __DeleteGroup($potential_delete);
  }

}

sub LookForSomethingLike {
  my ( $package, $opts_ref ) = @_;
  my $object = $opts_ref->{object} or confess "need object";
  my $start_position = $opts_ref->{start_position}
  or confess "need start_position";
  my $direction = $opts_ref->{direction} or confess "need direction";

  my @objects_at_that_location;
  given ($direction) {
    when ($DIR::RIGHT) {
      @objects_at_that_location =
      __GetObjectsWithEndsExactly( $start_position, undef );
    }
    when ($DIR::LEFT) {
      @objects_at_that_location =
      __GetObjectsWithEndsExactly( undef, $start_position );
    }
  }

  my $expected_structure_string = $object->get_structure_string();

  my ( @matching_objects, @potentially_matching_objects );
  for (@objects_at_that_location) {
    if ( $_->GetEffectiveObject()->get_structure_string() eq
      $expected_structure_string )
    {
      push @matching_objects, $_;
    }
    else {
      push @potentially_matching_objects, $_;
    }
  }

  my $is_object_literally_present;
  my $to_ask;

  eval {
    $is_object_literally_present = SWorkspace->check_at_location(
      { direction => $direction, start => $start_position, what => $object } );
  };
  if ( my $err = $EVAL_ERROR ) {
    CATCH_BLOCK: {
      if ( UNIVERSAL::isa( $err, 'SErr::ElementsBeyondKnownSought' ) ) {
        $to_ask = {
          expected_object => $object,
          exception       => $err,
          start_position  => $start_position,
        };
        last CATCH_BLOCK;
      }
      die $err;
    }
  }

  $is_object_literally_present = [ $start_position, $direction, $object ]
  if $is_object_literally_present;

  return Seqsee::ResultOfGetSomethingLike->new(
    {
      to_ask            => $to_ask,
      literally_present => $is_object_literally_present,
      probable_matches  => \@matching_objects,
      potential_matches => \@potentially_matching_objects,
    }
  );
}

1;
