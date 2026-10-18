package Seqsee::Anchored;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use List::Util qw(min max sum);
use Try::Tiny;
use Class::Multimethods;
multimethod 'ApplyMapping';
multimethod 'SanityCheck';

extends 'Seqsee::Object';

# with 'Categorizable';

has left_edge => (
  is => 'rw',

  #isa        => 'Int',
  reader   => 'get_left_edge',
  writer   => 'set_left_edge',
  init_arg => 'left_edge',
  required => 1,
);

has right_edge => (
  is => 'rw',

  #isa        => 'Int',
  reader   => 'get_right_edge',
  writer   => 'set_right_edge',
  init_arg => 'right_edge',
  required => 1,
);

#has object => (
#    is         => 'rw',
#    isa        => 'Seqsee::Object',
#    required   => 1,
#    handles => [qw{get_strength set_strength
#      get_history_object get_parts_ref
#      get_group_p set_group_p get_metonym
#      get_metonym_activeness get_is_a_metonym set_is_a_metonym
#      get_direction set_direction get_reln_scheme set_reln_scheme
#      get_underlying_reln
#      get_history AddHistory search_history UnchangedSince GetAge
#      history_as_text get_items get_items_array
#      annotate_with_cat get_structure
#      get_flattened get_parts_count apply_blemish_at
#      describe_as redescribe_as
#      set_underlying_ruleapp get_structure_string GetAnnotatedStructureString
#      apply_reln_scheme recalculate_categories get_pure
#      HasAsItem HasAsPartDeep
#      SetMetonym SetMetonymActiveness GetEffectiveObject GetEffectiveStructure
#      GetEffectiveStructureString GetConcreteObject AnnotateWithMetonym
#      MaybeAnnotateWithMetonym IsThisAMetonymedObject
#      ContainsAMetonym
#      AddRelation RemoveRelation RemoveAllRelations get_relation
#      recalculate_relations CanBeSeenAs_ByPart
#      CanBeSeenAs_Meto CanBeSeenAs_Literal CanBeSeenAs_LiteralOrMeto
#      GeteffectiveSlippages CheckSquintability CheckSquintabilityForCategory
#      UpdateStrength
#
#      get_cats_hash add_category remove_category get_categories get_categories_as_string
#      is_of_category_p GetBindingForCategory get_blemish_cats
#      HasNonAdHocCategory CopyCategoriesTo
#    }],
#);

has is_locked_against_deletion => (
  is       => 'rw',
  isa      => 'Bool',
  reader   => 'get_is_locked_against_deletion',
  writer   => 'set_is_locked_against_deletion',
  init_arg => 'is_locked_against_deletion',
);

sub set_edges {
  my ( $self, $left, $right ) = @_;
  $self->set_left_edge($left);
  $self->set_right_edge($right);
  return $self;
}

sub get_edges {
  my ($self) = @_;
  return ( $self->get_left_edge(), $self->get_right_edge() );
}

sub get_bounds_string {
  my ($self) = @_;
  return
    q{ <}
  . $self->get_left_edge() . q{, }
  . $self->get_right_edge() . q{> };
}

sub get_span {
  my ($self) = @_;
  return $self->get_right_edge() - $self->get_left_edge() + 1;
}

sub as_text {
  my ($self)           = @_;
  my $bounds_string    = $self->get_bounds_string();
  my $structure_string = $self->GetAnnotatedStructureString();
  my $ruleapp = $self->get_underlying_reln ? 'u' :'';
  return "Seqsee::Anchored $ruleapp$bounds_string $structure_string";
}

sub get_next_pos_in_dir {
  my ( $self, $direction ) = @_;
  if ( $direction eq DIR::RIGHT() ) {
    return $self->get_right_edge() + 1;
  }
  elsif ( $direction eq DIR::LEFT() ) {
    my $le = $self->get_left_edge();
    return unless $le > 0;
    return $le - 1;
  }
  else {
    confess "funny direction to extnd in!!";
  }

}

sub spans {
  my ( $self, $other ) = @_;
  my ( $sl,   $sr )    = $self->get_edges;
  my ( $ol,   $or )    = $other->get_edges;
  return ( $sl <= $ol and $or <= $sr );
}

sub overlaps {
  my ( $self, $other ) = @_;
  my ( $sl,   $sr )    = $self->get_edges;
  my ( $ol,   $or )    = $other->get_edges;
  return ( ( $sr <= $or and $sr >= $ol ) or ( $or <= $sr and $or >= $sl ) );
}

sub IsFlushRight {
  my ($self) = @_;
  $self->get_right_edge() == $SWorkspace::ElementCount - 1 ? 1 :0;
}

sub IsFlushLeft {
  my ($self) = @_;
  $self->get_left_edge() == 0 ? 1 :0;
}

sub recalculate_edges {
  my ($self) = @_;
  my $subobjects_ref = $self->get_items;
  $self->set_left_edge( $subobjects_ref->[0]->get_left_edge() );
  $self->set_right_edge( $subobjects_ref->[-1]->get_right_edge() );
}

sub _CheckValidity {
  my @items = @_;
  SErr->throw("EmptyCreate") unless @items;

  # Check all anchored, no holes, no overlap.
  my $first_item = shift(@items);
  confess "Unanchored object $first_item" unless $first_item->isa('Seqsee::Anchored');
  my $most_recent_right_edge = $first_item->get_right_edge;

  while ( my $next_item = shift(@items) ) {
    confess "Unanchored object $next_item" unless $next_item->isa('Seqsee::Anchored');
    return unless $next_item->get_left_edge() == $most_recent_right_edge + 1;
    $most_recent_right_edge = $next_item->get_right_edge();
  }

  return 1;
}

sub create {
  my ( $package, @items ) = @_;
  return unless _CheckValidity(@items);

  my $anchored_object;
  if ( @items == 1 ) {
    my $only_item = $items[0];
    $anchored_object = $only_item;
  }
  else {
    $anchored_object = $package->new(
      left_edge  => $items[0]->get_left_edge(),
      right_edge => $items[-1]->get_right_edge(),
      items      => \@items,
      group_p    => 1
    );
  }
  $anchored_object->UpdateStrength();
  return $anchored_object;
}

sub Extend {
  scalar(@_) == 3 or confess "Need 3 arguments";
  my ( $self, $to_insert, $insert_at_end_p ) = @_;

  my @current_subojects = $self->get_items_array;

  my @new_subobjects;
  if ($insert_at_end_p) {
    @new_subobjects = ( @current_subojects, $to_insert );
  }
  else {
    @new_subobjects = ( $to_insert, @current_subojects );
  }

  my $potential_new_group = Seqsee::Anchored->create(@new_subobjects)
  or SErr::CouldNotCreateExtendedGroup->new("Extended group creation failed")
  ->throw();
  my $conflicts = SWorkspace::__FindGroupsConflictingWith($potential_new_group);
  if ($conflicts) {
    $conflicts->Resolve( { IgnoreConflictWith => $self } ) or return;
  }

  # If there are supergroups, they must die. Kludge, for now:
  if ( my @supergps = SWorkspace->GetSuperGroups($self) ) {
    if ( SUtil::toss(0.5) ) {
      for (@supergps) {
        SWorkspace::__DeleteGroup($_);
      }
    }
    else {
      return;
    }
  }

  # If we get here, all conflicting incumbents are dead.
  @{ $self->get_parts_ref } = @new_subobjects;

  $self->Update();
  $self->AddHistory( "Extended to become " . $self->get_bounds_string() );

  # SanityCheck($self);
  return 1;
}

sub SafeExtend { # Same as Extend, but traps CouldNotCreateExtendedGroup
  scalar(@_) == 3 or confess "Need 3 arguments";
  my ( $self, $to_insert, $insert_at_end_p ) = @_;

  eval { $self->Extend($to_insert, $insert_at_end_p) };
  
  my $e;
  if ( $e = Exception::Class->caught('SErr::CouldNotCreateExtendedGroup') ) {
    return 0;
  }
  elsif($e = Exception::Class->caught()) {
    ref $e ? $e->rethrow : die $e;
  }

  return 1;
}

sub Update {
  my ($self) = @_;
  $self->recalculate_edges();
  $self->recalculate_categories();
  $self->recalculate_relations();
  $self->UpdateStrength();
  if ( my $underlying_reln = $self->get_underlying_reln() ) {

    # print "UPDATING RULEAPP $underlying_reln\n";
    my $error;
    try { $self->set_underlying_ruleapp( $underlying_reln->get_rule() ) }
    catch {
      SWorkspace->remove_gp($self);
      $error = 1;
    };
    return if $error;
    confess "underlying_reln lost here" unless $self->get_underlying_reln;
  }

  # SWorkspace::UpdateGroupsContaining($self);
  SWorkspace::__UpdateGroup($self);
}

sub FindExtension {
  @_ == 3 or confess "FindExtension for an object requires 3 args";
  my ( $self, $direction_to_extend_in, $skip ) = @_;

  my $underlying_ruleapp = $self->get_underlying_reln() or return;
  return $underlying_ruleapp->FindExtension(
    {
      direction_to_extend_in  => $direction_to_extend_in,
      skip_this_many_elements => $skip
    }
  );
}

__PACKAGE__->meta->make_immutable;
1;

