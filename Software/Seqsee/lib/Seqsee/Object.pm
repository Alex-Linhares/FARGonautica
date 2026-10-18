package Seqsee::Object;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use Class::Multimethods;
with 'Categorizable';
use overload (
  '~~'     => 'literal_comparison_hack_for_smart_match',
  '@{}'    => sub { $_[0]->get_parts_ref },
  fallback => 1
);

sub literal_comparison_hack_for_smart_match {
  return $_[0] eq $_[1];
}

multimethod 'FindMapping';
multimethod 'ApplyMapping';

has strength => (
  is       => 'rw',
  reader   => 'get_strength',
  writer   => 'set_strength',
  init_arg => 'strength',
  default  => 0,
);

has history_obj => (
  is       => 'rw',
  isa      => 'SHistory',
  reader   => 'get_history_obj',
  writer   => 'set_history_obj',
  default  => sub { SHistory->new },
  required => 0,
  weak_ref => 0,
  handles  => [
    qw{get_history AddHistory UnchangedSince
    search_history history_as_text GetAge}
  ],
);

has group_p => (
  is       => 'rw',
  isa      => 'Bool',
  reader   => 'get_group_p',
  writer   => 'set_group_p',
  init_arg => 'group_p',
  required => 1,
  weak_ref => 0,
);

has metonym => (
  is     => 'rw',
  reader => 'get_metonym',
  writer => 'set_metonym',
);

has metonym_activeness => (
  is      => 'rw',
  isa     => 'Bool',
  reader  => 'get_metonym_activeness',
  writer  => 'set_metonym_activeness',
  default => 0,
);

# should be: is_a_metonym_of
has is_a_metonym => (
  is     => 'rw',
  reader => 'get_is_a_metonym',
  writer => 'set_is_a_metonym',
);

has direction => (
  is      => 'rw',
  reader  => 'get_direction',
  writer  => 'set_direction',
  default => $DIR::RIGHT,
);

has reln_scheme => (
  is     => 'rw',
  reader => 'get_reln_scheme',
  writer => 'set_reln_scheme',
);

has reln_other_end => (
  traits  => ['Hash'],
  is      => 'ro',
  isa     => 'HashRef',
  default => sub { {} },
  handles => {
    'get_relation'       => 'get',
    'set_relation_to'    => 'set',
    'remove_reln_to'     => 'delete',
    'relation_exists_to' => 'exists',
    'all_relations'      => 'values',
  }
);

has underlying_reln => (
  is     => 'rw',
  reader => 'get_underlying_reln',
  writer => 'set_underlying_reln',
);

has item => (
  traits   => ['Array'],
  is       => 'rw',
  isa      => 'ArrayRef',
  default  => sub { [] },
  reader   => 'get_parts_ref',
  init_arg => 'items',
  handles  => {
    'get_items_array' => 'elements',
    'get_parts_count' => 'count',
  }
);

sub get_items {
  shift->get_parts_ref;
}

sub create {
  my ( $package, @arguments ) = @_;
  if ( !@arguments ) {
    return $package->new( { group_p => 1, items => [] } );
  }
  if ( scalar(@arguments) == 1 ) {
    my $sole_argument = $arguments[0];

    # If it is unblessed, it better be a number!
    unless ( ref $sole_argument ) {
      return Seqsee::Element->create( $sole_argument, 0 );
    }

    if ( ref($sole_argument) eq 'ARRAY' ) {
      return $package->create( @{$sole_argument} );
    }

    # So it is an Seqsee::Object of some sort...
    my @categories = @{ $sole_argument->get_categories };
    my $new_object;
    if ( ref($sole_argument) eq 'Seqsee::Element' ) {
      $new_object = Seqsee::Element->create( $sole_argument->get_mag, 0 );
    }
    else {
      $new_object = $package->create( $sole_argument->get_items_array );
    }

    for (@categories) {
      $new_object->describe_as($_);
    }

    return $new_object;
  }
  my @new_arguments = map { Seqsee::Object->create($_) } @arguments;
  return $package->new( { group_p => 1, items => \@new_arguments } );
}

sub annotate_with_cat {
  my ( $self, $cat ) = @_;
  my $bindings = $self->describe_as($cat);

  SErr->throw("Not of category") unless $bindings;
  return $bindings;
}

sub get_structure {
  my ($self) = shift;
  return $self->get_parts_ref()->[0]->get_structure()
  if $self->get_parts_count() == 1;
  return [ map { $_->get_structure() } $self->get_items_array() ];
}

sub get_flattened {
  my ($self) = @_;
  return [ map { @{ $_->get_flattened() } } $self->get_items_array() ];
}

sub apply_blemish_at {
  my ( $object, $meto_type, $position ) = @_;
  my (@indices) = @{ $position->find_range($object) };

  #XXX assumption in prev line that a single item returned
  my @metonyms;

  my @subobjects = $object->get_items_array;
  my $meto_cat   = $meto_type->get_category;
  my $meto_name  = $meto_type->get_name;

  for my $index (@indices) {
    my $obj_at_pos              = $subobjects[$index];
    my $blemished_object_at_pos = $meto_type->blemish($obj_at_pos);
    my $metonym                 = SMetonym->new(
      {
        category  => $meto_cat,
        name      => $meto_name,
        info_loss => $meto_type->get_info_loss,
        starred   => $obj_at_pos,
        unstarred => $blemished_object_at_pos,
      },
    );
    push @metonyms, $metonym;
    ## $metonym
    ## $blemished_object_at_pos->get_structure()
    ## $blemished_object_at_pos->get_metonym
    $subobjects[$index] = $blemished_object_at_pos;
  }
  my $ret = Seqsee::Object->create(@subobjects);
  ## $ret->get_structure()
  for my $index (@indices) {
    my $metonym = shift(@metonyms);
    $ret->[$index]->describe_as($meto_cat);
    $ret->[$index]->SetMetonym($metonym);
    $metonym->get_starred()->set_is_a_metonym( $ret->[$index] );
    $ret->[$index]->SetMetonymActiveness(1);
  }
  return $ret;

  # maybe make it belong to the category...
}

# method: describe_as
# Try to describe the object sa belonging to that category
#

sub describe_as {
  my ( $self, $cat ) = @_;
  my $is_of_cat = $self->is_of_category_p($cat);

  return $is_of_cat if $is_of_cat;

  my $bindings = $cat->is_instance($self);
  if ($bindings) {
    ## describe_as succeeded!
    $self->add_category( $cat, $bindings );
  }

  return $bindings;
}

# method: describe_as
# Try to describe the object sa belonging to that category
#

sub redescribe_as {
  my ( $self, $cat ) = @_;
  my $bindings = $cat->is_instance($self);
  if ($bindings) {
    ## describe_as succeeded!
    $self->AddHistory(
      "redescribe as instance of category " . $cat->get_name . " succeded" );
    $self->add_category( $cat, $bindings );
  }
  else {
    $self->AddHistory(
      "redescribe as instance of category " . $cat->get_name . " failed" );
    $self->remove_category($cat);
  }

  return $bindings;

}

sub get_structure_string {
  my ($self) = @_;
  my $struct = $self->get_structure;
  SUtil::StructureToString($struct);
}

sub GetAnnotatedStructureString {
  my ($self) = @_;
  my $body =
    $self->isa('Seqsee::Element')
  ? $self->get_mag()
  :'['
  . join( ', ', map { $_->GetAnnotatedStructureString } $self->get_items_array )
  . ']';

  if ( $self->get_metonym_activeness() ) {
    my $meto_structure_string =
    $self->GetEffectiveObject()->get_structure_string();
    $body .= " --*-> $meto_structure_string";
  }

  return $body;
}

# XXX(Assumption): [2006/09/16] Parts are non-overlapping.
sub get_span {
  my ($self) = @_;
  return List::Util::sum( map { $_->get_span } $self->get_items_array() );
}

sub apply_reln_scheme {
  my ( $self, $scheme ) = @_;
  return unless $scheme;
  if ( $scheme == RELN_SCHEME::CHAIN() ) {
    my $parts_ref = $self->get_parts_ref;
    my $cnt       = $self->get_parts_count;
    for my $i ( 0 .. ( $cnt - 2 ) ) {
      my ( $a, $b ) = ( $parts_ref->[$i], $parts_ref->[ $i + 1 ] );
      next if $a->get_relation($b);
      my $transform = FindMapping( $a, $b );
      my $rel =
      SRelation->new( { first => $a, second => $b, type => $transform } );
      $rel->insert() if $rel;
    }
    $self->AddHistory("Relation scheme \"chain\" applied");
  }
  else {
    confess "Relation scheme $scheme not implemented";
  }
}

# XXX(Board-it-up): [2006/09/16] Recalculation ignores categories.
# XXX(Assumption): [2006/09/16] Unique relation between two objects.

sub recalculate_categories {
  my ($self) = @_;

  my $cats = $self->get_categories();
  for my $cat (@$cats) {
    $self->redescribe_as($cat);
  }

  unless ( $self->category_list_as_strings ) {
    confess "LOST ALL CATEGORIES!!! $self. Had @$cats\n";
  }
}

sub get_pure {
  my ($self) = @_;
  return SLTM::Platonic->create( $self->get_structure_string() );
}

sub HasAsItem {
  my ( $self, $item ) = @_;
  for ( $self->get_items_array() ) {
    return 1 if $_ eq $item;
  }
  return 0;
}

sub Seqsee::Element::HasAsPartDeep {
  my ( $self, $item ) = @_;
  return $self eq $item;
}

sub HasAsPartDeep {
  my ( $self, $item ) = @_;
  for ( $self->get_items_array() ) {
    return 1 if $_ eq $item;
    return 1 if $_->HasAsPartDeep($item);
  }
  return 0;
}

# ###################################################################
# VERSION POST REFACTORING

# METONYM MANAGEMENT
sub SetMetonym {
  my ( $self, $meto ) = @_;
  my $starred = $meto->get_starred();
  SErr->throw("Metonym must be an Seqsee::Object! Got: $starred")
  unless UNIVERSAL::isa( $starred, "Seqsee::Object" );
  $starred->set_is_a_metonym($self);
  $self->set_metonym($meto);
}

sub SetMetonymActiveness {
  my ( $self, $value ) = @_;
  if ($value) {
    return if $self->get_metonym_activeness;
    unless ( $self->get_metonym ) {
      SErr->throw("Cannot SetMetonymActiveness without a metonym");
    }
    $self->AddHistory("Metonym activeness turned on");
    $self->set_metonym_activeness(1);
  }
  else {
    $self->AddHistory("Metonym activeness turned off");
    $self->set_metonym_activeness(0);
  }
}

sub GetEffectiveObject {
  my ($self) = @_;
  return $self->get_metonym_activeness()
  ? $self->get_metonym()->get_starred()
  :$self;
}

sub GetEffectiveStructure {
  my ($self) = @_;
  return [ map { $_->GetEffectiveObject()->get_structure }
    $self->get_items_array() ];
}

sub Seqsee::Element::GetEffectiveStructure {
  my ($self) = @_;
  return $self->get_mag();
}

sub GetEffectiveStructureString {
  my ($self) = @_;
  return SUtil::StructureToString( $self->GetEffectiveStructure() );
}

sub GetConcreteObject {
  my ($self) = @_;
  return $self->get_is_a_metonym() || $self;
}

sub AnnotateWithMetonym {
  my ( $self, $cat, $name ) = @_;
  my $is_of_cat = $self->is_of_category_p($cat);

  unless ($is_of_cat) {
    $self->annotate_with_cat($cat);
  }

  my $meto = $cat->find_metonym( $self, $name );
  SErr::MetonymNotAppicable->throw() unless $meto;

  $self->AddHistory( "Added metonym \"$name\" for cat " . $cat->get_name() );
  $self->SetMetonym($meto);
}

sub MaybeAnnotateWithMetonym {
  my ( $self, $cat, $name ) = @_;

  eval {  $self->AnnotateWithMetonym( $cat, $name ) };
  
  my $e;
  if ( $e = Exception::Class->caught('SErr::MetonymNotAppicable') ) {
    # Ignore.
  }
  else {
    $e = Exception::Class->caught();
    ref $e ? $e->rethrow : die $e;
  }
}

sub IsThisAMetonymedObject {
  my ($self) = @_;
  my $is_a_metonym_of = $self->get_is_a_metonym();
  return 0 if ( not($is_a_metonym_of) or $is_a_metonym_of eq $self );
  return 1;
}

sub ContainsAMetonym {
  my ($self) = @_;
  return 1 if $self->IsThisAMetonymedObject;
  for ( $self->get_items_array() ) {
    return 1 if $_->ContainsAMetonym;
  }
  return 0;
}

sub Seqsee::Element::ContainsAMetonym {
  return 0;
}

# #################################
# RELATION MANAGEMENT
# Relevant variables:
# %reln_other_of

sub AddRelation {
  my ( $self, $reln ) = @_;
  my $other = $self->_get_other_end_of_reln($reln);

  if ( $self->relation_exists_to($other) ) {
    SErr->throw("duplicate reln being added");
  }
  $self->AddHistory( "added reln to " . $other->get_bounds_string() );
  $self->set_relation_to( $other, $reln );
}

sub RemoveRelation {
  my ( $self, $reln ) = @_;
  my $other = $self->_get_other_end_of_reln($reln);
  $self->AddHistory( "removed reln to " . $other->get_bounds_string() );
  $self->remove_reln_to($other);
}

sub RemoveAllRelations {
  my ($self) = @_;
  for ( $self->all_relations() ) {
    $_->uninsert();
  }
}

sub _get_other_end_of_reln {
  my ( $self, $reln ) = @_;
  my ( $f,    $s )    = $reln->get_ends();
  return $s if $f eq $self;
  return $f if $s eq $self;
  SErr->throw("relation error: not an end");
}

sub recalculate_relations {
  my ($self) = @_;
  for my $reln ( $self->all_relations() ) {
    my $type     = $reln->get_type();
    my $new_type = $type->get_category()->FindMappingForCat( $reln->get_ends );

    if ($new_type) {
      my ( $f, $s ) = $reln->get_ends;
      my $new_rel =
      SRelation->new( { first => $f, second => $s, type => $new_type } );
      $reln->uninsert;
      $new_rel->insert;
    }
    else {
      $reln->uninsert;

      #main::message("A relation no longer valid, removing!");
    }
  }
}

sub as_text {
  my ($self) = @_;
  my $structure_string = $self->get_structure_string();
  return "Seqsee::Object $structure_string";
}

multimethod CanBeSeenAs => ( '#', '#' ) => sub {
  my ( $a, $b ) = @_;
  return Seqsee::ResultOfCanBeSeenAs->newUnblemished() if $a == $b;
  return Seqsee::ResultOfCanBeSeenAs->NO();
};

multimethod CanBeSeenAs => ( 'Seqsee::Object', 'Seqsee::Object' ) => sub {
  my ( $obj, $structure ) = @_;
  return CanBeSeenAs( $obj, $structure->get_structure() );
};

multimethod CanBeSeenAs => ( 'Seqsee::Object', '#' ) => sub {
  my ( $object, $int ) = @_;
  my $lit_or_meto = $object->CanBeSeenAs_Literal0rMeto($int);

  #if (not $object->isa('Seqsee::Element')) {
  #  print "LIT OR METO: ", $object->as_text, "===> $int\n";
  #}
  ## lit_or_meto(elt): $lit_or_meto, $object->as_text()
  return $lit_or_meto if defined $lit_or_meto;
  return Seqsee::ResultOfCanBeSeenAs::NO();

};

multimethod CanBeSeenAs => ( 'Seqsee::Element', '#' ) => sub {
  return ( $_[0]->get_mag() == $_[1] )
  ? Seqsee::ResultOfCanBeSeenAs->newUnblemished()
  :Seqsee::ResultOfCanBeSeenAs::NO();
};

multimethod CanBeSeenAs => ( 'Seqsee::Element', '$' ) => sub {
  if ( $_[1] =~ m#^-?\d+$# ) {
    return ( $_[0]->get_mag() == $_[1] )
    ? Seqsee::ResultOfCanBeSeenAs->newUnblemished()
    :Seqsee::ResultOfCanBeSeenAs::NO();
  }
  confess "SAW CanBeSeenAs(Seqsee::Element, \$): " . $_[0]->as_text . " '" . $_[1];
};

multimethod CanBeSeenAs => ( 'Seqsee::Object', 'ARRAY' ) => sub {
  my ( $object, $structure ) = @_;
  my $meto_activeness = $object->get_metonym_activeness();
  my $metonym         = $object->get_metonym();
  my $starred         = $metonym ? $metonym->get_starred() :undef;
  ## before active meto
  if ($meto_activeness) {
    my $meto_seen_as =
    $object->CanBeSeenAs_Meto( $structure, $starred, $metonym );
    return $meto_seen_as if defined $meto_seen_as;
  }

  ## before by part
  my $part_seen_as = $object->CanBeSeenAs_ByPart($structure);
  return $part_seen_as if defined $part_seen_as;

  ## before meto
  if ($metonym) {
    my $meto_seen_as =
    $object->CanBeSeenAs_Meto( $structure, $starred, $metonym );
    return $meto_seen_as if defined $meto_seen_as;
  }
  ## failed CanBeSeenAs
  return Seqsee::ResultOfCanBeSeenAs::NO();
};

sub CanBeSeenAs_ByPart {
  my ( $object, $structure ) = @_;
  my $seen_as_part_count = scalar(@$structure);
  ## $seen_as_part_count
  return
  unless scalar(@$object) == $seen_as_part_count;
  my %blemishes;
  my $obj_part_ref = $object->get_parts_ref();
  for my $i ( 0 .. $seen_as_part_count - 1 ) {
    my $obj_part            = $obj_part_ref->[$i];
    my $seen_as_part        = $structure->[$i];
    my $part_can_be_seen_as = CanBeSeenAs( $obj_part, $seen_as_part );
    ## obj, seen_as: $obj_part->as_text(), $seen_as_part, $part_can_be_seen_as
    return unless $part_can_be_seen_as;
    return if $part_can_be_seen_as->ArePartsBlemished();
    ## is part blemished: $part_can_be_seen_as->IsBlemished()
    next unless $part_can_be_seen_as->IsBlemished();
    $blemishes{$i} = $part_can_be_seen_as->GetEntireBlemish();
  }
  ## %blemishes
  return Seqsee::ResultOfCanBeSeenAs->newUnblemished() unless %blemishes;
  return Seqsee::ResultOfCanBeSeenAs->newByPart( \%blemishes );
}

sub CanBeSeenAs_Meto {
  scalar(@_) == 4 or confess;
  my ( $object, $structure, $starred, $metonym ) = @_;
  return Seqsee::ResultOfCanBeSeenAs->newEntireBlemish($metonym)
  if SUtil::compare_deep( $starred->get_structure(), $structure );
}

sub CanBeSeenAs_Literal {
  my ( $object, $structure ) = @_;
  return Seqsee::ResultOfCanBeSeenAs->newUnblemished()
  if SUtil::compare_deep( $object->get_structure(), $structure );
}

sub CanBeSeenAs_Literal0rMeto {
  my ( $object, $structure ) = @_;
  $structure = $structure->get_structure()
  if UNIVERSAL::isa( $structure, 'Seqsee::Object' );

  my $meto_activeness = $object->get_metonym_activeness();
  my $metonym         = $object->get_metonym();
  my $starred         = $metonym ? $metonym->get_starred() :undef;

  if ($meto_activeness) {
    ## active metonym
    return Seqsee::ResultOfCanBeSeenAs->newEntireBlemish($metonym)
    if SUtil::compare_deep( $starred->get_structure(), $structure );
  }

  return Seqsee::ResultOfCanBeSeenAs->newUnblemished()
  if SUtil::compare_deep( $object->get_structure(), $structure );

  if ($metonym) {
    ## inactive metonym
    return Seqsee::ResultOfCanBeSeenAs->newEntireBlemish($metonym)
    if SUtil::compare_deep( $starred->get_structure(), $structure );
  }

#if we get here, it means that the metonym, if present,is not active. and also that the metonym or the object itself cannot be seen as structure
  return;
}

# Returns active metonyms, for use in, for example, bindings creation.
sub GetEffectiveSlippages {
  my ($self)      = @_;
  my @parts       = $self->get_items_array();
  my $parts_count = scalar(@parts);
  my $return      = {};
  for my $idx ( 0 .. $parts_count - 1 ) {
    next unless $parts[$idx]->get_metonym_activeness();
    $return->{$idx} = $parts[$idx]->get_metonym();
  }
  return $return;
}

sub CheckSquintability {
  my ( $self, $intended ) = @_;
  my $intended_structure_string = $intended->get_structure_string();
  return
  map { $self->CheckSquintabilityForCategory( $intended_structure_string, $_ ) }
  @{ $self->get_categories() };
}

sub CheckSquintabilityForCategory {
  my ( $self, $intended_structure_string, $category ) = @_;

  #main::message("CheckSquintabilityForCategory: $category");
  my $bindings = $self->GetBindingForCategory($category)
  or confess
  "CheckSquintabilityForCategory called on object not an instance of the category";

  my @meto_types = $category->get_meto_types();
  my @return;
  for my $name (@meto_types) {

    #main::message("Meto type: $name");
    my $finder = $category->get_meto_finder($name);
    my $squinted = $finder->( $self, $category, $name, $bindings ) or next;

#main::message("Squinted: " . $squinted->get_starred->get_structure_string . '=?=' . $intended_structure_string);
    next
    unless $squinted->get_starred()->get_structure_string() eq
      $intended_structure_string;
    push @return, $squinted->get_type();
  }
  return @return;
}

sub UpdateStrength {
  my ($self) = @_;
  my $strength_from_parts =
  20 + 0.2 *
  ( List::Util::sum( map { $_->get_strength() } @{ $self->get_parts_ref() } )
    || 0 );
  my $strength_from_categories = 30 * (
    List::Util::sum(
      @{ SLTM::GetRealActivationsForConcepts( $self->get_categories() ) }
    )
    || 0
  );
  my $strength = $strength_from_parts + $strength_from_categories;
  $strength += ( $Global::GroupStrengthByConsistency{$self} || 0 );
  $strength = 100 if $strength > 100;
  ## p, c, t: $strength_from_parts, $strength_from_categories, $strength
  $self->set_strength($strength);
}

# XXX(Board-it-up): [2007/02/03] changing reln to ruleapp!
sub set_underlying_ruleapp {
  my ( $self, $reln ) = @_;
  $reln or confess "Cannot set underlying relation to be an undefined value!";

  if ( UNIVERSAL::isa( $reln, "SRelation" )
    or UNIVERSAL::isa( $reln, 'Mapping' ) )
  {
    $reln = SRule->create($reln) or return;
  }
  my $ruleapp;
  if ( UNIVERSAL::isa( $reln, "SRule" ) ) {
    $ruleapp = $reln->CheckApplicability(
      {
        objects   => [ $self->get_items_array() ],
        direction => $DIR::RIGHT,
      }
    );    # could be undef.
  }
  else {
    print "Funny argument $reln to set_underlying_ruleapp!";
    confess "Funny argument $reln to set_underlying_ruleapp!";
  }

  $self->AddHistory("Underlying relation set: $ruleapp ");
  $self->set_underlying_reln($ruleapp);
}

1;

__PACKAGE__->meta->make_immutable;
1;
