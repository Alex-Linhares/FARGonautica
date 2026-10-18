package SCategory::Alternating;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use Class::Multimethods;
use Memoize;
use Carp;

multimethod 'FindMapping';
multimethod 'ApplyMapping';

has object1 => (
  is       => 'rw',
  reader   => 'object1',
  writer   => 'set_object1',
  init_arg => 'object1',
  required => 1,
);

has object2 => (
  is       => 'rw',
  reader   => 'object2',
  writer   => 'set_object2',
  init_arg => 'object2',
  required => 1,
);

with 'SCategory::MetonymySpec::NotMetonyable';
with 'SCategory';

sub is_pure {
  1;
}

sub Instancer {
  my ( $self, $object ) = @_;
  my $pure = $object->get_pure;
  return SBindings->new( raw_slippages => {},
    bindings => { which => SInt->new(0) } )
  if ( $pure eq $self->object1 );
  return SBindings->new( raw_slippages => {},
    bindings => { which => SInt->new(1) } )
  if ( $pure eq $self->object2 );
  return;
}

sub FindMappingForCat {
  my ( $self, $a, $b ) = @_;
  my ( $a_pure,  $b_pure )  = ( $a->get_pure,   $b->get_pure );
  my ( $object1, $object2 ) = ( $self->object1, $self->object2 );

  if ( $a_pure eq $b_pure ) {
    if ( $a_pure eq $object1 or $a_pure eq $object2 ) {
      return Mapping::Numeric->create( 'no_flip', $self );
    }
    else {
      return;
    }
  }
  else {
    if ( $a_pure eq $object1 and $b_pure eq $object2 ) {
      return Mapping::Numeric->create( 'flip', $self );
    }
    elsif ( $a_pure eq $object2 and $b_pure eq $object1 ) {
      return Mapping::Numeric->create( 'flip', $self );
    }
    else {
      return;
    }
  }
}

sub ApplyMappingForCat {
  my ( $self, $transform, $original_object ) = @_;
  my $is_object_a_ref = ref($original_object);

  my ($original_object_pure) =
    $is_object_a_ref
  ? $original_object->get_pure()
  :SLTM::Platonic->create($original_object);
  my ( $object1, $object2 ) = ( $self->object1, $self->object2 );

  my $name = $transform->get_name();
  unless ($name) {
    confess "transform without name! " . $transform->as_text;
  }

  given ($name) {
    when ('flip') {
      if ( $original_object_pure eq $object1 ) {
        my $structure = $object2->get_structure();
        return $is_object_a_ref ? Seqsee::Object->create($structure) :$structure;
      }
      elsif ( $original_object_pure eq $object2 ) {
        my $structure = $object1->get_structure();
        return $is_object_a_ref ? Seqsee::Object->create($structure) :$structure;
      }
      else {
        return;
      }
    }
    when ('no_flip') {
      if ( $original_object_pure eq $object1 ) {
        my $structure = $object1->get_structure();
        return $is_object_a_ref ? Seqsee::Object->create($structure) :$structure;
      }
      elsif ( $original_object_pure eq $object2 ) {
        my $structure = $object2->get_structure();
        return $is_object_a_ref ? Seqsee::Object->create($structure) :$structure;
      }
      else {
        return;
      }
    }
    default { confess "Should not be here!"; }
  }
}

sub FlippingMapping {
  my ($self) = @_;
  return Mapping::Numeric->create( 'flip', $self );
}

sub Create {
  my ( $package, $o1, $o2 ) = @_;
  state %MEMO;

  my ( $pure1, $pure2 ) = sort( $o1->get_pure(), $o2->get_pure() );
  my $string = "$pure1#$pure2";
  return $MEMO{$string} //= $package->new(
    {
      object1 => $pure1,
      object2 => $pure2,
    }
  );
}

sub build {
  my ( $self, $opts_ref ) = @_;
  my $which = $opts_ref->{which} or confess "need which";
  my $structure_of_object;
  given ( $which->get_mag ) {
    when (0) { $structure_of_object = $self->object1->get_structure() }
    when (1) { $structure_of_object = $self->object2->get_structure() }
    default { confess "Should not be here" };
  }
  my $object = Seqsee::Object->create($structure_of_object);
  $object->describe_as($self);
  return $object;
}

sub get_name {
  my ($self) = @_;
  return $self->object1 . ' or ' . $self->object2;
}

sub as_text {
  my ($self) = @_;
  return $self->get_name();
}

memoize('get_name');
memoize('as_text');

sub AreAttributesSufficientToBuild {
  my ( $self, @atts ) = @_;
  return 1 if 'which' ~~ @atts;
  return;
}

sub get_pure {
  return $_[0];
}

sub get_memory_dependencies {
  my ($self) = @_;
  return ( $self->object1, $self->object2 );
}

sub serialize {
  my ($self) = @_;
  return SLTM::encode( $self->object1, $self->object2 );
}

sub deserialize {
  my ( $package, $string ) = @_;
  my ( $o1,      $o2 )     = SLTM::decode($string);
  return $package->Create( $o1, $o2 );
}

sub CheckForAlternation {
  my ( $package, $first, $second, $third ) = @_;
  main::message(
    "CheckForAlternation: "
    . join( '; ', $first->as_text, $second->as_text, $third->as_text ),
    1
  );
  if ( $first->get_pure() eq $third->get_pure() ) {
    my $alternating_category =
    $package->Create( $first->get_pure(), $second->get_pure(), );
    for ( $first, $second, $third ) {
      if ( $_->isa('SInt') ) {
        my $val = ( $_ eq $second ) ? 1 :0;
        $_->add_category( $alternating_category,
          SBindings->create( {}, { which => SInt->new($val) }, $_ ) );
      }
      else {
        $_->describe_as($alternating_category);
      }
    }
    return $alternating_category->FlippingMapping();
  }

  my ($cat) = $first->get_common_categories( $second, $third ) or return;
  return if $cat->IsNumeric();    # No structure to descend into!

  my $b1 = $first->is_of_category_p($cat)  or return;
  my $b2 = $second->is_of_category_p($cat) or return;
  my $b3 = $third->is_of_category_p($cat)  or return;

  ( $b1, $b2, $b3 ) = map { $_->get_bindings_ref() } ( $b1, $b2, $b3 );
  my @keys = keys %$b1;
  return unless $cat->AreAttributesSufficientToBuild(@keys);

  my %changed_bindings;
  for my $key (@keys) {
    my ( $v1, $v2, $v3 ) = ( $b1->{$key}, $b2->{$key}, $b3->{$key} );
    my $t1 = FindMapping( $v1, $v2 );
    my $t2 = FindMapping( $v2, $v3 );
    if ( $t1 and $t1 eq $t2 ) {
      $changed_bindings{$key} = $t1;
      next;
    }

    main::message( "CheckForAlternation recursing (for $key)!", 1 );
    my $new_transform = $package->CheckForAlternation( $v1, $v2, $v3 )
    or return;
    $changed_bindings{$key} = $new_transform;
  }
  return Mapping::Structural->create(
    {
      category         => $cat,
      meto_mode        => $METO_MODE::NONE,
      direction_reln   => $Mapping::Dir::Same,
      slippages        => {},
      changed_bindings => \%changed_bindings,
    }
  );
}

__PACKAGE__->meta->make_immutable;
1;
