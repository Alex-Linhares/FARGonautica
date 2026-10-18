package SCategory::Sameness;
use Moose;

use overload
'~~'     => 'literal_comparison_hack_for_smart_match',
fallback => 1;

sub literal_comparison_hack_for_smart_match {
  return $_[0] eq $_[1];
}

with 'LTMStorable::Independent';
with 'SCategory::MetonymySpec::Metonyable';
with 'SCategory';

has '+metonym_finders' => (
  default => sub {
    {
      each => sub {
        my ( $object, $cat, $name, $bindings ) = @_;
        my $starred =
        Seqsee::Object->create( $bindings->GetBindingForAttribute("each") );
        my $info_lost =
        { length => $bindings->GetBindingForAttribute("length") };

        return SMetonym->new(
          {
            category  => $cat,
            name      => $name,
            starred   => Seqsee::Anchored->create($starred),
            unstarred => $object,
            info_loss => $info_lost,
          }
        );
      }
    }
  },
);

has '+metonymy_unfinder' => (
  default => sub {
    {
      each => sub {
        my ( $cat, $name, $info_loss, $object ) = @_;
        unless ( exists $info_loss->{length} ) {
          confess "Length missing in info_loss: " . %$info_loss;
        }
        return $cat->build( { each => $object, %$info_loss } );
      }
    }
  },
);

sub string_to_recreate {
  q{SCategory::Sameness->new()};
}

sub get_name {
  return "sameness";
}

sub as_text {
  return "sameness";
}

sub _guesser {
  my $subobject        = shift;
  my $effective_object = $subobject->GetEffectiveObject();
  return unless ( ref($effective_object) eq 'Seqsee::Element' );
  return SInt->new( $effective_object->get_mag );
}

sub Instancer {
  my ( $self, $object ) = @_;
  my %guess = (
    length => SInt->new( $object->get_parts_count ),
    each   => $object->get_items()->[0]
  );
  my $guess_built = $self->build( \%guess );

  my $result_of_can_be_seen_as = $object->CanBeSeenAs($guess_built);
  return unless $result_of_can_be_seen_as;

  my $slippages = $result_of_can_be_seen_as->GetPartsBlemished() || {};

  if ( $object->isa('Seqsee::Element') ) {
    if ( my $entire_blemish = $result_of_can_be_seen_as->GetEntireBlemish() ) {
      $slippages = { 0 => $entire_blemish };
    }
  }

  return SBindings->create( $slippages, \%guess, $object );
}

sub build {
  my ( $self, $args_ref ) = @_;
  confess 'Too few params'
  unless $self->AreAttributesSufficientToBuild( keys %{$args_ref} );

  my $length_ref = $args_ref->{length};
  my $each       = $args_ref->{each};

  my $length = ref($length_ref) ? $length_ref->get_mag :$length_ref;

  return if $length < 1;

  my $ret = Seqsee::Object->create( map { $each } ( 1 .. $length ) );
  $ret->add_category( $self, SBindings->create( {}, $args_ref, $ret ) );
  $ret->set_reln_scheme( RELN_SCHEME::CHAIN() );
  return $ret;
}

sub AreAttributesSufficientToBuild {
  my ( $self, @args ) = @_;
  return unless 'each'   ~~ @args;
  return unless 'length' ~~ @args;
  return 1;
}

__PACKAGE__->meta->make_immutable;

1;
