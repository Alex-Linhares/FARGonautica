package SCategory::Mountain;
use Moose;

use overload
'~~'     => 'literal_comparison_hack_for_smart_match',
fallback => 1;

sub literal_comparison_hack_for_smart_match {
  return $_[0] eq $_[1];
}

with 'LTMStorable::Independent';
with 'SCategory::MetonymySpec::NotMetonyable';
with 'SCategory';

sub string_to_recreate {
  q{SCategory::Mountain->new()};
}

sub get_name {
  return "mountain";
}

sub as_text {
  return "mountain";
}

sub _guesser {
  my $subobject        = shift;
  my $effective_object = $subobject->GetEffectiveObject();
  return unless ( ref($effective_object) eq 'Seqsee::Element' );
  return SInt->new( $effective_object->get_mag );
}

sub Instancer {
  my ( $self, $object ) = @_;

  my $object_size = $object->get_parts_count;
  return unless $object_size % 2;

  my $foot = _guesser( $object->get_items()->[0] ) or return;
  my $peak = _guesser( $object->get_items()->[ ( $object_size - 1 ) / 2 ] )
  or return;

  my %guess = ( foot => $foot, peak => $peak );
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

  my $foot = $args_ref->{foot};
  my $peak = $args_ref->{peak};

  my $foot_mag = ref($foot) ? $foot->get_mag :$foot;
  my $peak_mag = ref($peak) ? $peak->get_mag :$peak;

  return if $peak_mag < $foot_mag;

  my $ret =
  ( $foot_mag == $peak_mag )
  ? Seqsee::Object->create($foot_mag)
  :Seqsee::Object->create( $foot_mag .. $peak_mag,
    reverse( $foot_mag .. $peak_mag - 1 ) );
  $ret->add_category( $self, SBindings->create( {}, $args_ref, $ret ) );
  $ret->set_reln_scheme( RELN_SCHEME::CHAIN() );
  return $ret;
}

sub AreAttributesSufficientToBuild {
  my ( $self, @args ) = @_;
  return unless 'foot' ~~ @args;
  return unless 'peak' ~~ @args;
  return 1;
}

__PACKAGE__->meta->make_immutable;

1;
