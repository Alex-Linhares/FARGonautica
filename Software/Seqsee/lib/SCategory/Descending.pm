package SCategory::Descending;
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
  q{SCategory::Descending->new()};
}

sub get_name {
  return "descending";
}

sub as_text {
  return "descending";
}

sub _guesser {
  my $subobject = shift // return;
  my $effective_object = $subobject->GetEffectiveObject();
  return unless ( ref($effective_object) eq 'Seqsee::Element' );
  return SInt->new( $effective_object->get_mag );
}

sub Instancer {
  my ( $self, $object ) = @_;
  my $start = _guesser( $object->get_items()->[0] )  or return;
  my $end   = _guesser( $object->get_items()->[-1] ) or return;

  my %guess = ( start => $start, end => $end );
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

  my ( $start, $end );

  $start =
  exists( $args_ref->{start} )
  ? $args_ref->{start}
  :$args_ref->{end} + $args_ref->{length} - 1;

  $end =
  exists( $args_ref->{end} )
  ? $args_ref->{end}
  :$args_ref->{start} - $args_ref->{length} + 1;

  $args_ref->{start}  ||= $start;
  $args_ref->{end}    ||= $end;
  $args_ref->{length} ||= $start - $end + 1;

  my $start_mag = ref($start) ? $start->get_mag() :$start;
  my $end_mag   = ref($end)   ? $end->get_mag()   :$end;
  my $ret = Seqsee::Object->create( reverse( $end_mag .. $start_mag ) );
  $ret->add_category( $self, SBindings->create( {}, $args_ref, $ret ) );
  $ret->set_reln_scheme( RELN_SCHEME::CHAIN() );
  return $ret;
}

sub AreAttributesSufficientToBuild {
  my ( $self, @args ) = @_;
  my $params_count;
  for (qw{start end length}) {
    $params_count++ if $_ ~~ @args;
  }
  return if $params_count < 2;
  return 1;
}

__PACKAGE__->meta->make_immutable;

1;
