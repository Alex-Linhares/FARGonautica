package Seqsee::ResultOfPlonk;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use Seqsee::ResultOfAttributeCopy;

has object_being_plonked => (
  is       => 'rw',
  isa      => 'Seqsee::Object',
  required => 1,
  weak_ref => 1,
);

has resultant_object => (
  is        => 'rw',
  isa       => 'Seqsee::Object',
  weak_ref  => 1,
  predicate => 'has_resultant_object',
);

has attribute_copy_result => (
  is       => 'rw',
  isa      => 'Seqsee::ResultOfAttributeCopy',
  required => 1,
  weak_ref => 0,
  handles  => { 'AttributeCopyWasSuccessful' => 'success' },
);

sub Failed {
  my ( $package, $object_being_plonked ) = @_;
  return $package->new(
    object_being_plonked  => $object_being_plonked,
    attribute_copy_result => Seqsee::ResultOfAttributeCopy->Failed(),
  );
}

sub PlonkWasSuccessful {
  my ($self) = @_;
  return $self->has_resultant_object;
}

use overload (
  q{bool} => sub {
    my ($self) = @_;
    return $self->has_resultant_object;
  },
  fallback => 1,
);

__PACKAGE__->meta->make_immutable;
1;

