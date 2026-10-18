package Memory::Node;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

has core => (
  is       => 'rw',
  isa      => 'Memory::Storable',
  required => 1,
);

__PACKAGE__->meta->make_immutable;
1;
