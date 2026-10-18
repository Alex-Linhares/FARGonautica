package Seqsee::ResultOfAttributeCopy;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

has success => (
  traits  => ['Bool'],
  is      => 'rw',
  isa     => 'Bool',
  default => 1,
  handles => {

  }
);

sub Success {
  return Seqsee::ResultOfAttributeCopy->new;
}

sub Failed {
  return Seqsee::ResultOfAttributeCopy->new( success => 0 );
}

sub UpdateWith {
  my ( $self, $new_value ) = @_;
  $self->success( $self->success() && $new_value->success() );
}

__PACKAGE__->meta->make_immutable;
1;

