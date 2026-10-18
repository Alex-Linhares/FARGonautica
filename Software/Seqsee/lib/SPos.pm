package SPos;
use Carp;
use Moose;
use overload (
  'eq'     => '__equality__',
  '~~'     => '__equality__',
  fallback => 1
);

has position => (
  is       => 'rw',
  isa      => 'Int',
  required => 1,
);

sub BUILD {
  my $self     = shift;
  my $position = $self->position;
  if ( $position <= 0 and $position != -1 ) {
    confess "Attempt to set position to " . $self->position;
  }
}

sub BUILDARGS {
  my $class = shift;
  if ( @_ == 1 and not( ref( $_[0] ) ) ) {
    return { position => $_[0] };
  }
  return {@_};
}

sub __equality__ {
  $_[0]->position() == $_[1]->position();
}

sub find_range {
  my ( $self, $object ) = @_;
  my $index = $self->position;
  my $size  = $object->get_parts_count();

  my $object_str = $object->get_structure_string();
  $size == 0
  and
  SErr->throw("OutOfRange [obj=$object_str]index=$index, size=$size, ");

  if ( $index == -1 ) {
    return [ $size - 1 ];
  }

  $index < 1
  and
  SErr->throw("OutOfRange [obj=$object_str]index=$index, size=$size, ");
  $index > $size
  and
  SErr->throw("OutOfRange [obj=$object_str]index=$index, size=$size, ");

  return [ $index - 1 ];
}

1;
