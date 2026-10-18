package LTMStorable::Independent;
use Moose::Role;

requires 'string_to_recreate';

sub is_pure {
  1;
}

sub get_pure {
  $_[0];
}

sub get_memory_dependencies {
  return;
}

sub serialize {
  my ($self) = @_;
  return $self->string_to_recreate;
}

sub deserialize {
  my ( $package, $string ) = @_;
  return eval($string);
}

1;
