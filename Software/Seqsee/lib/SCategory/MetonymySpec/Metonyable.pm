package SCategory::MetonymySpec::Metonyable;
use Moose::Role;
use Carp;

has metonym_finders => (
  traits  => ['Hash'],
  is      => 'ro',
  isa     => 'HashRef',
  default => sub { {} },
  handles => {
    'get_meto_finder' => 'get',
    'get_meto_types'  => 'keys',
  }
);

has metonymy_unfinder => (
  traits  => ['Hash'],
  is      => 'ro',
  isa     => 'HashRef',
  default => sub { {} },
  handles => { 'get_meto_unfinder' => 'get', }
);

sub find_metonym {
  my ( $self, $object, $name ) = @_;
  my $finder = $self->get_meto_finder($name)
  or croak "No '$name' meto_finder installed for category $self";
  my $bindings = $object->GetBindingForCategory($self)
  or croak "Object must belong to category";

  my $obj = $finder->( $object, $self, $name, $bindings );
  ## next line kludgy
  if ( UNIVERSAL::isa( $object, "Seqsee::Anchored" ) ) {
    $obj->get_starred->set_edges( $object->get_edges );
  }

  return $obj;
}

sub is_metonyable {
  return 1;
}

1;
