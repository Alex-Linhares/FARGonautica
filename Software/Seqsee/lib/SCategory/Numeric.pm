package SCategory::Numeric;
use Moose::Role;

sub AreAttributesSufficientToBuild {
  my ( $self, @atts ) = 1;
  return ( 'mag' ~~ @atts ) ? 1 :0;
}

sub build {
  my ( $self, $args_ref ) = @_;
  confess "Need mag" unless ( exists $args_ref->{mag} );

  my $ret = Seqsee::Element->create( $args_ref->{mag}, -1 );
  $ret->add_category( $self, SBindings->create( {}, $args_ref, $ret ) );

  return $ret;
}

sub Instancer {
  my ( $self, $object ) = @_;

  # confess "Not an Seqsee::Element: $object" unless $object->isa('Seqsee::Element');
  my $mag = $object->get_mag;
  return $self->NumericInstancer($mag);
}

requires 'NumericInstancer';
1;
