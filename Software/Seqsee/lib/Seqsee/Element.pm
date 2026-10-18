package Seqsee::Element;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

extends 'Seqsee::Anchored';
has mag => (
  is       => 'rw',
  isa      => 'Int',
  reader   => 'get_mag',
  init_arg => 'mag',
  required => 1,
);

sub BUILD {
  my $self = shift;
  $self->describe_as($S::NUMBER);
  $self->describe_as($S::PRIME)
  if ( $Global::Feature{Primes}
    and SCategory::Prime::IsPrime( $self->get_mag ) );
  if ( $Global::Feature{Parity} ) {
    if ( $self->get_mag() % 2 ) {
      $self->describe_as($S::ODD);
    }
    else {
      $self->describe_as($S::EVEN);
    }
  }
}

sub get_structure {
  my ($self) = @_;
  $self->get_mag;
}

sub as_text {
  my ($self) = @_;
  my ( $l, $r ) = $self->get_edges;
  my $mag = $self->get_mag;
  return join( "", ( ref $self ), ":[$l,$r] $mag" );
}

my $POS_FIRST = SPos->new(1);
my $POS_LAST  = SPos->new(-1);

sub get_at_position {
  my ( $self, $position ) = @_;
  return $self if ( $position eq $POS_FIRST or $position eq $POS_LAST );
  SErr->throw("out of range for Seqsee::Element");
}

sub get_flattened {
  my ($self) = @_;
  return [ $self->get_mag ];
}

sub UpdateStrength {

  # do nothing.
}

sub CheckSquintability {
  my ( $self, $intended ) = @_;
  $self->describe_as($S::NUMBER);
  return Seqsee::Object::CheckSquintability( $self, $intended );
}

sub create {
  my ( $package, $mag, $pos ) = @_;
  my $selement = $package->new(
    left_edge  => $pos,
    right_edge => $pos,
    mag        => $mag,
    items      => [],
    group_p    => 0
  );
  $selement->get_parts_ref()->[0] = $selement;    #[sic]
  $selement->set_strength(20);
  return $selement;
}

__PACKAGE__->meta->make_immutable;
1;
