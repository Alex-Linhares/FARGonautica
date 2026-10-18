package SCodeletBase;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

has family => (
  is       => 'ro',
  isa      => 'Str',
  required => 1,
);

has urgency => (
  is       => 'ro',
  required => 1,
);

has arguments => (
  is       => 'ro',
  required => 1,
);

sub run {
  my $self = shift;

  $Global::CurrentCodelet       = $self;
  $Global::CurrentCodeletFamily = $self->family;

  no strict;
  my $method_name = "Seqsee::SCF::" . $self->family() . '::run';
  $method_name->( $self, $self->arguments );
}

__PACKAGE__->meta->make_immutable;
1;
