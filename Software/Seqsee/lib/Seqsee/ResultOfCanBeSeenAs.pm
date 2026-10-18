package Seqsee::ResultOfCanBeSeenAs;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

has success => (
  is       => 'ro',
  isa      => 'Bool',
  required => 1,
);

has entire_blemish => (
  is        => 'ro',
  predicate => 'IsEntireBlemished',
  reader    => 'GetEntireBlemish',
);

has part_blemish => (
  is        => 'ro',
  isa       => 'HashRef',
  reader    => 'GetPartsBlemished',
  predicate => 'ArePartsBlemished',
);

use overload (
  q{bool} => sub {
    my ($self) = @_;
    return $self->success;
  },
  fallback => 1,
);

sub IsBlemished {
  my ($self) = @_;
  return $self->IsEntireBlemished() || $self->ArePartsBlemished();
}

sub newUnblemished {
  my ($package) = @_;
  return $package->new( success => 1 );
}

sub newEntireBlemish {
  my ( $package, $meto ) = @_;
  return $package->new( success => 1, entire_blemish => $meto );
}

sub newByPart {
  my ( $package, $blemish_hash ) = @_;
  return $package->new( success => 1, part_blemish => $blemish_hash );
}

{
  my $NO = Seqsee::ResultOfCanBeSeenAs->new( success => 0 );

  sub NO() {
    return $NO;
  }

}

__PACKAGE__->meta->make_immutable;
1;

