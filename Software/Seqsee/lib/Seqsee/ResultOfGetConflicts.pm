package Seqsee::ResultOfGetConflicts;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

has challenger => (
  is       => 'ro',
  isa      => 'Seqsee::Object',
  weak_ref => 1,
  required => 1,
);

has exact_conflict => (
  is       => 'ro',
  required => 0,
  weak_ref => 1,
);

has overlapping_conflicts => (
  traits  => ['Array'],
  is      => 'ro',
  isa     => 'ArrayRef[Seqsee::Object]',
  default => sub { [] },
  handles => {
    has_overlapping_conflicts  => 'count',
    overlapping_conflict_count => 'count',
    all_overlapping_conflicts  => 'elements'
  }
);

use overload (
  q{bool} => sub {
    my ($self) = @_;
    return $self->exact_conflict() || $self->overlapping_conflict_count();
  },
  fallback => 1,
);

sub Resolve {
  my ( $self, $opts_ref ) = @_;
  my $challenger = $self->challenger();

  my $IgnoreConflictWith = '';
  my $FailIfExact        = 0;

  if ($opts_ref) {
    if ( exists( $opts_ref->{IgnoreConflictWith} ) ) {
      $IgnoreConflictWith = $opts_ref->{IgnoreConflictWith};
    }
    if ( $opts_ref->{FailIfExact} ) {
      $FailIfExact = $opts_ref->{FailIfExact};
    }
  }

  if ( my $exact = $self->exact_conflict() ) {
    if ( $IgnoreConflictWith ne $exact ) {
      return if $FailIfExact;
      SWorkspace->FightUntoDeath(
        {
          challenger => $challenger,
          incumbent  => $exact,
        }
      ) or return;
    }
  }

  for my $some_other ( $self->all_overlapping_conflicts() ) {
    next if $some_other eq $IgnoreConflictWith;
    next if ( !SWorkspace::__CheckLiveness($some_other) );
    SWorkspace->FightUntoDeath(
      {
        challenger => $challenger,
        incumbent  => $some_other,
      }
    ) or return;
  }

  return 1;
}

__PACKAGE__->meta->make_immutable;
1;

