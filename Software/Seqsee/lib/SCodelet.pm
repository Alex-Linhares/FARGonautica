package SCodelet;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

extends 'SCodeletBase';

has creation_time => (
  is       => 'ro',
  required => 1,
);

sub BUILDARGS {
  my ( $package, $family, $urgency, $args_ref ) = @_;
  $args_ref ||= {};
  return {
    family        => $family,
    urgency       => $urgency,
    creation_time => $Global::Steps_Finished,
    arguments     => $args_ref
  };
}

use overload (
  '@{}' => sub {
    my ($self) = @_;
    return [
      $self->family(),        $self->urgency(),
      $self->creation_time(), $self->arguments()
    ];
  },
  fallback => 1
);

around 'run' => sub {
  my $orig = shift;
  my $self = shift;

  if ($Global::debugMAX) {
    main::message(
      [
        $self->family, 'green',
        "About to run: " . SUtil::StringifyForCarp($self)
      ]
    );
  }

  return
  unless CheckFreshness( $self->creation_time, values %{ $self->arguments } );
  $self->$orig();
};

sub as_text {
  my ($self) = @_;
  return
    "Codelet(family="
  . $self->family()
  . ",urgency="
  . $self->urgency()
  . ",args="
  . SUtil::StringifyForCarp( $self->arguments() );
}

sub schedule {
  my ($self) = @_;
  SCoderack->add_codelet($self);
}

sub CheckFreshness {
  my $since = shift;    # Should not have changed since this time.
  for (@_) {
    return unless ( IsFresh( $_, $since ) );
  }
  return 1;
}

use Class::Multimethods;
multimethod IsFresh => ( '*', '#' ) => sub {

  # detualt case:fresh.
  return 1;
};

multimethod IsFresh => ( 'HASH', '#' ) => sub {
  return 1;
};

multimethod IsFresh => ( 'Seqsee::Anchored', '#' ) => sub {
  my ( $obj, $since ) = @_;
  return $obj->UnchangedSince($since);
};

multimethod IsFresh => ( 'SRelation', '#' ) => sub {
  my ( $rel, $since ) = @_;
  my @ends = $rel->get_ends();
  return ( $ends[0]->UnchangedSince($since)
    and $ends[1]->UnchangedSince($since) );
};


__PACKAGE__->meta->make_immutable;
1;

