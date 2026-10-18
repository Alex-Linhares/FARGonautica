package SAction;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Carp;
use Smart::Comments;
extends 'SCodeletBase';

use Scalar::Util qw(blessed);

# method: conditionally_run
# run with probability equal to urgency.
sub conditionally_run {
  my ($self) = @_;

  return unless ( SUtil::toss( $self->urgency() / 100 ) );
  $self->run();
}

before 'run' => sub {
  my $self = shift;

  if ($Global::debugMAX) {
    main::message(
      [
        $self->family, 'green',
        "About to run: " . SUtil::StringifyForCarp($self)
      ]
    );
  }
};


__PACKAGE__->meta->make_immutable;
1;

