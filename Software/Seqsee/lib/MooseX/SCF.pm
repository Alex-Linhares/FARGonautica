package MooseX::SCF;

use strict;
use warnings;
use Carp;

use version; our $VERSION = qv('0.01');

use Moose ();
use Moose::Exporter;
use MooseX::Params::Validate;

Moose::Exporter->setup_import_methods(
  with_caller => ['Codelet_Family'],
  as_is       => ['ACTION'],
  also        => 'Moose',
);

sub Codelet_Family {
  my ( $caller, %options ) = @_;
  my $meta = Class::MOP::Class->initialize($caller);

  Carp::confess 'Require attributes' unless $options{attributes};
  Carp::confess 'Require body'       unless $options{body};

  my $run_subroutine = sub {
    my $action_object = shift;
    my @arguments = validated_list( [ %{ $_[0] } ], @{ $options{attributes} } );
    $options{body}->(@arguments);
  };
  $meta->add_method( 'run' => $run_subroutine );
  return;
}

sub ACTION {
  my ( $urgency, $family, $options ) = @_;
  SAction->new(
    {
      family  => $family,
      urgency => $urgency,
      arguments    => $options,
    }
  )->conditionally_run();
}
1;

