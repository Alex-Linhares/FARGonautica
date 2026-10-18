package Seqsee::Scripts;
use 5.010;
use Moose;
use MooseX::ClassAttribute;
use English qw( -no_match_vars );
use Carp;
use Smart::Comments;
use MooseX::Params::Validate;


use Exception::Class (
  'SErr::ScriptReturn' => {},
  'SErr::CallSubscript' => {fields => ['name', 'arguments']},
);

sub RETURN {
  print "RETURN\n";
  SErr::ScriptReturn->throw();
}

sub SCRIPT {
  my ($name, $arguments) = @_;
  print "SCRIPT($name)\n";
  SErr::CallSubscript->throw(name => $name, arguments => $arguments);
}

class_has attributes => (
  traits => ['Array'],
  is        => 'ro',
  isa       => 'ArrayRef',
  handles => {
    expected_attributes => 'elements',
  }
);

class_has step => (
  traits => ['Array'],
  is        => 'ro',
  isa       => 'ArrayRef',
  handles => {
    get_step => 'get',
    number_of_steps => 'count',
  }
);

sub run {
  my ( $action_object, $args_ref ) = @_;
  
  # args_ref may or may not have any information about this being a script.
  # If it does not, it defaults to doing the first step...
  my ( $stack, $step_, $opts_ref );
  if ( exists $args_ref->{__S_T_A_C_K__} ) {
    ( $stack, $step_, $opts_ref ) = (
      $args_ref->{__S_T_A_C_K__},
      $args_ref->{__S_T_E_P__},
      $args_ref->{__A_R_G_S__}
    );
  }
  else {
    ( $stack, $step_, $opts_ref ) = ( [], 0, $args_ref );
  }

  # Package name for class attributes may be inferred from the family:
  my $package = 'Seqsee::SCF::' . $action_object->family;

  # Validate arguments...
  my @arguments = validated_list( [ %{ $opts_ref } ], $package->expected_attributes() );

  while ($step_ < $package->number_of_steps()) {
    my $step = $package->get_step($step_);
  
    # Let's do the step. It may throw an exception, however, asking us to return from this codelet without
    # doing further steps.
    eval { print "Step#: $step_; $step==>@arguments\n"; $step->(@arguments) };
    my $e;
    # catch
    if ( $e = Exception::Class->caught('SErr::ScriptReturn') ) {
      my @new_stack = @$stack;
      return unless @new_stack;
      my $top_frame = pop(@new_stack);
      my ( $step_no, $args, $name ) = @$top_frame;
      SCodelet->new(
        $name, 10000,
        {
          __S_T_E_P__   => $step_no,
          __A_R_G_S__   => $args,
          __S_T_A_C_K__ => \@new_stack,
        }
      )->schedule();
      return;  
    } elsif ( $e = Exception::Class->caught('SErr::CallSubscript') ) {
      my $new_stack = [ @$stack, [ $step_ + 1, $opts_ref, $action_object->family() ] ];
      my ($name, $arguments) = ($e->name(), $e->arguments());
      say "SUBSCRIPT: $name, $arguments";
      SCodelet->new(
        $name,
        10000,
        {
          __S_T_E_P__   => 0,
          __A_R_G_S__   => $arguments,
          __S_T_A_C_K__ => $new_stack
        }
      )->schedule();
      return;  
    } elsif ($e = Exception::Class->caught()) {
        ref $e ? $e->rethrow : die $e;
    }
    $step_++;
  }
}

__PACKAGE__->meta->make_immutable;
1;
