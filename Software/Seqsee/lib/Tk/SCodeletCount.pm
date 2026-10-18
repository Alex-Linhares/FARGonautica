package Tk::SCodeletCount;
use Tk::widgets qw{Label};
use base qw/Tk::Derived Tk::Label/;
use Smart::Comments;

our $label;
our $UPDATABLE = 1;
Construct Tk::Widget 'SCodeletCount';

sub Populate {
  my ( $self, $args ) = @_;
  $self->SUPER::Populate(%$args);
  $label = $self;
}

sub clear {
  $label->configure( -text => $Global::Steps_Finished || 0 );
}

sub Update {
  $label->configure( -text => $Global::Steps_Finished || 0 );
}

1;

