package UI::Graphical;

package main;
use strict;

sub update_display {
  SGUI::Update();
}

sub default_error_handler {
  my ($err) = @_;
  $Tk::Carp::MainWindow = $SGUI::MW;
  my $msg =
  UNIVERSAL::isa( $err, 'Exception::Class' ) ? $err->as_string() :$err;
  if ( $msg !~ m#\S# ) {
    $msg .= "<EMPTY MESSAGE>";
    confess $msg;
  }
  if ( $msg eq "_TK_EXIT_(0)\n" ) {
    return;
  }
  tkdie( "tkdie notes: '" . $msg . q{'} );
}

sub pop_message {
  my ( $msg, $no_break ) = @_;
  my $btn = $SGUI::MW->messageBox( -message => $msg, -type => "OkCancel" );
  ## $btn
  $Global::Break_Loop = 1 unless $no_break;
}

sub message {
  my ( $msg, $no_break ) = @_;
  print "MSG=$msg\n";
  if ($no_break) {
    my @msg = ( ref($msg) eq 'ARRAY' ) ? @$msg :("$msg\n");
    $SGUI::Commentary->MessageRequiringNoResponse(@msg);
  }
  else {
    my @msg = ref($msg) eq 'ARRAY' ? @$msg :($msg);
    $SGUI::Commentary->MessageRequiringAResponse( ['continue'], @msg );
  }
}

sub ask_user_extension {
  my ( $arr_ref, $msg_suffix ) = @_;

  return if Seqsee::already_rejected_by_user($arr_ref);

  my $cnt = scalar(@$arr_ref);
  my $msg =
  ( $cnt == 1 )
  ? "Is the next term @$arr_ref?"
  :"Are the next terms: @$arr_ref?";

  my $ok =
  $Global::Feature{debug}
  ? $SGUI::Commentary->MessageRequiringBooleanResponse( $msg, '', $msg_suffix,
    ['debug'] )
  :$SGUI::Commentary->MessageRequiringBooleanResponse($msg);
  if ($ok) {
    $Global::AtLeastOneUserVerification = 1;
  }
  return $ok;
}

sub ask_for_more_terms {
  my $window = SGUI::ask_for_more_terms();
  $window->waitWindow();
}

1;
