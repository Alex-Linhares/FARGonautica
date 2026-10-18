#####################################################
#
#    Package: SHistory
#
#####################################################
#####################################################

package SHistory;
use strict;
use Carp;
use Class::Std;
use base qw{};

my %messages_of : ATTR( :get<history>);
my %message_count_of : ATTR();
my %dob_of : ATTR();

$Global::Steps_Finished        ||= '';
$Global::CurrentRunnableString ||= '';

sub BUILD {
  my ( $self, $id, $opts ) = @_;
  $messages_of{$id} = [ history_string("created") ];
  $dob_of{$id}      = $Global::Steps_Finished;
}

sub history_string {
  my ($msg) = @_;
  my $steps = $Global::Steps_Finished || 0;
  return "[$steps]$Global::CurrentRunnableString\t$msg";
}

sub AddHistory {
  my ( $self, $msg ) = @_;
  my $id = ident $self;
  push @{ $messages_of{$id} }, history_string($msg);
  $message_count_of{$id}++;
}

sub search_history {
  my ( $self, $re ) = @_;
  return
  map { m/^ \[ (\d+) \]/ox; $1 } ( grep $re, @{ $messages_of{ ident $self} } );
}

sub UnchangedSince {
  my ( $self, $since ) = @_;
  my $last_msg_str = $messages_of{ ident $self}->[-1];
  $last_msg_str =~ /^ \[ (\d+) \]/ox or confess "Huh '$last_msg_str'";
  return $1 > $since ? 0 :1;
}

sub GetAge {
  my ($self) = @_;
  return $Global::Steps_Finished - $dob_of{ ident $self};
}

sub history_as_text {
  my ($self) = @_;
  my $id = ident $self;
  return join( "\n", "History:", @{ $messages_of{$id} } );
}

1;

