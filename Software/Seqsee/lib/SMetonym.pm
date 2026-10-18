#####################################################
#
#    Package: SMetonym
#
#####################################################
#   A specific metonym of one object
#
#   This is intended to be a replacement for SBlemish. I am going to stop using the word
# blemish, prefering metonym instead.
#
#   A metonym would include the category that the object belongs to that allows this slippage,
# and the name of the slippage.
#####################################################

package SMetonym;
use strict;
use Carp;
use Class::Std;
use Scalar::Util qw{weaken};
use base qw{};

my %type_of : ATTR(:get<type>);               # Points to the SMetonymType
my %starred_of : ATTR( :get<starred> );       # The unreal, hallucinated object.
my %unstarred_of : ATTR( :get<unstarred> );   # What is physically present.

sub BUILD {
  my ( $self, $id, $opts_ref ) = @_;

  $type_of{$id}      = SMetonymType->create($opts_ref);
  $starred_of{$id}   = $opts_ref->{starred} or confess "Need starred";
  $unstarred_of{$id} = $opts_ref->{unstarred} or confess "Need unstarred";
  weaken $unstarred_of{$id};
}

sub intersection {
  my ( $package, @meto ) = @_;
  @meto or confess "Cannot take intersection of empty set";
  my $type = shift(@meto)->get_type();
  for (@meto) {
    return unless $_->get_type() eq $type;
  }
  return $type;
}

sub get_category  { return $_[0]->get_type()->get_category(); }
sub get_name      { return $_[0]->get_type()->get_name(); }
sub get_info_loss { return $_[0]->get_type()->get_info_loss(); }

1;
