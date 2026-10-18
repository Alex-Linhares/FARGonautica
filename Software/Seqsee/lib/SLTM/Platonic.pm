#####################################################
#
#    Package: SLTM::Platonic
#
#####################################################
#   Memory core for things corresponding to objects in the workspace
#####################################################

package SLTM::Platonic;
use 5.10.0;
use strict;
use Carp;
use Class::Std;
use Smart::Comments;

our %StructureString_of : ATTR(:name<structure_string>);
our %Structure_of : ATTR(:name<structure>);

{
  my %MEMO = ();

  sub create {
    my ( $package, $structure_string ) = @_;
    my $structure = structure_from_string($structure_string);
    return $MEMO{$structure_string} //= $package->new(
      {
        structure        => $structure,
        structure_string => $structure_string
      }
    );
  }
}

sub as_text {
  my ($self) = @_;
  return 'plat' . $StructureString_of{ ident $self};
}

sub get_memory_dependencies {
  return;
}

sub serialize {
  my ($self) = @_;
  return $StructureString_of{ ident $self};
}

sub deserialize {
  my ( $package, $string ) = @_;
  return $package->create($string);
}

sub get_pure {
  return $_[0];
}

sub structure_from_string {
  my ($string) = @_;
  $string =~ s#\s+##g;
  my @tokens = split( /([\[\]])/, $string );
  ## tokens: @tokens
  if ( $tokens[0] =~ /\d+/ ) {
    return $tokens[0];
  }
  my @result = ( [] );
  while (@tokens) {
    my $token = shift(@tokens);
    given ($token) {
      when ('[') {
        push @result, [];
      }
      when (']') {
        my $result_top_frame = pop(@result);
        push @{ $result[-1] }, $result_top_frame;
      }
      default {
        my @numbers = grep { /\d/ } split( /,/, $token );
        push @{ $result[-1] }, @numbers;
      }
    }
  }
  unless ( scalar(@result) == 1 and scalar( @{ $result[0] } ) == 1 ) {
    confess "Problematic string $string!";
  }
  return $result[0][0];
}

1;

