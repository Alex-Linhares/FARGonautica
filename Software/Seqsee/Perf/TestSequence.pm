package Perf::TestSequence;

## STANDARD MODULES THAT I INCLUDE EVERYWHERE
use strict;
use warnings;

use List::Util qw{min max sum first};
use Time::HiRes;
use Getopt::Long;
use Storable;

use File::Slurp;
use Smart::Comments;
use IO::Prompt;
use Class::Std;
use Class::Multimethods;

use Carp;
## END OF STANDARD INCLUDES

my %String_of : ATTR(:name<string>);
my %Revealed_of : ATTR(:get<revealed>);
my %Unrevealed_of : ATTR(:get<unrevealed>);
my %Even_More_Terms_of : ATTR(:get<even_more_terms>);
my %All_Unrevealed_of : ATTR(:get<all_unrevealed>);

sub START {
    my ( $self, $id, $opts_ref ) = @_;
    my $string = $opts_ref->{string}
      // confess "Missing required argument 'string'";

    ( $Revealed_of{$id}, $Unrevealed_of{$id}, $Even_More_Terms_of{$id} ) =
      _NormalizeTestSequence($string);
    $All_Unrevealed_of{$id} =
      $Unrevealed_of{$id} . ' ' . $Even_More_Terms_of{$id};

    $string =~ s# ^ ([^\|]*) \| ([^\|]*) .*#$1\|$2#x;
    $String_of{$id} = $string;
}

sub IsCompatibleWith {
    my ( $self, $other ) = @_;
    my $id = ident $self;
    UNIVERSAL::isa($other, "Perf::TestSequence")
      or confess "Expected \$other ($other) to be of type Perf::TestSequence."
      . "Instead, it is of type '"
      . ref($other) . "'";
    return unless $self->get_revealed() eq $other->get_revealed();

    my ( $unrevealed_1, $unrevealed_2 ) =
      ( $self->get_all_unrevealed(), $other->get_all_unrevealed() );
    return 1 if $unrevealed_1 =~ m#^$unrevealed_2#;
    return 1 if $unrevealed_2 =~ m#^$unrevealed_1#;
    return;
}

sub _TrimSequence {
    my ($sequence_string) = @_;
    $sequence_string =~ s#[^\d\-]# #g;
    $sequence_string =~ s#^\s*##;
    $sequence_string =~ s#\s*##;
    return join( ' ', split( /[^\d\-]+/, $sequence_string ) );
}

sub _NormalizeTestSequence {
    my ($sequence_string) = @_;
    my ( $prior, $posterior, $even_more ) = split( /\|/, $sequence_string );
    $even_more //= '';
    return (
        _TrimSequence($prior),
        _TrimSequence($posterior),
        _TrimSequence($even_more)
    );
}

sub ArgumentForSeqsee {
    my ($self) = @_;
    my $id = ident $self;
    $Revealed_of{$id} . '|' . $All_Unrevealed_of{$id};
}

1;
