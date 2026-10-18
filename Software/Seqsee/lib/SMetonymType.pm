#####################################################
#
#    Package: SMetonymType
#
#####################################################
#    A metonym type just keeps enough information of a metonym to check that two metonyms are essentially the same.
#####################################################

package SMetonymType;
use strict;
use Carp;
use Class::Std;
use base qw{};

my %category_of : ATTR( :get<category> );    # Category metonym is based on.
my %meto_name_of : ATTR( :get<name> );       # Name of metonym type.
my %info_loss_of : ATTR( :get<info_loss> )
;    # Information (bindings) lost going from unstarred to starred.

sub BUILD {
  my ( $self, $id, $opts_ref ) = @_;

  $category_of{$id}  = $opts_ref->{category}  || confess "Need category";
  $meto_name_of{$id} = $opts_ref->{name}      || confess "Need name";
  $info_loss_of{$id} = $opts_ref->{info_loss} || confess "Need info_loss";

# XXX(Board-it-up): [2006/11/05] Ensure all info lost is "pure" (storable into memory).
}

{
  my %METO;

  sub create {
    my ( $package, $opts_ref ) = @_;
    my %opts = %$opts_ref;
    my $key  = join( ';',
      map { ref($_) eq 'SInt' ? $_->[0] :$_ }
      ( @opts{qw(category name)}, %{ $opts{info_loss} } ) );
    return $METO{$key} ||= $package->new($opts_ref);
  }
}

# method: blemish
# Applies the blemish to the object
#
#    Finds current bindings, adds the info lost

sub blemish {
  my ( $type, $object ) = @_;
  my $id = ident $type;

  my ( $cat, $name, $info_loss ) =
  ( $category_of{$id}, $meto_name_of{$id}, $info_loss_of{$id} );
  my $unfinder = $cat->get_meto_unfinder($name);
  my $obj = $unfinder->( $cat, $name, $info_loss, $object );

  #$obj->SetMetonym($object);
  #$obj->SetMetonymActiveness(1);
  $obj->describe_as($cat);
  return $obj;
}

sub as_text {
  my ($self) = @_;
  return "Metotype: $self";
}

sub GetCatAndName {
  my ($self) = @_;
  my $id = ident $self;
  return ( $category_of{$id}, $meto_name_of{$id} );
}

sub get_memory_dependencies {
  my ($self) = @_;
  my $id = ident $self;
  return map { $_->get_pure() }
  grep { ref($_) and ref($_) ne 'SInt' }
  ( $category_of{$id}, values %{ $info_loss_of{$id} } );
}

sub serialize {
  my ($self) = @_;
  my $id = ident $self;
  return SLTM::encode( $category_of{$id}, $meto_name_of{$id},
    $info_loss_of{$id} );
}

sub deserialize {
  my ( $self, $str ) = @_;
  my $id = ident $self;
  my %opts;
  @opts{ 'category', 'name', 'info_loss' } = SLTM::decode($str);
  return SMetonymType->create( \%opts );
}

sub as_text {
  my ($self) = @_;
  return "SMetonymType";
}

sub get_pure {
  return $_[0];
}

1;
