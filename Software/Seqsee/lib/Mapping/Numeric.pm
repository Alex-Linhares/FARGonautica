package Mapping::Numeric;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Carp;
use Smart::Comments;
use Memoize;

extends 'Mapping';
has name => (
    is         => 'rw',
    isa        => 'Str',
    reader     => 'get_name',
    writer     => 'set_name',
    init_arg   => 'name',
    required   => 1,
    weak_ref   => 0,
);

has category => (
    is         => 'rw',
    reader     => 'get_category',
    writer     => 'set_category',
    init_arg   => 'category',
    required   => 1,
    weak_ref   => 0,
);


sub create {
  my ( $package, $name, $category ) = @_;
  die "Mapping::Numeric creation attempted without name!" unless $name;
  state %MEMO;
  return $MEMO{ SLTM::encode( $name, $category ) } //= $package->new(
    {
      name     => $name,
      category => $category,
    }
  );
}

sub serialize {
  my ($self) = @_;
  return SLTM::encode( $self->get_name(), $self->get_category() );
}

sub deserialize {
  my ( $package, $str ) = @_;
  $package->create( SLTM::decode($str) );
}

sub get_memory_dependencies {
  my ($self) = @_;
  return $self->get_category();
}

sub get_pure {
  return $_[0];
}

sub FlippedVersion {
  my ($self) = @_;
  state $FlipName =
  {qw{same same pred succ succ pred flip flip no_flip no_flip}};
  return Mapping::Numeric->create( $FlipName->{ $self->get_name() },
    $self->get_category() );
}

sub IsEffectivelyASamenessRelation {
  my ($self) = @_;
  return $self->get_name() eq 'same' ? 1 :0;
}

sub as_text {
  my ($self) = @_;
  my $cat    = $self->get_category;
  my $cat_string = ( $cat eq $S::NUMBER ) ? '' :$cat->as_text() . ' ';
  return $cat_string . $self->get_name();
}
memoize('as_text');

sub GetRelationBasedCategory {
  my ($self) = @_;

  return SCategory::MappingBased->Create($self)
  unless $self->get_category() eq $S::NUMBER;

  my $name = $self->get_name;
  given ($name) {
    when ('succ') { return $S::ASCENDING; }
    when ('same') { return $S::SAMENESS; }
    when ('pred') { return $S::DESCENDING; }
    default       { confess "Should not reach herre" }
  }
}

sub get_complexity {
  my ($self)   = @_;
  my $category = $self->get_category;
  my $name     = $self->get_name;

  given ($category) {
    when ( $category eq $S::NUMBER ) {
      given ($name) {
        when ('same') { return 0; }
        default       { return 0.1; }
      }
    }
    when ( $_->isa('SCategory::Alternating') ) {
      return 0.7;
    }
    default { return 0.4; }
  }
}

memoize('get_complexity');

__PACKAGE__->meta->make_immutable;
1;
