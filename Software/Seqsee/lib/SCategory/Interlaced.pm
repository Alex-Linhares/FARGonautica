package SCategory::Interlaced;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use Memoize;
use Class::Multimethods;

multimethod 'FindMapping';
multimethod 'ApplyMapping';

with 'SCategory::MetonymySpec::NotMetonyable';
with 'SCategory';

has parts_count => (
  is       => 'rw',
  isa      => 'Int',
  reader   => 'get_parts_count',
  writer   => 'set_parts_count',
  init_arg => 'parts_count',
  required => 1,
);

sub Create {
  my ( $package, $parts_count ) = @_;
  state %MEMO;
  return ( $MEMO{$parts_count} //=
    $package->new( { parts_count => $parts_count } ) );
}

sub is_pure {
  1;
}

sub Instancer {
  my ( $self, $object ) = @_;
  my $parts_count = $self->get_parts_count;

  my @parts = $object->get_items_array;
  return unless scalar(@parts) == $parts_count;

  my %bdgs = ();
  for my $i ( 1 .. $parts_count ) {
    $bdgs{"part_no_$i"} = $parts[ $i - 1 ];
  }
  return SBindings->create( {}, \%bdgs );
}

# Create an instance of the category stored in $self.
sub build {
  my ( $self, $opts_ref ) = @_;
  my $parts_count = $self->get_parts_count;

  my @ret_parts;

  for my $i ( 1 .. $parts_count ) {
    push @ret_parts, $opts_ref->{"part_no_$i"};
  }

  return Seqsee::Object->create(@ret_parts);
}

sub get_name {
  my ($self) = @_;
  my $parts_count = $self->get_parts_count;
  return "Interlaced_$parts_count";
}

sub as_text {
  my ($self) = @_;
  return $self->get_name();
}

memoize('get_name');
memoize('as_text');

sub get_pure {
  return $_[0];
}

sub get_memory_dependencies {
  return;
}

sub serialize {
  my ($self) = @_;
  my $id = ident $self;
  return $self->get_parts_count;
}

sub deserialize {
  my ( $package, $string ) = @_;
  $package->Create($string);
}

sub AreAttributesSufficientToBuild {
  my ( $self, @atts ) = @_;
  return 1
  if scalar( grep { /^part_no_/ } ( SUtil::uniq(@atts) ) ) ==
    $self->get_parts_count;
  return;
}

sub longer_description {
  my ($self) = @_;
  my $count = $self->get_parts_count();
  return
  "That is, the sequence consists of $count simpler sequences interlaced together";
}

__PACKAGE__->meta->make_immutable;
1;
