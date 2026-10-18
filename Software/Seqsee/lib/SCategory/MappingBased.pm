package SCategory::MappingBased;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use Class::Multimethods;
use Memoize;

multimethod 'FindMapping';
multimethod 'ApplyMapping';

with 'SCategory::MetonymySpec::NotMetonyable';
with 'SCategory';

has transform => (
  is       => 'rw',
  isa      => 'Mapping',
  reader   => 'get_transform',
  writer   => 'set_transform',
  init_arg => 'transform',
  required => 1,
);

sub Create {
  my ( $package, $transform ) = @_;
  if ( $transform->isa('SRelation') ) {
    $transform = $transform->get_type;
  }

  state %MEMO;

  return ( $MEMO{$transform} //=
    $package->new( { transform => $transform, } ) );
}

sub Instancer {
  my ( $self, $object ) = @_;
  my $transform = $self->get_transform;

  my @parts           = $object->get_items_array;
  my $parts_count     = scalar(@parts);
  my @effective_parts = map { $_->GetEffectiveObject() } @parts;

  return if $parts_count == 0;

  my $failure = 0;
  for my $idx ( 0 .. $parts_count - 2 ) {
    my $predicted_next = ApplyMapping( $transform, $parts[$idx] );
    unless ( $predicted_next
      and $parts[ $idx + 1 ]->CanBeSeenAs( $predicted_next->get_structure ) )
    {
      $failure = 1;
      last;
    }
  }

  return SBindings->new(
    {
      raw_slippages => $object->GetEffectiveSlippages(),
      bindings      => {
        first  => $parts[0],
        last   => $parts[-1],
        length => SInt->new($parts_count)
      },
      object => $object,
    }
  ) if!$failure;

  # maybe this is a length 1 instance!
  my $cat = $transform->get_category;
  if ( $cat->is_instance($object) ) {
    return SBindings->new(
      {
        raw_slippages => $object->GetEffectiveSlippages(),
        bindings =>
        { first => $object, last => $object, length => SInt->new(1) },
        object => $object,
      }
    );
  }
}

# Create an instance of the category stored in $self.
sub build {
  my ( $self, $opts_ref ) = @_;
  my $transform = $self->get_transform;

  # xxx: only uses start and length for now.
  my $start  = $opts_ref->{first}  or return;
  my $length = $opts_ref->{length} or return;
  my $length_as_num = ref($length) ? $length->[0] :$length;
  return unless $length_as_num > 0;
  my @ret         = ($start);
  my $current_end = $start;
  for ( 1 .. $length_as_num - 1 ) {
    my $next = ApplyMapping( $transform, $current_end ) or return;
    push @ret, $next;
    $current_end = $next;
  }
  my $ret = Seqsee::Object->create(@ret);
  $ret->add_category(
    $self,
    SBindings->new(
      {
        raw_slippages => {},
        bindings      => {
          first  => $ret->[0],
          last   => $ret->[-1],
          length => $length,
        },
        object => $ret
      }
    )
  );
  $ret->set_reln_scheme( RELN_SCHEME::CHAIN() );
  return $ret;
}

sub get_name {
  my ($self) = @_;
  return 'Gp based on ' . $self->get_transform->as_text();
}

sub as_text {
  my ($self) = @_;
  return $self->get_name();
}

memoize('get_name');
memoize('as_text');

sub is_pure {
  1;
}

sub get_pure {
  $_[0];
}

sub get_memory_dependencies {
  my ($self) = @_;
  return $self->get_transform;
}

sub serialize {
  my ($self) = @_;
  return SLTM::encode( $self->get_transform );
}

sub deserialize {
  my ( $package, $string ) = @_;
  my ($type) = SLTM::decode($string);
  return $package->Create($type);
}

sub AreAttributesSufficientToBuild {
  my ( $self, @atts ) = @_;
  return unless 'first'  ~~ @atts;
  return unless 'length' ~~ @atts;
  return 1;
}

__PACKAGE__->meta->make_immutable;
1;
