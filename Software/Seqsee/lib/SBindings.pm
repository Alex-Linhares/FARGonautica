package SBindings;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

has bindings_ref => (
  traits   => ['Hash'],
  is       => 'ro',
  isa      => 'HashRef[Any]',
  required => 1,
  handles  => { GetBindingForAttribute => 'get', },
  reader   => 'get_bindings_ref',
  init_arg => 'bindings',
);

#    Hash ref: indexed by absolute positions in the object, and having values that are SMetonyms
has squinting_raw => (
  traits   => ['Hash'],
  is       => 'ro',
  isa      => 'HashRef[Any]',
  required => 1,
  handles  => {
    slippages_count    => 'count',
    all_slippages      => 'values',
    slippage_positions => 'keys',
  },
  reader   => 'get_squinting_raw',
  init_arg => 'raw_slippages',
);

has metonymy_mode => (
  is     => 'rw',
  isa    => 'METO_MODE',
  reader => 'get_metonymy_mode',
);

has position_mode => (
  is     => 'rw',
  isa    => 'POS_MODE',
  reader => 'get_position_mode',
);

has position => (
  is     => 'rw',
  isa    => 'SPos',
  reader => 'get_position',
);

has metonymy_type => (
  is      => 'rw',
  isa     => 'Any',
  reader  => 'get_metonymy_type',
  handles => {
    get_metonymy_cat  => 'get_category',
    get_metonymy_name => 'get_name'
  },
);

sub BUILD {
  my ($self) = @_;
  if ( $self->slippages_count == 0 ) {
    $self->metonymy_mode( METO_MODE::NONE() );
  }
  else {
    $self->metonymy_type( SMetonym->intersection( $self->all_slippages ) );
    if ( $self->slippages_count == 1 ) {
      $self->metonymy_mode( METO_MODE::SINGLE() );
      $self->position_mode( POS_MODE::FORWARD() );
      my @slippage_positions = $self->slippage_positions;
      $self->position( SPos->new( $slippage_positions[0] + 1 ) );
    }
  }
}

sub create {
  my ( $package, $slippage_ref, $bindings_ref, $object ) = @_;
  ## SBindings constructor: $slippage_ref, $bindings_ref, $object
  ( defined($slippage_ref) and defined($bindings_ref) )
  or confess "Need two args (plus possibly a third arg (ignored)!";
  return $package->new(
    {
      raw_slippages => $slippage_ref,
      bindings      => $bindings_ref,
    }
  );
}

sub TellDirectedStory {

  # NO-OP
}

sub tell_backward_story {

  # NO-OP
}

sub tell_forward_story {

  # NO-OP
}

__PACKAGE__->meta->make_immutable;
1;

1;
