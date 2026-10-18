package Mapping::Position;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Carp;
use Smart::Comments;
use Class::Multimethods;

has text => (
    is         => 'rw',
    isa        => 'Str',
    reader     => 'get_text',
    writer     => 'set_text',
    init_arg   => 'text',
    required   => 1,
    weak_ref   => 0,
);

sub create {
  my ( $package, $text ) = @_;
  state %MEMO;
  return $MEMO{$text} ||= $package->new( { text => $text } );
}

my $Successor   = Mapping::Position->create('succ');
my $Predecessor = Mapping::Position->create('pred');
my $SamePos     = Mapping::Position->create('same');
my %ComplexityLookup =
( $Successor => 0.9, $Predecessor => 0.9, $SamePos => 1 );

sub get_memory_dependencies { return; }

sub serialize {
  my ($self) = @_;
  return $self->get_text;
}

sub deserialize {
  my ( $package, $str ) = @_;
  $package->create($str);
}

my $relation_finder = sub {
  my ( $p1, $p2 ) = @_;
  my $index1 = $p1->position();
  my $index2 = $p2->position();
  my $diff   = $index2 - $index1;
   $diff == 1  ? return $Successor
  :$diff == -1 ? return $Predecessor
  :$diff == 0  ? return $SamePos
  :              return;
};

sub as_text {
  my ($self) = @_;
  return "Mapping::Position " . $self->get_text;
}

multimethod FindMapping => qw(SPos SPos) => $relation_finder;

multimethod ApplyMapping => qw(Mapping::Position SPos) => sub {
  my ( $rel, $pos ) = @_;
  my $index = $pos->position();
   ( $rel eq $Successor )   ? return ( SPos->new( $index + 1 ) )
  :( $rel eq $Predecessor ) ? return ( SPos->new( $index - 1 ) )
  :( $rel eq $SamePos )     ? return $pos
  :                           return;
};

sub get_pure {
  return $_[0];
}

sub IsEffectivelyASamenessRelation {
  my ($self) = @_;
  return $self eq $SamePos ? 1 :0;
}

sub FlippedVersion {
  my ($self) = @_;
  state $FlipName = {qw{same same pred succ succ pred }};
  return Mapping::Position->create( $FlipName->{ $self->get_text() } );
}
__PACKAGE__->meta->make_immutable;
1;

