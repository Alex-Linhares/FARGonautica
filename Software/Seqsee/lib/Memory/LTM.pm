package Memory::LTM;
use 5.010;
use English qw( -no_match_vars );
use Smart::Comments;

my %CoreToNode;    # Maps stored cores to Memory::Node objects.
                   # Core must do Memory::Storable.

my %_currently_installing;    # Used to detect dependency loops.

sub InsertItem {
  my ( $package, $item ) = @_;
  $item->does('Memory::Insertible') or confess "Non-insertible object '$item'";

  my $storable = $item->GetNormalizedForMemory();
  return $CoreToNode{$storable} //= _InsertMissingItem($storable);
}

sub SpikeBy {
  my ( $package, $amount, @items ) = @_;
  for (@items) {
    my $normalized = $_->GetNormalizedForMemory();
    my $node = $CoreToNode{$normalized} //= _InsertMissingItem($normalized);
    $node->SpikeBy($amount);
  }
}

sub WeakenBy {
  my ( $package, $amount, @items ) = @_;
  for (@items) {
    my $normalized = $_->GetNormalizedForMemory();
    my $node = $CoreToNode{$normalized} //= _InsertMissingItem($normalized);
    $node->WeakenBy($amount);
  }
}

sub _InsertMissingItem {
  my $storable = shift;

  if ( $_currently_installing{$storable} ) {

    # There is a dependency loop!
    confess "Loop in dependencies detected! Currently installing: "
    . join( ", ", values(%_currently_installing) );
  }

  $_currently_installing{$storable} = 1;
  my @depends_on = $storable->GetMemoryDependencies();
  for (@depends_on) {
    Memory::LTM->InsertItem($_);
  }

  my $node = $CoreToNode{$storable} = Memory::Node->new( core => $storable );
  delete $_currently_installing{$storable};
  return $node;
}

1;
