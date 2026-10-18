package PositionStructure;
use strict;

sub Create {
  my ( $package, $group ) = @_;
  bless [
    map { SUtil::StringifyDeepArray( SWorkspace::__GetPositionStructure($_) ) }
    @$group ],
  $package;
}

sub IsASubsetOf {
  my ( $self, $position_structure ) = @_;
  my @self               = @$self;
  my @position_structure = @$position_structure;
  my ( $size1, $size2 ) = ( scalar(@self), scalar(@position_structure) );
  return unless $size2 >= $size1;

  OUTER: for my $i ( 0 .. ( $size2 - $size1 ) ) {
    INNER: for my $j ( 0 .. $size1 - 1 ) {
      next OUTER unless $self[$j] eq $position_structure[ $i + $j ];
    }
    return 1;
  }
  return;
}

1;

