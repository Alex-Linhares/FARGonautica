package Set::Weighted;
use 5.10.0;
use strict;

sub new {
  my $package = shift;
  my $self = bless [], $package;
  $self->insert(@_);

  return $self;
}

sub is_not_empty {
  return @{ $_[0] } ? 1 :0;
}

sub is_empty {
  return @{ $_[0] } ? 0 :1;
}

sub insert {
  my $self = shift;
  push @$self, @_;
}

sub merge_keys {
  my ($self) = @_;
  my %vivify;
  my %sums;
  for (@$self) {
    my ( $k, $v ) = @$_;
    $vivify{$k} = $k;
    $sums{$k} += $v;
  }
  @$self = map { [ $vivify{$_}, $sums{$_} ] } keys %sums;
}

sub get_elements {
  my ( $self, $threshold ) = @_;
  $threshold //= 0;

  return map { $_->[0] } grep { $_->[1] >= $threshold } @$self;
}

sub delete_below_threshold {
  my ( $self, $threshold ) = @_;
  @$self = grep { $_->[1] >= $threshold } @$self;
}

sub delete_key {
  my ( $self, $key ) = @_;
  $self->merge_keys();
  @$self = grep { $_->[0] ne $key } @$self;
}

sub choose {
  my ($self) = @_;
  return SChoose->choose( [ map( $_->[1], @$self ) ],
    [ map( $_->[0], @$self ) ] );
}

sub choose_a_few_nonzero {
  my ( $self, $howmany ) = @_;
  return SChoose->choose_a_few_nonzero(
    $howmany,
    [ map( $_->[1], @$self ) ],
    [ map( $_->[0], @$self ) ]
  );
}
1;
