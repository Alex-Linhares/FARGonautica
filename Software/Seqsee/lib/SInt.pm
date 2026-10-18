package SInt;
use 5.10.0;
use strict;
use Smart::Comments;

use overload
'+'  => \&add_SInt,
'-'  => \&subtract_SInt,
'eq' => \&SInt_eq,
'ne' => \&SInt_ne,
'""' => \&as_text;

sub new {
  my ( $package, $mag ) = @_;
  my $self = bless [ $mag, [$S::NUMBER] ], $package;
  $self->add_category($S::PRIME)
  if ( $Global::Feature{Primes} and SCategory::Prime::IsPrime($mag) );
  if ( $Global::Feature{Parity} ) {
    if ( $mag % 2 ) {
      $self->add_category($S::ODD);
    }
    else {
      $self->add_category($S::EVEN);
    }
  }
  $self;
}

sub add_SInt {
  my ( $f, $s ) = @_;
  my $s_mag = ref($s) ? $s->[0] :$s;
  return SInt->new( $f->[0] + $s_mag );
}

sub subtract_SInt {
  my ( $f, $s, $is_reversed ) = @_;
  my $s_mag   = ref($s) ? $s->[0] :$s;
  my $f_mag   = $f->[0];
  my $new_mag = $is_reversed ? $s_mag - $f_mag :$f_mag - $s_mag;
  return SInt->new($new_mag);
}

sub as_text {
  my ($self) = @_;
  my $mag = $self->[0];
  return "SInt($mag)";
}

sub SInt_eq {
  my ( $f, $s ) = @_;
  my $s_mag = ref($s) ? $s->[0] :$s;
  return 1 if $s_mag == $f->[0];
}

sub SInt_ne {
  my ( $f, $s ) = @_;
  my $s_mag = ref($s) ? $s->[0] :$s;
  return 1 if $s_mag != $f->[0];
}

#remove.
sub get_direction {
  return DIR::RIGHT();
}

sub get_categories {
  my ($self) = @_;
  return $self->[1];
}

sub add_category {
  my ( $self, $cat ) = @_;
  my @cats = @{ $self->[1] };
  unless ( $cat ~~ @cats ) {
    push @{ $self->[1] }, $cat;
  }
}

sub get_common_categories {
  my $count = scalar(@_);
  my %counter;
  my %str2cat;
  for my $sint (@_) {
    for ( @{ $sint->[1] } ) {
      $counter{$_}++;
      $str2cat{$_} = $_;
    }
  }
  return map { $str2cat{$_} } grep { $counter{$_} == $count } keys %counter;
}

sub get_mag {
  $_[0][0];
}

sub get_pure {
  return SLTM::Platonic->create( $_[0][0] );
}

1;
