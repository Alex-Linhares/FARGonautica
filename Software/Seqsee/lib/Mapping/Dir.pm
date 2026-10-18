package Mapping::Dir;
use 5.10.0;
use strict;
use Class::Multimethods;

sub create {
  my ( $package, $string ) = @_;
  state %MEMO;
  return $MEMO{$string} ||= $package->new($string);
}

sub new {
  my ( $package, $string ) = @_;
  bless \$string, $package;
}

sub get_memory_dependencies {
  return;
}

our $Same      = Mapping::Dir->create('Same');
our $Different = Mapping::Dir->create('Different');
our $Unknown   = Mapping::Dir->create('Unknown');

sub IsEffectivelyASamenessRelation {
  my ($self) = @_;
  return $self eq $Same ? 1 :0;
}

multimethod FindMapping => qw(DIR DIR) => sub {
  my ( $da, $db ) = @_;
  if ( $da eq DIR::RIGHT() ) {
    return
     ( $db eq DIR::RIGHT() ) ? $Same
    :( $db eq DIR::LEFT() )  ? $Different
    :                          $Unknown;
  }
  elsif ( $da eq DIR::LEFT() ) {
    return
     ( $db eq DIR::RIGHT() ) ? $Different
    :( $db eq DIR::LEFT() )  ? $Same
    :                          $Unknown;
  }
  else {
    return $Unknown;
  }
};

multimethod ApplyMapping => qw{Mapping::Dir DIR} => sub {
  my ( $transform, $dir ) = @_;
  if ( $transform eq $Same ) {
    return $dir;
  }
  elsif ( $transform eq $Different ) {
    return $dir->Flip();
  }
  return $DIR::UNKNOWN;
};

sub FlippedVersion {
  return $_[0];
}

sub get_pure {
  return $_[0];
}

sub serialize {
  my ($self) = @_;
  return $$self;
}

sub deserialize {
  my ( $package, $str ) = @_;
  $package->create($str);
}

1;
