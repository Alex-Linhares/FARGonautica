package SCategory::Prime;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

our $Prime;
my @Primes = qw{2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59
61 67 71 73 79 83 89 97};
my %Primes = map { $_ => 1 } @Primes;
my $LargestPrime = List::Util::max(@Primes);

sub IsPrime {
  my ($num) = @_;
  my $reply = $num ~~ %Primes ? 1 :0;

  # say "IsPrime called on >>$num<< ==> $reply";
  return $reply;
}

sub NextPrime {
  my ($num) = @_;
  return if $num >= $LargestPrime;
  $num++;

  while ( not( $num ~~ %Primes ) ) { $num++ }
  return $num;
}

sub PreviousPrime {
  my ($num) = @_;
  return if $num <= 2;
  $num--;

  while ( not( $num ~~ %Primes ) ) { $num-- }
  return $num;
}

sub NumericInstancer {
  my ( $self, $mag ) = @_;
  return unless IsPrime($mag);
  return SBindings->create( {}, {} );
}

sub FindMappingForCat {
  my ( $me, $a, $b ) = @_;

  # Assume $a, $b are integers.

  my $str;
  if ( $a == $b ) {
    $str = "same";
  }
  elsif ( NextPrime($a) == $b ) {
    $str = "succ";
  }
  elsif ( PreviousPrime($a) == $b ) {
    $str = "pred";
  }
  else {
    return;
  }
  return Mapping::Numeric->create( $str, $me );
}

sub ApplyMappingForCat {
  my ( $cat, $transform, $object ) = @_;

  # Assume $object is number..

  my $name = $transform->get_name();
  my $mag  = $object;
  my $new_mag;
  given ($name) {
    when ('same') { $new_mag = $mag }
    when ('succ') { $new_mag = NextPrime($mag) }
    when ('pred') { $new_mag = PreviousPrime($mag) }
  }
  $new_mag // return;
  return $new_mag;
}

with 'LTMStorable::Independent';
with 'SCategory::MetonymySpec::NotMetonyable';
with 'SCategory::Numeric';
with 'SCategory';

sub string_to_recreate {
  q{SCategory::Prime->new()};
}

sub get_name {
  return "Prime";
}

sub as_text {
  return "Prime";
}

__PACKAGE__->meta->make_immutable;
1;
