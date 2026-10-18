package SCategory::Number;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

use overload
'~~'     => 'literal_comparison_hack_for_smart_match',
fallback => 1;

sub literal_comparison_hack_for_smart_match {
  return $_[0] eq $_[1];
}

sub NumericInstancer {
  my ( $self, $mag ) = @_;
  return SBindings->create( {}, {} );
}

sub FindMappingForCat {
  my ( $me, $a, $b ) = @_;

  # Assume $a, $b are integers.

  my $str;
  if ( $a == $b ) {
    $str = "same";
  }
  elsif ( $a + 1 == $b ) {
    $str = "succ";
  }
  elsif ( $a - 1 == $b ) {
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
    when ('succ') { $new_mag = $mag + 1 }
    when ('pred') { $new_mag = $mag - 1 }
  }
  $new_mag // return;
  return $new_mag;
}

with 'LTMStorable::Independent';
with 'SCategory::MetonymySpec::NotMetonyable';
with 'SCategory::Numeric';
with 'SCategory';

sub string_to_recreate {
  q{SCategory::Number->new()};
}

sub get_name {
  return "number";
}

sub as_text {
  return "number";
}

__PACKAGE__->meta->make_immutable;
1;
