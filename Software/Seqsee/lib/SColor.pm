#####################################################
#
#    Package: SColor
#
#####################################################
#####################################################

package SColor;
use strict;
use Carp;
use Class::Std;
use Smart::Comments;
use Memoize;

use base qw{};
{
  my %D2H;
  @D2H{ 0 .. 15 } = ( 0 .. 9, 'A' .. 'F' );

  sub HSV2RGB {
    my ( $h, $s, $v ) = @_;
    my ( $r, $g, $b );
    if ( $s <= 0 ) {
      return ( $v, $v, $v );
    }
    else {
      $h /= 60;    #now 0-5
      $s /= 100;
      my $f = $h - int($h);
      $h = int($h);
      my $p = $v * ( 1 - $s );
      my $q = $v * ( 1 - $s * $f );
      my $t = $v * ( 1 - $s * ( 1 - $f ) );

      return ( $v, $t, $p ) if ( $h == 0 );
      return ( $q, $v, $p ) if ( $h == 1 );
      return ( $p, $v, $t ) if ( $h == 2 );
      return ( $p, $q, $v ) if ( $h == 3 );
      return ( $t, $p, $v ) if ( $h == 4 );
      return ( $v, $p, $q ) if ( $h == 5 );
    }
  }

  sub HSV2Color {
    my ( $h, $s, $v ) = @_;
    my @rgb = HSV2RGB( $h, $s, $v );

    return '#'
    . join( '',
      map { ( $D2H{ int( $_ / 16 ) }, $D2H{ $_ % 16 } ) }
      map { int( $_ * 2.56 ) } @rgb );
  }
  memoize('HSV2Color');

}

