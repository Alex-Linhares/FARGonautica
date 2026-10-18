{
package SGUI::Slipnet;

use strict;
use Carp;
use Class::Std;
use Config::Std;
use base qw{};
use List::Util qw(min max);
use Sort::Key qw(rikeysort);

my $Canvas;
my ( $Height,  $Width );
my ( $XOffset, $YOffset );

my $Margin;
my $EffectiveHeight;
my $EffectiveWidth;


my ($ColumnWidth, $RowHeight, $EntriesPerColumn, $ColumnCount, $MaxOvalRadius, $MaxTextWidth, $MinActivationForDisplay);


    BEGIN {
        read_config 'config/GUI_ws3.conf' => my %config;
        $Margin = $config{Layout}{Margin};

        my %layout_options = %{ $config{SlipnetLayout} };
        ($EntriesPerColumn, $ColumnCount, $MaxOvalRadius, $MaxTextWidth, $MinActivationForDisplay) = @layout_options{ qw{EntriesPerColumn ColumnCount MaxOvalRadius MaxTextWidth MinActivationForDisplay} };
    }
     
sub Setup {
    my $package = shift;
    ( $Canvas, $XOffset, $YOffset, $Width, $Height ) = @_;
    $EffectiveHeight = $Height - 2 * $Margin;
    $EffectiveWidth  = $Width - 2 * $Margin;
    
    $ColumnWidth = int( $EffectiveWidth / $ColumnCount );
    $RowHeight   = int( $EffectiveHeight / $EntriesPerColumn );
  ;
}
  sub DrawIt {my $self = shift; 
    my @concepts_with_activation = SLTM::GetTopConcepts(10);
    my ( $row, $col ) = ( -1, 0 );
    for (@concepts_with_activation) {
      last if $col >= $ColumnCount;
      next unless $_->[1] > $MinActivationForDisplay;
      $row++;
      if ( $row >= $EntriesPerColumn ) {
        $row = 0;
        $col++;
      }
      DrawNode(
        $_,
        $XOffset + $Margin + $col * $ColumnWidth,
        $YOffset + $Margin + $row * $RowHeight
      );

    }
  }  

    sub DrawNode {
      my ( $con_ref, $left, $top ) = @_;
      my ( $concept, $activation, $raw_activation, $raw_significance ) =
      @{$con_ref};
      my $radius = $activation * $MaxOvalRadius;

      #main::message("Rad: $radius");
      $Canvas->createOval(
        $left + 2 + $MaxOvalRadius - $radius,
        $top + 2 + $MaxOvalRadius - $radius,
        $left + 2 + $MaxOvalRadius + $radius,
        $top + 2 + $MaxOvalRadius + $radius,
        Style::NetActivation( int($raw_significance) ),
      );
      my $text = $concept->as_text();
      $text = substr( $text, 0, $MaxTextWidth );
      $Canvas->createText(
        $left + 6 + 2 * $MaxOvalRadius,
        $top + 2 + $MaxOvalRadius,
        -anchor => 'w',
        -text   => $text,
      );
    }

  
    }
1;


1;
