{
package SGUI::Stream;

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


my ($ColumnWidth, $RowHeight, $EntriesPerColumn, $ColumnCount);


    BEGIN {
        read_config 'config/GUI_ws3.conf' => my %config;
        $Margin = $config{Layout}{Margin};

        my %layout_options = %{ $config{StreamLayout} };
        ($EntriesPerColumn, $ColumnCount) = @layout_options{ qw{EntriesPerColumn ColumnCount} };
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
    DrawThought(
      $Global::MainStream->{CurrentThought},
      $XOffset + $Margin / 2, $YOffset,
      1,    # i.e., is current tht
    ) if $Global::MainStream->{CurrentThought};
    my ( $row, $col ) = ( 0, 0 );
    for my $tht ( @{ $Global::MainStream->{OlderThoughts} } ) {
      next unless $tht;
      $row++;
      if ( $row >= $EntriesPerColumn ) {
        $row = 0;
        $col++;
      }
      DrawThought(
        $tht,
        $XOffset + $Margin + $col * $ColumnWidth,
        $YOffset + $Margin + $row * $RowHeight,
        0,    # not current tht
      );
    }
  }  

    sub DrawThought {
      my ( $tht, $left, $top, $is_current ) = @_;
      my $hit_intensity = $Global::MainStream->{thought_hit_intensity}{$tht};
      $Canvas->createRectangle(
        $left, $top,
        $left + $ColumnWidth,
        $top + $RowHeight,
        Style::ThoughtBox( $hit_intensity, $is_current ),
      );
      $Canvas->createText(
        $left + 1, $top + 1,
        -anchor => 'nw',
        -text   => $tht->as_text(),
        Style::ThoughtHead(),
      );
      my $fringe = $tht->stored_fringe() or return;
      my $count = 0;
      for ( (@$fringe)[ 0 .. 2 ] ) {
        last unless $_;
        my ( $component, $activation ) = @$_;
        $count++;
        $Canvas->createText(
          $left + 10,
          $top + 15 * $count,
          -text   => $component,
          -anchor => 'nw',
          Style::ThoughtComponent(
            $activation, $Global::MainStream->{hit_intensity}{$component},
          ),
        );
      }
    }
  
    }
1;

1;
