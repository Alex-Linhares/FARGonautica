# Mostly copied from SGUI/Workspace.
# Before I ever make a third similar class, I must refactor.
{
package SGUI::Workspace_Attention;

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


my ($ElementsY, $MinGpHeight, $MaxGpHeight, $BarlineTop, $BarlineBottom, $MetoY, $SpacePerElement, $GroupHtPerUnitSpan, $AttentionDistribution, $ElementsYFraction, $MinGpHeightFraction, $MaxGpHeightFraction, $MetoYFraction, $RelnZenithFraction, $BarlineHeightFraction);

    use Smart::Comments;
    my %Id2Obj;
    my %Obj2Id;
    my %RelationsToHide;
    my %AnchorsForRelations;
  

    BEGIN {
        read_config 'config/GUI_ws3.conf' => my %config;
        $Margin = $config{Layout}{Margin};

        my %layout_options = %{ $config{Workspace_AttentionLayout} };
        ($ElementsYFraction, $MinGpHeightFraction, $MaxGpHeightFraction, $MetoYFraction, $RelnZenithFraction, $BarlineHeightFraction) = @layout_options{ qw{ElementsYFraction MinGpHeightFraction MaxGpHeightFraction MetoYFraction RelnZenithFraction BarlineHeightFraction} };
    }
     
sub Setup {
    my $package = shift;
    ( $Canvas, $XOffset, $YOffset, $Width, $Height ) = @_;
    $EffectiveHeight = $Height - 2 * $Margin;
    $EffectiveWidth  = $Width - 2 * $Margin;
    
    $ElementsY   = $YOffset + $Margin + $EffectiveHeight * $ElementsYFraction;
    $MinGpHeight = $EffectiveHeight * $MinGpHeightFraction;
    $MaxGpHeight = $EffectiveHeight * $MaxGpHeightFraction;
    $MetoY       = $YOffset + $Margin + $EffectiveHeight * $MetoYFraction;
    $BarlineTop  = $ElementsY - $EffectiveHeight * 0.5 * $BarlineHeightFraction;
    $BarlineBottom =
    $ElementsY + $EffectiveHeight * 0.5 * $BarlineHeightFraction;
  ;
}
  sub DrawIt {my $self = shift; 
    $self->PrepareForDrawing();
    $self->DrawLegend( 10, 10 );
    $GroupHtPerUnitSpan =
    ( $MaxGpHeight - $MinGpHeight ) / ( $SWorkspace::ElementCount || 1 );
    $SpacePerElement     = $EffectiveWidth / ( $SWorkspace::ElementCount + 1 );
    %AnchorsForRelations = ();
    %RelationsToHide     = ();
    $self->DrawGroups();
    $self->DrawElements();
    $self->DrawRelations();
    $self->DrawBarLines();
    $self->DrawLastRunnable();
  }  

    sub DrawLegend {

    }

    sub PrepareForDrawing {
      my ($self) = @_;
      $self->DrawBlackRectangle();
      $AttentionDistribution = SCoderack->AttentionDistribution();
      ## AttentionDistribution: $AttentionDistribution
    }

    sub find_element_style {
      my ( $display, $element ) = @_;
      my $attention = $AttentionDistribution->{$element} || 0;
      return Style::ElementAttention($attention);
    }

    sub find_group_style {
      my ( $display, $group, $is_meto, $is_largest ) = @_;
      my $attention = $AttentionDistribution->{$group} || 0;
      return Style::GroupAttention($attention);
    }

    sub find_group_border_style {
      return Style::GroupBorderAttention();
    }

    sub find_relation_style {
      my ( $display, $reln, $is_hilit ) = @_;
      my $attention = $AttentionDistribution->{$reln} || 0;
      return Style::RelationAttention($attention);
    }

    sub DrawBlackRectangle {
      $Canvas->createRectangle(
        $XOffset + $Margin,
        $YOffset + $Margin,
        $XOffset + $Margin + $EffectiveWidth,
        $YOffset + $Margin + $EffectiveHeight,
        -fill => '#000000',
      );
    }

    sub Seqsee::Element::draw_attention {
      my ( $self, $display, $idx, @rest ) = @_;
      ## drawing element: @_
      my $id = $Canvas->createText(
        @rest,
        -text => $self->get_mag(),
        -tags => [ $self, 'element', $idx ],

        # Style::Element($is_hilit),
        $display->find_element_style($self),
      );
      if ( $Global::Feature{debug} ) {
        $Canvas->createText( $rest[0] + 5, $rest[1] + 10, -text => $idx );
      }
      $AnchorsForRelations{$self} ||= [ $rest[0], $rest[1] - 10 ];
      return $id;
    }

    sub Seqsee::Anchored::draw_attention {
      my ( $self, $display, $is_largest ) = @_;
      ## drawing group: @_
      my @items = @$self;
      my @edges = $self->get_edges();
      $is_largest ||= 0;

      my $howmany = scalar(@items);
      for ( 0 .. $howmany - 2 ) {
        $RelationsToHide{ $items[$_] . $items[ $_ + 1 ] } = 1;
        $RelationsToHide{ $items[ $_ + 1 ] . $items[$_] } = 1;
      }

      my $leftx  = $XOffset + $Margin + ( $edges[0] + 0.1 ) * $SpacePerElement;
      my $rightx = $XOffset + $Margin + ( $edges[1] + 0.9 ) * $SpacePerElement;
      my $span   = $self->get_span();
      my $top    = $ElementsY - $MinGpHeight - $span * $GroupHtPerUnitSpan;
      my $bottom = $ElementsY + $MinGpHeight + $span * $GroupHtPerUnitSpan;

      my $is_meto;
      if ( $is_meto = $self->get_metonym_activeness() ) {
        DrawMetonym(
          $display,
          {
            actual_string => $self->get_structure_string(),
            starred => $self->GetEffectiveObject()->get_structure_string(),
            x1      => $leftx,
            x2      => $rightx,
          }
        );
      }
      $AnchorsForRelations{$self} = [ ( $leftx + $rightx ) / 2, $top ];
      my $is_hilit = $Global::Hilit{$self} || 0;
      my $tags = $is_hilit ? ['hilit'] :[];
      my $canvas_obj =
      $Canvas->createOval( $leftx, $top, $rightx, $bottom,
        $display->find_group_style( $self, $is_meto, $is_largest ) );

      $Canvas->createOval(
        $leftx, $top, $rightx, $bottom,
        -tags => $tags,
        $display->find_group_border_style($is_hilit),
      );
      return $canvas_obj;
    }

    sub SReln::draw_attention {
      my ( $self, $display ) = @_;
      ## draw relation: @_
      my @ends = $self->get_ends();

      # Unlike SGUI::Workspace, don't hide.
      return
      if ( $RelationsToHide{ join( '', @ends ) }
        and not( $Global::Hilit{$self} ) );

      my $is_hilit = $Global::Hilit{$self} || 0;
      my ( $x1, $y1 ) = @{ $AnchorsForRelations{ $ends[0] } || [] };
      my ( $x2, $y2 ) = @{ $AnchorsForRelations{ $ends[1] } || [] };
      return unless ( $x1 and $x2 );
      ## drawing a relation: $x1, $y1, $x2, $y2
      ## $RelnZenithFraction, $EffectiveHeight: $RelnZenithFraction, $EffectiveHeight
      return $Canvas->createLine(
        $x1,
        $y1,
        ( $x1 + $x2 ) / 2,
        $YOffset + $Margin + $RelnZenithFraction * $EffectiveHeight,
        $x2,
        $y2,
        $display->find_relation_style( $self, $is_hilit ),
      );
    }

    sub DrawItemOnCanvas {
      my ( $obj, $display, @rest ) = @_;
      my $id = $obj->draw_attention( $display, @rest );
      $Id2Obj{$id} = $obj;
      return $id;
    }

    sub DrawElements {
      my $self    = shift;
      my $counter = 0;
      for my $elt ( SWorkspace->GetElements() ) {
        $elt->draw_attention( $self, $counter,
          $Margin + ( 0.5 + $counter ) * $SpacePerElement, $ElementsY );
        $counter++;
      }
    }

    sub DrawGroups {
      my $self          = shift;
      my @groups        = SWorkspace->GetGroups() or return;
      my $largest_group = shift(@groups);
      $largest_group->draw_attention( $self, 1 );    # Argument is: $is_largest

      for my $gp (@groups) {
        $gp->draw_attention($self);
      }
      for my $elt ( SWorkspace::GetElements() ) {
        Seqsee::Anchored::draw_attention( $elt, $self )
        if ( $elt->get_group_p() or $elt->get_metonym_activeness() );
      }
      $Canvas->raise('hilit');
    }

    sub DrawRelations {
      my $self = shift;
      for my $rel ( values %SWorkspace::relations ) {
        $rel->draw_attention($self);
      }
    }

    sub DrawBarLines {
      my $self     = shift;
      my @barlines = SWorkspace->GetBarLines();

      for my $index (@barlines) {
        my $xpos = $Margin + $index * $SpacePerElement;
        $Canvas->createLine( $xpos, $BarlineTop, $xpos, $BarlineBottom );
      }
    }

    sub DrawMetonym {
      my ( $self, $opts_ref ) = @_;
      my $id = $Canvas->createText(
        ( $opts_ref->{x1} + $opts_ref->{x2} ) / 2,
        $MetoY + 20,
        Style::Element(0), -text => $opts_ref->{actual_string},
      );
      my @bbox = $Canvas->bbox($id);
      $Canvas->createLine( @bbox, );
      $Canvas->createLine( @bbox[ 2, 1, 0, 3 ], );
      $Canvas->createText( ( $opts_ref->{x1} + $opts_ref->{x2} ) / 2,
        $MetoY, Style::Starred(), -text => $opts_ref->{starred}, );

    }

    sub DrawLastRunnable {
      my $self = shift;
      $Canvas->createText(
        $Margin, $Height - $Margin,
        -anchor => 'sw',
        -text   => $Global::CurrentRunnableString
      );
    }

  
    }
1;


1;
