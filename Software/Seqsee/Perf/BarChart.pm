package Perf::BarChart;
use 5.10.0;

## STANDARD MODULES THAT I INCLUDE EVERYWHERE
use strict;
use warnings;

use List::Util qw{min max sum first};
use Time::HiRes;
use Getopt::Long;
use Storable;

use File::Slurp;
use Smart::Comments;
use IO::Prompt;
use Class::Std;
use Class::Multimethods;

use Carp;
## END OF STANDARD INCLUDES

use Sort::Key qw(rikeysort);
use Exception::Class ( 'Y_TOO_BIG' => {} );

use constant { CHART_BG_COLOR => '#F8F8F8', };

use constant {
    GROUP_A_OPTIONS     => [ -fill    => '#DDDDDD' ],
    GROUP_B_OPTIONS     => [ -fill    => '#BBBBBB' ],
    FADED_GROUP_OPTIONS => [ -stipple => 'gray25', -dash => '._.' ],
    DISTRACTOR_OPTIONS  => [ -outline => '#0000FF', -width => 4 ],

    SCALE_LINE_OPTIONS => [ -fill => '#DDDDDD' ],
    SCALE_TEXT_OPTIONS => [ -font => 'Lucida 10', -fill => '#666666' ],

    CHART_RECTANGLE_OPTIONS =>
      [ -fill => CHART_BG_COLOR(), -outline => '#EEEEEE' ],
    BAR_HEIGHT_TEXT_OPTIONS => [ -font => 'Lucida 8' ],
    FONT                    => 'Lucida 14',
    SEQUENCE_LABEL_TEXT_OPTIONS => [ -font => 'Lucida 12', -fill => '#0000FF' ],
    CHART_TITLE_OPTIONS         => [ -font => 'Lucida 12', -fill => '#000000' ],
    FIGURE_TITLE_OPTIONS        => [ -font => 'Lucida 16', -fill => '#FF0000' ],
};

my (
    $FIG_WIDTH,                $FIG_L_MARGIN,
    $FIG_R_MARGIN,             $CHART_CHART_SEPARATION,
    $CHART_L_MARGIN,           $CHART_R_MARGIN,
    $INTER_CLUSTER_SEPARATION, $INTER_BAR_SEPARATION,
    $MAX_BAR_WIDTH,            $EFFECTIVE_FIG_WIDTH,
    $CHART_WIDTH,              $SEQUENCES_H_OFFSET,
    $CHART1_H_OFFSET,          $CHART2_H_OFFSET,
    $EFFECTIVE_CHART_WIDTH
);

my (
    $MAX_CLUSTER_WIDTH, $BAR_WIDTH,
    $CLUSTER_WIDTH,     $HORIZONTAL_FIRST_BAR_IN_CLUSTER_OFFSET,
    $SEQUENCES_HEIGHT,  $SEQUENCES_BOTTOM,
    $CHART_V_OFFSET,    $CHART_BOTTOM,
    $BAR_BOTTOM,        $FIG_HEIGHT
);

my (
    $PERCENT_CORRECT_CLUSTER_WIDTH,
    $PERCENT_CORRECT_BAR_WIDTH,
    $MAX_PERCENT_CORRECT_BAR_WIDTH,
    $PERCENT_CORRECT_HORIZONTAL_FIRST_BAR_IN_CLUSTER_OFFSET
);

my (
    $FIG_T_MARGIN,               $FIG_B_MARGIN,
    $INTER_SEQUENCE_SEPARATION,  $SEQUENCE_HEIGHT,
    $SEQUENCES_CHART_SEPARATION, $CHART_T_MARGIN,
    $CHART_B_MARGIN,             $CHART_HEIGHT
);

my ( $SEQUENCES_V_OFFSET, $MAX_BAR_HEIGHT );

my ( $LEGEND_HEIGHT, $LEGEND_V_OFFSET,         $LEGEND_CHART_SEPARATION );
my ( $CLUSTER_COUNT, $SEQUENCES_TO_PLOT_COUNT, $SEQUENCES_TO_DISPLAY_COUNT );
my ($Canvas);

my ( $DRAW_CHARTS, $DRAW_SEQUENCES );

my ($CUSTOM_LEGEND);

sub Setup {
    my ( $graph_spec, $no_ovals, $no_chart, $no_seq ) = @_;
    $graph_spec->isa("Perf::Figure::Specification")
      or confess
      "Expected \$graph_spec to be of type Perf::Figure::Specification."
      . "Instead, it is of type "
      . ref($graph_spec);

    $CUSTOM_LEGEND = $graph_spec->get_custom_legend;

    $CLUSTER_COUNT              = $graph_spec->get_cluster_count;
    $SEQUENCES_TO_PLOT_COUNT    = $graph_spec->get_sequences_to_chart_count;
    $SEQUENCES_TO_DISPLAY_COUNT = $graph_spec->get_sequences_to_draw_count;

    $DRAW_CHARTS    = $graph_spec->get_draw_chart;
    $DRAW_CHARTS    = 0 if $no_ovals;
    $DRAW_CHARTS    = 0 if $no_chart;
    $DRAW_SEQUENCES = $no_seq ? 0 : 1;

    #==== HORIZONTAL
    $FIG_WIDTH                     = 600;
    $FIG_L_MARGIN                  = 20;
    $FIG_R_MARGIN                  = 20;
    $CHART_CHART_SEPARATION        = 20;
    $CHART_L_MARGIN                = 30;
    $CHART_R_MARGIN                = 20;
    $INTER_CLUSTER_SEPARATION      = 10;
    $INTER_BAR_SEPARATION          = 10;
    $MAX_PERCENT_CORRECT_BAR_WIDTH = 12;
    $MAX_BAR_WIDTH                 = 20;

    #=== HORIZONTAL CALCULATED
    $EFFECTIVE_FIG_WIDTH = $FIG_WIDTH - $FIG_L_MARGIN - $FIG_R_MARGIN;
    $CHART_WIDTH = ( $EFFECTIVE_FIG_WIDTH - $CHART_CHART_SEPARATION ) / 2;
    $SEQUENCES_H_OFFSET = $FIG_L_MARGIN;
    $CHART1_H_OFFSET    = $FIG_L_MARGIN;
    $CHART2_H_OFFSET =
      $CHART1_H_OFFSET + $CHART_WIDTH + $CHART_CHART_SEPARATION;
    $EFFECTIVE_CHART_WIDTH = $CHART_WIDTH - $CHART_L_MARGIN - $CHART_R_MARGIN;

    #=== VERTICAL
    $FIG_T_MARGIN               = $no_ovals ? 15 : 20;
    $FIG_B_MARGIN               = 14;
    $INTER_SEQUENCE_SEPARATION  = $DRAW_SEQUENCES ? 30 : 0;
    $SEQUENCE_HEIGHT            = $DRAW_SEQUENCES ? 20 : 0;
    $SEQUENCES_CHART_SEPARATION = ( $DRAW_CHARTS and $DRAW_SEQUENCES ) ? 20 : 0;
    $LEGEND_CHART_SEPARATION    = $DRAW_CHARTS ? 5 : 0;
    $CHART_T_MARGIN             = 20;
    $CHART_B_MARGIN             = 30;
    $CHART_HEIGHT               = $DRAW_CHARTS ? 140 : 0;

    #=== VERTICAL CALCULATED
    $SEQUENCES_V_OFFSET = $FIG_T_MARGIN;
    $MAX_BAR_HEIGHT     = $CHART_HEIGHT - $CHART_T_MARGIN - $CHART_B_MARGIN;
    $LEGEND_HEIGHT =
      $DRAW_CHARTS
      ? 10 * ( $CUSTOM_LEGEND ? scalar(@$CUSTOM_LEGEND) : $CLUSTER_COUNT )
      : 0;

    $MAX_CLUSTER_WIDTH =
      ( $EFFECTIVE_CHART_WIDTH -
          $INTER_CLUSTER_SEPARATION * ( $SEQUENCES_TO_PLOT_COUNT - 1 ) ) /
      $SEQUENCES_TO_PLOT_COUNT;
    $BAR_WIDTH = min( $MAX_BAR_WIDTH,
        ( $MAX_CLUSTER_WIDTH - $INTER_BAR_SEPARATION * ( $CLUSTER_COUNT - 1 ) )
          / $CLUSTER_COUNT );
    $PERCENT_CORRECT_BAR_WIDTH = min( $MAX_PERCENT_CORRECT_BAR_WIDTH,
        ( $MAX_CLUSTER_WIDTH - $INTER_BAR_SEPARATION * ( $CLUSTER_COUNT - 1 ) )
          / $CLUSTER_COUNT );
    $CLUSTER_WIDTH =
      $CLUSTER_COUNT * ( $BAR_WIDTH + $INTER_BAR_SEPARATION ) -
      $INTER_BAR_SEPARATION;

    $PERCENT_CORRECT_CLUSTER_WIDTH =
      $CLUSTER_COUNT * ( $PERCENT_CORRECT_BAR_WIDTH + $INTER_BAR_SEPARATION ) -
      $INTER_BAR_SEPARATION;

    $HORIZONTAL_FIRST_BAR_IN_CLUSTER_OFFSET =
      ( $MAX_CLUSTER_WIDTH - $CLUSTER_WIDTH ) / 2;

    $PERCENT_CORRECT_HORIZONTAL_FIRST_BAR_IN_CLUSTER_OFFSET =
      ( $MAX_CLUSTER_WIDTH - $PERCENT_CORRECT_CLUSTER_WIDTH ) / 2;

    $SEQUENCES_HEIGHT =
      ( $SEQUENCE_HEIGHT * $SEQUENCES_TO_DISPLAY_COUNT ) +
      ( $INTER_SEQUENCE_SEPARATION * ( $SEQUENCES_TO_DISPLAY_COUNT - 1 ) );
    $SEQUENCES_BOTTOM = $SEQUENCES_HEIGHT + $SEQUENCES_V_OFFSET;
    $LEGEND_V_OFFSET  = $SEQUENCES_BOTTOM + $SEQUENCES_CHART_SEPARATION;
    $CHART_V_OFFSET =
      $LEGEND_V_OFFSET + $LEGEND_HEIGHT + $LEGEND_CHART_SEPARATION;
    $CHART_BOTTOM = $CHART_V_OFFSET + $CHART_HEIGHT;

    $BAR_BOTTOM = $CHART_BOTTOM - $CHART_B_MARGIN;
    $FIG_HEIGHT = $CHART_BOTTOM + $FIG_B_MARGIN;

}

sub HorizontalClusterOffset {
    my ( $chart_num, $sequence_num ) = @_;
    my $chart_offset =
      ( $chart_num == 1 ) ? $CHART1_H_OFFSET : $CHART2_H_OFFSET;
    return $chart_offset + $CHART_L_MARGIN +
      $sequence_num * ( $MAX_CLUSTER_WIDTH + $INTER_CLUSTER_SEPARATION );
}

sub HorizontalClusterCenterOffset {
    my ( $chart_num, $sequence_num ) = @_;
    HorizontalClusterOffset( $chart_num, $sequence_num ) +
      $MAX_CLUSTER_WIDTH / 2;
}

sub HorizontalBarOffset {
    my ( $chart_num, $sequence_num, $cluster_num ) = @_;
    my $first_bar_offset =
      ( $chart_num == 1 )
      ? $PERCENT_CORRECT_HORIZONTAL_FIRST_BAR_IN_CLUSTER_OFFSET
      : $HORIZONTAL_FIRST_BAR_IN_CLUSTER_OFFSET;
    my $bar_width =
      ( $chart_num == 1 ) ? $PERCENT_CORRECT_BAR_WIDTH : $BAR_WIDTH;
    HorizontalClusterOffset( $chart_num, $sequence_num ) + $first_bar_offset +
      $cluster_num * ( $bar_width + $INTER_BAR_SEPARATION );
}

sub BarCoordinateToFigCoordinate {

    # $x $y each in [0,1]. y=0 is bottom.
    my ( $chart_num, $sequence_num, $cluster_num, $x, $y ) = @_;
    my $horiz_offset =
      HorizontalBarOffset( $chart_num, $sequence_num, $cluster_num );
    my $bar_width =
      ( $chart_num == 1 ) ? $PERCENT_CORRECT_BAR_WIDTH : $BAR_WIDTH;
    my $x_ret = $horiz_offset + $x * $bar_width;
    my $y_ret = $BAR_BOTTOM - $y * $MAX_BAR_HEIGHT;
    return ( $x_ret, $y_ret );
}

my $MAX_TERMS                = 35;
my $MIN_TERMS                = 10;
my $OVAL_MINOR_AXIS_FRACTION = 15;
my $OVAL_MINOR_AXIS_MIN      = 10;
my $WIDTH_PER_TERM;
my $Y_DELTA_PER_UNIT_SPAN;
my $FADE_AFTER;
my %ARROW_ANCHORS;

sub WIDTH {
    return $EFFECTIVE_FIG_WIDTH;
}

sub HEIGHT {
    return $SEQUENCE_HEIGHT;
}

sub Y_CENTER {
    $SEQUENCE_HEIGHT / 2;
}

sub SeqCoordToCanvasCoord {
    my ( $seq_num, $x, $y ) = @_;
    my $new_y =
      $SEQUENCES_V_OFFSET +
      $seq_num * ( $SEQUENCE_HEIGHT + $INTER_SEQUENCE_SEPARATION ) +
      $y;
    return ( $SEQUENCES_H_OFFSET + $x, $new_y );
}

sub _LegendOffset {
    my ($number) = @_;
    my $x        = $FIG_WIDTH - $FIG_L_MARGIN - 5;
    my $y        = $LEGEND_V_OFFSET + 12 * $number;
    return ( $x, $y );
}

sub _DrawLegend {
    my ( $canvas, $number, $color, $label ) = @_;
    my ( $x, $y ) = _LegendOffset($number);
    $canvas->createRectangle(
        $x - 4, $y - 4, $x + 4, $y + 4,
        -fill    => $color,
        -outline => $color
    );
    $canvas->createText(
        $x - 10, $y,
        -text   => $label,
        -fill   => $color,
        -anchor => 'e'
    );
}

## Method ######################
#  Name             : Plot
#  Returns          : -
#  Params via href  : Yes
#  Parameters       : spec_object, outfile?, no_ovals?
#  Purpose          : Creates a Tk::Canvas with the figure, with a button to
#                   : save as eps.
##
#  Multi?           : No
#  Usage            : $obj->Plot(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a
sub Plot {
    my ( $self, $opts_ref ) = @_;
    my $id = ident $self;

    my $spec_object = $opts_ref->{spec_object}
      // confess "Missing required argument 'spec_object'";
    my $outfile  = $opts_ref->{outfile}  // undef;
    my $no_ovals = $opts_ref->{no_ovals} // 0;
    my $no_chart = $opts_ref->{no_chart} // !$spec_object->get_draw_chart();
    my $no_seq   = $opts_ref->{no_seq}   // !$spec_object->get_draw_seq();
    my $chart_style = $opts_ref->{chart_style}
      // confess "Missing required argument 'chart_style'";
    Setup( $spec_object, $no_ovals, $no_chart, $no_seq );

    use Tk;
    our $MW = new MainWindow( -title => '> ' . $outfile );
    $Canvas = $MW->Canvas(
        -background => '#FFFFFF',
        -height     => $FIG_HEIGHT,
        -width      => $FIG_WIDTH
    )->pack( -side => 'top' );

    $MW->bind(
        '<KeyPress-q>' => sub {
            exit;
        }
    );
    $MW->focusmodel('active');

    DrawChart( $spec_object, $chart_style ) if $DRAW_CHARTS;
    DrawSequences( $spec_object, $no_ovals ) if $DRAW_SEQUENCES;
    if ($outfile) {
        my $button;
        $button = $MW->Button(
            -text    => 'Save',
            -command => sub {
                $Canvas->postscript(
                    -file       => $outfile,
                    -pageheight => '10c',
                    -height     => $FIG_HEIGHT,
                );
                $button->configure( -text => 'Saved', -state => 'disabled' );
              }

        )->pack( -side => 'top' );
    }
    MainLoop();

}

sub DrawChart {
    my ( $spec_object, $chart_style ) = @_;
    my @sequences_to_chart = @{ $spec_object->get_sequences_to_chart };

    my $MaxSteps =
      $chart_style eq 'Bar'
      ? max( map { $_->get_max_avg_steps() } @sequences_to_chart )
      : max( map { $_->get_max_max_steps() } @sequences_to_chart );

    $MaxSteps = first { $_ > $MaxSteps } (
        500,
        ( map { $_ * 1000 } ( 1 .. 10 ) ),
        ( map { $_ * 5000 } ( 3 .. 100 ) )
    );

    DrawChartTitles();
    DrawPercentCorrectScale();
    ## MaxSteps: $MaxSteps
    DrawCodeletCountScale( $MaxSteps, $spec_object->get_has_human_data() );
    ## Scales Drawn

    my $seq_num = 0;
    for my $seq (@sequences_to_chart) {
        ### seq: $seq->_DUMP
        for my $chart_num ( 1, 2 ) {
            $Canvas->createText(
                HorizontalClusterCenterOffset( $chart_num, $seq_num ),
                $CHART_BOTTOM - $CHART_B_MARGIN + 5,
                -text   => $seq->get_label(),
                -anchor => 'n',
                @{ SEQUENCE_LABEL_TEXT_OPTIONS() },
            );
        }

        my $data_by_cluster = $seq->get_data_indexed_by_cluster();
        my @cluster_specs =
            ( $spec_object->get_figure_type() eq 'LTM_SELF_CONTEXT' )
          ? ( $spec_object->get_clusters()->[0] )
          : @{ $spec_object->get_clusters() };
        my @cluster_names =
            ( $spec_object->get_figure_type() eq 'LTM_SELF_CONTEXT' )
          ? ( map { 'cluster_' . $_ } ( 0 .. 9 ) )
          : @cluster_specs;

        my $subcounter = 0;
        for my $cluster_name (@cluster_names) {
            my $cluster_spec =
              ( $spec_object->get_figure_type() eq 'LTM_SELF_CONTEXT' )
              ? $cluster_specs[0]
              : $cluster_specs[$subcounter];

            my $color             = $cluster_spec->get_color;
            my $is_human          = $cluster_spec->is_human();
            my $data_for_this_bar = $data_by_cluster->{$cluster_name};
            my $stats             = $data_for_this_bar;

            ## cluster_name, data_by_cluster, data_for_this_bar: $cluster_name, $data_by_cluster, $data_for_this_bar, @cluster_names

            my $fraction_correct = $stats->get_success_percentage() / 100;
            $Canvas->createRectangle(
                BarCoordinateToFigCoordinate(
                    1, $seq_num, $subcounter, 0.2, 0
                ),
                BarCoordinateToFigCoordinate(
                    1, $seq_num, $subcounter, 0.8, $fraction_correct
                ),
                -fill    => $color,
                -outline => $color,
            );

            # Num correct
            $Canvas->createText(
                BarCoordinateToFigCoordinate(
                    1, $seq_num, $subcounter, 0.5, $fraction_correct + 0.1
                ),
                -text   => $stats->get_successful_count() . '/',
                -anchor => 's',
                @{ BAR_HEIGHT_TEXT_OPTIONS() }
            );

            $Canvas->createText(
                BarCoordinateToFigCoordinate(
                    1, $seq_num, $subcounter, 0.5, $fraction_correct
                ),
                -text   => $stats->get_total_count(),
                -anchor => 's',
                @{ BAR_HEIGHT_TEXT_OPTIONS() }
            );

            my $drawer =
                $chart_style eq 'Bar'        ? \&Draw_TimeTaken
              : $chart_style eq 'BoxPlot'    ? \&Draw_TimeTaken_BoxPlot
              : $chart_style eq 'Percentile' ? \&Draw_TimeTaken_Percentile
              :   confess "Unknown chart_style $chart_style";

            $drawer->(
                {
                    seq_num    => $seq_num,
                    subcounter => $subcounter,
                    MaxSteps   => $MaxSteps,
                    stats      => $stats,
                    is_human   => $is_human,
                    color      => $color,
                }
            );
            _DrawLegend( $Canvas, $subcounter, $color,
                $cluster_spec->get_label() )
              if ( $seq_num == 0 and !$CUSTOM_LEGEND
                   and scalar(@cluster_names) > 1);
            if ( $spec_object->get_figure_type eq 'LTM_SELF_CONTEXT' ) {
                $Canvas->createText(
                    BarCoordinateToFigCoordinate(
                        1, $seq_num, $subcounter, 0.5, -0.04,
                    ),
                    -text   => $subcounter,
                    -anchor => 'n',
                );
               $Canvas->createText(
                    BarCoordinateToFigCoordinate(
                        2, $seq_num, $subcounter, 0.5, -0.04,
                    ),
                    -text   => $subcounter,
                    -anchor => 'n',
                );
   
            }
            $subcounter++;
        }
        $seq_num++;
    }

    if ($CUSTOM_LEGEND) {
        for my $i ( 0 .. scalar(@$CUSTOM_LEGEND) ) {
            my ( $c, $l ) = split( ';', $CUSTOM_LEGEND->[$i], 2 );
            _DrawLegend( $Canvas, $i, $c, $l );
        }
    }
}

sub DrawSequences {
    my ( $graph_spec, $no_ovals ) = @_;
    my $seq_num = 0;
    for my $seq ( @{ $graph_spec->get_sequences_to_draw } ) {
        $Canvas->createText(
            SeqCoordToCanvasCoord( $seq_num, -10, -5 ),
            -text   => $seq->get_label(),
            -anchor => 'sw',
            @{ SEQUENCE_LABEL_TEXT_OPTIONS() }
        );

        my $sequence_to_show = $seq->get_sequence_with_markup;
        my @distractor       = @{ $seq->get_distractors };

        Show( $seq_num, $sequence_to_show, 0, $no_ovals );
        if ( not $no_ovals ) {
            for my $dist (@distractor) {
                my ( $start, $end ) = split( ' ', $dist );
                DrawGroup( $seq_num, $start, $end, 3, DISTRACTOR_OPTIONS, 1 );
            }
        }

        $seq_num++;
    }

}

{
    my @GroupA;
    my @GroupB;
    my @BarLines;

    sub Parse {
        my ($string) = @_;
        @GroupA = @GroupB = ();
        my @tokens = Tokenize($string);
        my @Elements = grep { m#\d|\.# } @tokens;
        ReadGroups( \@tokens, '{', '}', \@GroupB );
        ReadGroups( \@tokens, '[', ']', \@GroupB );
        ReadGroups( \@tokens, '(', ')', \@GroupA );
        ReadGroups( \@tokens, '<', '>', \@GroupA );
        @BarLines = ();
        ReadBarLines( \@tokens, \@BarLines );

        ## GroupA: @GroupA
        ## GroupB: @GroupB

        @GroupA = rikeysort { $_->[1] - $_->[0] } @GroupA;
        @GroupB = rikeysort { $_->[1] - $_->[0] } @GroupB;

        ## GroupA: @GroupA
        ## GroupB: @GroupB
        return ( \@Elements, \@GroupA, \@GroupB, \@BarLines );
    }

    sub Tokenize {
        my ($string) = @_;

        # print $string, "\n";
        $string =~ s#,# #g;

        # print $string, "\n";
        $string =~ s#([\(\)\[\]\<\>\{\}\|])# $1 #g;
        $string =~ s#\.\.\.# ... #g;
        $string =~ s#^\s*##;
        $string =~ s#\s*$##;

        # print $string, "\n";
        return split( /\s+/, $string );
    }

    sub ReadGroups {
        my ( $tokens_ref, $start_token, $end_token, $groups_set ) = @_;
        my $stack_size = 0;
        my @stack;
        my $element_count = 0;
        for my $token (@$tokens_ref) {
            if ( $token eq $start_token ) {
                $stack_size++;
                push @stack, $element_count;
            }
            elsif ( $token eq $end_token ) {
                die "Mismatched $end_token" unless $stack_size;
                my $group_start = pop(@stack);
                push @$groups_set, [ $group_start, $element_count ];
                $stack_size--;
            }
            elsif ( $token =~ m#^ \-? \d+ | \.#x ) {
                $element_count++;
            }
        }
        if ($stack_size) {
            die "Mismatched $start_token";
        }
    }

    sub ReadBarLines {
        my ( $tokens_ref, $barlines_ref ) = @_;
        ## In ReadBarLines:
        my $elements_seen = 0;
        for my $token (@$tokens_ref) {
            if ( $token eq '|' ) {
                ## Token: $token
                push @$barlines_ref, $elements_seen;
            }
            elsif ( $token =~ m#^ \-? \d+ #x ) {
                $elements_seen++;
            }
        }
    }
}

sub Show {
    my ( $seq_num, $SequenceString, $IS_MOUNTAIN, $no_ovals ) = @_;
    %ARROW_ANCHORS = ();
    if ($IS_MOUNTAIN) {
        ShowMountain();
        return;
    }
    my $string = $SequenceString;
    if ($no_ovals) {
        $string =~ s{ \| .* }{ \| }x;
    }

    # print "Will Parse: >$SequenceString<\n";
    my ( $Elements_ref, $GroupA_ref, $GroupB_ref, $BarLines_ref ) =
      Parse($string);

    $FADE_AFTER = $BarLines_ref->[0];
    my $ElementsCount = scalar(@$Elements_ref);
    confess "Too mant elements!" if $ElementsCount > $MAX_TERMS;

    my $PretendWeHaveElements =
      ( $ElementsCount < $MIN_TERMS ) ? $MIN_TERMS : $ElementsCount;
    $WIDTH_PER_TERM = WIDTH() / ( $PretendWeHaveElements + 1 );
    $Y_DELTA_PER_UNIT_SPAN =
      ( HEIGHT() * $OVAL_MINOR_AXIS_FRACTION * 0.1 ) /
      ( 2 * $PretendWeHaveElements );

    unless ($no_ovals) {
        for (@$GroupA_ref) {
            DrawGroup( $seq_num, @$_, 3, GROUP_A_OPTIONS );
        }
        for (@$GroupB_ref) {
            DrawGroup( $seq_num, @$_, 0, GROUP_B_OPTIONS );
        }
    }
    DrawElements( $seq_num, $Elements_ref );
    DrawArrows($seq_num);

}

sub DrawElements {
    my ( $seq_num, $Elements_ref ) = @_;
    my $label = 'a';
    my $x_pos = 3 + $WIDTH_PER_TERM * 0.5;
    my $count = 0;
    for my $elt (@$Elements_ref) {
        my $fill = ( $count >= $FADE_AFTER ) ? '#CCCCCC' : 'black';
        $Canvas->createText(
            SeqCoordToCanvasCoord( $seq_num, $x_pos, $SEQUENCE_HEIGHT / 2 ),
            -text   => $elt,
            -font   => FONT,
            -fill   => $fill,
            -anchor => 'center',
        );

        # $ARROW_ANCHORS{"$count"} //= [$x_pos, Y_CENTER - 10 ];
        $count++;
        $x_pos += $WIDTH_PER_TERM;
    }
}

sub DrawArrows {
    return;
    my $Arrows;
    $Arrows =~ s#\s##g;
    return unless $Arrows;
    my @pieces = split( /;/, $Arrows );
    for my $piece (@pieces) {
        $piece =~ m#^ ([\d,]+) : ([\d,]+) $#x
          or confess
          "Cannot parse $Arrows (specifically, the piece >>$piece<<)";
        my ( $left, $right ) = ( $1, $2 );
        my $left_arrow_pos = $ARROW_ANCHORS{$left}
          or confess "No group $left";
        my $right_arrow_pos = $ARROW_ANCHORS{$right}
          or confess "No group $right";
        my ( $x1, $y1, $x2, $y2 ) = ( @$left_arrow_pos, @$right_arrow_pos );
        $Canvas->createLine(
            $x1, $y1,
            ( $x1 + $x2 ) / 2,
            Y_CENTER -30,
            $x2, $y2,
            -arrowshape => [ 8, 12, 10 ],
            -smooth     => 1,
            -arrow      => 'last',
            -width      => 2,
            -fill       => 'black',
        );
    }
}

sub DrawGroup {
    my ( $seq_num, $start, $end, $extra_width, $options_ref, $is_distractor ) =
      @_;
    my $faded = 1 if $end > $FADE_AFTER;
    my $span = $end - $start;
    my ( $x1, $x2 ) = (
        3 + $WIDTH_PER_TERM * ( $start + 0.1 ) - $extra_width,
        3 + $WIDTH_PER_TERM * ( $end - 0.1 ) + $extra_width
    );
    my $y_delta =
      $OVAL_MINOR_AXIS_MIN + $extra_width + $span * $Y_DELTA_PER_UNIT_SPAN;

    #if ( $y_delta > Y_CENTER() - 7 ) {    # Center is off a bit.
    #    $OVAL_MINOR_AXIS_FRACTION--;
    #    Y_TOO_BIG->throw();
    #}

    my ( $y1, $y2 ) = ( Y_CENTER() - $y_delta, Y_CENTER() + $y_delta );

    my @options = @$options_ref;
    push @options, @{ FADED_GROUP_OPTIONS() }
      if ( $faded and not $is_distractor );
    $Canvas->createOval( SeqCoordToCanvasCoord( $seq_num, $x1, $y1 ),
        SeqCoordToCanvasCoord( $seq_num, $x2, $y2 ), @options );
    my $upto = $end - 1;

    # say "Drew >>$start,$upto<<";
    $ARROW_ANCHORS{"$start,$upto"} //= [ ( $x1 + $x2 ) / 2, $y1 ];
}

sub DrawPercentCorrectScale {
    my ( $left, $bottom ) = BarCoordinateToFigCoordinate( 1, 0, 0, -1, 0 );
    my $top = $bottom - $MAX_BAR_HEIGHT;
    my ($right) = BarCoordinateToFigCoordinate( 1, $SEQUENCES_TO_PLOT_COUNT - 1,
        $CLUSTER_COUNT, 1.1, 0 );

    my $height_per_percent = $MAX_BAR_HEIGHT / 100;

    for ( 0, 25, 50, 75, 100 ) {
        DrawScaleLine(
            {
                left  => $left,
                right => $right,
                label => $_ . '%',
                y     => $bottom - $_ * $height_per_percent
            }
        );
    }
}

sub DrawCodeletCountScale {
    my ( $MaxSteps, $DrawHumanScale ) = @_;
    ## h: $DrawHumanScale
    my $x_tab_step;
    given ($MaxSteps) {
        when ( $_ < 100 ) { $x_tab_step = 10 }
        when ( $_ < 8000 ) {
            my $approx_steps = $_ / 4;
            $x_tab_step = 100 * int( $approx_steps / 100 );
        }
        when ( $_ < 50000 ) {
            my $approx_steps = $_ / 4;
            $x_tab_step = 1000 * int( $approx_steps / 1000 );
        }
        $x_tab_step = 10000;
    }

    ## x_tab_step: $x_tab_step
    $x_tab_step = $MaxSteps unless $x_tab_step;
    my ( $left, $bottom ) = BarCoordinateToFigCoordinate( 2, 0, 0, -0.2, 0 );
    my $top = $bottom - $MAX_BAR_HEIGHT;
    my ($right) = BarCoordinateToFigCoordinate(
        2,
        $SEQUENCES_TO_PLOT_COUNT - 1,
        $CLUSTER_COUNT - 1,
        1.1, 0
    );

    my $height_per_codelet = $MAX_BAR_HEIGHT / $MaxSteps;

    for ( my $count = 0 ; $count <= $MaxSteps ; $count += $x_tab_step ) {
        my $label = $count;
        if ( $x_tab_step % 1000 == 0 ) {
            $label = ( $count / 1000 ) . 'k';
        }
        DrawScaleLine(
            {
                left  => $left,
                right => $right,
                label => $label,
                y     => $bottom - $count * $height_per_codelet,
            }
        );
    }

    return unless $DrawHumanScale;

    # Draw Seconds axis.
    my $max_seconds =
      int( $MaxSteps / $Perf::AllCollectedData::CODELETS_PER_SECOND );
    my $new_left         = $right + 3;
    my $new_right        = $new_left + 10;
    my $seconds_tab_step = int( $max_seconds / 6 ) || 1;
    for (
        my $seconds = 0 ;
        $seconds <= $max_seconds ;
        $seconds += $seconds_tab_step
      )
    {
        my $label = $seconds . 's';
        DrawScaleLine(
            {
                left  => $new_left,
                right => $new_right,
                label => $label,
                y     => $bottom -
                  $seconds *
                  $Perf::AllCollectedData::CODELETS_PER_SECOND *
                  $height_per_codelet,
                label_to_right => 1,
            }
        );
    }

}

sub DrawScaleLine {
    my ($opts_ref) = @_;
    my %opts_ref = %$opts_ref;
    my $left = $opts_ref->{left} // confess "Missing required argument 'left'";
    my $right = $opts_ref->{right}
      // confess "Missing required argument 'right'";
    my $label = $opts_ref->{label}
      // confess "Missing required argument 'label'";
    my $y = $opts_ref->{y} // confess "Missing required argument 'y'";
    my $label_to_right = $opts_ref->{label_to_right} // 0;

    $Canvas->createLine( $left, $y, $right, $y, @{ SCALE_LINE_OPTIONS() } );
    if ($label_to_right) {
        $Canvas->createText(
            $right + 3, $y,
            -text   => $label,
            -anchor => 'w',
            @{ SCALE_TEXT_OPTIONS() }
        );
    }
    else {
        $Canvas->createText(
            $left - 3, $y,
            -text   => $label,
            -anchor => 'e',
            @{ SCALE_TEXT_OPTIONS() }
        );
    }
}

sub DrawChartTitles {
    $Canvas->createRectangle(
        $CHART1_H_OFFSET,                $CHART_V_OFFSET,
        $CHART1_H_OFFSET + $CHART_WIDTH, $CHART_BOTTOM,
        @{ CHART_RECTANGLE_OPTIONS() },
    );
    $Canvas->createRectangle(
        $CHART2_H_OFFSET,                $CHART_V_OFFSET,
        $CHART2_H_OFFSET + $CHART_WIDTH, $CHART_BOTTOM,
        @{ CHART_RECTANGLE_OPTIONS() },
    );
    my $center1 = $CHART1_H_OFFSET + $CHART_WIDTH / 2;
    my $center2 = $CHART2_H_OFFSET + $CHART_WIDTH / 2;
    $Canvas->createText(
        $center1, $CHART_BOTTOM,
        -text   => 'Percent correct',
        -anchor => 's',
        @{ CHART_TITLE_OPTIONS() }
    );
    $Canvas->createText(
        $center2, $CHART_BOTTOM,
        -text   => 'Time taken when correct',
        -anchor => 's',
        @{ CHART_TITLE_OPTIONS() }
    );
}

sub Draw_TimeTaken {
    my ($opts_ref) = @_;
    my $seq_num = $opts_ref->{seq_num}
      // confess "Missing required argument 'seq_num'";
    my $subcounter = $opts_ref->{subcounter}
      // confess "Missing required argument 'subcounter'";
    my $MaxSteps = $opts_ref->{MaxSteps}
      // confess "Missing required argument 'maxsteps'";
    my $stats = $opts_ref->{stats}
      // confess "Missing required argument 'stats'";
    my $is_human = $opts_ref->{is_human}
      // confess "Missing required argument 'is_human'";
    my $color = $opts_ref->{color}
      // confess "Missing required argument 'color'";

    # Draw Time Taken
    my $fraction_of_max_steps = $stats->get_avg_time_to_success() / $MaxSteps;
    $fraction_of_max_steps *= $Perf::AllCollectedData::CODELETS_PER_SECOND
      if $is_human;
    $Canvas->createRectangle(
        BarCoordinateToFigCoordinate( 2, $seq_num, $subcounter, 0, 0 ),
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 1, $fraction_of_max_steps
        ),
        -fill => $color
    );

}

sub Draw_TimeTaken_BoxPlot {
    my ($opts_ref) = @_;
    my $seq_num = $opts_ref->{seq_num}
      // confess "Missing required argument 'seq_num'";
    my $subcounter = $opts_ref->{subcounter}
      // confess "Missing required argument 'subcounter'";
    my $MaxSteps = $opts_ref->{MaxSteps}
      // confess "Missing required argument 'maxsteps'";
    my $stats = $opts_ref->{stats}
      // confess "Missing required argument 'stats'";
    my $is_human = $opts_ref->{is_human}
      // confess "Missing required argument 'is_human'";
    my $color = $opts_ref->{color}
      // confess "Missing required argument 'color'";

    # Draw Time Taken
    my @quartiles = (
        $stats->get_min_time_to_success, $stats->get_quartile_1,
        $stats->get_median,              $stats->get_quartile_3,
        $stats->get_max_time_to_success
    );

    my @quartile_fractions = map {
        my $ret = $_ / $MaxSteps;
        $ret *= $Perf::AllCollectedData::CODELETS_PER_SECOND if $is_human;
        $ret;
    } @quartiles;

    $Canvas->createLine(
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 0, $quartile_fractions[0]
        ),
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 1, $quartile_fractions[0]
        ),
    );

    $Canvas->createLine(
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 0, $quartile_fractions[4]
        ),
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 1, $quartile_fractions[4]
        ),
    );

    $Canvas->createRectangle(
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 0, $quartile_fractions[1]
        ),
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 1, $quartile_fractions[3]
        ),

        #-fill => $color
    );

    $Canvas->createLine(
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 0, $quartile_fractions[2]
        ),
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 1, $quartile_fractions[2]
        ),
    );
}

sub Draw_TimeTaken_Percentile {
    my ($opts_ref) = @_;
    my $seq_num = $opts_ref->{seq_num}
      // confess "Missing required argument 'seq_num'";
    my $subcounter = $opts_ref->{subcounter}
      // confess "Missing required argument 'subcounter'";
    my $MaxSteps = $opts_ref->{MaxSteps}
      // confess "Missing required argument 'maxsteps'";
    my $stats = $opts_ref->{stats}
      // confess "Missing required argument 'stats'";
    my $is_human = $opts_ref->{is_human}
      // confess "Missing required argument 'is_human'";
    my $color = $opts_ref->{color}
      // confess "Missing required argument 'color'";

    # Draw Time Taken
    my @deciles =
      map { $stats->TimeToSuccessAtPercentile( $_ / 10 ) } ( 1 .. 10 );

    my @decile_fractions = map {
        my $ret = $_ / $MaxSteps;
        $ret *= $Perf::AllCollectedData::CODELETS_PER_SECOND if $is_human;
        $ret;
    } @deciles;

    for ( 1 .. 10 ) {
        $Canvas->createLine(
            BarCoordinateToFigCoordinate( 2, $seq_num, $subcounter, $_ / 10, 0,
            ),
            BarCoordinateToFigCoordinate(
                2,           $seq_num,
                $subcounter, $_ / 10,
                $decile_fractions[ $_ - 1 ]
            ),
            -fill => $color,
        );
    }

    # Draw a horizontal "average" line.
    my $mean_fraction = $stats->get_avg_time_to_success() / $MaxSteps;
    $mean_fraction *= $Perf::AllCollectedData::CODELETS_PER_SECOND if $is_human;
    $Canvas->createLine(
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 0, $mean_fraction,
        ),
        BarCoordinateToFigCoordinate(
            2, $seq_num, $subcounter, 1.2, $mean_fraction
        ),
        -fill => CHART_BG_COLOR(),
    );

}

1;
