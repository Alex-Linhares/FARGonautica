# Coderack drawing golden (loop0002 item 008): lib/SGUI/Coderack.pm's Setup geometry
# (config/GUI_ws3.conf [Layout] Margin and [CoderackLayout]: MaxColumns, MaxRows, NameOffset,
# CountOffset, UrgencyOffset, HistoricalFractionOffset) and DrawIt (the column headers, then
# one row per key of %SCoderack::HistoryOfRunnable: name, count, urgency bar, historical
# fraction bar, historical count, and the lowered background stripes), for the GuiRecipes
# states in several rectangles.
# Each case: { recipe, rect, redraw, items => canvas dump, died => DrawIt's die message
# (without " at FILE line N.") or null, codelets => [[family, urgency], ...]
# (@SCoderack::CODELETS), urgencies_sum, history => [[key, count], ...] (%HistoryOfRunnable
# before the draw, sorted by key), drawn => [[key, count], ...] (%HistoryOfRunnable after the
# draw, in hash order: the order DrawIt's `each` walked it, i.e. the row order) }.
# DrawIt writes `$HistoryOfRunnable{$_} ||= 0` for the families on the rack, so a later draw
# still shows them. The `redraw` cases draw coderack_small once (on a canvas thrown away),
# then replace the rack by one Mid codelet (urgency 10) and dump a second draw.
use strict;
use warnings;
no warnings 'once';
use Tk;
use Oracle;
use CanvasDump;

# Test::Seqsee runs INITIALIZE_for_testing at load, which prints "View: 1!".
BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}
use Themes::Std2;    # must come before any SGUI::* module
use SGUI::Coderack;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ] );

my %PNG = ( coderack_small => 'coderack_small', coderack_many => 'coderack_many',
  coderack_zero_urgency => 'coderack_zero_urgency' );

sub history_sorted {
  return [ map { [ $_, 0 + $SCoderack::HistoryOfRunnable{$_} ] }
      sort keys %SCoderack::HistoryOfRunnable ];
}

sub history_in_hash_order {
  return [ map { [ $_, 0 + $SCoderack::HistoryOfRunnable{$_} ] }
      keys %SCoderack::HistoryOfRunnable ];
}

sub new_canvas {
  my ( $x, $y, $w, $h ) = @_;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  SGUI::Coderack->Setup( $c, $x, $y, $w, $h );
  return $c;
}

sub draw_case {
  my ( $recipe, $rect, $redraw ) = @_;
  build_recipe($recipe);
  if ($redraw) {
    my $c0 = new_canvas(@$rect);
    SGUI::Coderack->DrawIt();
    $c0->destroy;
    @SCoderack::CODELETS      = ();
    $SCoderack::URGENCIES_SUM = 0;
    SCoderack->add_codelet( SCodelet->new( 'Mid', 10, {} ) );
  }
  my $history = history_sorted();
  my $c       = new_canvas(@$rect);
  my $died;
  eval { SGUI::Coderack->DrawIt(); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  record(
    recipe        => $recipe,
    rect          => [@$rect],
    redraw        => ( $redraw ? 1 : 0 ),
    items         => dump_canvas($c),
    died          => $died,
    codelets      => [ map { [ $_->[0], 0 + $_->[1] ] } @SCoderack::CODELETS ],
    urgencies_sum => 0 + $SCoderack::URGENCIES_SUM,
    history       => $history,
    drawn         => history_in_hash_order(),
  );
  my ( $x, $y, $w ) = @$rect;
  if ( !$redraw && $PNG{$recipe} && $x == 0 && $w == 780 ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (qw(empty one_element attention_elements coderack_small coderack_zero_urgency
  coderack_history_only coderack_many))
{
  draw_case( $recipe, $_ ) for @RECTS;
}
draw_case( 'coderack_small', $_, 1 ) for @RECTS[ 0, 2 ];

emit();
