# Slipnet drawing golden (loop0002 item 007): lib/SGUI/Slipnet.pm's Setup geometry
# (config/GUI_ws3.conf [Layout] Margin and [SlipnetLayout]: EntriesPerColumn, ColumnCount,
# MaxOvalRadius, MaxTextWidth, MinActivationForDisplay) and DrawIt / DrawNode (one oval sized
# by activation and one text per node of SLTM::GetTopConcepts(10) whose activation is above
# MinActivationForDisplay, in columns), for the GuiRecipes states in several rectangles.
# Each case: { recipe, rect, items => canvas dump, died => the message DrawIt died with
# (without " at FILE line N."), or null, concepts => [[as_text or null, real activation,
# raw activation, ref], ...] (GetTopConcepts, every node, in index order) }. A concept
# without an as_text method (Mapping::Dir) makes DrawNode die after drawing its oval; the
# items drawn before stay on the canvas.
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
use SGUI::Slipnet;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ] );

my %PNG = ( slipnet_small => 'slipnet_small', slipnet_full => 'slipnet_full',
  slipnet_long_text => 'slipnet_long_text', slipnet_dir => 'slipnet_dir' );

sub concepts {
  return [
    map {
      my $c = $_->[0];
      [ ( $c->can('as_text') ? $c->as_text : undef ), 0 + $_->[1], 0 + $_->[2], ref $c ]
    } SLTM::GetTopConcepts(10)
  ];
}

sub draw_case {
  my ( $recipe, $rect ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  SGUI::Slipnet->Setup( $c, $x, $y, $w, $h );
  my $died;
  eval { SGUI::Slipnet->DrawIt(); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  record( recipe => $recipe, rect => [@$rect], items => dump_canvas($c), died => $died,
    concepts => concepts() );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (qw(empty one_element groups_relations slipnet_small slipnet_full
  slipnet_long_text slipnet_dir))
{
  draw_case( $recipe, $_ ) for @RECTS;
}

emit();
