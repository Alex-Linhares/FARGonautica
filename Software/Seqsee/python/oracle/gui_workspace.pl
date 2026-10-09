# Workspace drawing golden (loop0002 items 004 and 005): lib/SGUI/Workspace.pm's Setup
# geometry, elements (Seqsee::Element::draw_ws3), bar lines and the last-runnable label
# (004); groups (Seqsee::Anchored::draw_ws3: nested, squinted elements, raise('hilit')),
# metonyms (DrawMetonym) and relations (SRelation::draw_ws3: hidden ones, anchors) (005);
# for the GuiRecipes states, in several rectangles (with and without x/y offsets, small and
# degenerate ones).
# Each case: { recipe, rect => [x, y, w, h], items => canvas dump, metrics => [[font, text,
# fontMeasure width, linespace], ...] for each distinct text item }. DrawMetonym crosses out
# the metonym text with two lines through its bbox, which depends on the font metrics of the
# X server; the Python test measures text with these metrics. Element tags that are
# stringified refs are recorded as 'obj<oid>' (GuiRecipes::object_tags).
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
use SGUI::Coderack;  # family_to_name
use SGUI::Workspace;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ] );

my %PNG = ( 'hilit_debug' => 'workspace_hilit_debug', 'twenty_elements' => 'workspace_twenty',
  'bar_lines' => 'workspace_bar_lines', 'groups_relations' => 'workspace_groups_relations',
  'nested_groups' => 'workspace_nested_groups', 'metonyms' => 'workspace_metonyms',
  'overlapping_relations' => 'workspace_overlapping_relations', 'large' => 'workspace_large' );

sub text_metrics {
  my ($c) = @_;
  my ( %seen, @out );
  for my $id ( $c->find('all') ) {
    next unless $c->type($id) eq 'text';
    my $font = $c->itemcget( $id, '-font' );
    $font = $$font if ref $font && $font->isa('Tk::Font');    # its name, as dump_canvas
    my $text = $c->itemcget( $id, '-text' );
    next if $seen{"$font\n$text"}++;
    push @out,
      [ $font, $text, 0 + $c->fontMeasure( $font, $text ),
      0 + $c->fontMetrics( $font, '-linespace' ) ];
  }
  return \@out;
}

sub draw_case {
  my ( $recipe, $rect ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  SGUI::Workspace->Setup( $c, $x, $y, $w, $h );
  SGUI::Workspace->DrawIt();
  record( recipe => $recipe, rect => [@$rect], items => dump_canvas( $c, object_tags() ),
    metrics => text_metrics($c) );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (qw(empty one_element six_elements twenty_elements bar_lines hilit_debug
  runnable_thought groups_relations large nested_groups metonyms overlapping_relations
  squint_no_groups))
{
  draw_case( $recipe, $_ ) for @RECTS;
}

emit();
