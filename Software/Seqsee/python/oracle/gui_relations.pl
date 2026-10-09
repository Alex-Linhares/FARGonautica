# Relations pane drawing golden (loop0002 item 010): lib/SGUI/Relations.pm's Setup geometry
# (config/GUI_ws3.conf [Layout] Margin and [RelationsLayout]: RowCount and the column offsets)
# and DrawIt / DrawRelation (one row per relation: strength, type complexity, the two ends'
# bounds strings and the type's as_text, with a pale green stripe lowered under every other
# row), for the GuiRecipes states in several rectangles.
# Each case: { recipe, rect, items => canvas dump, died => DrawIt's die message (without
# " at FILE line N.") or null, compound => how many relations are Mapping::Numeric,
# relations => one entry per row in the order DrawIt drew them (values %SWorkspace::relations,
# hash order): { ends => [bounds, bounds], strength, complexity, type => as_text, class,
# isa_numeric, isa_structural } }.
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
use SGUI::Relations;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ] );

my %PNG = ( relations_pane => 'relations_pane', relations_many => 'relations_many',
  groups_relations => 'relations_groups_relations' );

sub row {
  my ($r) = @_;
  my ( $f, $s ) = $r->get_ends;
  no warnings 'uninitialized';
  return {
    ends           => [ $f->get_bounds_string, $s->get_bounds_string ],
    strength       => 0 + $r->get_strength,
    complexity     => 0 + $r->get_type->get_complexity,
    type           => $r->get_type->as_text,
    class          => ref($r),
    isa_numeric    => UNIVERSAL::isa( $r, 'Mapping::Numeric' ) ? 1 : 0,
    isa_structural => UNIVERSAL::isa( $r, 'Mapping::Structural' ) ? 1 : 0,
  };
}

sub draw_case {
  my ( $recipe, $rect ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  SGUI::Relations->Setup( $c, $x, $y, $w, $h );
  # The order DrawIt walks: (@compound, @simple) over one `values` call. `values` of an
  # unchanged hash gives the same order every time.
  my @relations = values %SWorkspace::relations;
  my @compound  = grep { UNIVERSAL::isa( $_, 'Mapping::Numeric' ) } @relations;
  my @simple    = grep { not UNIVERSAL::isa( $_, 'Mapping::Structural' ) } @relations;
  my $died;
  eval { SGUI::Relations->DrawIt(); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  record(
    recipe    => $recipe,
    rect      => [@$rect],
    items     => dump_canvas($c),
    died      => $died,
    compound  => scalar(@compound),
    relations => [ map { row($_) } @compound, @simple ],
  );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (
  qw(empty one_element groups_relations overlapping_relations large relations_pane relations_many))
{
  draw_case( $recipe, $_ ) for @RECTS;
}

emit();
