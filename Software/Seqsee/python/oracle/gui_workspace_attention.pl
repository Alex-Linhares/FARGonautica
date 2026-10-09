# Workspace attention drawing golden (loop0002 item 006): lib/SGUI/Workspace_Attention.pm's
# Setup geometry (config/GUI_ws3.conf [Workspace_AttentionLayout]), DrawIt (the black
# rectangle, groups and squinted elements with Style::GroupAttention /
# GroupBorderAttention, elements with Style::ElementAttention, bar lines, the raw
# last-runnable string) with SCoderack->AttentionDistribution, for the GuiRecipes states in
# several rectangles.
# Each case: { recipe, rect, items => canvas dump, metrics => [[font, text, width,
# linespace], ...], died => the message DrawIt died with (without " at FILE line N."), or
# null, attention => [[label, value], ...] }. DrawRelations calls $rel->draw_attention, but
# the method is defined in package SReln and relations are SRelation objects, so DrawIt dies
# at the first relation; the items drawn before it stay on the canvas (in the Tk GUI the die
# leaves Tk::Seqsee::Update, and the parts after it are not drawn).
# The attention labels: 'e<index>' for elements, 'g<bounds><structure>' for groups,
# 'r<bounds>-><bounds>' for relations; the values are AttentionDistribution's.
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
use SGUI::Workspace_Attention;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ] );

my %PNG = ( 'attention_elements' => 'attention_elements',
  'attention_groups' => 'attention_groups', 'attention_relations' => 'attention_relations' );

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

sub attention_labels {
  my %label;
  my $i = 0;
  $label{"$_"} = 'e' . $i++ for SWorkspace::GetElements();
  $label{"$_"} = 'g' . $_->get_bounds_string . $_->get_structure_string
    for SWorkspace::GetGroups();
  for my $r ( values %SWorkspace::relations ) {
    my ( $a, $b ) = $r->get_ends;
    $label{"$r"} = 'r' . $a->get_bounds_string . '->' . $b->get_bounds_string;
  }
  my $dist = SCoderack->AttentionDistribution();
  my @out;
  for my $k ( keys %$dist ) {
    push @out, [ $label{$k} // "scalar:$k", 0 + $dist->{$k} ];
  }
  return [ sort { $a->[0] cmp $b->[0] } @out ];
}

sub draw_case {
  my ( $recipe, $rect ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  SGUI::Workspace_Attention->Setup( $c, $x, $y, $w, $h );
  my $died;
  eval { SGUI::Workspace_Attention->DrawIt(); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  record( recipe => $recipe, rect => [@$rect], items => dump_canvas( $c, object_tags() ),
    metrics => text_metrics($c), died => $died, attention => attention_labels() );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (qw(empty one_element six_elements bar_lines hilit_debug runnable_thought
  groups_relations nested_groups metonyms squint_no_groups attention_elements
  attention_groups attention_relations))
{
  draw_case( $recipe, $_ ) for @RECTS;
}

emit();
