# Stream drawing golden (loop0002 item 009): lib/SGUI/Stream.pm's Setup geometry
# (config/GUI_ws3.conf [Layout] Margin and [StreamLayout]: EntriesPerColumn, ColumnCount) and
# DrawIt / DrawThought (the current thought's box at the top left, then one box per true entry
# of OlderThoughts: Style::ThoughtBox from thought_hit_intensity, the thought's as_text in
# Style::ThoughtHead, and up to 3 stored_fringe components in Style::ThoughtComponent), for the
# GuiRecipes states in several rectangles.
# Each case: { recipe, rect, items => canvas dump, died => DrawIt's die message (without
# " at FILE line N.") or null, stream => { current => THOUGHT or null, older => [THOUGHT or
# null, ...] } } where THOUGHT = { text => as_text, hit => thought_hit_intensity or null,
# fringe => [[component text, activation, hit_intensity or null], ...] or null }.
# Components are drawn as Perl stringifies them, so most objects show as "Class=HASH(0x…)";
# every address is replaced by ADDR (in the items and in the stream) before it is recorded.
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
use SGUI::Stream;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ] );

my %PNG = ( stream_small => 'stream_small', stream_full => 'stream_full',
  stream_real => 'stream_real' );

sub no_addr {
  my ($s) = @_;
  return $s unless defined $s;
  $s =~ s/=(\w+)\(0x[0-9a-f]+\)/=$1(ADDR)/g;
  return $s;
}

sub num { defined $_[0] ? 0 + $_[0] : undef }

sub thought {
  my ($t) = @_;
  return undef unless $t;
  my $stream = $Global::MainStream;
  my $fringe = $t->stored_fringe;
  return {
    text   => no_addr( $t->as_text ),
    hit    => num( $stream->{thought_hit_intensity}{$t} ),
    fringe => $fringe
    ? [ map { [ no_addr("$_->[0]"), num( $_->[1] ), num( $stream->{hit_intensity}{ $_->[0] } ) ] }
        @$fringe ]
    : undef,
  };
}

sub draw_case {
  my ( $recipe, $rect ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  SGUI::Stream->Setup( $c, $x, $y, $w, $h );
  my $died;
  eval { SGUI::Stream->DrawIt(); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  my $items = dump_canvas($c);
  for my $item (@$items) {
    $item->{opts}{text} = no_addr( $item->{opts}{text} ) if exists $item->{opts}{text};
  }
  record(
    recipe => $recipe,
    rect   => [@$rect],
    items  => $items,
    died   => no_addr($died),
    stream => {
      current => thought( $Global::MainStream->{CurrentThought} ),
      older   => [ map { thought($_) } @{ $Global::MainStream->{OlderThoughts} } ],
    },
  );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (qw(empty groups_relations stream_current stream_small stream_full stream_real)) {
  draw_case( $recipe, $_ ) for @RECTS;
}

emit();
