# Stream list drawing golden (loop0002 item 012): lib/SGUI/List.pm (Setup, DrawIt,
# GetEntriesOnCurrentPage, DrawBookKeeping; see gui_list_groups.pl) with
# lib/SGUI/List/Stream.pm (new's fields; GetItemList: nothing without a current thought,
# else the current thought and OlderThoughts sorted by thought_hit_intensity (rnkeysort);
# DrawOneItem: the hit intensity ('-' for the current thought), the thought's as_text and its
# stored_fringe sorted by activation as "[activation] text; ", where text is the component's
# as_text if it can, else "$component"), for the GuiRecipes states in several rectangles and
# on several pages.
# Each case: { recipe, rect, page, redraw, items, died, page_after, shown_from, shown_to,
# entries_count (as in gui_list_groups.pl), order => the tags of GetItemList's entries,
# stream => { current => THOUGHT or null, older => [THOUGHT or null, ...] } }, where
# THOUGHT = { text => as_text, hit => thought_hit_intensity or null, fringe => [[label,
# activation], ...] or null } and label is what DrawOneItem writes for the component.
# Every address is replaced by ADDR (in the items and in the stream) before it is recorded.
# Tags: the list object is 'SGUI::List::Stream'; the current thought is 'tht0' and
# OlderThoughts' i-th entry 'tht<i+1>' (a '' entry stays '').
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
use SGUI::List;
use SGUI::List::Stream;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ], [ 0, 0, 400, 90 ] );

my %PNG = ( stream_small => 'list_stream_small', stream_list_many => 'list_stream_many',
  stream_real => 'list_stream_real' );

my $LastItems;
{
  no warnings 'redefine';
  my $item_list = \&SGUI::List::Stream::GetItemList;
  *SGUI::List::Stream::GetItemList = sub {
    my @r = $item_list->(@_);
    $LastItems = [@r];
    return @r;
  };
}

sub no_addr {
  my ($s) = @_;
  return $s unless defined $s;
  $s =~ s/=(\w+)\(0x[0-9a-f]+\)/=$1(ADDR)/g;
  return $s;
}

sub num { defined $_[0] ? 0 + $_[0] : undef }

sub label {
  my ($c) = @_;
  return UNIVERSAL::can( $c, 'as_text' ) ? $c->as_text() : "$c";
}

sub thought {
  my ($t) = @_;
  return undef unless $t;
  my $fringe = $t->stored_fringe;
  return {
    text   => no_addr( $t->as_text ),
    hit    => num( $Global::MainStream->{thought_hit_intensity}{$t} ),
    fringe => $fringe ? [ map { [ no_addr( label( $_->[0] ) ), num( $_->[1] ) ] } @$fringe ]
    : undef,
  };
}

sub draw_case {
  my ( $recipe, $rect, $page, $redraw ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  my $list = SGUI::List::Stream->new();
  $list->Setup( $c, $x, $y, $w, $h );
  my $stream = $Global::MainStream;
  my %tags   = ( "$list" => 'SGUI::List::Stream' );
  $tags{"$list-$_"} = "SGUI::List::Stream-$_" for qw(Clickable-Item pageup pagedown);
  my @older = @{ $stream->{OlderThoughts} };
  for my $i ( reverse 0 .. $#older ) {
    $tags{"$older[$i]"} = 'tht' . ( $i + 1 ) if $older[$i];
  }
  $tags{"$stream->{CurrentThought}"} = 'tht0' if $stream->{CurrentThought};
  $LastItems = undef;
  my $died;
  eval {
    if ($redraw) {
      $list->DrawIt();
      # What the page-up binding does, without ReDrawIt's print.
      $list->{PageNumber}++;
      $list->Clear();
    }
    else {
      $list->{PageNumber} = $page;
    }
    $list->DrawIt();
    1;
  } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  my $items = dump_canvas( $c, \%tags );
  for my $item (@$items) {
    $item->{opts}{text} = no_addr( $item->{opts}{text} ) if exists $item->{opts}{text};
  }
  record(
    recipe        => $recipe,
    rect          => [@$rect],
    page          => $page,
    redraw        => $redraw ? 1 : 0,
    items         => $items,
    died          => no_addr($died),
    page_after    => $list->{PageNumber},
    shown_from    => $list->{EntriesShownFrom},
    shown_to      => $list->{EntriesShownTo},
    entries_count => $list->{EntriesCount},
    order         => [ map { $tags{"$_"} // "$_" } @{ $LastItems || [] } ],
    stream        => {
      current => thought( $stream->{CurrentThought} ),
      older   => [ map { thought($_) } @older ],
    },
  );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 && $page == 0 && !$redraw ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (
  qw(empty stream_current stream_small stream_full stream_real stream_list_many stream_hole))
{
  draw_case( $recipe, $_, 0, 0 ) for @RECTS;
}

# Paging: later pages, a page past the last (clamped to the last), and the page-up redraw.
for my $rect ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ] ) {
  draw_case( 'stream_list_many', $rect, $_, 0 ) for 1, 2, 99;
  draw_case( 'stream_list_many', $rect, 0, 1 );
}

emit();
