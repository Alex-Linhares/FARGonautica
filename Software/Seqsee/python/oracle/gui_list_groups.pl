# Groups list drawing golden (loop0002 item 011): lib/SGUI/List.pm (Setup geometry with
# config/GUI_ws3.conf's [Layout] Margin; DrawIt: GetEntriesOnCurrentPage's paging, one
# alternately coloured bar per entry lowered under the whole canvas, DrawOneItem;
# DrawBookKeeping: the "Page #..." text and the two red paging squares) and
# lib/SGUI/List/Groups.pm (DrawOneItem: lock "L", strength, bounds string, categories string;
# GetItemList = SWorkspace->GetGroups), for the GuiRecipes states in several rectangles and
# on several pages.
# Each case: { recipe, rect, page => the PageNumber set before DrawIt (Setup resets it to 0),
# redraw => 1 if the list was first drawn on page 0 and then redrawn the way a click on the
# page-up square does it (PageNumber++, Clear, DrawIt), items => canvas dump, died => the die
# message (without " at FILE line N.") or null, page_after / shown_from / shown_to /
# entries_count => the list's PageNumber, EntriesShownFrom, EntriesShownTo, EntriesCount after
# DrawIt (null if unset), groups => GetGroups' order (hash order for equal spans):
# { tag, bounds, strength, categories, locked } }.
# Tags: the list object is 'SGUI::List::Groups' (so its tags are 'SGUI::List::Groups',
# 'SGUI::List::Groups-Clickable-Item', '-pageup', '-pagedown'), and the K-th group of
# GetGroups is 'group<K>'.
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
use SGUI::List::Groups;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ], [ 0, 0, 200, 10 ] );

my %PNG = ( groups_list => 'list_groups', groups_many => 'list_groups_many' );

sub group_row {
  my ( $g, $k ) = @_;
  return {
    tag        => "group$k",
    bounds     => $g->get_bounds_string,
    strength   => 0 + $g->get_strength,
    categories => $g->get_categories_as_string,
    locked     => $g->get_is_locked_against_deletion ? 1 : 0,
  };
}

sub draw_case {
  my ( $recipe, $rect, $page, $redraw ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  my $list = SGUI::List::Groups->new();
  $list->Setup( $c, $x, $y, $w, $h );
  my @groups = SWorkspace->GetGroups();
  my %tags = ( "$list" => 'SGUI::List::Groups' );
  $tags{"$list-$_"} = "SGUI::List::Groups-$_" for qw(Clickable-Item pageup pagedown);
  $tags{"$groups[$_]"} = "group$_" for 0 .. $#groups;
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
  record(
    recipe        => $recipe,
    rect          => [@$rect],
    page          => $page,
    redraw        => $redraw ? 1 : 0,
    items         => dump_canvas( $c, \%tags ),
    died          => $died,
    page_after    => $list->{PageNumber},
    shown_from    => $list->{EntriesShownFrom},
    shown_to      => $list->{EntriesShownTo},
    entries_count => $list->{EntriesCount},
    groups        => [ map { group_row( $groups[$_], $_ ) } 0 .. $#groups ],
  );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 && $page == 0 && !$redraw ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  if ( $recipe eq 'groups_many' && $w == 400 && $page == 2 && !$redraw ) {
    save_png( $mw, $c, perl_screen_path('list_groups_many_page3') );
  }
  $c->destroy;
}

for my $recipe (qw(empty one_element groups_relations large nested_groups groups_list groups_many))
{
  draw_case( $recipe, $_, 0, 0 ) for @RECTS;
}

# Paging: later pages, a page past the last (clamped to the last), and the page-up redraw.
for my $rect ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 10, 100, 300, 150 ] ) {
  draw_case( 'groups_many', $rect, $_, 0 ) for 1, 2, 3, 99;
  draw_case( 'groups_many', $rect, 0, 1 );
}
draw_case( 'large', [ 10, 100, 300, 150 ], $_, 0 ) for 1, 5;
draw_case( 'empty', [ 0, 0, 400, 200 ], 3, 0 );
draw_case( 'groups_list', [ 0, 0, 400, 200 ], 0, 1 );

emit();
