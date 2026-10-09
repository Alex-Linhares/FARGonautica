# Categories list drawing golden (loop0002 item 012): lib/SGUI/List.pm (Setup, DrawIt,
# GetEntriesOnCurrentPage, DrawBookKeeping; see gui_list_groups.pl) with
# lib/SGUI/List/Categories.pm (new's fields; PrepareForDrawing: GroupHtPerUnitSpan and
# SpacePerElement from $SWorkspace::ElementCount; GetItemList: the groups by span (rikeysort),
# then the elements, each object's edges pushed onto %Cat2Objects for each of its
# categories, and the categories in hash order; DrawOneItem: the category name, a grey
# rectangle, a blue oval per instance and a small square per element), for the GuiRecipes
# states in several rectangles and on several pages.
# Each case: { recipe, rect, page, redraw, items, died, page_after, shown_from, shown_to,
# entries_count (as in gui_list_groups.pl), element_count, groups => the GetGroups order that
# GetItemList used (hash order for equal spans): [{ bounds, categories => sorted names }],
# categories => GetItemList's order (hash order): [{ tag, name, edges => [[l, r], ...] (the
# %Cat2Objects entry, in drawing order) }] }.
# Tags: the list object is 'SGUI::List::Categories'; the K-th category of GetItemList is
# 'cat<K>'.
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
use SGUI::List::Categories;
use GuiRecipes;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ], [ 0, 0, 400, 90 ] );

my %PNG = ( groups_list => 'list_categories', categories_many => 'list_categories_many' );

# Record what DrawIt's GetItemList saw: the categories it returned and the groups it sorted.
my ( $LastCats, $LastGroups );
{
  no warnings 'redefine';
  my $item_list = \&SGUI::List::Categories::GetItemList;
  *SGUI::List::Categories::GetItemList = sub {
    my @r = $item_list->(@_);
    $LastCats = [@r];
    return @r;
  };
  my $get_groups = \&SWorkspace::GetGroups;
  *SWorkspace::GetGroups = sub {
    my @g = $get_groups->(@_);
    $LastGroups = [@g];
    return @g;
  };
}

sub draw_case {
  my ( $recipe, $rect, $page, $redraw ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  my $list = SGUI::List::Categories->new();
  $list->Setup( $c, $x, $y, $w, $h );
  ( $LastCats, $LastGroups ) = ();
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
  my @cats = @{ $LastCats || [] };
  my %tags = ( "$list" => 'SGUI::List::Categories' );
  $tags{"$list-$_"} = "SGUI::List::Categories-$_" for qw(Clickable-Item pageup pagedown);
  $tags{"$cats[$_]"} = "cat$_" for 0 .. $#cats;
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
    element_count => $SWorkspace::ElementCount,
    groups        => [
      map {
        { bounds => $_->get_bounds_string,
          categories => [ sort map { $_->get_name } @{ $_->get_categories } ] }
      } @{ $LastGroups || [] }
    ],
    categories => [
      map {
        { tag   => "cat$_",
          name  => $cats[$_]->get_name,
          edges => [ map { [ map { 0 + $_ } @$_ ] }
              @{ $SGUI::List::Categories::Cat2Objects{ $cats[$_] } } ] }
      } 0 .. $#cats
    ],
  );
  if ( $PNG{$recipe} && $x == 0 && $w == 780 && $page == 0 && !$redraw ) {
    save_png( $mw, $c, perl_screen_path( $PNG{$recipe} ) );
  }
  $c->destroy;
}

for my $recipe (
  qw(empty one_element six_elements groups_relations large nested_groups groups_list
  categories_many)
  )
{
  draw_case( $recipe, $_, 0, 0 ) for @RECTS;
}

# Paging: later pages, a page past the last (clamped to the last), and the page-up redraw.
for my $rect ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ] ) {
  draw_case( 'categories_many', $rect, $_, 0 ) for 1, 3, 99;
  draw_case( 'categories_many', $rect, 0, 1 );
}
draw_case( 'groups_list', [ 0, 0, 400, 90 ], $_, 0 ) for 2, 4;
draw_case( 'empty', [ 0, 0, 400, 200 ], 3, 0 );

emit();
