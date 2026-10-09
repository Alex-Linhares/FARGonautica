# Rules list drawing golden (loop0002 item 012): lib/SGUI/List.pm (Setup, DrawIt,
# GetEntriesOnCurrentPage, DrawBookKeeping; see gui_list_groups.pl) with
# lib/SGUI/List/Rules.pm (HeightPerRow 15; GetItemList: SRule->GetListOfSimpleRules and
# SRule->GetListOfCompoundRules; DrawOneItem: the rule's as_text, anchored nw).
# lib/SRule.pm has neither method, so the real list dies in GetItemList, before anything is
# drawn ('real' cases). To check DrawOneItem anyway, the 'fake' cases install the two methods
# here, returning FakeRule objects whose as_text is given (an undef and a '' among them).
# Each case: { recipe, fake (0/1), rules => the fake rules' texts (simple then compound) or
# null, rect, page, redraw, items, died, page_after, shown_from, shown_to, entries_count (as
# in gui_list_groups.pl) }.
# Tags: the list object is 'SGUI::List::Rules'; the K-th fake rule is 'rule<K>'.
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
use SGUI::List::Rules;
use GuiRecipes;

package FakeRule;
sub new     { my ( $class, $text ) = @_; return bless { text => $text }, $class }
sub as_text { $_[0]{text} }

package main;

my $mw = MainWindow->new;

my @RECTS = ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ], [ 50, 30, 600, 300 ],
  [ 10, 100, 300, 150 ], [ 0, 0, 30, 30 ], [ 0, 0, 200, 10 ] );

# 20 simple and 13 compound rules: more than a 780x450 page holds (27).
my @SIMPLE = ( map( {"simple rule $_: [ascending] start => succ"} 0 .. 17 ), '', undef );
my @COMPOUND = map {"compound rule $_ (" . ( 'x' x $_ ) . ')'} 0 .. 12;
my @FAKE = ( @SIMPLE, @COMPOUND );
my @FAKE_OBJECTS = map { FakeRule->new($_) } @FAKE;

sub draw_case {
  my ( $recipe, $fake, $rect, $page, $redraw ) = @_;
  build_recipe($recipe);
  no warnings 'redefine';
  if ($fake) {
    *SRule::GetListOfSimpleRules   = sub { @FAKE_OBJECTS[ 0 .. $#SIMPLE ] };
    *SRule::GetListOfCompoundRules = sub { @FAKE_OBJECTS[ @SIMPLE .. $#FAKE ] };
  }
  else {
    undef *SRule::GetListOfSimpleRules;
    undef *SRule::GetListOfCompoundRules;
  }
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $mw->Canvas( -width => $x + $w, -height => $y + $h, -background => '#FFFFFF' )->pack;
  my $list = SGUI::List::Rules->new();
  $list->Setup( $c, $x, $y, $w, $h );
  my %tags = ( "$list" => 'SGUI::List::Rules' );
  $tags{"$list-$_"} = "SGUI::List::Rules-$_" for qw(Clickable-Item pageup pagedown);
  $tags{"$FAKE_OBJECTS[$_]"} = "rule$_" for 0 .. $#FAKE_OBJECTS;
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
    fake          => $fake ? 1 : 0,
    rules         => $fake ? [@FAKE] : undef,
    rect          => [@$rect],
    page          => $page,
    redraw        => $redraw ? 1 : 0,
    items         => dump_canvas( $c, \%tags ),
    died          => $died,
    page_after    => $list->{PageNumber},
    shown_from    => $list->{EntriesShownFrom},
    shown_to      => $list->{EntriesShownTo},
    entries_count => $list->{EntriesCount},
  );
  if ( $fake && $x == 0 && $w == 780 && $page == 0 && !$redraw ) {
    save_png( $mw, $c, perl_screen_path('list_rules_fake') );
  }
  $c->destroy;
}

for my $recipe (qw(empty groups_list)) {
  draw_case( $recipe, 0, $_, 0, 0 ) for @RECTS;
}
draw_case( 'empty', 1, $_, 0, 0 ) for @RECTS;
for my $rect ( [ 0, 0, 780, 450 ], [ 0, 0, 400, 200 ] ) {
  draw_case( 'empty', 1, $rect, $_, 0 ) for 1, 3, 99;
  draw_case( 'empty', 1, $rect, 0, 1 );
}

emit();
