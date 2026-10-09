# List interaction golden (loop0002 item 020): lib/SGUI/List.pm's Setup bindings (the
# page-up / page-down squares and "$self-Clickable-Item" '<1>' bindings, the 'current' item's
# tags), ProcessClickOnItem (SelectedItem, $Global::Break_Loop = 1, the popup deiconified)
# and CreatePopupWidget (a withdrawn Toplevel 'Actions for <class>' with one button per
# ActionButtons entry, in hash order; a button runs the action on SelectedItem, then
# Tk::Seqsee::Update and withdraw), with the ActionButtons of lib/SGUI/List/Groups.pm
# (Delete, Lock, Unlock, ShowFollowers, History, Fringe, ActionFringe) and
# lib/SGUI/List/Categories.pm (DeleteAllOther, AddBarlinesBefore).
#
# Cases (field 'kind'):
# - popup: { list, title, buttons => [texts in pack order], state } for each of the 4 lists.
# - clicks: { list, recipe, rect, clicks => [{ x, y, entries => the GetItemList order of the
#   drawing clicked on (Groups: bounds strings; Categories: names), page, selected (bounds /
#   name, or null),
#   break_loop, popup (state or null) }] }. The pointer is moved to (x, y) and button 1 is
#   pressed and released (eventGenerate). After each click the popup is withdrawn and
#   SelectedItem, Break_Loop reset (PERL-QUIRK: a click on an item while the popup is shown
#   blocks in waitVisibility until the popup's visibility changes; the oracle avoids it).
# - action: { list, recipe, action, item (bounds / name), groups_before / groups_after =>
#   [{ bounds, locked }] sorted by bounds, bar_lines, messages => main::message calls
#   [msg, no_break] with refs replaced by REF, popup => state after the button, updates =>
#   Tk::Seqsee::Update calls }.
# Screenshot: docs/gui/perl/list_popup_groups.png (the Groups popup, WinPhoto).
use strict;
use warnings;
no warnings qw(once redefine);
use Tk;
use Tk::WinPhoto;
use File::Spec;
use File::Temp qw(tempdir);
use Oracle;
use CanvasDump;

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}
use Themes::Std2;    # must come before any SGUI::* module
use SGUI::List;
use SGUI::List::Groups;
use SGUI::List::Categories;
use SGUI::List::Rules;
use SGUI::List::Stream;
use Tk::Seqsee;
use GuiRecipes;

my $MW = MainWindow->new;

my @MESSAGES;
my $UPDATES = 0;
*main::message = sub { push @MESSAGES, [ _noref( $_[0] ), $_[1] ? 1 : 0 ]; };
*Tk::Seqsee::Update = sub { $UPDATES++ };

sub _noref {
  my ($s) = @_;
  return $s unless defined $s;
  $s =~ s/=(HASH|SCALAR|ARRAY|CODE)\(0x[0-9a-f]+\)/=REF/g;
  utf8::encode($s);    # emit() prints characters as bytes; '«' etc. must come out as UTF-8
  return $s;
}

# Runs code with STDOUT silenced (ReDrawIt, the page bindings and ProcessClickOnItem print).
sub quiet {
  my ($code) = @_;
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  my @r = eval { $code->() };
  my $err = $@;
  open( STDOUT, '>&', $saved ) or die;
  die $err if $err;
  return @r;
}

sub label_of {
  my ( $list, $item ) = @_;
  return undef unless defined $item;
  return $list->isa('SGUI::List::Categories') ? $item->get_name : $item->get_bounds_string;
}

sub groups_state {
  return [ sort { $a->{bounds} cmp $b->{bounds} }
      map { { bounds => $_->get_bounds_string, locked => $_->get_is_locked_against_deletion ? 1 : 0 } }
      SWorkspace::GetGroups() ];
}

# ---- popups -------------------------------------------------------------------------------
for my $cls (qw(SGUI::List::Groups SGUI::List::Categories SGUI::List::Rules SGUI::List::Stream))
{
  my $c = $MW->Canvas( -width => 400, -height => 200 )->pack;
  my $l = $cls->new;
  $l->Setup( $c, 0, 0, 400, 200 );
  my $p = $l->CreatePopupWidget;
  $MW->update;
  record(
    kind    => 'popup',
    list    => $cls,
    title   => $p->title,
    buttons => [ map { $_->cget('-text') } $p->packSlaves ],
    state   => $p->state,
  );
  if ( $cls eq 'SGUI::List::Groups' ) {
    $p->deiconify;
    $p->raise;
    $MW->update;
    photo( $p, 'list_popup_groups' );
  }
  $p->destroy;
  $c->destroy;
}

sub photo {
  my ( $top, $name ) = @_;
  $MW->update;
  my $ppm = File::Spec->catfile( tempdir( CLEANUP => 1 ), 'w.ppm' );
  my $photo = $MW->Photo( -format => 'window', -data => oct( $top->id ) );
  $photo->write( $ppm, -format => 'ppm' );
  my $png = perl_screen_path($name);
  system( 'sh', '-c', 'pnmtopng "$1" > "$2" 2>/dev/null', 'sh', $ppm, $png ) == 0
    or die "pnmtopng failed\n";
}

# ---- clicks -------------------------------------------------------------------------------
# GetItemList's order is hash order and can change from one call to the next (the Categories
# list builds a new hash each time), so each click records the order of the drawing on the
# canvas when it happens.
our @LAST_ITEMS;
for my $cls (qw(SGUI::List::Groups SGUI::List::Categories)) {
  no strict 'refs';
  my $orig = \&{"${cls}::GetItemList"};
  *{"${cls}::GetItemList"} = sub { my @r = $orig->(@_); @LAST_ITEMS = @r; return @r };
}

sub click_case {
  my ( $cls, $recipe, $rect, $points ) = @_;
  build_recipe($recipe);
  my ( $x, $y, $w, $h ) = @$rect;
  my $c = $MW->Canvas( -width => $x + $w, -height => $y + $h, -highlightthickness => 0,
    -borderwidth => 0 )->pack;
  my $l = $cls->new;
  $l->Setup( $c, $x, $y, $w, $h );
  quiet( sub { $l->DrawIt } );
  $MW->update;
  my @clicks;
  $Global::Break_Loop = 0;
  for my $pt (@$points) {
    my ( $px, $py ) = @$pt;
    my $shown = [ map { label_of( $l, $_ ) } @LAST_ITEMS ];
    quiet(
      sub {
        $c->eventGenerate( '<Motion>',          -x => $px, -y => $py );
        $c->eventGenerate( '<ButtonPress-1>',   -x => $px, -y => $py );
        $c->eventGenerate( '<ButtonRelease-1>', -x => $px, -y => $py );
        $MW->update;
      }
    );
    my $popup = $l->{POPUP_WIDGET};
    push @clicks, {
      x          => 0 + $px,
      y          => 0 + $py,
      entries    => $shown,
      page       => $l->{PageNumber},
      selected   => label_of( $l, $l->{SelectedItem} ),
      break_loop => $Global::Break_Loop ? 1 : 0,
      popup      => $popup ? $popup->state : undef,
    };
    $popup->withdraw if $popup;
    $MW->update;
    delete $l->{SelectedItem};
    $Global::Break_Loop = 0;
  }
  record(
    kind    => 'clicks',
    list    => $cls,
    recipe  => $recipe,
    rect    => [@$rect],
    clicks  => \@clicks,
  );
  $l->{POPUP_WIDGET}->destroy if $l->{POPUP_WIDGET};
  $c->destroy;
}

# Rows are 15 (Groups) / 40 (Categories) px high from y + 20; the squares are at
# (x + w - 20, y + 20) and (x + w - 20, y + h - 20), 10 px wide.
click_case( 'SGUI::List::Groups', 'groups_list', [ 0, 0, 780, 450 ],
  [ [ 100, 27 ], [ 30, 42 ], [ 200, 57 ], [ 700, 72 ], [ 15, 22 ], [ 100, 87 ], [ 100, 435 ],
    [ 765, 25 ], [ 765, 435 ], [ 5, 5 ], [ 761, 21 ] ] );
click_case( 'SGUI::List::Groups', 'groups_many', [ 0, 0, 400, 200 ],
  [ [ 100, 27 ], [ 385, 185 ], [ 100, 27 ], [ 100, 162 ], [ 385, 185 ], [ 100, 42 ],
    [ 385, 185 ], [ 385, 185 ], [ 100, 57 ], [ 385, 25 ], [ 100, 57 ], [ 385, 25 ],
    [ 385, 25 ], [ 385, 25 ], [ 100, 57 ] ] );
click_case( 'SGUI::List::Groups', 'groups_many', [ 50, 30, 300, 150 ],
  [ [ 150, 57 ], [ 335, 165 ], [ 150, 57 ], [ 335, 50 ], [ 150, 72 ] ] );
click_case( 'SGUI::List::Categories', 'categories_many', [ 0, 0, 780, 450 ],
  [ [ 150, 40 ], [ 400, 80 ], [ 500, 380 ], [ 765, 435 ], [ 150, 40 ], [ 150, 140 ],
    [ 150, 300 ], [ 765, 25 ], [ 150, 140 ] ] );
click_case( 'SGUI::List::Groups', 'empty', [ 0, 0, 400, 200 ],
  [ [ 100, 27 ], [ 385, 185 ], [ 385, 25 ] ] );

# ---- actions ------------------------------------------------------------------------------
sub action_case {
  my ( $cls, $recipe, $action, $pick ) = @_;
  build_recipe($recipe);
  my $c = $MW->Canvas( -width => 400, -height => 200 )->pack;
  my $l = $cls->new;
  $l->Setup( $c, 0, 0, 400, 200 );
  my ($item) = grep { label_of( $l, $_ ) eq $pick } $l->GetItemList;
  die "no item $pick\n" unless $item;
  my $p = $l->CreatePopupWidget;
  my ($button) = grep { $_->cget('-text') eq $action } $p->packSlaves;
  @MESSAGES = ();
  $UPDATES = 0;
  my $before = groups_state();
  $l->{SelectedItem} = $item;
  $p->deiconify;
  $MW->update;
  my $died;
  srand(20);    # ActionFringe's get_actions draws random numbers
  eval { quiet( sub { $button->invoke } ); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  $MW->update;
  record(
    kind          => 'action',
    list          => $cls,
    recipe        => $recipe,
    action        => $action,
    item          => $pick,
    groups_before => $before,
    groups_after  => groups_state(),
    bar_lines     => [ SWorkspace->GetBarLines() ],
    messages      => [@MESSAGES],
    popup         => $p->state,
    updates       => $UPDATES,
    died          => _noref($died),
  );
  $p->destroy;
  $c->destroy;
}

for my $action (qw(Delete Lock Unlock ShowFollowers History Fringe ActionFringe)) {
  action_case( 'SGUI::List::Groups', 'groups_list', $action, $_ ) for ' <0, 2> ', ' <0, 5> ';
}
action_case( 'SGUI::List::Groups', 'groups_list', 'Delete',  ' <3, 5> ' );
action_case( 'SGUI::List::Groups', 'groups_list', 'Unlock',  ' <6, 8> ' );
action_case( 'SGUI::List::Groups', 'groups_list', 'Lock',    ' <6, 8> ' );
for my $name (qw(ascending descending sameness Prime number)) {
  action_case( 'SGUI::List::Categories', 'categories_many', $_, $name )
    for qw(DeleteAllOther AddBarlinesBefore);
}
action_case( 'SGUI::List::Categories', 'groups_list', $_, 'ascending' )
  for qw(DeleteAllOther AddBarlinesBefore);

emit();
