# Composite views golden (loop0002 item 013): lib/Tk/Seqsee.pm. The real Tk::Seqsee widget
# (Populate: the Menustrip and the canvas) is built at each canvas size; each case puts one of
# the 11 @ViewOptions in @Parts, as the View menu's entries do (@Parts = @{ $vo->[1] };
# SetupParts(); Update()), with or without AttentionNeeded, and dumps the whole canvas after
# Update (delete('all'), each part's DrawIt in order, then DrawAttentionDirectingArrows when
# attention is needed). A die inside a part's DrawIt is not caught by Update, so it leaves
# Update: the parts after it and the arrows are not drawn (the case records the message).
# @Parts and $Canvas are file lexicals of Tk/Seqsee.pm; PadWalker reaches them through
# SetupParts' closure, so the menu callback's three statements run as written.
# The four list viewers are shared objects (two views show the Groups list), but each list's
# Setup (called by SetupParts) sets its PageNumber back to 0, so a page only lasts until the
# next view change. Each case sets every list's PageNumber after SetupParts (0 unless the
# case gives a page); 'before_setup' cases set it before SetupParts instead.
# Each case: { recipe, view, title, size => [w, h], attention, pages => {list class =>
# page}, before_setup, items, died, metrics => [[font, text, width, linespace], ...] (for the Workspace's
# metonym bboxes), lists => {class => {page_after, shown_from, shown_to, entries_count}}
# (for the lists in the view, after Update), groups => [{tag, bounds, structure,
# categories}] (GetGroups' order: hash order for equal spans), relations => [{ends, type,
# class}] (values %SWorkspace::relations order), history => [[family, count], ...]
# (%HistoryOfRunnable after Update, in hash order: the Coderack's row order), categories =>
# [{tag, name}] (the Categories list's GetItemList order, when it ran) }.
# Tags: elements 'obj<index>'; the K-th group of GetGroups 'group<K>'; the K-th category of
# GetItemList 'cat<K>'; the current thought 'tht0' and OlderThoughts' i-th 'tht<i+1>'; a list
# object its class name ('SGUI::List::Groups', '…-Clickable-Item', '…-pageup', …).
# Every address in a text is replaced by ADDR.
use strict;
use warnings;
no warnings 'once';
use Tk;
use Oracle;
use CanvasDump;
use PadWalker qw(closed_over);

# Test::Seqsee runs INITIALIZE_for_testing at load, which prints "View: 1!".
BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}
use Themes::Std2;    # must come before any SGUI::* module
use Tk::Seqsee;
use GuiRecipes;

my $mw = MainWindow->new;

my %PNG = (
  'attention_groups:0'  => 'views_0_attention_groups',
  'groups_relations:0'  => 'views_0_groups_relations',
  'attention_groups:2'  => 'views_2_attention_groups',
  'groups_relations:2'  => 'views_2_groups_relations',
  'coderack_small:5'    => 'views_5_coderack_small',
  'stream_small:9'      => 'views_9_stream_small',
  'categories_many:4'   => 'views_4_categories_many',
  'stream_list_many:10' => 'views_10_stream_list_many',
  'groups_relations:6'  => 'views_6_groups_relations',
  'slipnet_small:3'     => 'views_3_slipnet_small',
  # Item 022: every view of the end state of a solved run (the Qt run's screenshots are
  # docs/gui/screens/e2e_run_<view>.png).
  map { ( "solution:$_" => "views_${_}_solution" ) } 0 .. 10,
);

my $LastCats;
{
  no warnings 'redefine';
  my $item_list = \&SGUI::List::Categories::GetItemList;
  *SGUI::List::Categories::GetItemList = sub {
    my @r = $item_list->(@_);
    $LastCats = [@r];
    return @r;
  };
}

my @LISTS;    # the shared list viewers
{
  my %seen;
  for my $vo (@Tk::Seqsee::ViewOptions) {
    for my $part ( @{ $vo->[1] } ) {
      my $p = $part->[0];
      push @LISTS, $p if ref $p && !$seen{"$p"}++;
    }
  }
}

sub no_addr {
  my ($s) = @_;
  return $s unless defined $s;
  $s =~ s/=(\w+)\(0x[0-9a-f]+\)/=$1(ADDR)/g;
  return $s;
}

sub text_metrics {
  my ($c) = @_;
  my ( %seen, @out );
  for my $id ( $c->find('all') ) {
    next unless $c->type($id) eq 'text';
    my $font = $c->itemcget( $id, '-font' );
    $font = $$font if ref $font && $font->isa('Tk::Font');    # its name, as dump_canvas
    next if $font eq 'Helvetica -12';    # the lists' and panes' default font: never measured
    my $text = $c->itemcget( $id, '-text' );
    next if $seen{"$font\n$text"}++;
    push @out,
      [ $font, no_addr($text), 0 + $c->fontMeasure( $font, $text ),
      0 + $c->fontMetrics( $font, '-linespace' ) ];
  }
  return \@out;
}

my ( $widget, $parts_ref, $canvas_ref );

sub make_widget {
  my ( $w, $h ) = @_;
  $widget->destroy if $widget;
  $widget = $mw->Seqsee( -width => $w, -height => $h )->pack;
  my $vars = closed_over( \&Tk::Seqsee::SetupParts );
  $parts_ref  = $vars->{'@Parts'}  or die "no \@Parts";
  $canvas_ref = $vars->{'$Canvas'} or die "no \$Canvas";
}

sub draw_case {
  my ( $recipe, $view, $size, %opt ) = @_;
  build_recipe($recipe);
  my $pages = $opt{pages} || {};
  # Pages turned before the view is chosen: SGUI::List::Setup sets PageNumber back to 0.
  $_->{PageNumber} = $pages->{ ref $_ } || 0 for @LISTS;
  my $title;
  if ( defined $view ) {
    my $vo = $Tk::Seqsee::ViewOptions[$view];
    $title = $vo->[0];
    # The View menu entry's callback.
    @$parts_ref = @{ $vo->[1] };
    Tk::Seqsee::SetupParts();
  }
  # Pages turned in this view (the page squares change PageNumber, then a redraw).
  unless ( $opt{before_setup} ) {
    $_->{PageNumber} = $pages->{ ref $_ } || 0 for @LISTS;
  }
  $opt{attention} ? Tk::Seqsee::AttentionNeeded() : Tk::Seqsee::AttentionNoLongerNeeded();
  $LastCats = undef;
  my $died;
  eval { Tk::Seqsee::Update(); 1 } or do {
    ( $died = "$@" ) =~ s/ at \S+ line \d+\.?\n?\z//;
  };
  my $c = $$canvas_ref;

  my %tags = %{ object_tags() };
  my @groups = SWorkspace::GetGroups();
  $tags{"$groups[$_]"} = "group$_" for 0 .. $#groups;
  my @cats = @{ $LastCats || [] };
  $tags{"$cats[$_]"} = "cat$_" for 0 .. $#cats;
  my $stream = $Global::MainStream;
  my @older  = @{ $stream->{OlderThoughts} || [] };
  for my $i ( 0 .. $#older ) {
    $tags{"$older[$i]"} = 'tht' . ( $i + 1 ) if $older[$i];
  }
  $tags{"$stream->{CurrentThought}"} = 'tht0' if $stream->{CurrentThought};
  my %lists;
  for my $l (@LISTS) {
    my $class = ref $l;
    $tags{"$l"} = $class;
    $tags{"$l-$_"} = "$class-$_" for qw(Clickable-Item pageup pagedown);
  }
  for my $part (@$parts_ref) {
    my $l = $part->[0];
    next unless ref $l;
    $lists{ ref $l } = {
      page_after    => $l->{PageNumber},
      shown_from    => $l->{EntriesShownFrom},
      shown_to      => $l->{EntriesShownTo},
      entries_count => $l->{EntriesCount},
    };
  }

  my $items = dump_canvas( $c, \%tags );
  for my $item (@$items) {
    $item->{opts}{text} = no_addr( $item->{opts}{text} ) if exists $item->{opts}{text};
  }
  record(
    recipe    => $recipe,
    view      => $view,
    title     => $title,
    size      => [@$size],
    attention => $opt{attention} ? 1 : 0,
    pages     => {%$pages},
    before_setup => $opt{before_setup} ? 1 : 0,
    items     => $items,
    died      => $died,
    metrics   => text_metrics($c),
    lists     => \%lists,
    groups    => [
      map {
        { tag        => "group$_",
          bounds     => $groups[$_]->get_bounds_string,
          structure  => $groups[$_]->get_structure_string,
          categories => [ sort map { $_->get_name } @{ $groups[$_]->get_categories } ] }
      } 0 .. $#groups
    ],
    relations => [
      map {
        my ( $a, $b ) = $_->get_ends;
        { ends  => [ $a->get_bounds_string, $b->get_bounds_string ],
          type  => $_->get_type->as_text,
          class => ref $_ }
      } values %SWorkspace::relations
    ],
    history => [ map { [ $_, 0 + $SCoderack::HistoryOfRunnable{$_} ] }
        keys %SCoderack::HistoryOfRunnable ],
    categories => [ map { { tag => "cat$_", name => $cats[$_]->get_name } } 0 .. $#cats ],
  );
  my $png = defined $view ? $PNG{"$recipe:$view"} : undef;
  if ( $png && $size->[0] == 780 && !$opt{attention} && !%$pages ) {
    save_png( $mw, $c, perl_screen_path($png) );
  }
}

my @RECIPES = qw(empty groups_relations attention_groups metonyms slipnet_small slipnet_dir
  coderack_small stream_small relations_pane groups_list categories_many stream_list_many
  solution);

make_widget( 780, 450 );
# The view Populate set up: @ViewOptions[ $Global::Options_ref->{view} || 0 ] (no view
# option in the tests, so view 0).
draw_case( 'groups_relations', undef, [ 780, 450 ] );
for my $recipe (@RECIPES) {
  draw_case( $recipe, $_, [ 780, 450 ] ) for 0 .. $#Tk::Seqsee::ViewOptions;
}
# AttentionNeeded: the arrow and the text after the parts (or nothing, if a part died).
for my $recipe (qw(empty attention_groups groups_relations)) {
  draw_case( $recipe, $_, [ 780, 450 ], attention => 1 ) for 0 .. $#Tk::Seqsee::ViewOptions;
}
# The shared lists on other pages.
draw_case( 'groups_many', $_, [ 780, 450 ], pages => { 'SGUI::List::Groups' => 1 } ) for 0, 8;
draw_case( 'groups_many', $_, [ 780, 450 ], pages => { 'SGUI::List::Groups' => 9 } ) for 0, 8;
draw_case( 'categories_many', 4, [ 780, 450 ], pages => { 'SGUI::List::Categories' => 1 } );
draw_case( 'stream_list_many', 10, [ 780, 450 ], pages => { 'SGUI::List::Stream' => 1 } );
# A page turned, then a view chosen: SetupParts resets the shared list to page 0.
draw_case( 'groups_many', $_, [ 780, 450 ], pages => { 'SGUI::List::Groups' => 1 },
  before_setup => 1 ) for 0, 8;

# Another canvas size (fractions of the canvas; Populate reads -width/-height).
make_widget( 1000, 700 );
for my $recipe (qw(groups_relations attention_groups stream_small)) {
  draw_case( $recipe, $_, [ 1000, 700 ] ) for 0 .. $#Tk::Seqsee::ViewOptions;
}
draw_case( 'attention_groups', 2, [ 1000, 700 ], attention => 1 );
make_widget( 333, 211 );
draw_case( 'groups_relations', $_, [ 333, 211 ] ) for 0 .. $#Tk::Seqsee::ViewOptions;
draw_case( 'empty', 1, [ 333, 211 ], attention => 1 );

emit();
