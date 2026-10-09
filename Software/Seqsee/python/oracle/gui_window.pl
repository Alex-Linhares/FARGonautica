# Main window golden (loop0002 item 015): lib/Tk/Seqsee.pm's Populate, as lib/SGUI.pm's
# CreateWidgets builds it with config/GUI_sparse.conf ([Seqsee]: -height 450, -width 780,
# background white). Dumps the window shell the Qt main window mirrors: the Menustrip's
# menus (label, side, entries and separators in order), the canvas's size, background,
# border and highlight, the frame's background, the View menu's effect (each entry invoked
# in turn; @Parts dumped as part names) and what the Help entries do (MenuEntry without an
# action: they print "[caption]"). A screenshot of the whole widget (menu strip + canvas,
# groups_relations in view 0) goes to docs/gui/perl/window_groups_relations.png
# (Tk::WinPhoto → PPM → pnmtopng).
use strict;
use warnings;
no warnings 'once';
use Tk;
use Tk::WinPhoto;
use Oracle;
use CanvasDump;
use PadWalker qw(closed_over);
use File::Temp qw(tempdir);
use File::Spec;

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
my $w  = $mw->Seqsee( -height => 450, -width => 780, background => 'white' );
$w->pack( -side => 'top' );
$mw->update;

my $vars   = closed_over( \&Tk::Seqsee::SetupParts );
my $Canvas = ${ $vars->{'$Canvas'} };
my $Parts  = $vars->{'@Parts'};

sub part_names { return [ map { ref( $_->[0] ) || $_->[0] } @$Parts ]; }

# The Menustrip (Tk::Menustrip, not Tk::Menu): each MenuLabel is a Frame holding a 'Label'
# Button and a 'Popup' Toplevel; the popup's packed slaves are the entries (Buttons) and
# the separators (Frames). An entry's -command hides the popup and runs the action at idle
# time; MenuEntry without an action uses printf("[%s]\n", caption).
my ($strip) = grep { $_->isa('Tk::Menustrip') } $w->children;
my ( @menus, %entry_buttons );
for my $label ( @{ $strip->{m_MenuList} } ) {
  my $frame = $label->parent;
  my $popup = $frame->Subwidget('Popup');
  my $menu  = $label->cget('-text');
  my @entries;
  for my $slave ( $popup->packSlaves ) {
    if ( $slave->isa('Tk::Button') ) {
      my $text = $slave->cget('-text');
      push @entries, { type => 'command', label => $text };
      $entry_buttons{$menu}{$text} = $slave;
    }
    else {
      push @entries, { type => 'separator' };
    }
  }
  my %pack = $frame->packInfo;
  push @menus, { label => $menu, side => $pack{'-side'}, entries => \@entries };
}

# Invoke an entry as a click does (the action runs at idle time); returns what it printed.
sub invoke_entry {
  my ( $menu, $entry ) = @_;
  my $out = '';
  open( my $saved, '>&', \*STDOUT ) or die;
  close STDOUT;
  open( STDOUT, '>', \$out ) or die;
  $entry_buttons{$menu}{$entry}->invoke;
  $mw->update;
  close STDOUT;
  open( STDOUT, '>&', $saved ) or die;
  return $out;
}

# The View menu's entries, invoked in turn; then the Help entries (no action given).
my @views = ( { label => undef, parts => part_names() } );
my ($view_menu) = grep { $_->{label} eq 'View' } @menus;
for my $entry ( @{ $view_menu->{entries} } ) {
  invoke_entry( 'View', $entry->{label} );
  push @views, { label => $entry->{label}, parts => part_names() };
}
my %help_prints = map { $_ => invoke_entry( 'Help', $_ ) } sort keys %{ $entry_buttons{Help} };

record(
  name   => 'window',
  menus  => \@menus,
  views  => \@views,
  canvas => {
    width       => $Canvas->cget('-width') + 0,
    height      => $Canvas->cget('-height') + 0,
    background  => normalise_colour( $Canvas, $Canvas->cget('-background') ),
    highlight   => $Canvas->cget('-highlightthickness') + 0,
    borderwidth => $Canvas->cget('-borderwidth') + 0,
  },
  frame_background => normalise_colour( $w, $w->cget('-background') ),
  help_prints      => \%help_prints,
);

# Screenshot of the widget showing groups_relations in view 0.
build_recipe('groups_relations');
invoke_entry( 'View', $views[1]{label} );
$mw->update;
my $ppm = File::Spec->catfile( tempdir( CLEANUP => 1 ), 'w.ppm' );
my $photo = $mw->Photo( -format => 'window', -data => oct( $w->id ) );
$photo->write( $ppm, -format => 'ppm' );
my $png = perl_screen_path('window_groups_relations');
system( 'sh', '-c', 'pnmtopng "$1" > "$2"', 'sh', $ppm, $png ) == 0
  or die "pnmtopng failed\n";

emit();
