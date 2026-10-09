# Controls golden (loop0002 item 016): the window lib/SGUI.pm's setup builds from
# config/GUI_sparse.conf (CreateWidgets, SetupButtons, SetupBindings), as Seqsee.pl's
# init_display does. Dumps what the Qt toolbar, key bindings and codelet count mirror:
# - the frames (name, class, pack side) and the geometry;
# - the button_frame's packed slaves in order: the buttons (text, side) and the
#   Tk::SCodeletCount label (side, font, text before and after SGUI::Update);
# - each button's and each key binding's effect: the Seqsee.pl Interaction_* subs, ask_seq and
#   exit are replaced by recorders, so invoking a callback records the call (and its argument)
#   instead of running the model; Break_Loop and debugMAX are read before and after.
# A screenshot of the button frame (Start / Pause / Quit and the count at 17) goes to
# docs/gui/perl/controls_buttons.png (Tk::WinPhoto → PPM → pnmtopng).
use strict;
use warnings;
no warnings 'once';

our @CALLS;

use Tk;
use Tk::WinPhoto;
use Oracle;
use CanvasDump;
use Config::Std;
use File::Temp qw(tempdir);
use File::Spec;

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}
use Themes::Std2;    # must come before any SGUI::* module
use SGUI;

# Seqsee.pl's Interaction_* subs (package main) and SGUI::ask_seq, recorded.
sub Interaction_continue { push @CALLS, ['Interaction_continue'] }
sub Interaction_step     { push @CALLS, ['Interaction_step'] }
sub Interaction_crawl    { push @CALLS, [ 'Interaction_crawl', $_[0] ] }
sub Interaction_step_n   { push @CALLS, [ 'Interaction_step_n', { %{ $_[0] } } ] }
{
  no warnings 'redefine';
  no warnings 'prototype';
  *SGUI::ask_seq = sub { push @CALLS, ['SGUI::ask_seq'] };
  # Tk.pm aliases CORE::GLOBAL::exit to Tk::exit; the bindings are compiled later (eval).
  *Tk::exit           = sub (;$) { push @CALLS, ['Tk::exit'] };
  *CORE::GLOBAL::exit = sub (;$) { push @CALLS, ['exit'] };
}

my $conf = 'config/GUI_sparse.conf';
read_config $conf => my %config;
{
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  SGUI::CreateWidgets( \%config );
  SGUI::SetupButtons( \%config );
  SGUI::SetupBindings( \%config );
  open( STDOUT, '>&', $saved ) or die;
}
my $MW = $SGUI::MW;
$MW->update;

sub globals {
  return {
    Break_Loop     => $Global::Break_Loop,
    debugMAX       => $Global::debugMAX,
    InterstepSleep => $Global::InterstepSleep,
  };
}

# Run a callback with the recorders; returns calls, printed text and the globals around it.
sub effect {
  my ($code) = @_;
  @CALLS = ();
  my $before = globals();
  my $out    = '';
  open( my $saved, '>&', \*STDOUT ) or die;
  close STDOUT;
  open( STDOUT, '>', \$out ) or die;
  $code->();
  close STDOUT;
  open( STDOUT, '>&', $saved ) or die;
  return { calls => [@CALLS], printed => $out, before => $before, after => globals() };
}

sub reset_globals {
  $Global::Break_Loop     = 0;
  $Global::debugMAX       = undef;
  $Global::InterstepSleep = 0;
}

my $button_frame = do { no strict 'refs'; ${'SGUI::button_frame'} };
my $count        = do { no strict 'refs'; ${'SGUI::Count'} };

my @frames;
for my $name (qw(button_frame Workspace Commentary Count)) {
  my $wd = do { no strict 'refs'; ${"SGUI::$name"} };
  my %pack = $wd->packInfo;
  push @frames, { name => $name, class => ref($wd), side => $pack{'-side'} };
}

my $count_text_initial = $count->cget('-text');
my @slaves;
for my $slave ( $button_frame->packSlaves ) {
  my %pack = $slave->packInfo;
  my %entry = ( class => ref($slave), side => $pack{'-side'}, text => $slave->cget('-text') );
  if ( $slave->isa('Tk::Button') ) {
    reset_globals();
    $entry{effect} = effect( sub { $slave->invoke } );
  }
  push @slaves, \%entry;
}

my @bindings;
# Tk lists <KeyPress-c> as "c"; a lone "c" would be read as a tag, so ask for "<KeyPress-c>".
# (The MainWindow's own <Button> binding isn't from the config.)
for my $seq ( sort grep { !/^</ } $MW->bind ) {
  my $cb = $MW->bind("<KeyPress-$seq>");
  reset_globals();
  my @effects = map { effect( sub { $cb->Call } ) } 1 .. 2;    # twice: m toggles
  push @bindings, { sequence => $seq, effects => \@effects };
}

my %count_text;
for my $steps ( undef, 0, 17 ) {
  $Global::Steps_Finished = $steps;
  SGUI::Update();
  $count_text{ defined $steps ? $steps : 'undef' } = $count->cget('-text');
}

record(
  name     => 'controls',
  config   => $conf,
  geometry => $config{frames}{geometry},
  frames   => \@frames,
  buttons  => \@slaves,
  bindings => \@bindings,
  count    => {
    font         => ${ $count->cget('-font') },
    font_actual  => { $count->fontActual( $count->cget('-font') ) },
    text_initial => $count_text_initial,
    text         => \%count_text,
  },
  scale_config => $config{Scale},
);

# Screenshot of the button frame with the count at 17.
$Global::Steps_Finished = 17;
SGUI::Update();
$MW->update;
my $ppm = File::Spec->catfile( tempdir( CLEANUP => 1 ), 'b.ppm' );
my $photo = $MW->Photo( -format => 'window', -data => oct( $button_frame->id ) );
$photo->write( $ppm, -format => 'ppm' );
my $png = perl_screen_path('controls_buttons');
system( 'sh', '-c', 'pnmtopng "$1" > "$2" 2>/dev/null', 'sh', $ppm, $png ) == 0
  or die "pnmtopng failed\n";

emit();
