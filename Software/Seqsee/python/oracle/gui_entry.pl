# Entry point golden (loop0002 item 021): what Seqsee.pl does with its command line before
# Tk's MainLoop.
# - launch: the real Seqsee.pl run with @ARGV = (--seq "1 1 2 1 2 3" --seed 7 --max_steps 40
#   --update_interval 5 --view 0), Tk::MainLoop replaced (before Seqsee.pl's `use Tk` imports
#   it) by a sub that records $Global::Options_ref, the workspace's elements, whether ask_seq
#   opened its window, and saves the whole window as docs/gui/perl/entry_window.png
#   (Tk::WinPhoto → PPM → pnmtopng); then the same without --seq (ask_seq's window).
# - options: Seqsee::_read_config(Seqsee::_read_commandline()) for several argv: seq,
#   max_steps, update_interval, view, gui_config, the seed when given, the features turned on
#   and the non-options left in @ARGV.
# - setup: SGUI::setup({gui_config => NAME}) for each config: died or not, and the first
#   line of the error.
use strict;
use warnings;
no warnings qw(once redefine);
use File::Spec;
use File::Temp qw(tempdir);
use Oracle;

my @launch;
my $png_name;

# Seqsee.pl's `use Tk` aliases main::MainLoop to Tk::MainLoop's code at import time.
require Tk;
*Tk::MainLoop = sub {
  my $mw = $SGUI::MW;
  $mw->update;
  my %o = %{$Global::Options_ref};
  my $seq_window = grep { $_->isa('Tk::Toplevel') && $_->title eq 'Seqsee Sequence Entry' }
    $mw->children;
  push @launch, {
    argv     => [@main::LaunchArgv],
    options  => { map { $_ => $o{$_} } qw(seq max_steps update_interval view gui_config) },
    seed     => ( ( grep { $_ eq '--seed' } @main::LaunchArgv ) ? $o{seed} + 0 : 'random' ),
    elements => [ map { $_->get_mag() + 0 } SWorkspace->GetElements() ],
    ask_seq  => ( $seq_window ? 1 : 0 ),
    steps    => ( $Global::Steps_Finished || 0 ) + 0,
  };
  if ($png_name) {
    require Tk::WinPhoto;
    my $ppm = File::Spec->catfile( tempdir( CLEANUP => 1 ), 'w.ppm' );
    my $photo = $mw->Photo( -format => 'window', -data => oct( $mw->id ) );
    $photo->write( $ppm, -format => 'ppm' );
    require CanvasDump;
    my $png = CanvasDump::perl_screen_path($png_name);
    system( 'sh', '-c', 'pnmtopng "$1" > "$2"', 'sh', $ppm, $png ) == 0
      or die "pnmtopng failed\n";
  }
  $_->destroy for grep { $_->isa('Tk::Toplevel') } $mw->children;
  $mw->withdraw;
};

sub quietly(&) {
  my ($code) = @_;
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  my @r = eval { $code->() };
  my $err = $@;
  open( STDOUT, '>&', $saved ) or die;
  die $err if $err;
  return @r;
}

our @LaunchArgv = ( '--seq', '1 1 2 1 2 3', '--seed', '7', '--max_steps', '40',
  '--update_interval', '5', '--view', '0' );
$png_name = 'entry_window';
{
  local @ARGV = @LaunchArgv;
  quietly { do './Seqsee.pl'; die $@ if $@; };
}

record(
  name   => 'launch',
  %{ $launch[0] },
);

# The same without --seq: INITIALIZE opens SGUI::ask_seq. (A fresh run of the file's code,
# with Seqsee.pl's subs already compiled.)
@LaunchArgv = ( '--seed', '7' );
$png_name   = undef;
{
  local @ARGV = @LaunchArgv;
  quietly {
    %Global::Feature = ();
    my $o = $Global::Options_ref = Seqsee::_read_config( Seqsee::_read_commandline() );
    no strict 'refs';
    # Seqsee.pl's file-level `my $OPTIONS_ref` is closed over by INITIALIZE; reach it.
    require PadWalker;
    ${ PadWalker::closed_over( \&main::INITIALIZE )->{'$OPTIONS_ref'} } = $o;
    main::INITIALIZE();
    Tk::MainLoop();
  };
}
record(
  name => 'launch_no_seq',
  %{ $launch[1] },
);

# ---- option parsing ---------------------------------------------------------------------
my @argvs = (
  [],
  [ '--seq', '1 1 2 1 2 3' ],
  [ '--seq', '1, 2, 3', '--seed', '7', '--max_steps', '50', '--update_interval', '5',
    '--view', '1', '--gui_config', 'GUI_sparse' ],
  [ '-n', '30', '--seq', '4 5' ],
  [ '--max_steps', '20', '-n', '30' ],
  [ '--gui', 'GUI_sparse', '--seq', '2 3' ],
  [ '--gui=GUI_sparse' ],
  [ '--gui_config', 'GUI_ws3', '--gui', 'GUI_sparse' ],
  [ '-f', 'debugMAX', '-f', 'Primes', '--seq', '7 11' ],
  [ '--seq', '1 2', 'extra', 'words' ],
  [ '--view', '3', '--seed=9' ],
);
for my $argv (@argvs) {
  local @ARGV = @$argv;
  %Global::Feature = ();
  my %o = quietly { %{ Seqsee::_read_config( Seqsee::_read_commandline() ) } };
  record(
    name     => 'options',
    argv     => $argv,
    options  => { map { $_ => $o{$_} } qw(seq max_steps update_interval view gui_config) },
    seed     => ( ( grep { /^--?seed/ } @$argv ) ? $o{seed} + 0 : 'random' ),
    features => [ sort keys %Global::Feature ],
    rest     => [@ARGV],
  );
}
%Global::Feature = ();

# ---- SGUI::setup per gui_config ----------------------------------------------------------
for my $name (qw(GUI_ws3 nosuch GUI_sparse)) {
  my $ok = eval { quietly { SGUI::setup( { gui_config => $name } ) }; 1 };
  my $err = $ok ? '' : ( split /\n/, "$@" )[0];
  $err =~ s/ at \S+ line \d+\.?$//;
  record(
    name       => 'setup',
    gui_config => $name,
    died       => ( $ok ? 0 : 1 ),
    error      => $err,
  );
}

emit();
