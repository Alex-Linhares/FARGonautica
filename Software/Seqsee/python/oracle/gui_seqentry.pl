# Sequence entry golden (loop0002 item 018): lib/SGUI.pm's ask_seq (the "Seqsee Sequence
# Entry" Toplevel with its Tk::ComboEntry filled from config/sequence.list, and the
# $check_and_accept_input_sequence closure behind -invoke, the composite's <Return> and Go)
# and ask_for_more_terms (the "Request for more terms" Toplevel and its Entry's <Return>).
#
# The model calls those callbacks make (SWorkspace->clear, SCoderack->clear,
# $Global::MainStream->clear, SWorkspace->insert_elements, SGUI::Update) are replaced by
# recorders, so a session records each call with its arguments instead of running the model.
# $SGUI::Commentary is a recorder too (ask_seq logs 'New Sequence Started: ').
#
# Dumps: the widgets (title, packed slaves in order with class/side/text, the combo's width,
# list as its listbox shows it, initial texts, bindtags), and scripted sessions: each step
# types a text (or selects a list row) and presses Return in the entry, clicks Go, or picks
# the row; after each step: the message label, whether the Toplevel still exists, the calls
# and what was printed.
# Screenshots: docs/gui/perl/seqentry.png (after an illformed input) and
# docs/gui/perl/more_terms.png (Tk::WinPhoto → PPM → pnmtopng).
use strict;
use warnings;
no warnings 'once';

our @CALLS;

use Tk;
use Tk::WinPhoto;
use Oracle;
use CanvasDump;
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
require Tk::ComboEntry;

package Recorder;
sub new { bless {}, shift }
sub MessageRequiringNoResponse {
  my ( $self, @args ) = @_;
  push @main::CALLS, [ 'Commentary::MessageRequiringNoResponse', @args ];
}
sub clear { push @main::CALLS, ['MainStream::clear'] }

package main;

{
  no warnings 'redefine';
  *SWorkspace::clear           = sub { push @CALLS, ['SWorkspace::clear'] };
  *SCoderack::clear            = sub { push @CALLS, ['SCoderack::clear'] };
  *SWorkspace::insert_elements = sub { shift; push @CALLS, [ 'SWorkspace::insert_elements', @_ ] };
  *SGUI::Update                = sub { push @CALLS, ['SGUI::Update'] };
}

my $MW = MainWindow->new;
$SGUI::MW          = $MW;
$SGUI::Commentary  = Recorder->new;
$Global::MainStream = Recorder->new;
$MW->update;

# Run code with the recorders; returns calls and printed text.
sub effect {
  my ($code) = @_;
  @CALLS = ();
  my $out = '';
  open( my $saved, '>&', \*STDOUT ) or die;
  close STDOUT;
  open( STDOUT, '>', \$out ) or die;
  {
    local $SIG{__WARN__} = sub { };    # "uninitialized value $seq"
    $code->();
    $MW->update;                       # runs DoInvokeCallback's afterIdle
  }
  close STDOUT;
  open( STDOUT, '>&', $saved ) or die;
  return { calls => [@CALLS], printed => $out };
}

sub toplevels { grep { $_->isa('Tk::Toplevel') && Tk::Exists($_) } $MW->children }

sub slaves {
  my ($top) = @_;
  my @ret;
  for my $w ( $top->packSlaves ) {
    my %pack = $w->packInfo;
    my $text = eval { $w->cget('-text') };
    push @ret, { class => ref($w), side => $pack{'-side'}, text => $text };
  }
  return \@ret;
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

# ---- ask_seq ------------------------------------------------------------------------------
sub open_ask_seq {
  my $eff = effect( sub { SGUI::ask_seq() } );
  my ($top) = toplevels();
  my ($ce)  = grep { $_->isa('Tk::ComboEntry') } $top->children;
  my ($label) = grep { $_->isa('Tk::Label') && $_->cget('-text') eq '' } $top->children;
  my ($go) = grep { $_->isa('Tk::Button') } $top->children;
  return ( $eff, $top, $ce, $label, $go );
}

{
  my ( $eff, $top, $ce, $label, $go ) = open_ask_seq();
  my $lb = $ce->{m_ListBox};
  my $en = $ce->Subwidget('Entry');
  record(
    name          => 'ask_seq_widget',
    sequence_list => [@SGUI::seq],
    opened        => $eff,
    title         => $top->title,
    slaves        => slaves($top),
    combo         => {
      width        => $ce->cget('-width'),
      list         => [ $lb->get( 0, 'end' ) ],
      showmenu     => $ce->cget('-showmenu'),
      entry_text   => $en->get,
      bindtags     => [ $ce->bindtags ],
      entry_bindtags => [ $en->bindtags ],
    },
    label_text => $label->cget('-text'),
  );
  $top->destroy;
}

# A session: steps [how, text] with how = 'return' (type text, Return in the entry),
# 'go' (type text, click Go) or 'select' (pick list row number `text`).
my @SESSIONS = (
  [ 'illformed_then_empty_then_ok',
    [ [ 'return', '1 x 2' ], [ 'go', '' ], [ 'return', '   ' ], [ 'go', ' 1, 2  3 ' ] ] ],
  [ 'select_first_row',   [ [ 'select', 0 ] ] ],
  [ 'select_last_row',    [ [ 'select', -1 ] ] ],
  [ 'negatives',          [ [ 'return', '-1 2,-3' ] ] ],
  [ 'dash_inside',        [ [ 'go', '1-2 3' ] ] ],
  [ 'leading_comma',      [ [ 'return', ',1 2' ] ] ],
  [ 'tabs_and_commas',    [ [ 'return', "1\t2,,3" ] ] ],
  [ 'decimal_is_illformed', [ [ 'go', '1.5 2' ], [ 'go', '1 5 2' ] ] ],
  [ 'letters_only',       [ [ 'return', 'abc' ] ] ],
  [ 'dash_only',          [ [ 'go', '-' ], [ 'go', ' , ' ] ] ],
);

my $shot_done = 0;
for my $session (@SESSIONS) {
  my ( $name, $steps ) = @$session;
  my ( $eff, $top, $ce, $label, $go ) = open_ask_seq();
  my $en = $ce->Subwidget('Entry');
  my $lb = $ce->{m_ListBox};
  my @results;
  for my $step (@$steps) {
    my ( $how, $text ) = @$step;
    my $r;
    if ( $how eq 'select' ) {
      my $row = $text < 0 ? $lb->size + $text : $text;
      $r = effect(
        sub {
          $lb->selectionClear( 0, 'end' );
          $lb->selectionSet($row);
          $ce->Select;
        }
      );
      $r->{entry_text} = Tk::Exists($en) ? $en->get : undef;
    }
    else {
      $en->delete( 0, 'end' );
      $en->insert( 'end', $text );
      if ( $how eq 'return' ) {
        my $cb = $en->bind('<Return>');
        $r = effect( sub { $cb->Call } );
      }
      else {
        $r = effect( sub { $go->invoke } );
      }
    }
    my $exists = Tk::Exists($top) ? 1 : 0;
    $r->{how}    = $how;
    $r->{text}   = $text;
    $r->{exists} = $exists;
    $r->{label}  = $exists ? $label->cget('-text') : undef;
    push @results, $r;
    if ( !$shot_done && $exists && $r->{label} ne '' ) {
      photo( $top, 'seqentry' );
      $shot_done = 1;
    }
    last unless $exists;
  }
  record( name => 'ask_seq_session', session => $name, steps => \@results );
  $top->destroy if Tk::Exists($top);
}

# ---- ask_for_more_terms ---------------------------------------------------------------------
{
  my $top = SGUI::ask_for_more_terms();
  my ($en) = grep { $_->isa('Tk::Entry') } $top->children;
  record(
    name       => 'more_terms_widget',
    title      => $top->title,
    slaves     => slaves($top),
    entry_text => $en->get,
    entry_width => $en->cget('-width'),
  );
  $en->insert( 'end', '7 8' );
  photo( $top, 'more_terms' );
  $top->destroy;
}

for my $text ( ' 7 8 ', '7,8', '', '   ', ',7', 'a b', "9\t10 ,11" ) {
  my $top = SGUI::ask_for_more_terms();
  my ($en) = grep { $_->isa('Tk::Entry') } $top->children;
  $en->insert( 'end', $text );
  my $cb = $en->bind('<Return>');
  my $r = effect( sub { $cb->Call } );
  $r->{text}   = $text;
  $r->{exists} = Tk::Exists($top) ? 1 : 0;
  record( name => 'more_terms_session', %$r );
  $top->destroy if Tk::Exists($top);
}

emit();
