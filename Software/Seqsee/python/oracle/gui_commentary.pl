# Commentary golden (loop0002 item 017): lib/Tk/SCommentary.pm as lib/SGUI.pm's CreateWidgets
# builds it from config/GUI_sparse.conf, and lib/UI/Graphical.pm's message / ask_user_extension.
# Dumps:
# - the widget: the ROText's options (height, width, wrap, font: XLFD + fontActual), every
#   [SCommentary_tags] tag's configuration, the button column (texts, widths, states, sides);
# - scripted sessions: each step calls one of MessageRequiringNoResponse,
#   MessageRequiringAResponse, MessageRequiringBooleanResponse, main::message or
#   main::ask_user_extension. Questions are answered by an `after` callback that first records
#   the buttons (text, state) and the AttentionNeeded flag while the question waits, then
#   presses a button or a key 1-4 (a key past the active buttons is ignored). After each step:
#   the return value, the text as runs of [text, [tags]] (Text->dump), and the globals.
# A screenshot of the commentary after a session goes to docs/gui/perl/commentary.png.
use strict;
use warnings;
no warnings 'once';

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
BEGIN {
  no warnings 'redefine';
  require UI::Graphical;    # main::message, main::ask_user_extension
}

my $conf = 'config/GUI_sparse.conf';
read_config $conf => my %config;
{
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  SGUI::CreateWidgets( \%config );
  open( STDOUT, '>&', $saved ) or die;
}
my $MW = $SGUI::MW;
my $C  = $SGUI::Commentary;
$MW->update;

# The ROText inside the Scrolled frame, and the button frame.
my ($text) = grep { $_->isa('Tk::Text') } $C->Descendants;
my ($bframe) = grep {
  my @s = $_->packSlaves;
  ref($_) eq 'Tk::Frame' and @s and !grep { !$_->isa('Tk::Button') } @s
} $C->children;
my @buttons = $bframe->packSlaves;

sub font_info {
  my ($font) = @_;
  return { xlfd => ( ref $font ? $$font : "$font" ), actual => { $text->fontActual($font) } };
}

sub buttons_state {
  return [ map { { text => $_->cget('-text'), state => $_->cget('-state') } } @buttons ];
}

sub runs {
  my @runs;
  my %on;
  for my $item ( $text->dump( '-text', '-tag', '1.0', 'end - 1 chars' ) ) {
    push @runs, $item;
  }
  my @out;
  my @flat = @runs;
  while (@flat) {
    my ( $key, $value, $index ) = splice( @flat, 0, 3 );
    if    ( $key eq 'tagon' )  { $on{$value} = 1 }
    elsif ( $key eq 'tagoff' ) { delete $on{$value} }
    elsif ( $key eq 'text' ) {
      my @tags = sort grep { $_ ne 'sel' } keys %on;
      if ( @out and join( ',', @{ $out[-1][1] } ) eq join( ',', @tags ) ) {
        $out[-1][0] .= $value;
      }
      else {
        push @out, [ $value, \@tags ];
      }
    }
  }
  return \@out;
}

sub attention {
  no strict 'refs';
  # Tk::Seqsee's lexical $AttentionNeeded, read through a closure of AttentionNeeded.
  require PadWalker;
  my $vars = PadWalker::closed_over( \&Tk::Seqsee::AttentionNeeded );
  return ${ $vars->{'$AttentionNeeded'} };
}

# Answer the next question: record the waiting state, then act ('button', n) or ('key', n...).
my $waiting;
sub answer_with {
  my (@actions) = @_;
  $MW->after(
    20,
    sub {
      $waiting = { buttons => buttons_state(), attention => attention() };
      for my $a (@actions) {
        my ( $how, $n ) = @$a;
        if ( $how eq 'button' ) { $buttons[$n]->invoke }
        else                    { $text->bind("<KeyPress-$n>")->Call }
      }
    }
  );
}

sub globals {
  return {
    AtLeastOneUserVerification => $Global::AtLeastOneUserVerification,
    debugMAX                   => $Global::debugMAX,
    Break_Loop                 => $Global::Break_Loop,
  };
}

sub quiet(&) {
  my ($code) = @_;
  my $out = '';
  open( my $saved, '>&', \*STDOUT ) or die;
  close STDOUT;
  open( STDOUT, '>', \$out ) or die;
  my @r = $code->();
  close STDOUT;
  open( STDOUT, '>&', $saved ) or die;
  return ( $out, @r );
}

# A session: clear the text, run the steps.
sub session {
  my ( $name, @steps ) = @_;
  $text->delete( '1.0', 'end' );
  $Global::AtLeastOneUserVerification = 0;
  $Global::debugMAX                   = 0;
  $Global::Break_Loop                 = 0;
  %Global::ExtensionRejectedByUser    = ();
  %Global::Feature                    = ();
  my @out;
  for my $step (@steps) {
    my ( $call, $args, $answer ) = @$step;
    $waiting = undef;
    answer_with(@$answer) if $answer;
    my ( $printed, $ret ) = quiet {
      if    ( $call eq 'no_response' ) { $C->MessageRequiringNoResponse(@$args) }
      elsif ( $call eq 'response' )    { $C->MessageRequiringAResponse(@$args) }
      elsif ( $call eq 'boolean' )     { $C->MessageRequiringBooleanResponse(@$args) }
      elsif ( $call eq 'message' )     { main::message(@$args) }
      elsif ( $call eq 'ask_user_extension' ) {
        my ( $items, $suffix, $setup ) = @$args;
        %Global::Feature = %{ $setup->{feature} || {} };
        $Global::ExtensionRejectedByUser{$_} = 1 for @{ $setup->{rejected} || [] };
        main::ask_user_extension( $items, $suffix );
      }
      elsif ( $call eq 'start_debug' ) { $buttons[-1]->invoke; undef }
      else                             { die "unknown $call" }
    };
    push @out, {
      call     => $call,
      args     => $args,
      answer   => $answer,
      returned => $ret,
      waiting  => $waiting,
      runs     => runs(),
      text     => $text->get( '1.0', 'end - 1 chars' ),
      globals  => globals(),
    };
  }
  record( name => $name, kind => 'session', steps => \@out );
}

# ---- the widget -------------------------------------------------------------------------
my %tags;
for my $tag ( sort keys %{ $config{SCommentary_tags} } ) {
  my %t = ( foreground => $text->tagCget( $tag, '-foreground' ) );
  my $font = $text->tagCget( $tag, '-font' );
  $t{font} = font_info($font) if $font;
  $tags{$tag} = \%t;
}
my %text_pack = $text->parent->packInfo;
record(
  name    => 'widget',
  kind    => 'widget',
  text    => {
    height => $text->cget('-height'),
    width  => $text->cget('-width'),
    wrap   => $text->cget('-wrap'),
    font   => font_info( $text->cget('-font') ),
    side   => $text_pack{'-side'},
  },
  tags    => \%tags,
  tags_config => $config{SCommentary_tags},
  # lowest priority first: where tags overlap, the later one's options win
  tag_priority => [ grep { $_ ne 'sel' } $text->tagNames ],
  buttons => [
    map {
      my %p = $_->packInfo;
      { text => $_->cget('-text'), width => $_->cget('-width'), state => $_->cget('-state'),
        side => $p{'-side'} }
    } @buttons
  ],
);

# ---- sessions -----------------------------------------------------------------------------
session(
  'no_response',
  [ 'no_response', ["Plain text\n"] ],
  [ 'no_response', [ 'New Sequence Started: ', [], "1 1 2 1 2 3\n" ] ],
  [ 'no_response', [ 'Reader', 'green', ' About to run: x', [], "\n" ] ],
  [ 'no_response', [ 'Fam', ['codelet_family'], ' codelet added', [ 'debug', 'green' ], "\n" ] ],
  [ 'no_response', [ 'tagged only', ['user_response'] ] ],
  [ 'no_response', ["\nafter"] ],
);

session(
  'response',
  [ 'response', [ [ 'Yes', 'No' ], 'Is this the right rule?' ], [ [ 'button', 1 ] ] ],
  [ 'response', [ [ 'a', 'b', 'c', 'd' ], 'Pick ', ['green'], 'one' ], [ [ 'button', 3 ] ] ],
  [ 'response', [ [ 'one', 'two' ], 'Keys: ' ], [ [ 'key', 4 ], [ 'key', 3 ], [ 'key', 2 ] ] ],
  [ 'response', [ ['continue'], "Multi\nline" ], [ [ 'key', 1 ] ] ],
);

session(
  'boolean',
  [ 'boolean', ['Is the next term 3?'], [ [ 'button', 0 ] ] ],
  [ 'boolean', ['Are the next terms: 4 5?'], [ [ 'button', 1 ] ] ],
  [ 'boolean', [ 'Is the next term 7?', '', 'suffix text', ['debug'] ], [ [ 'key', 1 ] ] ],
);

session(
  'message',
  [ 'message', [ 'Breaking message' ], [ [ 'button', 0 ] ] ],
  [ 'message', [ [ 'Reader', 'green', 'About to run: SCodelet' ] ], [ [ 'key', 1 ] ] ],
  [ 'message', [ 'No break', 1 ] ],
  [ 'message', [ [ 'Fam', ['codelet_family'], ' codelet added by thought: x' ], 1 ] ],
  [ 'message', [ 'after array', 1 ] ],
  [ 'start_debug', [] ],
  [ 'start_debug', [] ],
);

session(
  'ask_user_extension',
  [ 'ask_user_extension', [ [3] ], [ [ 'button', 0 ] ] ],
  [ 'ask_user_extension', [ [ 4, 5 ] ], [ [ 'button', 1 ] ] ],
  [ 'ask_user_extension', [ [ 6, 7 ], undef, { rejected => ['6'] } ] ],
  [ 'ask_user_extension', [ [8], 'because of x', { feature => { debug => 1 } } ],
    [ [ 'button', 0 ] ] ],
);

# Screenshot: the commentary after a short session, a question waiting.
$text->delete( '1.0', 'end' );
$Global::debugMAX = 0;
quiet {
  $C->MessageRequiringNoResponse( 'New Sequence Started: ', [], "1 1 2 1 2 3\n" );
  main::message( [ 'Reader', 'green', ' About to run: SCodelet' ], 1 );
  main::message( "\n", 1 );
  main::message( 'I will describe the solution now!', 1 );
};
$MW->after(
  50,
  sub {
    $MW->update;
    my $ppm = File::Spec->catfile( tempdir( CLEANUP => 1 ), 'c.ppm' );
    my $photo = $MW->Photo( -format => 'window', -data => oct( $C->id ) );
    $photo->write( $ppm, -format => 'ppm' );
    my $png = perl_screen_path('commentary');
    system( 'sh', '-c', 'pnmtopng "$1" > "$2" 2>/dev/null', 'sh', $ppm, $png ) == 0
      or die "pnmtopng failed\n";
    $buttons[0]->invoke;
  }
);
quiet { $C->MessageRequiringBooleanResponse('Is the next term 4?') };

emit();
