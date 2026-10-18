use 5.10.0;
use Tk;
use Tk::FileSelect;
use Tk::FBox;

## STANDARD MODULES THAT I INCLUDE EVERYWHERE
use Carp;
use English qw(-no_match_vars );
use File::Slurp;
use Config::Std;
use Getopt::Long;
use List::Util qw{min max sum first};
use Memoize;
use Scalar::Util qw(blessed);
use Smart::Comments;
use Sort::Key;
use Storable;
use Time::HiRes;
use strict;
use warnings;

## END OF STANDARD INCLUDES
use constant { WIDTH => 500, HEIGHT => 120, PAGEHEIGHT => '3c' };
my ($MW, $CANVAS, $BUTTON_FRAME);
my ($TEXTBOX_FIRST_DESC, $TEXTBOX_SECOND_DESC, $SAVE_FILENAME);
my ($INPUT_LEFT_CATEGORY, $INPUT_RIGHT_CATEGORY, $INPUT_DESCRIPTORS);
my $FSref;
sub MAIN {
    $MW = new MainWindow;
    $BUTTON_FRAME = $MW->Frame()->pack(-side => 'top');
    $FSref = $MW->FileSelect(-directory => "c:\\analogies");

    $MW->bind(
        '<KeyPress-q>' => sub {
            exit;
        }
            );
    $MW->focusmodel('active');
    SetupButton('Draw', \&Draw);
    SetupButton('Save', \&Save);
    SetupButton('Load', \&Load);

    SetupEntry('Left Category', \$INPUT_LEFT_CATEGORY);
    SetupEntry('Right Category', \$INPUT_RIGHT_CATEGORY);
    SetupEntry('Descriptors (comma separated)', \$INPUT_DESCRIPTORS);
    SetupText('First Object Description', \$TEXTBOX_FIRST_DESC);
    SetupText('Second Object Description', \$TEXTBOX_SECOND_DESC);

    $CANVAS = $MW->Canvas( 
        -height => HEIGHT(), -width => WIDTH(), -background => 'white' )
        ->pack( -side => 'top' );
    SetupEntry('Filename', \$SAVE_FILENAME);
    MainLoop;
}

sub SetupButton {
    my ($label, $command) = @_;
    $BUTTON_FRAME->Button(-text => $label,
                          -command => $command)->pack(-side => 'left');
}

sub SetupEntry {
    my ($label, $var) = @_;
    my $f = $MW->Frame()->pack( -side => 'top' );
    $f->Label( -text => $label )->pack( -side => 'left' );
    $f->Entry( -textvariable => $var, -width => 100 )->pack( -side => 'left' );
}

sub SetupText {
    my ($label, $box_var) = @_;
    my $f = $MW->Frame()->pack( -side => 'top' );
    $f->Label( -text => $label )->pack( -side => 'left' );
    $$box_var = $f->Text( -height => 10, -width => 100 )->pack( -side => 'left' );
}

sub Load {
    my $filename = $FSref->Show;
    return unless $filename;
    read_config $filename => my %config;
    
    my $categories = $config{categories};
    if ($categories->{left}) {
        $INPUT_LEFT_CATEGORY = $categories->{left};
        $INPUT_RIGHT_CATEGORY = $categories->{right};
    } else {
        $INPUT_LEFT_CATEGORY = $categories->{both};
        $INPUT_RIGHT_CATEGORY = $categories->{both};
    }

    my $descriptors_ref = $config{descriptors}{descriptor};
    my @descriptors = (ref $descriptors_ref) ? 
        @$descriptors_ref : ($descriptors_ref);
    $INPUT_DESCRIPTORS = join(", ", @descriptors);

    $TEXTBOX_FIRST_DESC->delete('0.0', 'end');
    $TEXTBOX_FIRST_DESC->insert('end', $config{descriptors}{first});

    $TEXTBOX_SECOND_DESC->delete('0.0', 'end');
    $TEXTBOX_SECOND_DESC->insert('end', $config{descriptors}{second});
}

MAIN();
