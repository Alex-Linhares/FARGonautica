use 5.010;
use lib 'util';
use strict;
use AnalogyImage;
use Smart::Comments;
use Tk;
use Tk::FileSelect;
use Cwd;
use Devel::CheckOS qw(os_is);

my $FSref;
my $ImageSpec;
use constant { WIDTH => 500, HEIGHT => 120, PAGEHEIGHT => '3c' };

my ( $MW, $BUTTON_FRAME, $FSref, $SAVE_FILENAME, $CANVAS );

sub MAIN {
    $MW           = new MainWindow;
    $BUTTON_FRAME = $MW->Frame()->pack( -side => 'top' );
    my $cwd = getcwd();
    my $newdir = "$cwd/images";
    $newdir =~ s#\/#\\#g if os_is("MicrosoftWindows");
    $FSref        = $MW->FileSelect( -directory => $newdir );

    $MW->bind(
        '<KeyPress-q>' => sub {
            exit;
        }
    );
    SetupButton( 'Save', \&Save );
    SetupButton( 'Load', \&Load );
    SetupEntry( 'Filename', \$SAVE_FILENAME );

    $CANVAS = $MW->Canvas(
        -height     => HEIGHT(),
        -width      => WIDTH(),
        -background => 'white'
    )->pack( -side => 'top' );

    MainLoop;
}

sub SetupButton {
    my ( $label, $command ) = @_;
    $BUTTON_FRAME->Button(
        -text    => $label,
        -command => $command
    )->pack( -side => 'left' );
}

sub SetupEntry {
    my ( $label, $var ) = @_;
    my $f = $MW->Frame()->pack( -side => 'top' );
    $f->Label( -text => $label )->pack( -side => 'left' );
    $f->Entry( -textvariable => $var, -width => 100 )->pack( -side => 'left' );

}

sub Load {
    my $filename = $FSref->Show or return;
    LoadFile($filename);
}

sub LoadFile {
    my $filename = shift || '/home/amahabal/tmp/1.anal';
    my $ImageSpec = AnalogyImage->new;
    $ImageSpec->load_from_file($filename);
    ( $SAVE_FILENAME = $filename ) =~ s#.anal$#.eps#;
    ### $ImageSpec
    $ImageSpec->draw( $CANVAS, HEIGHT() );
}

sub Save {
    $CANVAS->postscript(
        -file => $SAVE_FILENAME,
        -height => HEIGHT,
        -pageheight => '3c',
    );
}

MAIN();
