use strict;
use 5.10.0;
use Tk;
use Carp;
use Smart::Comments;
use Getopt::Long;
use List::Util qw(sum);
my %options;

my $filename = 'activations.log';
my %Id2Names;
my %Names2Id;
my @TimesWhenSampled;
my %Activations; # keys: ids. Values: { time => activation, time => activation, ...}
ReadFile($filename);

my $MW = new MainWindow();
$MW->focusmodel('active');
$MW->bind(
    '<KeyPress-q>' => sub {
        exit;
    }
);
my $frame = $MW->Frame()->pack( -side => 'top' );
my $combo1 = $frame->ComboEntry(
    -itemlist => [ sort keys %Names2Id ],
    -width    => 60,
)->pack( -side => 'left' );
$frame->Button(
    -text    => 'Show',
    -command => sub {
        ShowGraph( $combo1->get() );
        }

)->pack( -side => 'left' );

my $WIDTH = 700;
my $HEIGHT = 300;
my $MARGIN = 25;
my $EFFECTIVE_HEIGHT = $HEIGHT - 2 * $MARGIN;
my $EFFECTIVE_WIDTH = $WIDTH - 2 * $MARGIN;

my $Canvas = $MW->Canvas( 
    -background => '#FFFFFF',
    -height => $HEIGHT, -width => $WIDTH )->pack( -side => 'top' );
MainLoop;


sub ReadFile {
    my ( $filename ) = @_;
    open my $IN, '<', $filename or die "Failed to open >>$filename<<";
    while (my $line = <$IN>) {
        chomp($line);
        if ($line =~ m#^NewNode\s+(\d+)\s+(.*)#x) {
            $Id2Names{$1}=$2;
            $Names2Id{$2}=$1;
        } elsif ($line =~ m#^\d+#) {
            my ($time, %activations) = split(/\s+/, $line);
            push @TimesWhenSampled, $time;
            while (my($k, $v) = each %activations) {
                $Activations{$k}{$time} = $v;
            }
        }
    }
}

sub ShowGraph {
    my ($node) = @_;
    my %data = %{$Activations{$Names2Id{$node}}};
    my $maximum_timestamp = $TimesWhenSampled[-1];
    my $width_per_timestep = $EFFECTIVE_WIDTH / $maximum_timestamp;

    my $x_tab_step;
    given ($maximum_timestamp) {
        when ($_ < 100) { $x_tab_step = 10 }
        when ($_ < 1000) { $x_tab_step = 100 }

        my $approx_steps = $_/12;
        $x_tab_step = 100 * int($approx_steps/100);
    }

    $Canvas->delete('all');
    for (my $xtab = 0; $xtab <= $maximum_timestamp; $xtab += $x_tab_step) {
        my $x = $MARGIN + $xtab * $width_per_timestep;
        $Canvas->create('line',
                        $x, $HEIGHT - $MARGIN - 5, $x, $HEIGHT - $MARGIN + 5,
                            );
        $Canvas->create('line',
                        $x, $HEIGHT - $MARGIN - 5, $x, $MARGIN,
                        -fill => '#EEEEEE',
                            );
        $Canvas->create('text', $x, $HEIGHT - $MARGIN + 10, -anchor => 'n',
                        -text => $xtab
                            );
    }

    for (my $ytab = 0; $ytab <= 1; $ytab += 0.2) {
        my $y = $MARGIN + $EFFECTIVE_HEIGHT * (1 - $ytab);
        $Canvas->create('line',
                        $MARGIN - 5, $y, $MARGIN + 5, $y,
                            );
        $Canvas->create('line',
                        $WIDTH - $MARGIN, $y, $MARGIN + 5, $y,
                        -fill => '#EEEEEE',
                            );
        $Canvas->create('text', $MARGIN - 7, $y, -anchor => 'e',
                        -text => $ytab
                            );
    }

    $Canvas->create( 'rectangle', $MARGIN, $MARGIN, $WIDTH - $MARGIN,
                     $HEIGHT - $MARGIN, -outline => '#999999' );

    while (my($time, $value) = each %data) {
        my $x = $MARGIN + $time * $width_per_timestep;
        my $y = $HEIGHT - $MARGIN - $EFFECTIVE_HEIGHT * $value;
        $Canvas->create('rectangle', $x, $y, $x+1, $y+1, -fill=>'#0000FF');
    }
}
