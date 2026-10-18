use strict;
use 5.10.0;
use Tk;
use Carp;
use Smart::Comments;
use Getopt::Long;
use List::Util qw(sum max);
my %options;

my $filename = 'codelet_tree.log';

use constant CODELET => 1;
use constant THOUGHT => 2;

# Format of that file:
# An unindented line indicates a "parent": a runnable run, or "Initial" or "Background"
# An indented line indicates an object being added to the coderack.

our %ObjectCounts;          # Keys: families
our %ObjectUrgencySums;     # Keys: families
our @DistributionAtTime;    # Index: time. Entries: Hash with keys families, values probabilities.
our @TypeRunAtTime; # Index: time. Entries: families.

our $RunnableCount;
our %Type;
our %Family;
our %Urgency;

ReadFile();
my $MW = new MainWindow();
$MW->focusmodel('active');
$MW->bind(
    '<KeyPress-q>' => sub {
        exit;
    }
);
my $frame = $MW->Frame()->pack( -side => 'top' );
my $combo1 = $frame->ComboEntry(
    -itemlist => [ sort keys %ObjectCounts ],
    -width    => 40,
)->pack( -side => 'left' );
$frame->Button(
    -text    => 'Show',
    -command => sub {
        ShowGraph( $combo1->get() );
        }

)->pack( -side => 'left' );

my $WIDTH = 700;
my $HEIGHT = 300;
my $MARGIN = 30;
my $EFFECTIVE_HEIGHT = $HEIGHT - 2 * $MARGIN;
my $EFFECTIVE_WIDTH = $WIDTH - 2 * $MARGIN;

my $Canvas = $MW->Canvas( 
    -background => '#FFFFFF',
    -height => $HEIGHT, -width => $WIDTH )->pack( -side => 'top' );
MainLoop;

sub ReadFile {
    open my $file, '<', $filename;
    my $parent;
    while ( my $line = <$file> ) {
        if ( $line =~ /^Initial/ or $line =~ /^Background/ ) {
            $parent = '';
        }
        elsif ( $line =~ /^Expunge\s+(.*)$/ ) {

            # So some counts go down.
            my $expunged = $1;
            ProcessExpunging($expunged);
        }
        elsif ( $line =~ /^\S+ \s* (\S+)/x ) {

            # Running a runnable.
            ProcessRunnable($1);
        }
        elsif ( $line =~ /^\s* (\S+) \s* (.*)/x ) {

            # A new runnable added.
            ProcessAddition( $1, $2 );
        }
        else {
            confess "Unable to process line: >>$line<<\n";
        }
    }
}

sub ProcessExpunging {

    # An object is being expunged!
    my ($object) = @_;
    UncountRunnable($object);
}

sub ProcessAddition {
    my ( $object, $details ) = @_;
    if ( $Family{$object} ) {

        # Don't know why...
        warn "I already know family of >>$object<<";
    }
    if ( $object =~ /^SThought::(.*?)=/ ) {
        $Type{$object}   = THOUGHT;
        $Family{$object} = $1;
        push @TypeRunAtTime, $1;
    }
    else {
        $Type{$object} = CODELET;
        $details =~ /^\s* (\S+) \s* (\d+)/x
            or confess "Cannot understand details: >>$details<< for object >>$object<<";
        my ( $family, $urgency ) = ( $1, $2 );
        $Family{$object}  = $family;
        $Urgency{$object} = $urgency;
        $ObjectCounts{$family}++;
        $ObjectUrgencySums{$family} += $urgency;
        push @TypeRunAtTime, $family;
    }
}

sub UncountRunnable {
    my ($object) = @_;
    my $type = $Type{$object};    # 1=codelet, 2=thought
    return unless($type); # SActions, never on coderack...
    # confess "Missing type for >>$object<<" unless $type;
    if ( $type == CODELET ) {
        my $family  = $Family{$object}  or confess;
        my $urgency = $Urgency{$object} or confess;
        $ObjectCounts{$family}--;
        $ObjectUrgencySums{$family} -= $urgency;

        # delete now useless info
        delete $Family{$object};
        delete $Urgency{$object};
        delete $Type{$object};
    }
    else {
        delete $Type{$object};
        delete $Family{$object};
    }
}

sub ProcessRunnable {

    # This is the point just before a runnable is chose, and I can evaluate pressure here.
    my ($object) = @_;
    $RunnableCount++;
    PrintPressure();
    UncountRunnable($object);
}

sub PrintPressure {
    print "State before runnable#: $RunnableCount\n";
    my $urgencies_sum = sum( values %ObjectUrgencySums );
    unless ($urgencies_sum) {
        print "\tNone\n";
        return;
    }
    while ( my ( $k, $v ) = each %ObjectUrgencySums ) {
        next unless $v;

        # print "\t$k\t=> ", sprintf("%5.3f", $v/$urgencies_sum), "\n";
        $DistributionAtTime[$RunnableCount]{$k} = $v / $urgencies_sum;
    }
}

sub ShowGraph {
    my ($family) = @_;
    my @data = map { $DistributionAtTime[$_]{$family} || 0 } 1 .. $RunnableCount;


    # print "@data\n";
    $Canvas->delete('all');

    my $width_per_runnable = $EFFECTIVE_WIDTH / $RunnableCount;
    my $width_per_timestep = $width_per_runnable;
    my $x                  = $MARGIN;

    my $maximum_timestamp = $RunnableCount;

    my $x_tab_step;
    given ($RunnableCount) {
        when ($_ < 100) { $x_tab_step = 10 }
        when ($_ < 1000) { $x_tab_step = 100 }
        when ($_ < 10000) { 
            my $approx_steps = $_/12;
            $x_tab_step = 100 * int($approx_steps/100) }
        $x_tab_step = 10000;
    }

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

    for (my $ytab = 0; $ytab <= 100; $ytab += 20) {
        my $y = $MARGIN + $EFFECTIVE_HEIGHT * (1 - $ytab * 0.01);
        $Canvas->create('line',
                        $MARGIN - 1, $y, $MARGIN + 5, $y,
                            );
        $Canvas->create('line',
                        $WIDTH - $MARGIN, $y, $MARGIN + 5, $y,
                        -fill => '#EEEEEE',
                            );
        $Canvas->create('text', $MARGIN - 3, $y, -anchor => 'e',
                        -text => $ytab.'%'
                            );
    }

    for ( 0 .. $RunnableCount - 1 ) {
        $x += $width_per_runnable;
        my $data = $data[$_];
        next unless $data;
        my $y = $HEIGHT - $MARGIN - $EFFECTIVE_HEIGHT * $data;
        $Canvas->create('line', $x, $y, $x, $HEIGHT - $MARGIN, -fill=>'#0000FF');

        if ($TypeRunAtTime[$_] eq $family) {
            $Canvas->create('line', $x, $HEIGHT - $MARGIN - 1, $x, $HEIGHT - $MARGIN - 3,
                            -width => 3,
                            -fill => '#FF0000');
        }

    }
    $Canvas->create( 'rectangle', $MARGIN, $MARGIN, $WIDTH - $MARGIN,
                     $HEIGHT - $MARGIN, -outline => '#999999' );

}

