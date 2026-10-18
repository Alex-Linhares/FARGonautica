package RunResults;
our %Res;
our $SelectedSeq = undef;

package main;
use lib 'lib';
use Tk;
use Tk::Text;
use Tk::Listbox;
use Test::Seqsee;
use Getopt::Long;
use Seqsee;
use warnings;
use Smart::Comments;

my $MW = new MainWindow();
$MW->Button(
    -text    => 'start',
    -command => sub {
        $MW->after(10, \&StartRun);
    }
)->pack();
my $Text = $MW->Text(-width => 130)->pack();
my $List = $MW->Listbox(-width => 100)->pack();
$Text->tagConfigure('success', -background => 'blue', -foreground => 'white');
$Text->tagConfigure('error', -background => 'red', -foreground => 'white');
$Text->tagConfigure('nosuccess', -background => 'yellow', -foreground => 'white');
$List->bind(
    '<1>' => sub {
        my $name = $List->get('active');
        $RunResults::SelectedSeq = $name;
        Update();
    }
);
$MW->MainLoop();

sub update_every {
    my ($ms) = @_;
    print "updating...\n";
    $MW->after( $ms, \&update_every );
    Update();
}

update_every(100);

sub StartRun {
    my $pattern = '*';
    my %options = (
        f => sub {
            my ( $ignored, $feature_name ) = @_;
            print "$feature_name will be turned on\n";
            unless ( $Global::PossibleFeatures{$feature_name} ) {
                print "No feature named '$feature_name'. Typo?\n";
                exit;
            }
            $Global::Feature{$feature_name} = 1;
        },
        pattern => \$pattern,
        p       => \$pattern,
    );

    GetOptions( \%options, 'f=s', 'pattern=s', 'p=s' );
    $pattern = "Reg/$pattern.reg";
    my @files = glob($pattern);

    for $file (@files) {
        my ( $imp_ref, $worse_ref, $results_ref, $opts_ref ) = RegHarness($file);
        my $seq = join( ', ', @{ $opts_ref->{seq} } );
        ### results: $results_ref
        ### opts_ref: $opts_ref
        ### seq: $seq
        $RunResults::Res{$seq} = $results_ref;
        Update();
    }

}

sub Update {
    $List->delete( 0,     'end' );
    $Text->delete( '0.0', 'end' );
    for ( sort keys %RunResults::Res ) {
        ### Sequence key: $_
        $List->insert( 'end', $_ );
    }
    if ( my $selected = $RunResults::SelectedSeq ) {
        Display_Selected( $RunResults::Res{$selected} );
    }
    $MW->update();
}

sub find_tag{
    my ( $msg ) = @_;
    if ($msg =~ /^SUCCESS/) {
        return ["success"];
    } elsif ($msg =~ /^Error/) {
        return ["error"];
    } else {
        return ["nosuccess"];
    }
}

sub Display_Selected {
    my ($arr_ref) = @_;
    my @arr = @$arr_ref;

    my @string = map { ("X", find_tag($_)) } @arr;
    $Text->insert('end', @string, "\n");

    for my $line (@arr) {
        ### line: $line
        $Text->insert( 'end', "======\n\n" );
        $Text->insert( 'end', $line, '', "\n" );
    }
}

