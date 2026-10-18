use lib 'lib';
use Test::Seqsee;
use Getopt::Long;
use Seqsee;
use warnings;
use Smart::Comments;

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
    p => \$pattern,
);

GetOptions(\%options, 'f=s', 'pattern=s', 'p=s');
$pattern = "Reg/$pattern.reg";
### $pattern

my ( @improved, @became_worse );
my @files = glob($pattern);

for $file (@files) {
    my ( $imp_ref, $worse_ref ) = RegHarness($file);
    push @improved,     @$imp_ref;
    push @became_worse, @$worse_ref;
}

if (@improved) {
    print "\nThe following improved:\n";
    for (@improved) {
        print "\tFrom $_->[1] to $_->[2]\n\t\t", join( ", ", @{ $_->[0] } ), "\n";
    }
}
if (@became_worse) {
    print "\nThe following became worse:\n";
    for (@became_worse) {
        print "\tFrom $_->[1] to $_->[2]\n\t\t", join( ", ", @{ $_->[0] } ), "\n";
    }
}

