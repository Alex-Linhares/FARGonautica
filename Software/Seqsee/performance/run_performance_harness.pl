use 5.10.0;
use strict;

use lib 'lib';
use Test::Seqsee;
use Global;
use List::Util qw{min max sum};
use Time::HiRes qw{time};
use Getopt::Long;
use Storable;
use File::Slurp;
use Smart::Comments;

my $StartTime        = time();
my $VERSION_FILENAME = 'performance/version_file';

my %options = (
    f => sub {
        my ( $ignored, $feature_name ) = @_;
        print "$feature_name will be turned on\n";
        unless ( $Global::PossibleFeatures{$feature_name} ) {
            print "No feature $feature_name. Typo?\n";
            exit;
        }
        $Global::Feature{$feature_name} = 1;
    }
);
GetOptions( \%options, "times=i", "steps=i", "f=s", "filename=s" );

my $times = $options{times} || 10;
my $steps = $options{steps} || 10000;
my $sequence_filename = $options{filename}
  // 'config/sequence_list_for_multiple_simple';

my @selected_feature_set = map { "-f=$_" } keys %Global::Feature;
my $feature_set_string = join( ' ', @selected_feature_set );

my $version = GetVersionOfCode();

for ( 1 .. 3 ) {
    system
"perl performance/PerformanceHarness.pl --times=$times --outputdir=performance/data --code_version=$version --filename=$sequence_filename --steps=$steps --tempfilename=performance/temp_$_ $feature_set_string &";
}

# Get a version of code fowhich testing was done.
sub GetVersionOfCode {
    my ( $svn_version, $diff ) = FindLatestSVNVersion();
    ### Current Version: $svn_version

    my ( $version, $stored_svn_version, $stored_diff ) = GetLatestStoredDiff();
    ### Stored Version: $stored_svn_version

    return $version
      if ( $svn_version eq $stored_svn_version and $diff eq $stored_diff );

    # say ">>$diff<< >>$stored_diff<<"; exit;
    return GenerateNextVersion( $version, $svn_version, $diff );
}

sub GenerateNextVersion {
    my ( $previos_version, $current_svn_version, $diff ) = @_;
    my ( $previous_svn_version, $prev_code_change_without_commit ) =
      split( /:/, $previos_version );
    my $new_version;
    if ( $previous_svn_version == $current_svn_version ) {
        $prev_code_change_without_commit++;
        $new_version = "$previous_svn_version:$prev_code_change_without_commit";
    }
    else {
        $new_version = "$current_svn_version:1";
    }
    open my $VERSION_FILE, '>>', $VERSION_FILENAME;
    print {$VERSION_FILE}
      join( ';', $new_version, scalar(localtime), $current_svn_version, $diff ),
      "\n";
    close $VERSION_FILE;
    return $new_version;
}

sub FindLatestSVNVersion {
    my $svn_version = qx{svn info | grep 'Revision:'};
    $svn_version =~ s#\D##g;
    use Digest::MD5 qw{md5_hex};
    my $diff = md5_hex(qx{svn diff});
    return ( $svn_version, $diff );
}

sub GetLatestStoredDiff {
    my @lines = read_file($VERSION_FILENAME);
    @lines = grep { /\S/ } @lines;
    return ( '0:0', 0, '' ) unless @lines;
    chomp( $lines[-1] );
    my ( $version, $date, $svn_version, $diff ) = split( /;/, $lines[-1], 4 );
    return ( $version, $svn_version, $diff );
}
