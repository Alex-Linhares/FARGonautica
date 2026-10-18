package Perf::Version;
use 5.10.0;

## STANDARD MODULES THAT I INCLUDE EVERYWHERE
use strict;
use warnings;

use List::Util qw{min max sum first};
use Time::HiRes;
use Getopt::Long;
use Storable;

use File::Slurp;
use Smart::Comments;
use IO::Prompt;
use Class::Std;
use Class::Multimethods;

use Carp;
## END OF STANDARD INCLUDES

my $VERSION_FILENAME = 'performance/version_file';

my %Major_of : ATTR(:name<major>);
my %Minor_of : ATTR(:name<minor>);

sub BUILD {
    my ( $self, $id, $opts_ref ) = @_;
    my $string = $opts_ref->{string};
    $string =~ s#\s##g;

    if ($string eq 'human') {
        $Major_of{$id} = $Minor_of{$id} = '';
        return;
    }

    ( $Major_of{$id}, $Minor_of{$id} ) = split( /:/, $string );
    confess "Strange version '$string'" unless defined($Minor_of{$id});

}

sub _cmp {
    my ( $a, $b ) = @_;
    my ( $a1, $a2, $b1, $b2 ) =
      map { ( $_->get_major(), $_->get_minor() ) } ( $a, $b );
    return $a1 <=> $b1 || $a2 <=> $b2;
}

use overload '<=>' => \&_cmp;

## Sub ######################
#  Name             : GetVersionOfCode
#  Returns          : a version number
#  Params via href  : No
#  Parameters       : -
#  Purpose          : What should the version be currently?
##
#  Usage            : GetVersionOfCode(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

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

## Sub ######################
#  Name             : GenerateNextVersion
#  Returns          : Next version
#  Params via href  : No
#  Parameters       : previos_version, current_svn_version, diff
#  Purpose          : if I made changes without commiting, (or with commiting)
#                   : the version changes.
##
#  Usage            : GenerateNextVersion(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

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

## Sub ######################
#  Name             : FindLatestSVNVersion
#  Returns          : (latest_version, md5_hex(svn diff))
#  Params via href  : No
#  Parameters       : -
#  Purpose          : 
##
#  Usage            : FindLatestSVNVersion(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

sub FindLatestSVNVersion {
    my $svn_version = qx{svn info | grep 'Revision:'};
    $svn_version =~ s#\D##g;
    use Digest::MD5 qw{md5_hex};
    my $diff = md5_hex(qx{svn diff});
    return ( $svn_version, $diff );
}

## Sub ######################
#  Name             : GetLatestStoredDiff
#  Returns          : (version_number, svn_version, md5_hex(svn diff))
#  Params via href  : No
#  Parameters       : -
#  Purpose          : Read information stored in $VERSION_FILENAME
##
#  Usage            : Name(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

sub GetLatestStoredDiff {
    my @lines = read_file($VERSION_FILENAME);
    @lines = grep { /\S/ } @lines;
    return ( '0:0', 0, '' ) unless @lines;
    chomp( $lines[-1] );
    my ( $version, $date, $svn_version, $diff ) = split( /;/, $lines[-1], 4 );
    return ( $version, $svn_version, $diff );
}


1;

