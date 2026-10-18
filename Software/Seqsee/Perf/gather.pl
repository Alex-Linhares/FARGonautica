use 5.10.0;
## STANDARD MODULES THAT I INCLUDE EVERYWHERE
use Carp;
use Class::Multimethods;
use Config::Std;
use English qw(-no_match_vars );
use Exception::Class;
use File::Slurp;
use Getopt::Long;
use IO::Prompt;
use List::Util qw{min max sum first};
use Memoize;
use Scalar::Util qw(blessed);
use Smart::Comments '###';
use Sort::Key;
use Storable;
use Time::HiRes;
use strict;
use warnings;

## END OF STANDARD INCLUDES

# Do this to load seqsee.
use lib 'lib';
use Seqsee::ResultOfTestRun;

use Perf::TestSequence;
use Perf::Version;
use Perf::FeatureSet;
use Perf::AllCollectedData;
use Perf::Figure::Specification;
use Perf::Figure::SequenceToDraw;
use Perf::Figure::SequenceToChart;
use Perf::Figure::Cluster;
use Perf::BarChart;
use Perf::CollatedData;
use Perf::GatherDataFor;

use Getopt::QuotedAttribute;
exit if $Getopt::QuotedAttribute::exit_after_load;

our $FLAG_filename :
  Getopt("filename=s", doc => "(Optional) Chart specification file for which to gather data by running Seqsee.");

our $FLAG_times :
  Getopt("times=i", doc => "(Required) Run Seqsee enough times so that for each sequence in file, it has been run at least this many times.");
$FLAG_times
  // Getopt::QuotedAttribute::Usage("Missing required argument times");

our $FLAG_directory :
  Getopt("directory|dir=s", doc => "(Optional) Directory. Do for all files");

unless ( $FLAG_directory or $FLAG_filename ) {
    Getopt::QuotedAttribute::Error(
        "Exactly one of --directory and --filename is required");
}

if ( $FLAG_directory and $FLAG_filename ) {
    Getopt::QuotedAttribute::Error(
        "Exactly one of --directory and --filename is required");
}

if ($FLAG_filename) {
    Getopt::QuotedAttribute::Error("--filename $FLAG_filename does not exist!")
      unless -e $FLAG_filename;
}

our $FLAG_dry_run :
  Getopt("dry_run|dry!", doc => "(Optional) Don't actually run Seqsee");
$FLAG_dry_run //= 0;

my $AllData = Perf::AllCollectedData->new();

if ($FLAG_directory) {
    gather_for_directory($FLAG_directory);
} else {
    gather_for_file($FLAG_filename);
}

sub gather_for_directory {
    my ($directory) = @_;
    for my $f (<$directory/*>) {
        if (-d $f) {
            gather_for_directory($f);
        } else {
            gather_for_file($f);
        }
    }
}


sub gather_for_file {
    my ($filename) = @_;
    my $Spec = Perf::Figure::Specification->new_from_specfile(
        {
            all_read_data => $AllData,
            specfile      => $filename,
        }
    ) or confess $!;

    Perf::GatherDataFor->Gather(
        { spec => $Spec, min_result_set => $FLAG_times } );

}

