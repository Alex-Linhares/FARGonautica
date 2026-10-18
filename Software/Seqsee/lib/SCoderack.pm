#####################################################
#
#    Package: SCoderack
#
#####################################################
#   Manages the coderack.
#
#   TODO:
#    * I am thinking of limiting $MAX_CODELETS to about 25; In that scenario, the entire bucket
# system would be a needless overhead.
#    * When a codelet is created, it would have weakened any references it makes. There should
# be a function called purge_defunct() that would get rid of codelets whose some argument is
# undef. Also, a call to get codelet should check for this.
#   * need methods to schedule thoughts, to add several codelets and a method get_runnable()
#####################################################

package SCoderack;
use strict;
use Carp;
use Config::Std;
use Smart::Comments;

my $MAX_CODELETS  = 25;    # Maximum number of codelets allowed.
my $CODELET_COUNT = 0;     #    How many codelets are in there currently?
our @CODELETS;             #    The actual codelets
our $URGENCIES_SUM = 0;    #    Sum of all urgencies
our $LastSelectedRunnable; # Last selected codelet/thought

our %HistoryOfRunnable;

clear();

# method: clear
# makes it all empty
#
sub clear {
  $CODELET_COUNT     = 0;
  $URGENCIES_SUM     = 0;
  @CODELETS          = ();
  %HistoryOfRunnable = ();
}

# method: init
# Initializes codelets from a config file.
#
# Ignores the passed OPTIONS_ref, but reads initialization info from a config file
#
sub init {
  my $package     = shift;    # $package
  my $OPTIONS_ref = shift;
  print "Initializing Coderack...\n";
  $Global::Steps_Finished //= 0;
  if ( $Global::Feature{CodeletTree} ) {
    open my $handle, '>', $Global::CodeletTreeLogfile;
    select($handle);
    $| = 1;
    select(*STDOUT);
    $|                            = 1;
    $Global::CodeletTreeLogHandle = $handle;

    #print "Handle: $handle\n";
    print {$Global::CodeletTreeLogHandle} "Initial\n";
  }

# Codelet configuarion for startup should be read in from another configuration file config/start_codelets.conf
# die "This is where I left yesterday";

  read_config 'config/start_codelets.conf' => my %launch_config;
  for my $family ( keys %launch_config ) {
    next unless $family;
    ## Family: $family
    my $urgencies = $launch_config{$family}{urgency};
    ## $urgencies
    my @urgencies = ( ref $urgencies ) ? (@$urgencies) :($urgencies);
    ## @urgencies
    for (@urgencies) {

      # launch!
      $package->add_codelet( new SCodelet( $family, $_, {} ) );
    }
  }
}

# method: add_codelet
# Adds the given codelet to the coderack
#

sub add_codelet {
  my ( $package, $codelet ) = @_;
  confess "A non codelet is being added" unless $codelet->isa("SCodelet");
  $CODELET_COUNT++;
  push( @CODELETS, $codelet );
  if ( $Global::Feature{CodeletTree} ) {
    print {$Global::CodeletTreeLogHandle}
    "\t$codelet\t$codelet->[0]\t$codelet->[1]\n";
  }
  $URGENCIES_SUM += $codelet->[1];
  if ( $CODELET_COUNT > $MAX_CODELETS ) {
    expunge_codelet();
  }
}

# method: _choose_codelet
# Chooses a codelet, and returns the index of a codelet.
#

sub _choose_codelet {
  return undef unless $CODELET_COUNT;
  confess "In Coderack: urgencies sum 0, but codelet count non-zero"
  unless $URGENCIES_SUM;

  ## _choose_codelet: $CODELET_COUNT, $URGENCIES_SUM

  my $random_number = 1 + int( rand($URGENCIES_SUM) );
  ## _choose_codelet random_number: $random_number
  ## @CODELETS
  my $index = 0;
  while ( $random_number > $CODELETS[$index]->[1] ) {
    $random_number -= $CODELETS[$index]->[1];
    $index++;
  }
  ## _choose_codelet returning: $index
  return $index;

}

# ACCESSORS, mostly for testing

sub get_urgencies_sum { return $URGENCIES_SUM }
sub get_codelet_count { return $CODELET_COUNT }

# method: get_next_runnable
# returns a codelet or a thought.
#
#    If no thought is scheduled, just uses _choose_codelet to find the index of a codelet to return.
#
#    If there IS a scheduled though, though, with a 70% probability it is chosen, else a codelet is chosen. This is a first cut interface, of course, will update as I get wiser.
#
#    The scheduled thought, if not chosen, is NOT overwritten
sub get_next_runnable {
  my ($package) = @_;
  $Global::LogString = "\n\n=======\nLogged Message:\n===\n";

  unless ($CODELET_COUNT) {
    my $new_reader = SCodelet->new( 'FocusOn', 100, {} );
    if ( $Global::Feature{CodeletTree} ) {
      print {$Global::CodeletTreeLogHandle}
      "Background\n\t$new_reader\tFocusOn\t100\n";
    }
    return $new_reader;
  }

  my $idx = _choose_codelet();
  my $to_return = splice( @CODELETS, $idx, 1 );
  $HistoryOfRunnable{ 'Seqsee::SCF::' . $to_return->[0] }++;
  $URGENCIES_SUM -= $to_return->[1];
  $CODELET_COUNT--;
  return $LastSelectedRunnable = $to_return;
}

# method: expunge_codelet
# Gets rid of the minimum urgency codelet.
#
sub expunge_codelet {
  @CODELETS = sort { $b->[1] <=> $a->[1] } @CODELETS;
  my $cl = pop(@CODELETS);
  if ( $Global::Feature{CodeletTree} ) {
    print {$Global::CodeletTreeLogHandle} "Expunge $cl\n";
  }
  $CODELET_COUNT--;
  $URGENCIES_SUM -= $cl->[1];
}

sub AttentionDistribution {

# This returns a map from objects to probability of it being chosen next.
# Probabilities need not sum to 1, as some codelets may choose multiple objects.

  return {} unless $URGENCIES_SUM;
  my %return_distribution;
  my $reader_urgencies_sum = 0;
  for my $cl (@CODELETS) {
    if ( $cl->[0] eq 'FocusOn' ) {
      $reader_urgencies_sum += $cl->[1];
      next;
    }
    my $urgency = $cl->[1];
    while ( my ( $k, $v ) = each %{ $cl->[3] } ) {

      # print "\t$v updated by $urgency\n";
      $return_distribution{$v} += $urgency;
    }
  }

  # Account for reader's choice:
  if ($reader_urgencies_sum) {
    my ( $reader_distribution_prob, $reader_dist_objects ) =
    SWorkspace::__GetObjectOrRelationChoiceProbabilityDistribution();
    my @probs = @$reader_distribution_prob;
    for (@$reader_dist_objects) {
      my $prob = shift(@probs);
      $return_distribution{$_} += $prob * $reader_urgencies_sum;

      # print "\t[FocusOn] $_ updated by $prob * $reader_urgencies_sum\n";
    }
  }

  # Normalize:
  ## Before normalization: $URGENCIES_SUM, %return_distribution
  #while (my($k, $v) = each %return_distribution) {
  $_ /= $URGENCIES_SUM for ( values %return_distribution );

  #}

  return \%return_distribution;
}

1;
