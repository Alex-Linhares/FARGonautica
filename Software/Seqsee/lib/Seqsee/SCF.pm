#####################################################
#
#    Package: Seqsee::SCF
#
#####################################################
#####################################################

package Seqsee::SCF;
use strict;
use Carp;
use Class::Std;
use base qw{Exporter};

our @EXPORT = qw( ContinueWith );

sub ContinueWith {
  scalar(@_) == 1 or confess "ContinueWith takes a single argument!";
  UNIVERSAL::isa( $_[0], 'SThought' )
  or confess "ContinueWith takes a thought as argument";
  if ( $Global::Feature{CodeletTree} ) {
    print {$Global::CodeletTreeLogHandle} "\t$_[0]\n";
  }

  $Global::MainStream->add_thought( $_[0] );
}

1;
