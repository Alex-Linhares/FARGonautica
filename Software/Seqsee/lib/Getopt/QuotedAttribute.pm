package Getopt::QuotedAttribute;
use warnings;
use strict;
use Getopt::Long;
use Attribute::Handlers;

our $VERSION = '0.01';

BEGIN {
  our %options;
  our %doc;
  our %meta_options;
  our $error;
  our $exit_after_load;
}

sub UNIVERSAL::Getopt : ATTR(RAWDATA,BEGIN) {
  my ( $ref, $data ) = @_[ 2, 4 ];

  our %meta_options;
  our $error;
  our $exit_after_load;

  # Very crude parse!
  $data =~ m{
               ^ \s* # Skip space
               " (.*?) " \s* # A quoted string
               , \s* doc \s* => \s* " (.*) "
              }x;
  my $options_spec = $1;

  our ( %options, %doc );
  $doc{$options_spec}     = $2;
  $options{$options_spec} = $ref;
}

INIT {
  our %options;
  our %doc;
  our %meta_options;
  our $error;
  our $exit_after_load;

  $meta_options{'help!'} = sub { Usage(); };

  my %all_options = ( %options, %meta_options );
  $error = 0;
  GetOptions(%all_options);

  # exit if $exit_after_load;
}

sub Usage {
  my ($message) = @_;
  our %options;
  our %doc;
  our %meta_options;
  our $error;
  our $exit_after_load;

  if ($message) {
    print "\n\n$message\n\n";
  }

  print "=" x 10, "\n", "  Usage: \n";
  my $max_label_size = max( map { length $_ } keys %options );

  for my $k ( keys %options ) {
    print "\t", $k;
    print ' ' x ( $max_label_size - length($k) );
    print " => $doc{$k}\n";
  }
  print "=" x 10, "\n";

  $exit_after_load = 1;
}

sub max {
  return 0 unless @_;
  my $ret = shift;
  while (@_) {
    my $next = shift;
    $ret = $next if $next > $ret;
  }
  return $ret;
}

sub Error {
  my ($msg) = @_;
  Usage($msg);
  exit;
}

1;

