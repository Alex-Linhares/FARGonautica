# End-to-end run of the original Seqsee (headless), via Test::Seqsee::RunSeqsee.
# Usage: oracle/run_perl.sh oracle/e2e_run.perl SEED "SEQ" "CONTINUATION" MAX_STEPS [MAX_FALSE MIN_EXTENSION]
# Prints one JSON object on the last line of stdout. Not a golden-file oracle (no regen.py).
use 5.10.0;
use strict;
use Carp::Seqsee;
use Global;
use Test::Seqsee;
use JSON::PP;

my ($seed, $seq, $cont, $steps, $max_false, $min_ext) = @ARGV;
$max_false //= 3;
$min_ext   //= 3;
srand($seed);
my $r = RunSeqsee([split ' ', $seq], [split ' ', $cont], $steps, $max_false, $min_ext);
say JSON::PP->new->canonical->encode({
  seed   => $seed + 0,
  seq    => $seq,
  status => $r->get_status->get_status_string,
  steps  => $r->get_steps + 0,
  error  => $r->get_error,
});
