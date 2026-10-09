package Oracle;
# Helpers for oracle scripts: collect cases, print them as JSON on stdout.
use strict;
use warnings;
use JSON::PP;
use Exporter 'import';
our @EXPORT = qw(record emit dies);

my @cases;

# record(name => $value, ...) - add one case; values must be JSON-serialisable
sub record { push @cases, {@_}; }

# dies(sub { ... }) - 1 if the code dies, 0 otherwise
sub dies { my ($code) = @_; return eval { $code->(); 1 } ? 0 : 1; }

sub emit {
  print JSON::PP->new->canonical->pretty->allow_nonref->encode(\@cases);
}
1;
