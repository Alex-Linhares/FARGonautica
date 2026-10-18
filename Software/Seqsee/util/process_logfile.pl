use strict;
my ($INFILE, $OUTFILE);
$INFILE = $ARGV[0] || "log/latest";
open IN, $INFILE;

my @pattern_subs =
    ( [qr{^=== (\d+) ==========\s+CODELET (.*)},
       sub {
           return "\\codelet{$2}\n";
       }],
      
      [qr{^=== (\d+) ==========\s+NEW THOUGHT (.*)},
       sub {
           return "\\thought{$2}\n";
       }],

            );

LOOP: while (my $in = <IN>) {
    chomp($in);
    for (@pattern_subs) {
        my ($pat, $sub) = @$_;
        if ($in =~ $pat) {
            print $sub ->($in), "\n";
            next LOOP;
        }
    }
}
