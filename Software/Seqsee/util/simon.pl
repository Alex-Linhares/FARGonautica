sub seq{join('', map { chr(65+($_[0] - 1+ ($_-1) * $_[2])%26)} (1..$_[1]));}
my $ARGC = scalar(@ARGV);
my @nums = map { ord($_) - 64 } @ARGV;
sub check_periodicity{
    my ( $p ) = @_; my @cont;
  LOOP: for my $i (0..$p-1) {
        my $s = int( ($ARGC-$i)/$p);
        ### p, i, s: $p, $i, $s
        my $substr = join('',@ARGV[map {$i+$p*$_} (0..$s-1)]);
        for (-3..3) {
            if ($substr eq seq($nums[$i],$s,$_)) {
                push @cont, $_; next LOOP;
            }
        }
        return;
    }
    return \@cont;
}
sub print_out{
    my ( $period, $cont_ref ) = @_;
    for (0..50) {
        print chr(65+($nums[$_%$period]+$cont_ref->[$_%$period]*int($_/$period)-1)%26);
    }
    print "\n";
}
for (1..4) {
    if (my $cont_ref = check_periodicity($_)) {
        print_out($_, $cont_ref); exit;
    }
}
print "Failed!\n";
