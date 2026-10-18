use lib 'genlib';
use strict;
use Test::More;
use Test::Exception;

use SPos;
my $p1 = SPos->new( position => 2 );
my $p2 = SPos->new( position => 2 );
my $p3 = SPos->new( position => 3 );

ok( $p1 eq $p2 );
ok( $p1 ~~ $p2 );

ok( $p1 ne $p3 );
ok( not( $p1 ~~ $p3 ) );

dies_ok { SPos->new( position => 0 ) } "Expected to die!";

# Current code uses ->new(3) syntax as well...
my $p4 = SPos->new(3);
ok( $p4 ~~ $p3 );

dies_ok { SPos->new(-3) } "Expected to die for negative positions!";

# A special exception for position -1 is made since it is widely used currently:
lives_ok { SPos->new(-1) } "Position -1 ok for now";

done_testing();
