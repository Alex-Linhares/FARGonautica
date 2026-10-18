use strict;
use Smart::Comments;
use Data::Dump::Streamer;
use GraphViz;

use lib 'lib';
use Seqsee::SCF::Load;
use SThought::Load;
use Seqsee::Scripts::Load;

use Seqsee;
use SStream2;

my $graph = GraphViz->new(
    rankdir => 1,
    width   => 12,
    height  => 20,
);

my @codefamilies  = GetListOfCodefamilies();
my @thought_types = GetListOfThoughts();
my %OtherMethods  = (
    Background       => 'Seqsee::do_background_activity',
    Stream           => 'SStream::_think_the_current_thought',
    GetSomethingLike => 'SWorkspace::GetSomethingLike',
);

## CREATE NODES
for (@codefamilies) {
    $graph->add_node(
        $_,
        shape => 'trapezium',
        color => '0.5,0.2,0.8',
        style => 'filled'
    );
}
for (@thought_types) {
    $graph->add_node(
        $_,
        shape => 'octagon',
        color => '0.6,0.2,0.8',
        style => 'filled'
    );
}
for ( keys %OtherMethods ) {
    $graph->add_node(
        $_,
        shape => 'triangle',
        color => '0.7,0.2,0.8',
        style => 'filled'
    );
}

## ADD EDGES:
for (@codefamilies) {
    my $code = GetSubroutineCode("Seqsee::SCF::${_}::run");
    AddEdges( $_, $code );
}

for (@thought_types) {
    my $code = GetSubroutineCode("SThought::${_}::get_actions");
    AddEdges( $_, $code );
}

while ( my ( $name, $subroutine_name ) = each %OtherMethods ) {
    my $code = GetSubroutineCode($subroutine_name);
    AddEdges( $name, $code );
}

DisplayGraph();
###############
sub GetAllLaunches {
    my ($code)               = @_;
    my @codelet_launches     = GetCodeletLaunchesFromString($code);
    my @actions_launches     = GetActionLauncesFromString($code);
    my @thought_launches     = GetThoughtLaunchesFromString($code);
    my $any_thought_creation = AreThereAnyCreateThoughtsInString($code);

    return ( \@codelet_launches, \@actions_launches, \@thought_launches, $any_thought_creation );
}

sub AddEdges {
    my ( $name, $code ) = @_;
    my ( $codelet_ref, $actions_ref, $thought_ref, $any_creation ) = GetAllLaunches($code);
    print "$name\n";

    for (@$codelet_ref) {
        $graph->add_edge( $name, $_ );
    }

    for (@$actions_ref) {
        $graph->add_edge( $name, $_ );
    }

    for (@$thought_ref) {
        $graph->add_edge( $name, $_ );
    }

    if ($any_creation) {
        $graph->add_edge( $name, 'SThought::??' );
    }
}

sub GetListOfCodefamilies {
    return sort map { my $x = $_; chop($x); chop($x); $x } grep {/::$/} keys %Seqsee::SCF::;
}

sub GetListOfThoughts {
    return sort map { my $x = $_; chop($x); chop($x); $x } grep {/::$/} keys %SThought::;
}

sub GetSubroutineCode {
    my ($full_name) = @_;
    my $sub_ref     = \&$full_name;
    my $as_str      = Dump($sub_ref)->Out();
    return $as_str;
}

sub GetCodeletLaunchesFromString {
    my ($string) = @_;

    # print $string; <STDIN>;
    my @ret = $string =~ /'SCodelet' -> new \( \s* ['"] (\w+) /xg;
}

sub GetThoughtLaunchesFromString {
    my ($string) = @_;

    # print $string; <STDIN>;
    my @ret = $string =~ /'SThought::([\w:]+)' -> new \( /xg;
}

sub GetActionLauncesFromString {
    my ($string) = @_;

    #print $string; <STDIN>;
    my @ret = $string =~ /'SAction' -> new \( \{ 'family', \s* '([\w:]+)'/xg;
}

sub AreThereAnyCreateThoughtsInString {
    my ($string) = @_;
    return 1 if $string =~ /'SThought'->create\(/;
    return 0;
}

sub DisplayGraph {
    open my $OUT, ">", 'graph.jpg';
    binmode $OUT;
    print {$OUT} $graph->as_jpeg;
    close $OUT;
    open my $OUT2, '>', 'graph.dot';
    print {$OUT2} $graph->as_canon;
    close $OUT2;

    # system "ps2pdf graph.ps; rm graph.ps";
    system 'rundll32.exe c:\Windows\system32\shimgvw.dll,ImageView_Fullscreen D:\seqsee\graph.jpg';
}
