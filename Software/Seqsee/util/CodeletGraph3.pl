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

use Tk;
use Tk::GraphViz;

my %Nodes;
my %IncomingEdges;
my %OutgoingEdges;

my $graph = GraphViz->new(
    rankdir => 1,
    width   => 12,
    height  => 20,
);

my @codefamilies  = GetListOfCodefamilies();
my @thought_types = GetListOfThoughts();
my %OtherMethods  = (
    Background       => 'Seqsee::do_background_activity',
    Stream           => 'SStream2::_think_the_current_thought',
    GetSomethingLike => 'SWorkspace::GetSomethingLike',
);

## CREATE NODES
for (@codefamilies) {
    $Nodes{$_} = [
        label => $_,
        # shape => 'trapezium',
        color => '#FF0000',
        style => 'filled'
            ];
}
for (@thought_types) {
    $Nodes{$_} = [
        label => $_,
        shape => 'octagon',
        color => '#0000FF',
        style => 'filled'
            ];
}
for ( keys %OtherMethods ) {
    $Nodes{$_} = [
        label => $_,
        shape => 'triangle',
        color => '#00FF00',
        style => 'filled'
            ];
}

## ADD EDGES:
for (@codefamilies) {
    my $code = GetSubroutineCode("Seqsee::SCF::${_}::run");
    AddEdges( $_, $code );
}

for (@thought_types, 'SThought::??') {
    my $code = GetSubroutineCode("SThought::${_}::get_actions");
    AddEdges( $_, $code );
}

while ( my ( $name, $subroutine_name ) = each %OtherMethods ) {
    my $code = GetSubroutineCode($subroutine_name);
    AddEdges( $name, $code );
}

my $MW = new MainWindow();
my $gv = $MW->GraphViz( -width => 1200, -height => 600 )->pack(
    -expand => 'yes',
    -fill   => 'both',
    -side   => 'top'
);
$gv->bind(
    'node',
    '<Button-1>',
    sub {
        my @tags = $gv->gettags('current');
        push @tags, undef unless ( @tags % 2 ) == 0;
        my %tags = @tags;
        Recenter( $tags{label} );

        # printf( "Clicked node: '%s' => %s\n", $tags{node}, $tags{label} );
    }
);


$MW->bind(
    '<KeyPress-b>' => sub {
        RedrawOriginal();
    },
);
$MW->bind(
    '<KeyPress-q>' => sub {
        exit;
    },
);

my $original = CreateGraphviz();
RedrawOriginal();
$gv->zoom( -in => 800 );
MainLoop;

sub Recenter {
    my ($node) = @_;
    $gv->show(
        CreateSurroundingGraphViz( $node, 3 ),
        layout     => 'dot',
        graphattrs => [qw( overlap false spline true )]
    );
    $gv->fit();
    $gv->zoom(-out => 2);
}

sub RedrawOriginal {
    $gv->show( $original, layout => 'neato', graphattrs => [qw( overlap false spline true )] );
    # $gv->fit();
   # $gv->zoom(-out => 2);
}

###############
sub CreateGraphviz {
    my $g = GraphViz->new(bgcolor=> 'green');
    while ( my ( $k, $v ) = each %Nodes ) {
        my %v = @{$v};
        next unless $v{label};
        $g->add_node($k, @$v);
        ## outgoing: $k, $OutgoingEdges{$k}
        for my $dest ( @{ $OutgoingEdges{$k} } ) {
            next unless exists $Nodes{$dest};
            $g->add_edge( $k, $dest );
        }
    }
    return $g;
}

sub CreateSurroundingGraphViz {
    my ( $center_node, $distance ) = @_;
    my %NodesToKeep = ( $center_node, 1 );

    my @left_expand = ($center_node);
    for ( 1 .. $distance ) {
        @left_expand = map { @{ $IncomingEdges{$_} || [] } } @left_expand;
        $NodesToKeep{$_} = 1 for @left_expand;
    }

    my @right_expand = ($center_node);
    for ( 1 .. $distance ) {
        @right_expand = map { @{ $OutgoingEdges{$_} || [] } } @right_expand;
        $NodesToKeep{$_} = 1 for @right_expand;
    }

    my $g = GraphViz->new(rankdir => 1);
    while ( my ( $k, $v ) = each %NodesToKeep ) {
        $g->add_node($k, @{$Nodes{$k}});
        for my $dest ( @{ $OutgoingEdges{$k} } ) {
            next unless $NodesToKeep{$dest};
            $g->add_edge( $k, $dest );
        }
    }
    return $g;
}

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
        push @{$IncomingEdges{$_}}, $name;
        push @{$OutgoingEdges{$name}}, $_;
    }

    for (@$actions_ref) {
        $graph->add_edge( $name, $_ );
        push @{$IncomingEdges{$_}}, $name;
        push @{$OutgoingEdges{$name}}, $_;
    }

    for (@$thought_ref) {
        $graph->add_edge( $name, $_ );
        push @{$IncomingEdges{$_}}, $name;
        push @{$OutgoingEdges{$name}}, $_;
    }

    if ($any_creation) {
        $graph->add_edge( $name, 'SThought::??' );
        push @{$IncomingEdges{'SThought::??'}}, $name;
        push @{$OutgoingEdges{$name}}, 'SThought::??';
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
