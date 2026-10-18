use strict;
use Tk;
use GraphViz;
use Tk::GraphViz 1.0;
use strict;
use Smart::Comments;
use Data::Dump::Streamer;

use lib 'lib';
use Seqsee::SCF::All2;
use SThought::All2;
use SThought::LargeGp2;
use Seqsee::Scripts::DescribeSolution2;
use Seqsee;
use SStream2;

my %Nodes;
my %IncomingEdges;
my %OutgoingEdges;

sub CreateGraphviz {
    my $g = GraphViz->new(bgcolor=> 'green');
    while ( my ( $k, $v ) = each %Nodes ) {
        $g->add_node($k, $v);
        for my $dest ( @{ $OutgoingEdges{$k} } ) {
            $g->add_edge( $k, @{$dest} );
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

    my $g = GraphViz->new();
    while ( my ( $k, $v ) = each %NodesToKeep ) {
        $g->add_node($k, $v);
        for my $dest ( @{ $OutgoingEdges{$k} } ) {
            next unless $NodesToKeep{$dest};
            $g->add_edge( $k, @{$dest} );
        }
    }
    return $g;
}

{
    my $ACTION_PATTERN   = qr{SAction->new\(\s*\{\s*'family'\s*,\s*['"](\w+)['"]};
    my $CODELET_PATTERN  = qr{new SCodelet\(\s*"(\w+)"};
    my $CODELET_PATTERN2 = qr{SCodelet->new\(\s*['"](\w+)['"]};
    my $THOUGHT_PATTERN  = qr{SThought::(\w+)->new};
    my $THOUGHT_PATTERN2 = qr{SThought->create\(};

    sub AddEdges {
        my ( $source_node, $code ) = @_;
        my $type;
        if ($source_node =~ /^Seqsee::SCF::/) {
            $type = [qw(fillcolor yellow shape trapezium)];
        } elsif ($source_node =~ /^SThought::/) {
            $type = [qw(fillcolor red shape trapezium)];
        } else {
            $type = [];
        }
        $Nodes{$source_node} = [@$type, label=>$source_node];

        #print $code;
        while ( $code =~ m/$ACTION_PATTERN/g ) {

            #$g->add_edge($source_node, "Seqsee::SCF::$1");
            push @{ $OutgoingEdges{$source_node} }, [$1];
            push @{ $IncomingEdges{$1} },           $source_node;
            print "$source_node =>(a) $1\n";
        }
        while ( $code =~ m/$CODELET_PATTERN/g ) {

            #$g->add_edge($source_node, "Seqsee::SCF::$1", style => 'dotted');
            push @{ $OutgoingEdges{$source_node} }, [$1];
            push @{ $IncomingEdges{$1} },           $source_node;
            print "$source_node =>(c) $1\n";
        }
        while ( $code =~ m/$CODELET_PATTERN2/g ) {

            #$g->add_edge($source_node, "Seqsee::SCF::$1", style => 'dotted');
            push @{ $OutgoingEdges{$source_node} }, [$1];
            push @{ $IncomingEdges{$1} },           $source_node;
            print "$source_node =>(c) $1\n";
        }
        while ( $code =~ m/$THOUGHT_PATTERN/g ) {

            #$g->add_edge($source_node, "SThought::$1");
            push @{ $OutgoingEdges{$source_node} }, [$1];
            push @{ $IncomingEdges{$1} },           $source_node;
            print "$source_node =>(t) $1\n";
        }
        while ( $code =~ m/$THOUGHT_PATTERN2/g ) {

            #$g->add_edge($source_node, "SThought::??");
            push @{ $OutgoingEdges{$source_node} }, [$1];
            push @{ $IncomingEdges{$1} },           $source_node;
            print "$source_node =>(t) (create)\n";
        }
    }
}

sub ProcessFile {
    my ($filename) = @_;
    open my $IN, "<", $filename or die "Could not open $filename";
    my $package;
    my $last_package;
    my $package_block;
    while ( my $line = <$IN> ) {
        if ( $line =~ m/package\s+(\S+)\s*;/o ) {
            $last_package = $package;
            $package      = $1;
            AddEdges( $last_package, $package_block ) if $last_package;
            $package_block = '';
        }
        else {
            $package_block .= $line;
        }
    }
    AddEdges( $package, $package_block );
}

sub CodeletFile {
    my ($filename) = @_;
    ProcessFile($filename);
    return;
    open my $in, "<", $filename;
    my @packages = map { m/ \[ package \] \s* (\S+)/x; $1 }
        grep {m/^ \s* \[ package \] /x} <$in>;
    close $in;
    for my $p (@packages) {
        ### Package: $p
        # print "Package: $p\n";
        my $sub_name = $p . '::run';
        my $run_sub  = \&$sub_name;
        my $as_str   = Dump($run_sub)->Out();

        # print $as_str;
        AddEdges( $p, $as_str );
    }
}

sub ThoughtFile {
    my ($file) = @_;
    ProcessFile($file);
    return;
    open my $in, "<", $file;
    my @packages = map { m/ \[ package \] \s* (\S+)/x; $1 }
        grep {m/^ \s* \[ package \] /x} <$in>;
    close $in;
    for my $p (@packages) {

        # print "Package: $p\n";
        ### Thought Package: $p
        my $sub_name = $p . '::get_actions';
        my $sub_ref  = \&$sub_name;
        my $as_str   = Dump($sub_ref)->Out();

        # print $as_str;
        AddEdges( $p, $as_str );
    }
}

sub GetFromSubroutine {
    my ( $node_name, $sub_name ) = @_;
    my $sub_ref = \&$sub_name;
    my $as_str  = Dump($sub_ref)->Out();

    # print $as_str;
    $Nodes{UnusualNode} = 1;
    push @{ $OutgoingEdges{UnusualNode} }, [ $node_name ];
    push @{ $IncomingEdges{$node_name} }, 'UnusualNode';
    AddEdges( $node_name, $as_str );
}

GetFromSubroutine( 'Background',       'Seqsee::do_background_activity' );
GetFromSubroutine( 'Stream',           'SStream::_think_the_current_thought' );
GetFromSubroutine( 'GetSomethingLike', 'SWorkspace::GetSomethingLike' );

for (<lib/SThought/*2.pm>) {
    ### Thoughtfile: $_
    ThoughtFile($_);
}

for (<lib/Seqsee::SCF/*2.pm>) {
    ### Codeletfile: $_
    CodeletFile($_);
}

for (<lib/Seqsee::Scripts/*2.pm>) {
    ### Scriptfile: $_
    CodeletFile($_);
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
$gv->zoom( -in => 400 );
MainLoop;

sub Recenter {
    my ($node) = @_;
    $gv->show(
        CreateSurroundingGraphViz( $node, 2 ),
        layout     => 'neato',
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

