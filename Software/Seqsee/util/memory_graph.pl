use strict;
use Smart::Comments;
use File::Slurp;
use GraphViz;

my $g = GraphViz->new(
    rankdir => 1,
    width   => 32,
    height  => 42,
    layout => 'neato',
);

my @MEMORY = ('!!!');

sub Load {
    my ($filename) = @_;
    my $string = read_file($filename);
    my ( $nodes, $links ) = split( q{#####}, $string );
    ## nodes: $nodes
    ### links: $links

    my $node_counter = 0;
    my @nodes = split( qr{=== \d+:}, $nodes );
    for (@nodes) {
        s#^\s*##;
        s#\s*$##;
        next if m#^$#;
        $node_counter++;
        my ( $type_and_sig, $val ) = split( /\n/, $_, 2 );
        my ( $type, $significance, $stability ) = split( /\s/, $type_and_sig, 3 );
        ## type, val: $type, $val
        # my $pure = $type->deserialize($val);
        ## pure: $pure
        # confess qq{Could not find pure: type='$type', val='$val'} unless defined($pure);
        $g->add_node( "node$node_counter", GetNodeAttributesAndLabel( $type, $val ), );

        # SetSignificanceAndStabilityForIndex( $index, $significance, $stability );
    }
    ## nodes: @nodes

    my @links = split( /\n+/, $links );
    ## links split: @links
    my $linklabelcount = 0;
    for (@links) {
        s#^\s*##;
        s#\s*$##;
        next if m#^$#;
        my ( $from, $to, $type, $modifier_index, $significance, $stability ) = split( /\s+/, $_ );
        if ($modifier_index) {
            $linklabelcount++;
            my $nodename = "linklabel$linklabelcount";
            my @link_attributes = GetLinkAttributes($type);
            $g->add_node($nodename,
                         label => '',
                         #color => 'black',
                         #shape => 'octagon',
                         style => 'invis',
                             );
            $g->add_edge("node$from", $nodename, @link_attributes);
            $g->add_edge($nodename, "node$to", @link_attributes);
            $g->add_edge($nodename, "node$modifier_index",
                         color => 'black',
                         style => 'dotted',
                             );
        } else {
            $g->add_edge( "node$from", "node$to",
                          GetLinkAttributes($type)
                              );
        }
        #my $activation = __InsertLinkUnlessPresent( $from, $to, $modifier_index, $type );
        #$activation->[ SActivation::RAW_SIGNIFICANCE() ]     = $significance;
        #$activation->[ SActivation::STABILITY_RECIPROCAL() ] = $stability;
    }

    ## links: $links

    # print "Would have loaded the file\n";
}

sub GetNodeAttributesAndLabel {
    my ( $type, $val ) = @_;
    if ( $type eq 'SLTM::Platonic' ) {
        return ( label => $val, color => 'yellow', style => 'filled' );
    }
    if ( $type eq 'SRelnType::Simple' ) {
        return ( label => $val, shape => 'diamond', color => 'blue', style => 'filled' );
    }
    if ( $type eq 'SCat::OfObj' ) {
        if ( $val =~ m/S::AD_HOC.*parts_count\s*=>\s*(\d+)/ ) {
            return (
                label => "Interlaced $1",
                shape => 'triangle',
                color => 'red',
                style => 'filled',
            );
        } elsif ($val =~ m/\$S::([a-zA-Z_0-9]+)$/) {
            return (
                label => "$1",
                shape => 'triangle',
                color => 'red',
                style => 'filled',
            );
        }
        else {
            return (
                label => "Cat ???",
                shape => 'triangle',
            );
        }
    }
    return ( label => "$type: ???" );
}

sub GetLinkAttributes {
    my ( $type ) = @_;
    if ($type == 1) {
        return (color=> 'blue', style =>  'dashed');
    }
    if ($type == 2) {
        return (color => 'red', style => 'solid');
    }
    if ($type == 3) {
        return (color => 'yellow', style => 'dotted');
    }
}


Load('memory_dump.dat');
open my $OUT, ">", 'memory.jpg';
binmode $OUT;
print {$OUT} $g->as_jpeg;
close $OUT;
system 'rundll32.exe c:\Windows\system32\shimgvw.dll,ImageView_Fullscreen D:\seqsee\memory.jpg';
