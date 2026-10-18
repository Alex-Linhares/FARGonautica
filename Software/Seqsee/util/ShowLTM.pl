use 5.10.0;
use strict;
use lib 'lib';
use English qw(-no_match_vars);
use S;
use SLinkActivation;

SLTM->Load('memory_dump.dat');
SLTM->init();

say "Nodes seen: $SLTM::NodeCount";

use Tk;
my $MW = new MainWindow();
$MW->focusmodel('active');

my $TB = $MW->Scrolled('Text', -scrollbars => 'e', -height => 40)->pack();
$TB->focus();
$TB->tagConfigure('Node', -font => '{Lucida Bright} -18 bold',
                  -foreground => '#0000FF',
                      );
$TB->tagConfigure('NodeType',);
$TB->tagConfigure('Depth', -foreground => '#FF0000');
$TB->tagConfigure('Significance',-foreground => '#FF0000');
$TB->tagConfigure('Stability',-foreground => '#FF0000');
$TB->tagConfigure('LinkType', -foreground => '#00FF00');
$TB->tagConfigure('LinkTarget', -foreground => '#0000FF');
$TB->bind(
    '<KeyPress-q>' => sub {
        exit;
    }
);
SLTM::DeleteInactiveLinks($SLinkActivation::Initial_Raw_Significance,
                          $SLinkActivation::Initial_Stability_Reciprocal
                              );
SLTM::NullifyInactiveNodes(SNodeActivation::Initial_Depth_Reciprocal);
SLTM::Show($TB);
MainLoop();

package SLTM;
use Sort::Key qw(keysort);
our (@MEMORY, @ACTIVATIONS, %LinkType2Str, $NodeCount, @OUT_LINKS, @LINKS);
sub DeleteInactiveLinks {
    my ( $significance, $stability_reciprocal ) = @_;
    say "DeleteInactiveLinks(@_)";
    for my $index ( 1..$NodeCount) {
        my $links_ref = $OUT_LINKS[$index];
        for my $type ( 1 .. LTM_TYPE_COUNT ) {
            my $links_of_this_type = $links_ref->[$type] || {};
            next unless %$links_of_this_type;
            for my $to_node ( keys %$links_of_this_type ) {
                my $link = $links_of_this_type->{$to_node};
                if ($link->[SLinkActivation::RAW_SIGNIFICANCE] <= $significance
                    and $link->[SLinkActivation::STABILITY_RECIPROCAL] >= $stability_reciprocal) {
                    delete $links_of_this_type->{$to_node};
                }
            }
        }
    }
}
sub NullifyInactiveNodes {
    my ( $depth_reciprocal ) = @_;
    LOOP: for my $index ( 1..$NodeCount) {
        my $links_ref = $OUT_LINKS[$index];
        my $activation = $ACTIVATIONS[$index];
        next LOOP if $activation->[SNodeActivation::DEPTH_RECIPROCAL] < $depth_reciprocal;
        for my $type ( 1 .. LTM_TYPE_COUNT ) {
            my $links_of_this_type = $links_ref->[$type] || {};
            next unless %$links_of_this_type;
            next LOOP;
        }
        $ACTIVATIONS[$index] = undef;
    }    
}
sub Show {
    my $TB = shift;
    my @showlist = keysort { ref($MEMORY[$_])} (1..$NodeCount);
    for my $index (@showlist) {
        next unless $ACTIVATIONS[$index];
        my ( $pure, $activation ) = ( $MEMORY[$index], $ACTIVATIONS[$index] );
        my ($depth_reciprocal) = ( $activation->[SNodeActivation::DEPTH_RECIPROCAL()], );
        $TB->insert('end', '_' x 30, '', "\n", '', ref($pure), 'NodeType', "\n", '',
                    $pure->as_text(), 'Node', "\n  depth = ", '',
                    int(1/$depth_reciprocal), 'Depth',
                    "\n"
                        );
        my $links_ref = $OUT_LINKS[$index];
        for my $type ( 1 .. LTM_TYPE_COUNT ) {
            my $links_of_this_type = $links_ref->[$type] || {};
            next unless (%$links_of_this_type and grep {$_} (values %$links_of_this_type));
            $TB->insert('end', "\t", '', $LinkType2Str{$type}, 'LinkType', "\n");
            while ( my ( $to_node, $link ) = each %$links_of_this_type ) {
                my $modifier_index = $link->[SLinkActivation::MODIFIER_NODE_INDEX];
                my ( $significance, $stability ) = (
                    $link->[SLinkActivation::RAW_SIGNIFICANCE],
                    $link->[SLinkActivation::STABILITY_RECIPROCAL]
                );

                my $modifier_name = '';
                if ($modifier_index) {
                    $modifier_name = $MEMORY[$modifier_index]->as_text();
                }
                my $to_name = $MEMORY[$to_node]->as_text();
                $TB->insert('end', "\t\t", '', $to_name, 'LinkTarget', "\n\t\t", '',
                            $significance, 'Significance', '  ', '',
                            int(1/$stability), 'Stability', "\n\n");
            }
        }
    }
}
