use strict;
use Text::Diff::Parser;

open my $PIPE, q{svn diff|} or die "Failed to open SVN";
my $diff = join( '', <$PIPE> );
close($PIPE);

my $parser = Text::Diff::Parser->new(
    # Simplify => 1,    # simplify the diff
);                    # strip 2 directories
$parser->parse($diff);

use Tk;
my $MW = new MainWindow();
$MW->focusmodel('active');

my $TB = $MW->Scrolled('Text', -scrollbars => 'e', -height => 40)->pack();
$TB->focus();
$TB->tagConfigure( 'file', -background => '#FFCCCC' );
$TB->tagConfigure( 'type',  -background => '#CCCCFF' );
$TB->bind(
    '<KeyPress-q>' => sub {
        exit;
    }
);
Show();
MainLoop();

sub Show {
    my $CurrentFile;
    foreach my $change ( $parser->changes ) {
        my $file = $change->filename1;
        if ($file ne $CurrentFile) {
            $CurrentFile = $file;
            $TB->insert('end', "$CurrentFile\n", ['file']);
        }
        my (@lines) = ($change->line1, $change->line2);
        my $type = $change->type;
        next unless $type =~ /\S/;
        $TB->insert('end', ' 'x4, [], $type, ['type']);
        if ($type eq 'ADD') {
            $TB->insert('end', " --> $lines[1]\n");
        } elsif ($type eq 'REMOVE') {
            $TB->insert('end', " $lines[0] --> \n");
        } elsif ($type eq 'MODIFY') {
            $TB->insert('end', " $lines[0] --> $lines[1]\n");
        } else {
            $TB->insert('end', "\n");
        }
        my $size = $change->size;
        foreach my $line ( 0 .. ( $size - 1 ) ) {
            $TB->insert('end', ' 'x8, '', '>', '', $change->text($line), '', "\n");
        }
    }
}
