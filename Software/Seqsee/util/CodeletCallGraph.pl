use 5.10.0;
use strict;
use Tk;
use Carp;
use Smart::Comments;
use Getopt::Long;

use lib 'lib';
use S;

use Memoize; 
memoize 'family_to_name';
sub family_to_name {
    my ( $family ) = @_;
    no strict 'refs';
    my $name_string = 'Seqsee::SCF::' . $family . '::NAME';
    my $name = ${$name_string} // confess "No name for family $family? [I looked for $name_string]";
    return $name;
}

memoize 'thought_to_name';
sub thought_to_name {
    my ( $thought ) = @_;
    no strict 'refs';
    my $name_string = 'SThought::' . $thought . '::NAME';
    my $name = ${$name_string} // confess "No name for thought $thought? [I looked for $name_string]";
    return $name;
}

my %options;
GetOptions( \%options, "JustTrees!", "CodeletView!", "TreeNums!",
    "TimeStamps!" );

my $filename = 'codelet_tree.log';

# Format of that file:
# An unindented line indicates a "parent": a runnable run, or "Initial" or "Background"
# An indented line indicates an object being added to the coderack.

# For these hashes, keys are as follows:
# If for a codelet $C, "$C" is usually the key. BUT: perl reuses freed locations, and multiple
#   codelets make stringify to the same value. In that case, a "#n" is appended to the name, where
#   n=2,3, etc.
our %SeenCount;             # Needed to enable the #n naming described above.
our @ExecuteOrder;          # Runnables, in the order they were run.
our %ExecutedAtPosition;    # The same information as in @ExecuteOrder
our @CreationOrder;   # indices are "time steps", elements are lists of objects.
our %CreatedAtPosition;    # The same information as in @CreationOrder

our %Progeny;              # Immediate descendents, or objects it launched.
our %Parent;               # The parent.

our %Details;    # A string, with such details as urgency, type, whatever.
our %AlreadyPrinted;

our $TreeCount = 0;
our %TreeNum;

my $MW = new MainWindow();
$MW->focusmodel('active');
my $text = $MW->Scrolled(
    'Text',
    -scrollbars => 'se',
    -width      => 100,
    -height     => 40,
)->pack( -side => 'bottom' );
$text->focus();
$text->tagConfigure( 'was_executed', );
$text->tagConfigure( 'wasnt_executed',   -overstrike => 1 );
$text->tagConfigure( 'execute_position', -foreground => '#FF6666' );
$text->tagConfigure( 'Codelet',          -background => '#FFAAAA' );
$text->tagConfigure( 'Action',           -background => '#AAFFAA' );
$text->tagConfigure( 'Thought',          -background => '#AAAAFF' );
$text->tagConfigure( 'leaders',          -foreground => '#BBBBBB' );
$text->tagConfigure( 'treenum',          -foreground => '#6666FF' );
$text->tagConfigure(
    'Hilit',
    -font => '-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4',
    -underline => 1
);
$MW->bind(
    '<KeyPress-q>' => sub {
        exit;
    }
);

my $counter_of_executions = 0;
Phase_One();
if ( $options{CodeletView} ) {
    CodeletView_Phase_Two();
}
else {
    TreeView_Phase_Two();
}
my %ObjectTypesSeen;
while ( my ( $k, $v ) = each %Details ) {
    my $v_copy = $v;
    $v_copy =~ s#^\s*(\S+).*#$1#;
    if ( $k =~ /^SThought::([^=]*)/i ) {
        $ObjectTypesSeen{$1} = 'Thought';
    }
    else {
        $ObjectTypesSeen{$v_copy} = 'Codelet';
    }
}
## ObjectTypesSeen: %ObjectTypesSeen
my $frame = $MW->Frame()->pack( -side => 'top' );
my $combo1 = $frame->ComboEntry(
    -itemlist =>
      [ sort map { thought_to_name($_) } grep { $ObjectTypesSeen{$_} eq 'Thought' } keys %ObjectTypesSeen ],
    -width => 40,
)->pack( -side => 'left' );
$frame->Button(
    -text    => 'Search',
    -command => sub {
        MaybeHilit( $combo1->get() );
      }

)->pack( -side => 'left' );
my $combo2 = $frame->ComboEntry(
    -itemlist =>
      [ sort map { family_to_name($_) }grep { $ObjectTypesSeen{$_} ne 'Thought' } keys %ObjectTypesSeen ],
    -width => 40
)->pack( -side => 'left' );
$frame->Button(
    -text    => 'Search',
    -command => sub {
        MaybeHilit( $combo2->get() );
      }

)->pack( -side => 'left' );
$frame->Button(
    -text    => '<',
    -command => sub {
        ShowPrevious();
    }
)->pack( -side => 'left' );
$frame->Button(
    -text    => '>',
    -command => sub {
        ShowNext();
    }
)->pack( -side => 'left' );
MainLoop();

sub Phase_One {    # Read file, populate %SeenCount, @ExecuteOrder etc.
    open my $file, '<', $filename;
    my $parent;
    while ( my $line = <$file> ) {
        chomp($line);
        if ( $line =~ /^Initial/ or $line =~ /^Background/ ) {
            $parent = '';
        }
        elsif ( $line =~ /^Expunge/ ) {
            next;
        }
        elsif ( $line =~ /^\S+ \s* (\S+)/x ) {
            my $object = $1;
            if ( exists $SeenCount{$object} ) {

                #print "$object ====> " , $SeenCount{$object}, "\n";
                $object .= "#$SeenCount{$object}";
            }
            push @ExecuteOrder, $object;
            $ExecutedAtPosition{$object} = $counter_of_executions;
            $parent = $object;
            $counter_of_executions++;
        }
        else {
            $line =~ /^ \s+ (\S+) \s* (.*) /x;
            my ( $object, $details ) = ( $1, $2 );
            if ( exists $Details{$object} ) {
                $SeenCount{$object}++;
                $object .= "#$SeenCount{$object}";
            }
            $CreatedAtPosition{$object} = $counter_of_executions;
            push @{ $CreationOrder[$counter_of_executions] }, $object;
            $Details{$object} = $details;
            if ($parent) {
                push @{ $Progeny{$parent} }, $object;
                $TreeNum{$object} = $TreeNum{$parent};
            }
            else {
                $TreeCount++;
                $TreeNum{$object} = $TreeCount;
            }
            $Parent{$object} = $parent;
        }
    }
}

sub CodeletView_Phase_Two {
    for my $idx ( 0 .. $counter_of_executions - 1 ) {
        my $object = $ExecuteOrder[$idx];
        if ( $options{TreeNums} ) {
            $text->insert( 'end', sprintf( "[ tree #% 4d ] ", $TreeNum{$object} ),
                'treenum' );
        }
        $text->insert( 'end', sprintf( "% 5d", $idx + 1 ) . ' ) ',
            'execute_position' )
          unless $options{TimeStamps};
        $text->insert( 'end',
            CreateDisplay( $object, $idx + 1, $Details{$object} ), "\n" );
    }
}

sub TreeView_Phase_Two {    # Print out the trees.
    for my $idx ( 0 .. $counter_of_executions - 1 ) {
        my $object = $ExecuteOrder[$idx];
        if ( not $AlreadyPrinted{$object} ) {
            my $treenum = $TreeNum{$object};
            $text->insert( 'end', "Tree #$treenum", 'treenum', "\n" );
            PrintProgeny( $object, 0 );
            $text->insert( 'end', "\n" );
        }
        for ( @{ $CreationOrder[$idx] || [] } ) {
            if ( !exists( $ExecutedAtPosition{$_} ) and !$Parent{$_} ) {
                my $treenum = $TreeNum{$_};
                $text->insert( 'end', "Tree #$treenum", 'treenum', "\n" );
                PrintProgeny( $_, 0 );
                $text->insert( 'end', "\n" );
            }
        }

    }
}

sub CreateDisplay {
    my ( $object, $execute_position, $details ) = @_;
    my $executed_tag =
      defined($execute_position) ? "was_executed" : "wasnt_executed";
    $execute_position = sprintf( '% 5d', $execute_position )
      if defined($execute_position);

    my $creation_position = sprintf( '% 5d', $CreatedAtPosition{$object} - 1 );
    my @position_text =
      defined($execute_position)
      ? ( "[ created at: $creation_position run at: $execute_position ] ", ['execute_position'] )
      : ( "[ created at: $creation_position run at: never ]", "wasnt_executed" );

    if ( $options{JustTrees} or $options{CodeletView} ) {
        @position_text = () unless $options{TimeStamps};
        if ( $object =~ /^SCodelet/ or $object =~ /^SAction/ ) {
            $details =~ m/^\s*(\S+)/;
            my $name = family_to_name($1);
            return ( @position_text, $name, [$executed_tag] );
        }
        elsif ( $object =~ /^SThought::(.*?)=/ ) {
            return ( @position_text, thought_to_name($1), [$executed_tag] );
        }
    }
    else {
        if ( $object =~ /^SCodelet=ARRAY/ ) {
            return (
                @position_text,
                "Codelet $details",
                [ 'Codelet', $executed_tag ]
            );
        }
        elsif ( $object =~ /^SAction=SCALAR/ ) {
            return ( @position_text, "Action $details",
                [ 'Action', $executed_tag ] );
        }
        elsif ( $object =~ /^SThought::(.*?)=/ ) {
            return ( @position_text, "$1", [ 'Thought', $executed_tag ] );
        }
        else {
            confess("Funny object >$object<");
        }
    }
}

sub PrintProgeny {
    my ( $object, $depth ) = @_;

    my $execute_position = $ExecutedAtPosition{$object};

    if ($depth) {
        $text->insert( 'end', q{      } x ( $depth - 1 ) );
        $text->insert( 'end', q{  |-- }, 'leaders' );
    }

    $text->insert( 'end',
        CreateDisplay( $object, $execute_position, $Details{$object} ), "\n" );

    $AlreadyPrinted{$object} = 1;
    my @progeny = @{ $Progeny{$object} || [] };
    for (@progeny) {
        PrintProgeny( $_, $depth + 1 );
    }
}

{
    my @selected_ranges;
    my $currently_visible = 0;
    my $selection_count   = scalar(@selected_ranges) / 2;

    sub MaybeHilit {
        my ($string) = @_;
        $string =~ s#^\s*##;
        $string =~ s#\s*$##;
        return unless $string;

        print "Searching for >>$string<<\n";
        my @hilited_range = $text->tagRanges('Hilit');
        $text->tagRemove( 'Hilit', @hilited_range ) if @hilited_range > 1;
        $text->FindAll( '-exact', '-nocase', $string );
        @selected_ranges = $text->tagRanges('sel');
        print "Ranges: @selected_ranges\n";
        if ( @selected_ranges > 1 ) {
            $text->tagAdd( 'Hilit', @selected_ranges );
            $text->see( $selected_ranges[0] );
            $currently_visible = 0;
            $selection_count   = scalar(@selected_ranges) / 2;
        }
        else {
            $selection_count = 0;
        }
    }

    sub ShowNext {
        return if $selection_count == 0;

        $currently_visible++;
        $currently_visible = 0 if $currently_visible >= $selection_count;
        $text->see( $selected_ranges[ 2 * $currently_visible ] );
    }

    sub ShowPrevious {
        return if $selection_count == 0;

        $currently_visible--;
        $currently_visible = $selection_count - 1 if $currently_visible < 0;
        $text->see( $selected_ranges[ 2 * $currently_visible ] );
    }

}
