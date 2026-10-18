# Great! Synchronized to give a centered image!
use 5.10.0;
use strict;
use Smart::Comments;
use Tk;
use Carp;
use Sort::Key qw(rikeysort);
use Exception::Class ( 'Y_TOO_BIG' => {} );
use English qw(-no_match_vars );

#use Tk::JFileDialog;

use Getopt::Long;

my $GENERATE_FILE_NAME;
my $IS_MOUNTAIN;
my $LABEL_ELEMENTS;
my %options;
GetOptions( \%options, 'generate_filename!', 'dir=s', 'sequence=s',
    'filename=s' );

my %ARROW_ANCHORS;

sub GenerateFilename {
    my ( $dir, $seq ) = @_;
    $seq =~ tr#{}[]()<>#abcdefgh#;
    $seq =~ s#\s*([a-h])\s*#$1#g;
    $seq =~ s#^\s*##;
    $seq =~ s#\s*$##;
    $seq =~ s#\s+# #g;
    $seq =~ tr# #i#;
    my $ret = "$dir/$seq.eps";
    print $ret;
    return $ret;
}

sub GenerateFilename_cleaner {
    my ( $dir, $seq ) = @_;
    use 5.10.0;
    say $seq;
    $seq =~ tr#()[]{}<>|#    ef   #;
    say $seq;
    $seq =~ s#\s+#.#g;
    say $seq;
    $seq =~ s#\.*f\.*e\.*#_#g;
    $seq =~ s#\.*[ef]\.*#_#g;
    say $seq;
    $seq =~ s#[fe]##g;
    say $seq;
    $seq =~ s#^\.+##;
    $seq =~ s#\.+$##;

    if ($IS_MOUNTAIN) {
        $seq = "mountain_$seq";
    }

    my $ret = "$dir$seq.eps";
    print $ret;
    return $ret;
}

use constant {
    WIDTH      => 500,
    HEIGHT     => 55,
    BACKGROUND => 'white',
    PAGEHEIGHT => '1c',

#FONT                     => '-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4',
    FONT      => 'Lucida 14',
    MAX_TERMS => 32,
    MIN_TERMS => 3,

    LABEL_X_OFFSET => 0,
    LABEL_Y_OFFSET => 12,
    LABEL_FONT     => 'Lucida 8',
};

use constant { Y_CENTER => 3 + HEIGHT() / 2, };

use constant {
    GROUP_A_OPTIONS => [ -fill => '#DDDDDD' ],
    GROUP_B_OPTIONS => [ -fill => '#BBBBBB' ],
};

my $MW = new MainWindow();
my $WIDTH_PER_TERM;
my $Y_DELTA_PER_UNIT_SPAN;
my $OVAL_MINOR_AXIS_FRACTION = 15;
my $OVAL_MINOR_AXIS_MIN      = 10;

$MW->bind(
    '<KeyPress-q>' => sub {
        exit;
    }
);
$MW->focusmodel('active');

my $SequenceString = $options{sequence} || '';
my $SaveFilename;
my $Arrows;

my $frame1 = $MW->Frame()->pack();
my $frame2 = $MW->Frame()->pack();
my $frame3 = $MW->Frame()->pack();
my $Entry =
  $frame1->Entry( -width => 100, -textvariable => \$SequenceString )
  ->pack( -side => 'left' );
my $FileEntry =
  $frame2->Entry( -width => 100, -textvariable => \$SaveFilename )
  ->pack( -side => 'left' );
my $ArrowsEntry = $frame3->Entry(
    -width        => 100,
    -textvariable => \$Arrows
)->pack( -side => 'left' );

my $fileDialog;

$frame2->Button(
    -text    => '...',
    -command => sub {
        $SaveFilename = undef;
        $SaveFilename = $fileDialog->Show();
        if ( defined($SaveFilename) ) {
            $SaveFilename .= '.eps' unless $SaveFilename =~ m#\.eps$#;
        }
        else {
            $SaveFilename = '';
        }
    }
)->pack( -side => 'left' );
$frame2->Button(
    -text    => 'Save',
    -command => sub {
        Save() if $SaveFilename;
    }
)->pack( -side => 'left' );

$Entry->focus();

$Entry->bind( '<Return>' => \&UpdateImage, );
$Entry->bind(
    '<KeyPress-,>' => sub {
        $OVAL_MINOR_AXIS_FRACTION--;
        Show();
    }
);
$Entry->bind(
    '<KeyPress-.>' => sub {
        $OVAL_MINOR_AXIS_FRACTION++;
        Show();
    }
);

$Entry->bind(
    '<F10>' => sub {
        Save() if $SaveFilename;
    }
);

$frame1->Button(
    -text    => 'Draw',
    -command => sub {
        UpdateImage();
    }
)->pack();

$MW->Scale(
    -orient       => 'horizontal',
    -length       => 200,
    -from         => 0,
    -to           => 50,
    -tickinterval => 1,
    -variable     => \$OVAL_MINOR_AXIS_FRACTION,
)->pack();
$MW->Scale(
    -orient       => 'horizontal',
    -length       => 200,
    -from         => 0,
    -to           => 50,
    -tickinterval => 1,
    -variable     => \$OVAL_MINOR_AXIS_MIN,
)->pack();
$MW->CheckBox(
    -label        => 'mountain?',
    -textvariable => \$IS_MOUNTAIN,
)->pack();

$MW->CheckBox(
    -label        => 'label elements?',
    -textvariable => \$LABEL_ELEMENTS,
)->pack();
my $Canvas = $MW->Canvas(
    -height     => HEIGHT() - 3,
    -width      => WIDTH(),
    -background => BACKGROUND()
)->pack();
MainLoop();

sub Show {
    %ARROW_ANCHORS = ();
    if ($IS_MOUNTAIN) {
        ShowMountain();
        return;
    }
    my $string = $SequenceString;

    print "Will Parse: >$SequenceString<\n";
    my ( $Elements_ref, $GroupA_ref, $GroupB_ref ) = Parse($string);

    my $ElementsCount = scalar(@$Elements_ref);
    print "ElementsCount: $ElementsCount\n";
    confess "Too mant elements!" if $ElementsCount > MAX_TERMS;

    my $PretendWeHaveElements =
      ( $ElementsCount < MIN_TERMS ) ? MIN_TERMS : $ElementsCount;
    $WIDTH_PER_TERM = WIDTH / ( $PretendWeHaveElements + 1 );
    $Y_DELTA_PER_UNIT_SPAN
      = ( HEIGHT() * $OVAL_MINOR_AXIS_FRACTION * 0.1 ) /
      ( 2 * $PretendWeHaveElements ),

      $Canvas->delete('all');
    for (@$GroupA_ref) {
        DrawGroup( @$_, 3, GROUP_A_OPTIONS );
    }
    for (@$GroupB_ref) {
        DrawGroup( @$_, 0, GROUP_B_OPTIONS );
    }
    DrawElements($Elements_ref);
    DrawArrows();

#my $distance_from_edge = 2;
#$Canvas->createLine(0, $distance_from_edge, WIDTH(), $distance_from_edge);
#$Canvas->createLine(0, HEIGHT - $distance_from_edge, WIDTH(), HEIGHT - $distance_from_edge);
}

sub ShowMountain {
    my $string = $SequenceString;
    my ( $Elements_ref, $GroupA_ref, $GroupB_ref, $BarLines_ref ) =
      Parse($string);
    ### barines: @$BarLines_ref

    my @numbers = grep { /^\-?\d+$/ } @$Elements_ref;
    my $min     = List::Util::min(@numbers);
    my $max     = List::Util::max(@numbers);

    my $ElementsCount = scalar(@numbers);
    confess "Too mant elements!" if $ElementsCount > MAX_TERMS;

    my $PretendWeHaveElements =
      ( $ElementsCount < MIN_TERMS ) ? MIN_TERMS : $ElementsCount;
    $WIDTH_PER_TERM = WIDTH / ( $PretendWeHaveElements + 1 );

    my $initial_x_pos = 3 + $WIDTH_PER_TERM * 0.5;
    my $x_pos         = $initial_x_pos;
    $Canvas->delete('all');
    my $available_height = 2 * ( HEIGHT() - Y_CENTER() - 9 );
    my $height_bottom    = Y_CENTER() + $available_height / 2;
    my $height_top       = Y_CENTER() - $available_height / 2;
    my $ht_per_range =
      ( $max - $min ) ? $available_height / ( $max - $min + 2 ) : 0;
    for my $elt (@numbers) {
        my $y_pos = $height_bottom - ( $elt - $min ) * $ht_per_range;
        $Canvas->createText(
            $x_pos, $y_pos,
            -text   => $elt,
            -font   => FONT,
            -fill   => 'black',
            -anchor => 's',
        );
        $x_pos += $WIDTH_PER_TERM;
    }
    for my $barline (@$BarLines_ref) {
        my $x_pos = $initial_x_pos + ( $barline - 0.5 ) * $WIDTH_PER_TERM;
        $Canvas->createLine( $x_pos, $height_top, $x_pos, $height_bottom );
    }
}

sub DrawElements {
    my ($Elements_ref) = @_;
    my $label          = 'a';
    my $x_pos          = 3 + $WIDTH_PER_TERM * 0.5;
    my $count          = 0;
    for my $elt (@$Elements_ref) {
        $elt =~ s#\.# #g;
        $Canvas->createText(
            $x_pos, Y_CENTER,
            -text   => $elt,
            -font   => FONT,
            -fill   => 'black',
            -anchor => 'center',
        );
        if ($LABEL_ELEMENTS) {
            $Canvas->createText(
                $x_pos + LABEL_X_OFFSET,
                Y_CENTER + LABEL_Y_OFFSET,
                -text   => $label,
                -font   => LABEL_FONT,
                -fill   => 'black',
                -anchor => 'center',
            );
            $label++;
        }
        $ARROW_ANCHORS{"$count"} //= [ $x_pos, Y_CENTER - 10 ];
        $count++;
        $x_pos += $WIDTH_PER_TERM;
    }
}

sub DrawArrows {
    $Arrows =~ s#\s##g;
    return unless $Arrows;
    my @pieces = split( /;/, $Arrows );
    for my $piece (@pieces) {
        $piece =~ m#^ ([\d,]+) : ([\d,]+) $#x
          or confess
          "Cannot parse $Arrows (specifically, the piece >>$piece<<)";
        my ( $left, $right ) = ( $1, $2 );
        my $left_arrow_pos = $ARROW_ANCHORS{$left} or confess "No group $left";
        my $right_arrow_pos = $ARROW_ANCHORS{$right}
          or confess "No group $right";
        my ( $x1, $y1, $x2, $y2 ) = ( @$left_arrow_pos, @$right_arrow_pos );
        $Canvas->createLine(
            $x1, $y1,
            ( $x1 + $x2 ) / 2,
            Y_CENTER - 30,
            $x2, $y2,
            -arrowshape => [ 8, 12, 10 ],
            -smooth     => 1,
            -arrow      => 'last',
            -width      => 2,
            -fill       => 'black',
        );
    }
}

sub DrawGroup {
    my ( $start, $end, $extra_width, $options_ref ) = @_;
    my $span = $end - $start;
    my ( $x1, $x2 ) = (
        3 + $WIDTH_PER_TERM * ( $start + 0.1 ) - $extra_width,
        3 + $WIDTH_PER_TERM * ( $end - 0.1 ) + $extra_width
    );
    my $y_delta =
      $OVAL_MINOR_AXIS_MIN + $extra_width + $span * $Y_DELTA_PER_UNIT_SPAN;

    if ( $y_delta > Y_CENTER() - 7 ) {    # Center is off a bit.
        $OVAL_MINOR_AXIS_FRACTION--;
        Y_TOO_BIG->throw();
    }

    my ( $y1, $y2 ) = ( Y_CENTER() - $y_delta, Y_CENTER() + $y_delta );
    $Canvas->createOval( $x1, $y1, $x2, $y2, @$options_ref );
    my $upto = $end - 1;
    say "Drew >>$start,$upto<<";
    $ARROW_ANCHORS{"$start,$upto"} //= [ ( $x1 + $x2 ) / 2, $y1 ];
}

{
    my @GroupA;
    my @GroupB;
    my @BarLines;

    sub Parse {
        my ($string) = @_;
        @GroupA = @GroupB = ();
        my @tokens = Tokenize($string);
        print "tokens: ", join('---', @tokens), "\n";
        my @Elements = grep { m#\d|\w# } @tokens;
       # print "tokens: ", join('---', @tokens), "\n";
        ReadGroups( \@tokens, '{', '}', \@GroupA );
        ReadGroups( \@tokens, '[', ']', \@GroupA );
        ReadGroups( \@tokens, '(', ')', \@GroupB );
        ReadGroups( \@tokens, '<', '>', \@GroupB );
        @BarLines = ();
        ReadBarLines( \@tokens, \@BarLines );

        ### GroupA: @GroupA
        ### GroupB: @GroupB

        @GroupA = rikeysort { $_->[1] - $_->[0] } @GroupA;
        @GroupB = rikeysort { $_->[1] - $_->[0] } @GroupB;

        ### GroupA: @GroupA
        ### GroupB: @GroupB
        return ( \@Elements, \@GroupA, \@GroupB, \@BarLines );
    }

    sub Tokenize {
        my ($string) = @_;
        print $string, "\n";
        $string =~ s#,# #g;
        print $string, "\n";
        $string =~ s#([\(\)\[\]\<\>\{\}\|])# $1 #g;
        $string =~ s#^\s*##;
        $string =~ s#\s*$##;
        print $string, "\n";
        return split( /\s+/, $string );
    }

    sub ReadGroups {
        my ( $tokens_ref, $start_token, $end_token, $groups_set ) = @_;
        my $stack_size = 0;
        my @stack;
        my $element_count = 0;
        for my $token (@$tokens_ref) {
            if ( $token eq $start_token ) {
                $stack_size++;
                push @stack, $element_count;
            }
            elsif ( $token eq $end_token ) {
                die "Mismatched $end_token" unless $stack_size;
                my $group_start = pop(@stack);
                push @$groups_set, [ $group_start, $element_count ];
                $stack_size--;
            }
            elsif ( $token =~ m#^ \-? \d+ | \w #x ) {
                $element_count++;
            }
        }
        if ($stack_size) {
            die "Mismatched $start_token";
        }
    }

    sub ReadBarLines {
        my ( $tokens_ref, $barlines_ref ) = @_;
        ## In ReadBarLines:
        my $elements_seen = 0;
        for my $token (@$tokens_ref) {
            if ( $token eq '|' ) {
                ## Token: $token
                push @$barlines_ref, $elements_seen;
            }
            elsif ( $token =~ m#^ \-? \d+ #x ) {
                $elements_seen++;
            }
        }
    }
}

sub Save {
    my $filename = $SaveFilename;
    use 5.10.0;
    if ( -e $filename ) {
        say
"File exists, perhaps you already saved this? \n ***NOT SAVING AGAIN***";
        return;
    }
    $Canvas->postscript(
        -file       => $filename,
        -pageheight => PAGEHEIGHT,
        -height     => HEIGHT
    );

    # or confess "Failed to save $filename";
    say "Saved file $filename";
}

sub UpdateImage {
    eval { Show() };
    if ($EVAL_ERROR) {
        my $counter = 0;
        while ( $counter < 10 and $EVAL_ERROR ) {
            confess $EVAL_ERROR
              unless UNIVERSAL::isa( $EVAL_ERROR, 'Y_TOO_BIG' );
            eval { Show() };
        }
    }
    $SaveFilename =
      GenerateFilename_cleaner( '',  $SequenceString );
}
