use strict;
use Config::Std;
# use Smart::Comments;
use List::Util;
use Tk;
use Time::HiRes qw{time};
use List::Util;
use POSIX;

use constant {
    ASK_SEQUENCE_MAXIMUM_TERMS_IN_REPLY  => 10,
    ASK_SEQUENCE_REQUIRED_TERMS_IN_REPLY => 5,

    ASK_VARIANT_MAXIMUM_TERMS_IN_REPLY  => 20,
    ASK_VARIANT_REQUIRED_TERMS_IN_REPLY => 10,

    INPUT_CONFIGURATION_FILE => 'InputList3.txt',
    OUTPUT_FILE_NAME         => strftime( "%Y%B%d%p%I%M%S", localtime ),
    EACH_TERM_ENTRY_WIDTH    => 3,
    LABEL_CONFIG             => [ -foreground => 'blue' ],

    INITIAL_MESSAGE => q{
Thank you for choosing to participate in this experiment.

This experiment has two stages, the first stage being long, and the second stage much shorter.

Please take a few minutes to look at the accompanying handout, which has screenshots of what you will see, with instructions.

Click the button below after you have looked at the handout.
    },
    INITIAL_MESSAGE_COLOR => '#0000FF',
    INTERMEDDIATE_MESSAGE =>
        q{That completes stage I. If you have not already done so, please take a few minutes to look at stage II in the accompanying handout.},

    FINAL_MESSAGE =>
        q{That completes the experiment. Thank you for participating! If you have questions, please do not hesitate to ask. You can also email me at amahabal@indiana.edu},

    SEQUENCE_LABEL_CONFIG => [ -foreground => 'blue' ],
    HIDING_BUTTON_CONFIG  => [ -foreground => 'red' ],
};

use constant {
    ASK_SEQUENCE_NEXT_TERMS_MESSAGE =>
        q{Provide next few terms in the following sequence. At least }
        . ASK_SEQUENCE_REQUIRED_TERMS_IN_REPLY
        . q{ terms should be provided, but you can provide more if you deem appropriate.},
    ASK_VARIANT_NEXT_TERMS_MESSAGE =>
        'Provide another sequence like the following sequence. At least '
        . ASK_VARIANT_REQUIRED_TERMS_IN_REPLY
        . ' terms should be provided, but you can provide more if you deem appropriate. Be Creative!',
};

use constant {
    SEQUENCE_BUTTON_HIDING_ENTRY_TERMS_WIDTH => ( EACH_TERM_ENTRY_WIDTH + 3 )
        * ASK_SEQUENCE_MAXIMUM_TERMS_IN_REPLY,
    VARIANT_BUTTON_HIDING_ENTRY_TERMS_WIDTH => ( EACH_TERM_ENTRY_WIDTH + 3 )
        * ASK_VARIANT_MAXIMUM_TERMS_IN_REPLY,
    BUTTON_HIDING_ENTRY_TERMS_MESSAGE => 'I am ready to enter the answer',
};

use constant {
    SEQUENCE_BUTTON_HIDING_PROBLEM_SEQUENCE_WIDTH => 50,
    VARIANT_BUTTON_HIDING_PROBLEM_SEQUENCE_WIDTH  => VARIANT_BUTTON_HIDING_ENTRY_TERMS_WIDTH,
    BUTTON_HIDING_PROBLEM_SEQUENCE_MESSAGE        => 'Click to view sequence',

    NEXT_SEQUENCE_BUTTON_TEXT => 'Proceed to the next sequence',
};

use constant {
    HEADER_MESSAGE      => 'The Sequence Extension Experiment',
    FOOTER_MESSAGE      => 'Percepts and Concepts Lab, Indiana University',
    HEADER_FOOTER_WIDTH => List::Util::max(
        120,
        VARIANT_BUTTON_HIDING_ENTRY_TERMS_WIDTH,
        SEQUENCE_BUTTON_HIDING_PROBLEM_SEQUENCE_WIDTH + SEQUENCE_BUTTON_HIDING_ENTRY_TERMS_WIDTH + 5
    ),
};

use constant {
    SPLASH_TEXT_WIDTH                 => HEADER_FOOTER_WIDTH - 10,
    SPLASH_TEXT_HEIGHT                => 12,
    SPLASH_SCREEN_PROCEED_BUTTON_TEXT => q{I have read the handout, let's start the experiment},
};

#print "WIDTH: ", SEQUENCE_BUTTON_HIDING_ENTRY_TERMS_WIDTH, ' ',
#    VARIANT_BUTTON_HIDING_ENTRY_TERMS_WIDTH, "\n";

# exit;

my ( $extend_sequences_ref, $variation_sequences_ref ) = ReadInputConfig(INPUT_CONFIGURATION_FILE);
our $Position;
our %InfoToWriteOut;
$InfoToWriteOut{BOOK_KEEPING}{start_time} = OUTPUT_FILE_NAME;
### Sequences: $extend_sequences_ref, $variation_sequences_ref

my $MW                = new MainWindow();
my $HeaderFrame       = $MW->Frame()->pack( -side => 'top' );
my $FooterFrame       = $MW->Frame()->pack( -side => 'bottom' );
my $CentralFrameCover = $MW->Frame()->pack( -side => 'top', -fill => 'both', -expand => 1 );
my $CentralFrame      = $CentralFrameCover->Frame(
    -borderwidth => 2,
    -relief      => 'groove'
)->pack( -expand => 1, -fill => 'both' );

SetupHeader($HeaderFrame);
ShowInitialSplashScreen($CentralFrame);
SetupFooter($FooterFrame);

MainLoop();

sub ReadInputConfig {
    my ($filename) = @_;
    read_config $filename => my %SequenceConfig;
    my @sequences_extend;
    my @sequences_variation;

    while ( my ( $set, $values ) = each %SequenceConfig ) {
        my $is_extend;
        next if $set eq '';

        my $type = $values->{Type};
        my @sequences_in_set = @{ $values->{seq} } or die "No sequences for set $set!";

        if ( $set =~ /Extend/i ) {
            $is_extend = 1;
            @sequences_in_set = map { [ 'extend', $_ ] } @sequences_in_set;
        }
        elsif ( $set =~ /Variation/i ) {
            $is_extend = 0;
            @sequences_in_set = map { [ 'variation', $_ ] } @sequences_in_set;
        }
        else {
            die "Set <$set> neither 'Extend' nor 'Variation'";
        }

        if ( $type eq 'AskOne' ) {
            if ( $set =~ /Extend/i ) {
                push @sequences_extend, ChooseOneRandomly(@sequences_in_set);
            }
            else {
                push @sequences_variation, ChooseOneRandomly(@sequences_in_set);
            }
        }
        elsif ( $type eq 'AskAll' ) {
            if ( $set =~ /Extend/i ) {
                push @sequences_extend, @sequences_in_set;
            }
            else {
                push @sequences_variation, @sequences_in_set;
            }
        }
        else {
            die "Unknown or missing type for set $set\n";
        }
    }

    my $practice_extend = ['extend', 'This is the single practice sequence 1 1 1 1 1 1'];
    my $practice_var = ['variation', 'This is the single practice sequence 7 7 7 7 7 7 7 7'];

    return (
        [$practice_extend, List::Util::shuffle(@sequences_extend) ],
        [$practice_var, List::Util::shuffle(@sequences_variation) ]
    );
}

sub ChooseOneRandomly {
    my $count    = scalar(@_);
    my $position = int( rand() * $count );
    return $_[$position];
}

sub ShowInitialSplashScreen {
    my ($frame) = @_;
    ShowSplashScreen( $frame, \&InsertInitialSplashMessage, );
}

sub ShowIntermediateSplashScreen {

}

sub ShowSplashScreen {
    my ( $frame, $message_inserter_ref ) = @_;

    my $Text = $frame->Scrolled(
        'Text',
        -scrollbars => 'e',
        -width      => SPLASH_TEXT_WIDTH,
        -height     => SPLASH_TEXT_HEIGHT,
    )->pack( -side => 'top' );

    #InsertSplashMessage($Text);
    $message_inserter_ref->($Text);
    my $button;
    $button = $MW->Button(
        -text    => SPLASH_SCREEN_PROCEED_BUTTON_TEXT,
        -command => sub {
            $Text->destroy();
            $button->destroy();
            AskSequences();
        }
    )->pack( -side => 'top' );
    $button->focus();
}

sub InsertInitialSplashMessage {
    my ($text) = @_;
    $text->tagConfigure( 'message', -foreground => INITIAL_MESSAGE_COLOR );
    $text->insert( 'end', INITIAL_MESSAGE, ['message'] );
}

sub AskSequences {
    $MW->packPropagate(0);
    for my $sequence ( @{$extend_sequences_ref} ) {
        $Position++;
        our $GoOnToNextSequence = 0;
        my ( $type, $seq ) = @$sequence;

        AskSequence(
            {   sequence          => $seq,
                message           => ASK_SEQUENCE_NEXT_TERMS_MESSAGE,
                sequence_stacking => 'left',
                genre             => 'extend',
                max_next_terms    => ASK_SEQUENCE_MAXIMUM_TERMS_IN_REPLY,
                reqd_next_terms   => ASK_SEQUENCE_REQUIRED_TERMS_IN_REPLY,
            }
        );
        $MW->waitVariable( \$GoOnToNextSequence );
    }
    $MW->messageBox(
        -icon    => 'info',
        -title   => '',
        -type    => 'Ok',
        -message => INTERMEDDIATE_MESSAGE,
    );

    for my $sequence ( @{$variation_sequences_ref} ) {
        $Position++;
        our $GoOnToNextSequence = 0;
        my ( $type, $seq ) = @$sequence;
        AskSequence(
            {   sequence          => $seq,
                message           => ASK_VARIANT_NEXT_TERMS_MESSAGE,
                sequence_stacking => 'top',
                genre             => 'variation',
                max_next_terms    => ASK_VARIANT_MAXIMUM_TERMS_IN_REPLY,
                reqd_next_terms   => ASK_VARIANT_REQUIRED_TERMS_IN_REPLY,
            }
        );
        $MW->waitVariable( \$GoOnToNextSequence );
    }
    write_config %InfoToWriteOut, OUTPUT_FILE_NAME;

    $MW->messageBox(
        -icon    => 'info',
        -title   => '',
        -type    => 'Ok',
        -message => FINAL_MESSAGE,
    );

    exit;
}

sub AskSequence {
    my ($opts_ref) = @_;
    my %opts_ref = %$opts_ref;

    my ( $sequence, $message, $sequence_stacking, $genre, $max_terms, $reqd_terms )
        = @opts_ref{qw{sequence message sequence_stacking genre max_next_terms reqd_next_terms}};

    ## seq, msg: $sequence, $message
    # my $Text  = $MW->Text()->pack( -side  => 'top' );
    my $frame = $CentralFrame->Frame()->pack( -side => 'top', -expand => 1, -fill => 'both' );

    {
        my $label_frame = $frame->Frame()->pack( -side => 'top' );
        $label_frame->Label( -text => $message, @{ LABEL_CONFIG() } )->pack( -side => 'left' );
    }

    my ( $subframe_given_sequence, $subframe_for_extension );

    {
        my $sequences_frame = $frame->Frame( -relief => 'groove' )->pack(
            -side   => 'top',
            -expand => 1,
            -fill   => 'both',
        );
        $subframe_given_sequence = $sequences_frame->Frame()->pack(
            -side   => $sequence_stacking,
            -expand => 1,
            -fill   => 'both'
        );
        $subframe_for_extension = $sequences_frame->Frame()->pack(
            -side   => $sequence_stacking,
            -expand => 1,
            -fill   => 'both'
        );
    }

    my $sequence_to_show = FormatSequenceToShow( $sequence, $genre );
    my $reveal_button;
    my $reveal_button_for_extension;
    my $button_hiding_entry_terms_width =
        ( $genre eq 'extend' )
        ? SEQUENCE_BUTTON_HIDING_ENTRY_TERMS_WIDTH
        : VARIANT_BUTTON_HIDING_ENTRY_TERMS_WIDTH;

    my $button_hiding_problem_sequence_width =
        ( $genre eq 'extend' )
        ? SEQUENCE_BUTTON_HIDING_PROBLEM_SEQUENCE_WIDTH
        : VARIANT_BUTTON_HIDING_PROBLEM_SEQUENCE_WIDTH;

    # Times
    my $TimeOfSequenceDisplay;
    my $TimeOfUnderstanding;
    my @TimesOfChange;
    my $TimeOfFinish;
    my @next_terms_entered = map {''} 0 .. $max_terms - 1;
    my @TermEntryBoxes;

    my $DoneButton;
    $DoneButton = $CentralFrame->Button(
        -text    => NEXT_SEQUENCE_BUTTON_TEXT,
        -state   => 'disabled',
        -command => sub {
            our $GoOnToNextSequence;
            $frame->destroy();
            $DoneButton->destroy();

            my $TimeOfFinish      = time();
            my $UnderstandingTime = $TimeOfUnderstanding - $TimeOfSequenceDisplay;
            my @TypingTimes;
            $TypingTimes[0] = $TimesOfChange[0] ? $TimesOfChange[0] - $TimeOfUnderstanding : '?';
            for ( 1 .. $max_terms - 1 ) {
                if ( $TimesOfChange[$_] and $TimesOfChange[ $_ - 1 ] ) {
                    $TypingTimes[$_] = $TimesOfChange[$_] - $TimesOfChange[ $_ - 1 ];
                }
                else {
                    $TypingTimes[$_] = '?';
                }
            }

            ## Times: $UnderstandingTime, @TypingTimes
            ## Sequence: @next_terms_entered
            my $info = ( $InfoToWriteOut{"$genre $sequence"} = {} );
            $info->{position}           = $Position;
            $info->{time_to_understand} = $UnderstandingTime;
            $info->{typing_times}       = [@TypingTimes];
            $info->{total_typing_time}  = $TimeOfFinish - $TimeOfUnderstanding;
            $info->{genre}              = $genre;
            $info->{next_terms_entered} = [@next_terms_entered];
            $GoOnToNextSequence         = 1;
            }

    )->pack( -side => 'bottom', -expand => 1, -fill => 'x' );

    $reveal_button = $subframe_given_sequence->Button(
        -text       => BUTTON_HIDING_PROBLEM_SEQUENCE_MESSAGE,
        -width      => $button_hiding_problem_sequence_width,
        -foreground => 'red',
        -command    => sub {
            $reveal_button->destroy();
            my $sequence_label = $subframe_given_sequence->Label(
                -textvariable => \$sequence_to_show,
                -width        => $button_hiding_problem_sequence_width,
                @{ SEQUENCE_LABEL_CONFIG() },
            )->pack( -side => 'left' );
            $TimeOfSequenceDisplay       = time();
            $reveal_button_for_extension = $subframe_for_extension->Button(
                -text => BUTTON_HIDING_ENTRY_TERMS_MESSAGE,
                @{ HIDING_BUTTON_CONFIG() },
                -width   => $button_hiding_entry_terms_width,
                -command => sub {
                    $reveal_button_for_extension->destroy();
                    $TimeOfUnderstanding = time();
                    @TermEntryBoxes      = ();
                    for my $pos ( 0 .. $max_terms - 1 ) {
                        my $entry = $subframe_for_extension->Entry(
                            -textvariable    => \$next_terms_entered[$pos],
                            -width           => EACH_TERM_ENTRY_WIDTH,
                            -validate        => 'key',
                            -state           => 'disabled',
                            -validatecommand => sub {
                                my ( $new_value, $chars, $old_value ) = @_;
                                if ( length($old_value) == 0 ) {
                                    unless ( $new_value =~ /^\-?\d*$/ ) {
                                        return;
                                    }
                                }
                                if ( length($old_value) == 1 and $old_value eq '-' ) {
                                    unless ( $new_value =~ /^\-?\d+$/ ) {
                                        return;
                                    }
                                }
                                if ( $pos != $max_terms - 1 ) {
                                    $TermEntryBoxes[ $pos + 1 ]->configure( -state => 'normal' );
                                }
                                unless ( $new_value =~ /^\-?\d*$/ ) {
                                    $TermEntryBoxes[$pos]->focusNext();
                                    return 0;
                                }

                                #print "Key pressed. Value now: >$new_value<.\n";
                                $TimesOfChange[$pos] = time();
                                if ( $pos == $reqd_terms - 1 ) {
                                    $DoneButton->configure( -state => 'normal' );
                                }
                                1;

                                # print "VALIDATE COMMAND CALLED: $TimesOfChange[$pos]\n";
                            },
                        )->pack( -side => 'left' );
                        push @TermEntryBoxes, $entry;
                        $subframe_for_extension->Label( -text => ', ' )->pack( -side => 'left' );
                    }
                    $TermEntryBoxes[0]->focus();
                    $TermEntryBoxes[0]->configure( -state => 'normal' );
                },
            )->pack( -side => 'left' );
            $reveal_button_for_extension->focus();
        }
    )->pack( -side => 'left' );
    $reveal_button->focus();
}

sub SetupHeader {
    my ($frame) = @_;
    $frame->Label( -text => HEADER_MESSAGE, -width => HEADER_FOOTER_WIDTH )->pack( -side => 'top' );
}

sub SetupFooter {
    my ($frame) = @_;
    $frame->Label( -text => FOOTER_MESSAGE, -width => HEADER_FOOTER_WIDTH )->pack( -side => 'top' );
}

sub FormatSequenceToShow {
    my ( $sequence, $genre ) = @_;
    $sequence =~ s#^\s*##;
    $sequence =~ s#\s*$##;
    my @seq = split( /\s+/, $sequence );
    $sequence = join( ', ', @seq, '' );
    return $genre eq 'extend' ? sprintf( "%35s", $sequence ) : "$sequence ...";
}

1;
