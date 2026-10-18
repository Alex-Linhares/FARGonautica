use 5.10.0;
use strict;
use lib 'lib';

my $PAR_PREFIX = '';

# Added to ensure that distribution runs with Par
# If Seqsee.par missing, that is not an issue.
BEGIN {
    if (-e 'Seqsee.par' and not -e 'lib') {
        eval "use PAR 'Seqsee.par'";
        $PAR_PREFIX = ' -MPAR=Seqsee ';
    }
}

use Smart::Comments;
use Tk;
use Tk::ComboEntry;
use Tk::StatusBar;

use Global;
use Tk::Seqsee;
use English qw{-no_match_vars};

$SIG{INT} = sub {

};

$SIG{SIGHUP} = sub {
    exit;
};

my $MW        = new MainWindow();
my $SB        = $MW->StatusBar();
my $StatusMsg = "";
$SB->addLabel( -textvariable => \$StatusMsg );


INSERT_BUTTONS: {
    my $button_frame = $MW->Frame()->pack( -side => 'top' );
    my $buttons_per_row = 3;

    my @button_config = (
        [ Compile => CreateRunPerlScriptCommand('Compiler\Compile.pl') ],
        [   DeleteGenlib => sub {
                unlink(<lib/*.pm>);
                unlink(<lib/*/*.pm>);
                unlink(<lib/*/*.pm>);
                }
        ],
        [ CPAN     => CreateRunPerlScriptCommand(qw{-MCPAN -e shell}) ],
        [ SVNDiff  => CreateRunPerlScriptCommand('util\ShowDiff.pl') ],
        [ RunTests => CreateRunPerlScriptCommand( 'c:\Perl\bin\prove.bat', 't\*' ) ],
        [   ClearMemory => sub {
                open my $MEMORY_HANDLE, '>', 'memory_dump.dat';
                print {$MEMORY_HANDLE} ' ';
                close $MEMORY_HANDLE;
                }
        ],
        [   CodeletLevelView =>
                CreateRunPerlScriptCommand( 'util\CodeletCallGraph.pl', '--CodeletView' )
        ],
        [   TimestampedCodeletLevelView => CreateRunPerlScriptCommand(
                'util\CodeletCallGraph.pl', '--CodeletView', '--Timestamps'
            )
        ],
        [   TreeLevelView => CreateRunPerlScriptCommand( 'util\CodeletCallGraph.pl', '--JustTrees' )
        ],
        [   LabeledCodeletLevelView => CreateRunPerlScriptCommand(
                'util\CodeletCallGraph.pl', '--CodeletView', '--TreeNums'
            )
        ],
        [ TreeLevelDebugView => CreateRunPerlScriptCommand('util\CodeletCallGraph.pl') ],
        [ PressureView       => CreateRunPerlScriptCommand('util\PressureViewer.pl') ],
        [ ActivationViewer   => CreateRunPerlScriptCommand('util\ActivationsViewer.pl') ],
        [ ViewLTM   => CreateRunPerlScriptCommand('util\ShowLTM.pl') ],
    );

    my $button_count = 0;
    my $button_subframe;
    for my $button_info (@button_config) {
        my ( $text, $command ) = @{$button_info};
        ## t, c: $text, $command
        if ( $button_count % $buttons_per_row == 0 ) {
            $button_subframe = $button_frame->Frame()->pack( -side => 'top' );
        }
        $button_count++;
        $button_subframe->Button(
            -text    => $text,
            -command => $command,
            -width   => 30,
        )->pack( -side => 'left' );
    }
}


CreateFrameForLaunchingSeqsee();

INSERT_INPUT_REQUIRING_COMMANDS: {
    my @input_requiring_commands_config = (
        [   "Search",    #command name
            [            # inputs
                [   "SearchString",    #input name
                    "S",               # default
                ]
            ],
            [                          # Command constructor
                'CreateRunPerlScriptCommand',
                qw{util/Search.pl},
                qq{\$SearchString},
            ],
        ],
    );

    for my $cmd_config (@input_requiring_commands_config) {
        my ( $command_name, $inputs, $command ) = @$cmd_config;
        my $frame = $MW->LabFrame(
            -label     => $command_name,
            -labelside => 'acrosstop'
        )->pack( -side => 'top' );
        my %Inputs;
        for my $input_config ( @{$inputs} ) {
            my ( $name, $default ) = @$input_config;
            my $__this_var = $default;
            $Inputs{$name} = \$__this_var;
            print "SET \$Inputs{$name}\n";
            my $subframe = $frame->Frame()->pack( -side => 'left' );
            $subframe->Label( -text => $name )->pack( -side => 'left' );
            $subframe->Entry( -textvariable => \$__this_var )->pack( -side => 'left' );
        }

        $frame->Button(
            -text    => 'Go',
            -command => sub {
                my ( $cmd_constructor, @args ) = @{$command};
                ## old args: @args
                @args = map {
                    my $string = $_;
                    $string =~ s<\$([a-zA-Z_][a-zA-Z0-9_]*)>
                                           <print "SAW $1#", %Inputs, "\n";
                                           if (exists($Inputs{$1})) {
                                               print "$1 exists in \%Inputs\n";
                                               ${$Inputs{$1}}
                                           } else {
                                               print "$1 missing from \%Inputs\n";
                                               $1
                                           }>ge;
                    $string;
                } @args;
                ## new args: @args
                #<STDIN>;
                no strict;
                my $cmd = $cmd_constructor->(@args);
                $cmd->();
            }
        )->pack( -side => 'left' );
    }
}


MainLoop();

sub CreateRunSystemCommand {
    my (@cmd) = @_;
    return sub {
        # system("cls");
        $StatusMsg = "Running $cmd[1]";
        $SB->update();
        ### Running: @cmd
        my $any_error = system(@cmd) ? 1 : 0;

        my $message;
        if ($? == -1) {
            $message = "failed to execute: $!";
        }
        elsif ($? & 127) {
            $message = join('', "child died with signal %d, %s coredump\n",
                ($? & 127),  ($? & 128) ? 'with' : 'without');
        }


        my $ret = $any_error ? "Maybe there was an error ($message)" : "OK";
        $StatusMsg = "Finished $cmd[1]: $ret";

        if ($any_error) {
            use Tk::MsgBox;
            my $d = $MW->MsgBox(-title => "Error running $cmd[1]",
                                -type => 'ok',
                                -icon => 'warning',
                                -message => "There might have been an error while running $cmd[1]"
                                    )->Show();
        }

    };
}

sub CreateRunPerlScriptCommand {
    my (@args) = @_;
    my $executable = "perl";
    if ($OSNAME eq 'MSWin32') {
        $executable = $EXECUTABLE_NAME;
    } else {
        $args[0] =~ s#\\#/#g;
    }
    return CreateRunSystemCommand( $executable, @args );
}

sub CreateFrameForLaunchingSeqsee {
    my $frame = $MW->LabFrame(
        -label      => "Run Seqsee",
        -labelside  => 'acrosstop',
        -background => '#CCCCCC',
    )->pack( -side => 'top' );

    my %PossibleFeatures = %Global::PossibleFeatures;
    my %FeatureValues = map { $_ => 0 } keys %PossibleFeatures;

SHOW_FEATURES: {
        my $subframe = $frame->LabFrame(
            -label      => 'Optional Features',
            -labelside  => 'acrosstop',
            -background => '#CCCCCC',
        )->pack( -side => 'top' );
        my @features                  = sort keys %FeatureValues;
        my $space_left_in_current_row = 0;
        my $subsubframe;
        while (@features) {
            if ( !$space_left_in_current_row ) {
                $subsubframe = $subframe->Frame()->pack( -side => 'top' );
                $space_left_in_current_row = 6;
            }
            my $feature = shift(@features);
            $subsubframe->Checkbutton(
                -variable => \$FeatureValues{$feature},
                -text     => $feature,
            )->pack( -side => 'left' );
            $space_left_in_current_row--;
        }
    }

    my $single_frame = $frame->LabFrame(
        -label      => 'Run A Single Time',
        -labelside  => 'acrosstop',
        -background => '#CCCCCC',
    )->pack( -side => 'top' );

    my $multiple_frame = $frame->LabFrame(
        -label      => 'Run Multiple Time',
        -labelside  => 'acrosstop',
        -background => '#CCCCCC',
    )->pack( -side => 'top' );

    my $all_frame = $frame->LabFrame(
        -label      => 'Run All Sequences Multiple Times',
        -labelside  => 'acrosstop',
        -background => '#CCCCCC',
    )->pack( -side => 'top' );

SINGLE_RUN: {
        my $View;
        my $Sequence;
        my $SequenceEntry;
    ENTRY: {
            my $sequence_list_filename = 'config/sequence_list';
            open my $LIST, '<', $sequence_list_filename
                or die "Failed to open list $sequence_list_filename";
            my @sequence_list = <$LIST>;
            close $LIST;
            @sequence_list = grep {$_} map { s#^\s*##; s#\s*$##; $_ } @sequence_list;
            my $subframe = $single_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "Sequence: " )->pack( -side => 'left' );
            $SequenceEntry = $subframe->ComboEntry(
                -itemlist => \@sequence_list,
                -width    => 60
            )->pack( -side => 'left' );

        }
    VIEW: {
            my @view_options = @Tk::Seqsee::ViewOptions;
            my @options_to_list;
            my %names_to_values;

            my $counter = 0;
            for (@view_options) {
                my $name = $_->[0];
                $names_to_values{$name} = $counter++;
                push @options_to_list, $name;
            }
            $names_to_values{''} = 0;

            my $subframe = $single_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "View: " )->pack( -side => 'left' );
            $subframe->ComboEntry(
                -itemlist => \@options_to_list,
                -invoke   => sub {
                    my ($comboentry) = @_;
                    my $choice = $comboentry->get();
                    $View = $names_to_values{$choice};
                },
                -width    => 40,
                -showmenu => 1,
            )->pack( -side => 'left' );
        }
        $single_frame->Button(
            -text    => 'RUN ONCE',
            -command => sub {
                $Sequence = $SequenceEntry->get();
                my @features_turned_on        = grep { $FeatureValues{$_} } keys %FeatureValues;
                my @feature_related_arguments = map  {"-f=$_"} @features_turned_on;
                my @cmds                      = (
                    'Seqsee.pl',      qq{--seq=$Sequence},
                    qq{--view=$View}, @feature_related_arguments
                );
                my $subprocess_cmd = CreateRunPerlScriptCommand(@cmds);
                $subprocess_cmd->();
            }
        )->pack( -side => 'top' );
    }

MULTIPLE_RUN: {
        my $Sequence;
        my $SequenceEntry;
        my $TimesToRun = 10;
    ENTRY: {
            my $sequence_list_filename = 'config/sequence_list_for_multiple';
            open my $LIST, '<', $sequence_list_filename
                or die "Failed to open list $sequence_list_filename";
            my @sequence_list = <$LIST>;
            close $LIST;
            @sequence_list = grep {$_} map { s#^\s*##; s#\s*$##; $_ } @sequence_list;
            my $subframe = $multiple_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "Sequence: " )->pack( -side => 'left' );
            $SequenceEntry = $subframe->ComboEntry(
                -itemlist => \@sequence_list,
                -width    => 60
            )->pack( -side => 'left' );
        }
    TIMES: {
            my $subframe = $multiple_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "Number of Times to Run " )->pack( -side => 'left' );
            $subframe->Entry( -width => 5, -textvariable => \$TimesToRun )->pack( -side => 'left' );
        }
        $multiple_frame->Button(
            -text    => 'RUN MULTIPLE TIMES',
            -command => sub {
                $Sequence = $SequenceEntry->get();
                my @features_turned_on        = grep { $FeatureValues{$_} } keys %FeatureValues;
                my @feature_related_arguments = map  {"-f=$_"} @features_turned_on;
                my @cmds                      = (
                    'util/RunMultipleTimes.pl', qq{--seq=$Sequence},
                    qq{--times=$TimesToRun},    qq{--steps=25000},
                    @feature_related_arguments
                );
                my $subprocess_cmd = CreateRunPerlScriptCommand(@cmds);
                $subprocess_cmd->();
            }
        )->pack( -side => 'top' );
    }
ALL_MULTIPLE_TIMES: {
        my $TimesToRun    = 3;
        my $StepsToRunFor = 10000;
        my $FILENAME = 'config/sequence_list_for_testing';
    TIMES: {
            my $subframe = $all_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "Number of Times to Run " )->pack( -side => 'left' );
            $subframe->Entry( -width => 5, -textvariable => \$TimesToRun )->pack( -side => 'left' );
        }
    STEPS: {
            my $subframe = $all_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "Maximum Number of Steps " )->pack( -side => 'left' );
            $subframe->Entry( -width => 5, -textvariable => \$StepsToRunFor )
                ->pack( -side => 'left' );
        }
    FILENAME: {
            my $subframe = $all_frame->Frame()->pack( -side => 'top' );
            $subframe->Label( -text => "Filename with sequences " )->pack( -side => 'left' );
            $subframe->Entry( -width => 50, -textvariable => \$FILENAME )
                ->pack( -side => 'left' );
        }
        $all_frame->Button(
            -text    => 'RUN FOR ALL SEQUENCES',
            -command => sub {
                my @features_turned_on        = grep { $FeatureValues{$_} } keys %FeatureValues;
                my @feature_related_arguments = map  {"-f=$_"} @features_turned_on;
                my @cmds                      = (
                    'util/RunAllMultipleTimes.pl', qq{--times=$TimesToRun},
                    qq{--steps=$StepsToRunFor}, 
                    qq{--filename=$FILENAME},
                    @feature_related_arguments
                );
                my $subprocess_cmd = CreateRunPerlScriptCommand(@cmds);
                $subprocess_cmd->();
            }
        )->pack( -side => 'top' );

    }
}
