# Copyright (C) 2005-2008, Abhijit Mahabal.
use 5.010.001;
use strict;
use lib 'lib/';

use Carp::Seqsee;

use Config::Std;
use Getopt::Long;

use Carp;
use Tk::Carp qw{tkdie};
use Tk::Menustrip;
use Tk::ComboEntry;

use Smart::Comments;
use List::Util qw(min);
use UNIVERSAL::require;
use Sub::Installer;
use English qw(-no_match_vars );

use Sort::Key;
use Memoize;
use Exception::Class;
use Class::Multimethods;

use PerlIO;
use File::Slurp;
use Tk::ROText;

use Tk;
use SGUI;
use UI::Graphical;

use S;
use SUtil;
use Seqsee;

# variable: $OPTIONS_ref
#    final configuration hash
#
#    This is the result after passing through all the three stages
#  (config, command line, default)
#
#     This is passed on to initialize several others, and is thus very important
#
#  seed - the random number seed
#  log  - whether logging should be on or off
#  tk   - to tk or not
#  seq  - the sequence seqsee will deal with: an arrayref
#  update_interval - force redisplay after so many steps
#  interactive - for non-tk, this specifies interactivity

my $OPTIONS_ref = $Global::Options_ref = Seqsee::_read_config( Seqsee::_read_commandline() );
INITIALIZE();
GET_GOING();    # Potentially "infinite" loop

# method: INITIALIZE
# pulls all the pieces(logging, display etc) in, initializes
#   them
#
#context of call:
#   should get called only once, at the beginning

sub INITIALIZE {

    # Initialize Coderack
    SCoderack->clear();
    SCoderack->init($OPTIONS_ref);

    # Initialize Stream
    $Global::MainStream->clear();
    $Global::MainStream->init($OPTIONS_ref);

    # Initialize Workspace
    SWorkspace->clear();
    SWorkspace->init($OPTIONS_ref);

    # XXX(Board-it-up): [2006/10/23] Will need to pull memory in about here
    #    SNode->clear(); SNode->init( $OPTIONS_ref );

    # Initialize display
    init_display($OPTIONS_ref);

    my @seq = @{ $OPTIONS_ref->{seq} };
    my $tk  = $OPTIONS_ref->{tk};
    unless (@seq) {
        SGUI->ask_seq();
    }

    if ($Global::Feature{LTM}) {
      SLTM->Load('memory_dump.dat');
    }
    SLTM->init();
}

# method: GET_GOING
#      Goes into an infinite loop: what loop depends upon whether there is interaction, whether or not we are running Tk

sub GET_GOING {
    MainLoop();
}

# method: Interaction_continue
# Keeps taking steps until done
#
# The word Interaction is a misnomer

sub Interaction_continue {
    $Global::InterstepSleep = 0;
    return Seqsee::Interaction_step_n(
        {   n            => $OPTIONS_ref->{max_steps},
            update_after => $OPTIONS_ref->{update_interval},
            max_steps    => $OPTIONS_ref->{max_steps},
        }
    );
}

sub Interaction_step_n {
    return Seqsee::Interaction_step_n( { %{ $_[0] }, max_steps => $OPTIONS_ref->{max_steps}, } );
}

# method: Interaction_step
# A single step, with update display
#
#    return value:
#      True if program should stop

sub Interaction_step {
    return Seqsee::Interaction_step_n(
        {   n            => 1,
            update_after => 1,
            max_steps    => $OPTIONS_ref->{max_steps},
        }
    );
}

sub Interaction_crawl {
    my ($sleep_time_in_ms) = @_;
    $Global::InterstepSleep = $sleep_time_in_ms;
    return Seqsee::Interaction_step_n(
        {   n            => $OPTIONS_ref->{max_steps},
            update_after => 1,
            max_steps    => $OPTIONS_ref->{max_steps},
        }
    );
}

#method: init_display
# Sets up update_display() as well.

sub init_display {
    SGUI::setup($OPTIONS_ref);
    SGUI::Update();
    my $update_display_sub = sub { };

    my $commentary_displayer_debug = $Global::Feature{debug}
        ? sub {
        my ( $msg, $no_break, $add_newline ) = @_;
        my $newline = $add_newline ? "\n" : '';
        message( [ "[DEBUG: $msg]$newline", ['debug'] ], $no_break );
        }
        : sub { };

    "main"->install_sub( { debug_message => $commentary_displayer_debug } );

}
