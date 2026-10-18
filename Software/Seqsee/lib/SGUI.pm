package SGUI;
use strict;
use warnings;
use Carp;
use Config::Std;
use English qw(-no_match_vars);
use Smart::Comments;

our $MW;
our $Coderack;
our $Stream;
our $Components;
our $Workspace;
our $Info;
our $Activations;

sub setup {
  my ($options_ref) = @_;
  my $gui_config_name = $options_ref->{gui_config} or confess;
  my $config_filename = "config/${gui_config_name}.conf";
  read_config $config_filename => my %config;
  CreateWidgets( \%config );
  SetupButtons( \%config );
  SetupBindings( \%config );
}

sub tags_to_aref {
  my ($href) = @_;
  my @ret = ();
  while ( my ( $k, $v ) = each %$href ) {
    push @ret, [ $k, split( /\s+/, $v ) ];
  }
  return \@ret;
}

my $check_and_accept_input_sequence = sub {
  my ( $v, $msg_label ) = @_;
  if ( $v =~ /[^\d\s,-]/ ) {
    $msg_label->configure( -text => "Illformed input: $v" );
    return;
  }
  return unless $v =~ /\d/;
  $v =~ s/^\s+//;
  $v =~ s/\s+$//;
  my @seq = split( /[,\s]+/, $v );
  print "Return pressed; Seq is: @seq";
  SWorkspace->clear();
  SCoderack->clear();
  $Global::MainStream->clear();

  SWorkspace->insert_elements(@seq);
  Update();
  return 1;
};

BEGIN {
  open my $in, 'config/sequence.list';
  our @seq = <$in>;
  chomp(@seq);
  @seq = grep { /\d/ } @seq;
}

sub ask_seq {
  my $top = $MW->Toplevel( -title => "Seqsee Sequence Entry" );
  $top->Label( -text => "Enter sequence(space separated): " )
  ->pack( -side => 'left' );
  $top->focusmodel('active');
  my $label = $top->Label( -text => '' )->pack( -side => 'bottom' );
  our @seq;
  my $seq;    # selected sequence

  my $f = $top->ComboEntry(
    -invoke => sub {
      my ($comboentry) = @_;
      $seq = $comboentry->get();
      if ( $check_and_accept_input_sequence->( $seq, $label ) ) {
        $top->destroy;
      }
    },

    # -list => ['1 1 2 1 2 3',
    #                   '1 7 2 8 3 9',
    #                   '1 7 1 2 8 1 2 3 9'],
    -list     => \@seq,
    -showmenu => 1,
    -width    => 40,
  )->pack( -side => 'top', -expand => 'true', -fill => 'both' );
  $f->bind(
    '<Return>' => sub {
      $seq = $f->get();
      if ( $check_and_accept_input_sequence->( $seq, $label ) ) {
        $top->destroy;
      }
    }
  );
  $f->focus();
  $top->Button(
    -text    => 'Go',
    -command => sub {
      $seq = $f->get();
      if ( $check_and_accept_input_sequence->( $seq, $label ) ) {
        $top->destroy;
      }
    }
  )->pack( -side => 'right' );
  $SGUI::Commentary->MessageRequiringNoResponse( 'New Sequence Started: ',
    [], "$seq\n" );

  #my $e = $top->Entry()->pack( -side => 'left' );
  #$e->focus();
  #$e->bind(
  #     '<Return>' => sub {
  #             my $v = $e->get();
  #             $v =~ s/^\s+//;
  #             $v =~ s/\s+$//;
  #             my @seq = split( /[,\s]+/, $v );
  #             print "Return pressed; Seq is: @seq";
  #             SWorkspace->clear();
  #             SWorkspace->insert_elements(@seq);
  #             Update();
  #             $top->destroy;
  #         }
  #);
}

sub ask_for_more_terms {
  my $top = $MW->Toplevel( -title => "Request for more terms" );
  $top->Label(
    -text => "I am stuck! Please provide more terms! (space separated): " )
  ->pack( -side => 'top' );
  $top->focusmodel('active');
  my $e = $top->Entry()->pack( -side => 'top' );
  $e->focus();
  $e->bind(
    '<Return>' => sub {
      my $v = $e->get();
      $v =~ s/^\s+//;
      $v =~ s/\s+$//;
      my @seq = split( /[,\s]+/, $v );
      print "Return pressed; Seq is: @seq";
      SWorkspace->insert_elements(@seq);
      Update();
      $top->destroy;
    }
  );
  return $top;
}

sub SetupButtons {
  my ($config_ref) = @_;

  my $parent_name = $config_ref->{frames}{buttons_widget} or confess;
  my $parent;
  { no strict; $parent = ${$parent_name}; }
  ## parent: $parent_name, $parent

  my $button_order = $config_ref->{frames}{button_order} or confess;
  my @buttons_names =
  map { s#^\s*##; s#\s*$##; s#\s+# #g; $_ } split( qq{\n}, $button_order );

  my $options_ref = $config_ref->{Button};
  my %options = ( defined $options_ref ) ? %$options_ref :();

  for (@buttons_names) {
    my $command_string = $config_ref->{buttons}{$_} or confess;
    my $command = eval qq{ sub {$command_string}; };
    confess if $EVAL_ERROR;
    $parent->Button( -text => $_, -command => $command, %options )
    ->pack( -side => 'left' );
  }
}

sub SetupBindings {
  my ($config_ref) = @_;
  my @names = keys %{ $config_ref->{bindings} };
  for my $name (@names) {
    ## $name: $name
    my $command_string = $config_ref->{bindings}{$name} or confess;
    my $command = eval qq{ sub {$command_string}; };
    confess if $EVAL_ERROR;
    $MW->bind( $name => $command );
  }
}

{
  my %SeqseeWidgets = map { $_ => 1 } qw( SCommentary);
  my %Updatable     = map { $_ => 1 } qw(Seqsee);
  my @to_Update     = ();

  sub CreateWidgets {
    my ($config_ref) = @_;

    my $MW_options = $config_ref->{MainWindow} || {};
    $MW = new MainWindow(%$MW_options);

    if ( exists $config_ref->{frames}{geometry} ) {
      $MW->geometry( $config_ref->{frames}{geometry} );
    }

    my $frames_string = $config_ref->{frames}{frames} or confess;
    my @lines = split qq{\n}, $frames_string;
    for my $line (@lines) {
      $line =~ s#^\s*##;
      $line =~ s#\s*$##;
      my ( $name, $parent, $widget_type, $position, @rest ) =
      split( /\s+/, $line );
      require "Tk/$widget_type.pm";
      ## In CreateWidgets: $name, $parent, $widget_type, $position, @rest
      no strict;
      my $widget;

      $widget =
      ${$parent}
      ->$widget_type( GetWidgetOptions( $widget_type, $config_ref, @rest ) );

      $widget->pack( -side => $position ) if ( $widget_type ne 'Toplevel' );

      ${$name} = $widget unless $name eq '_';

      $Updatable{$widget_type} ||= ${ 'Tk::' . $widget_type . '::UPDATABLE' };

      if ( $Updatable{$widget_type} ) {
        push @to_Update, $widget;
      }
    }
    $MW->focus();
  }

  sub GetWidgetOptions {
    my ( $type, $config_ref, @rest ) = @_;
    if ( exists $SeqseeWidgets{$type} ) {
      exists( $config_ref->{$type} ) or confess "Missing config for $type";
      my %ret = %{ $config_ref->{$type} };
      for ( values %ret ) {
        next unless m/^!(.*)/;
        $_ = eval($1);
      }
      my $tags_config = $config_ref->{ $type . '_tags' };
      if ( defined $tags_config ) {
        $ret{'-tags_provided'} = tags_to_aref($tags_config);
      }
      return ( %ret, @rest );

    }
    else {
      my $extra_config = $config_ref->{$type};
      my %extra_config = ( defined $extra_config ) ? %$extra_config :();
      for ( values %extra_config ) {
        next unless m/^!(.*)/;
        $_ = eval($1);
      }
      return ( %extra_config, @rest );
    }
  }

  sub Update {
    for (@to_Update) {
      $_->Update();
    }

    $MW->update();
  }

}

#sub Tk::Error {
#    carp "Tk Error! @_";
#}

1;
