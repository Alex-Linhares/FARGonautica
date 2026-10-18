package SGUI::List::Groups;
use strict;
use Class::Multimethods;
multimethod 'get_fringe_for';
our @ISA = qw{SGUI::List};

sub new {
  my ($package) = @_;
  my $self = bless {
    lock_x       => -8,
    strength_x   => 0,
    ends_x       => 50,
    categories_x => 140,
    Font => '-adobe-helvetica-bold-r-normal--9-140-100-100-p-105-iso8859-4',
    HeightPerRow => 15,
  }, $package;

  $self->{ActionButtons} = {
    Delete => sub {
      my ($group) = @_;
      SWorkspace::__DeleteGroup($group);
    },
    Lock => sub {
      my ($group) = @_;
      $group->set_is_locked_against_deletion(1);
    },
    Unlock => sub {
      my ($group) = @_;
      $group->set_is_locked_against_deletion(0);
    },
    ShowFollowers => sub {
      my ($group) = @_;
      my $weighted_set = SLTM::FindActiveFollowers($group);
      return unless $weighted_set->is_not_empty();
      my @followers = $weighted_set->get_elements();
      main::message(
        "Followers of "
        . $group->as_text() . ':'
        . join( ' and ', map { $_->as_text } @followers ),
        1
      );
    },
    History => sub {
      my ($group) = @_;
      main::message( $group->history_as_text() );
    },
    Fringe => sub {
      my ($group) = @_;
      my @fringe_parts = @{ get_fringe_for($group) };
      my $fringe_string;
      for (@fringe_parts) {
        my ( $component, $activation ) = @$_;
        my $ref = ref($component);
        my $text =
        UNIVERSAL::can( $component, 'as_text' )
        ? $component->as_text()
        :$component;
        $fringe_string .= "[$activation] $text; ";
      }
      main::message($fringe_string);
    },
    ActionFringe => sub {
      my ($group) = @_;
      my @actions = SThought->create($group)->get_actions();
      my $msg = join( "\n", map { $_->as_text } @actions );
      main::message($msg);
    },
  };
  return $self;
}

sub GetItemList {
  return SWorkspace->GetGroups();
}

sub DrawOneItem {
  my ( $self, $Canvas, $left, $top, $group ) = @_;
  my @item_ids;
  if ( $group->get_is_locked_against_deletion() ) {
    push @item_ids,
    $Canvas->createText(
      $left + $self->{lock_x}, $top,
      -anchor => 'nw',
      -font   => $self->{Font},
      -text   => 'L',
    );
  }
  push @item_ids, $Canvas->createText(
    $left + $self->{strength_x}, $top,
    -anchor => 'nw',
    -font   => $self->{Font},
    -text   => sprintf( "%5.2f", $group->get_strength() ),

    # -tags => [$self],
  );
  push @item_ids, $Canvas->createText(
    $left + $self->{ends_x}, $top,
    -anchor => 'nw',
    -font   => $self->{Font},
    -text   => $group->get_bounds_string(),

    # -tags => [$self],
  );

  my $categories_string = $group->get_categories_as_string();
  push @item_ids, $Canvas->createText(
    $left + $self->{categories_x}, $top,
    -anchor => 'nw',
    -font   => $self->{Font},
    -text   => $categories_string,

    # -tags => [$self],
  );
  return @item_ids;
}

1;
