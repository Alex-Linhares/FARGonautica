package SGUI::List::Categories;
use strict;
use Sort::Key qw{rikeysort};
our @ISA = qw{SGUI::List};

our %Cat2Objects;

sub new {
  my ($package) = @_;
  my $self = bless {
    HeightPerRow   => 40,
    HeightForImage => 35,
    WidthForImage  => 300,
    MaxGpHeight    => 15,
    MinGpHeight    => 5,
  }, $package;

  $self->{ActionButtons} = {
    DeleteAllOther => sub {
      my ($category) = @_;
      my %Keep;
      for my $gp ( SWorkspace::GetGroups() ) {
        next unless $gp->is_of_category_p($category);
        MarkDescendentsToKeep( \%Keep, $gp );
      }
      for my $gp ( SWorkspace::GetGroups() ) {
        SWorkspace::__DeleteGroup($gp) unless $Keep{$gp};
      }
    },
    AddBarlinesBefore => sub {
      my ($category) = @_;
      SWorkspace::__ClearBarLines();
      for my $gp ( SWorkspace::GetGroups() ) {
        next unless $gp->is_of_category_p($category);
        SWorkspace::__AddBarLines( $gp->get_left_edge );
      }
      SWorkspace::__RemoveGroupsCrossingBarLines();
    }
  };
  return $self;
}

sub MarkDescendentsToKeep {
  my ( $hash_ref, $group ) = @_;
  return if $group->isa('Seqsee::Element');
  $hash_ref->{$group} = 1;
  MarkDescendentsToKeep( $hash_ref, $_ ) for @$group;
}

sub PrepareForDrawing {
  my ($self) = @_;
  $self->{GroupHtPerUnitSpan} =
  ( $self->{MaxGpHeight} - $self->{MinGpHeight} ) /
  ( $SWorkspace::ElementCount || 1 );
  $self->{SpacePerElement} =
  $self->{WidthForImage} / ( $SWorkspace::ElementCount + 1 );
}

sub GetItemList {
  my @sorted_objects = rikeysort { $_->get_span() } SWorkspace::GetGroups();
  push @sorted_objects, SWorkspace::GetElements();
  %Cat2Objects = ();
  my %VivifyCats = ();

  for my $obj (@sorted_objects) {
    my $edges_ref = [ $obj->get_edges() ];
    for my $cat ( @{ $obj->get_categories() } ) {
      push @{ $Cat2Objects{$cat} }, $edges_ref;
      $VivifyCats{$cat} = $cat;
    }
  }
  return values(%VivifyCats);
}

sub DrawOneItem {
  my ( $self, $Canvas, $left, $top, $cat ) = @_;
  my @item_ids;

  my $WidthForImage      = $self->{WidthForImage};
  my $HeightForImage     = $self->{HeightForImage};
  my $MinGpHeight        = $self->{MinGpHeight};
  my $SpacePerElement    = $self->{SpacePerElement};
  my $GroupHtPerUnitSpan = $self->{GroupHtPerUnitSpan};

  push @item_ids,
  $Canvas->createText(
    $left + $WidthForImage + 5,
    $top + 0.5 * $HeightForImage,
    -anchor => 'w',
    -text   => $cat->get_name()
  );
  push @item_ids,
  $Canvas->createRectangle(
    $left, $top,
    $left + $WidthForImage,
    $top + $HeightForImage,
    -fill => '#EEEEEE'
  );

  # Draw ovals for instances
  my $CenterY = $top + $HeightForImage / 2;
  for my $o ( @{ $Cat2Objects{$cat} } ) {
    my ( $l, $r ) = @$o;
    my $span = $r - $l + 1;
    my ( $oval_left, $oval_right ) = (
      $left + $SpacePerElement * ( $l + 0.8 ),
      $left + $SpacePerElement * ( $r + 1.2 )
    );

    #main::message("GroupHtPerUnitSpan: $GroupHtPerUnitSpan");
    my ( $oval_top, $oval_bottom ) = (
      $CenterY - $MinGpHeight - $span * $GroupHtPerUnitSpan,
      $CenterY + $MinGpHeight + $span * $GroupHtPerUnitSpan
    );
    push @item_ids,
    $Canvas->createOval( $oval_left, $oval_top, $oval_right, $oval_bottom,
      -fill => '#0000FF' );
  }

  # Draw ovals for elements
  my ( $oval_top, $oval_bottom ) = ( $CenterY - 1, $CenterY + 1 );
  my ( $oval_left, $oval_right ) =
  ( $left + $SpacePerElement - 1, $left + $SpacePerElement + 1 );
  for ( 1 .. $SWorkspace::ElementCount ) {
    push @item_ids,
    $Canvas->createRectangle( $oval_left, $oval_top, $oval_right,
      $oval_bottom );
    $oval_left  += $SpacePerElement;
    $oval_right += $SpacePerElement;
  }
  return @item_ids;
}

1;
