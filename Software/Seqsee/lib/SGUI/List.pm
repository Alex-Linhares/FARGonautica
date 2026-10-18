package SGUI::List;
use strict;
use Config::Std;
use List::Util qw{min};

my $Margin;

BEGIN {
  read_config 'config/GUI_ws3.conf' => my %config;
  $Margin = $config{Layout}{Margin};

# No options currently.
#my %layout_options = %{ $config{ListLayout} };
#( $EntriesPerColumn, $ColumnCount, $MinGpHeightFraction, $MaxGpHeightFraction )
#    = @layout_options{ qw{EntriesPerColumn ColumnCount MinGpHeightFraction MaxGpHeightFraction}
#    };
}

sub DrawIt {
  my ($self) = @_;
  $self->PrepareForDrawing();
  my @entries_to_draw = $self->GetEntriesOnCurrentPage();
  my $Canvas          = $self->{Canvas};
  my $left            = $self->{EffectiveXOffset};
  my $top             = $self->{EffectiveYOffset};
  my $HeightPerRow    = $self->{HeightPerRow};

  my $itemvivify_ref = $self->{ItemVivify} = {};

  my $counter = 0;
  for my $entry (@entries_to_draw) {
    $itemvivify_ref->{$entry} = $entry;
    my $bar_color = ( $counter % 2 ) ? '#FFDDCC' :'#FFFFDD';
    my $id = $Canvas->createRectangle(
      $left, $top,
      $left + $self->{EffectiveWidth},
      $top + $HeightPerRow,
      -fill    => $bar_color,
      -outline => '',
      -tags    => [ $self, $entry, "$self-Clickable-Item" ],
    );
    $Canvas->lower($id);
    my @item_ids = $self->DrawOneItem( $Canvas, $left, $top, $entry );
    for (@item_ids) {
      $Canvas->addtag( $self,                  withtag => $_ );
      $Canvas->addtag( $entry,                 withtag => $_ );
      $Canvas->addtag( "$self-Clickable-Item", withtag => $_ );
    }
    $top += $HeightPerRow;

    $counter++;
  }

  $self->DrawBookKeeping();
}

sub DrawBookKeeping {
  my ($self) = @_;
  my $entries_count = $self->{EntriesCount};
  my $string;
  if ($entries_count) {
    $string = join( '',
      'Page #', $self->{PageNumber} + 1,
      '. Entries ', $self->{EntriesShownFrom} + 1,
      '-',
      $self->{EntriesShownTo} + 1,
      " of $entries_count Shown." );
  }
  else {
    $string = 'No entries to show.';
  }

  my $Canvas = $self->{Canvas};
  $Canvas->createText(
    $self->{XPageNumberPos}, $self->{YPageNumberPos},
    -text   => $string,
    -anchor => 'nw',
    -tags   => [$self],
  );
  my $triangle_x        = $self->{XTrianglePos};
  my $triangle_y_top    = $self->{YTopTriangle};
  my $triangle_y_bottom = $self->{YBottomTriangle};
  $Canvas->createRectangle(
    $triangle_x, $triangle_y_top, $triangle_x + 10,
    $triangle_y_top + 10,
    -tags => [ "$self-pagedown", $self ],
    -fill => '#FF0000',
  );
  $Canvas->createRectangle(
    $triangle_x, $triangle_y_bottom, $triangle_x + 10,
    $triangle_y_bottom + 10,
    -tags => [ "$self-pageup", $self ],
    -fill => '#FF0000',
  );
}

sub ReDrawIt {
  my ($self) = @_;
  print "In Redraw\n";
  $self->Clear();
  $self->DrawIt();
}

sub Setup {
  my ( $self, $Canvas, $XOffset, $YOffset, $Width, $Height ) = @_;
  $self->{Canvas}           = $Canvas;
  $self->{XOffset}          = $XOffset;
  $self->{YOffset}          = $YOffset;
  $self->{Width}            = $Width;
  $self->{Height}           = $Height;
  $self->{EffectiveHeight}  = $Height - 2 * $Margin;
  $self->{EffectiveWidth}   = $Width - 2 * $Margin;
  $self->{EffectiveXOffset} = $XOffset + $Margin;
  $self->{EffectiveYOffset} = $YOffset + $Margin;
  $self->{YPageNumberPos}   = $YOffset + $Height - $Margin;
  $self->{XPageNumberPos}   = $XOffset + $Margin;
  $self->{XTrianglePos}     = $XOffset + $Width - $Margin;
  $self->{YTopTriangle}     = $YOffset + $Margin;
  $self->{YBottomTriangle}  = $YOffset + $Height - $Margin;
  $self->{PageNumber}       = 0;
  $self->{EntriesPerPage} =
  int( $self->{EffectiveHeight} / $self->{HeightPerRow} );

  $Canvas->bind(
    "$self-pageup",
    '<1>' => sub {
      $self->{PageNumber}++;
      print "[$self]Pagenumber now: $self->{PageNumber}\n";
      $self->ReDrawIt();
    }
  );
  $Canvas->bind(
    "$self-pagedown",
    '<1>' => sub {
      $self->{PageNumber}-- if $self->{PageNumber};
      print "[$self]Pagenumber now: $self->{PageNumber}\n";
      $self->ReDrawIt();
    }
  );

  $Canvas->bind(
    "$self-Clickable-Item",
    '<1>' => sub {
      my @tags = $Canvas->gettags('current');
      my ($useful_tag) =
      grep { $_ ne 'current' and $_ ne $self and $_ ne "$self-Clickable-Item" }
      @tags;
      $self->ProcessClickOnItem( $self->{ItemVivify}{$useful_tag} );
    }
  );
}

sub GetEntriesOnCurrentPage {
  my ($self)           = @_;
  my $page_number      = $self->{PageNumber};
  my $entries_per_page = $self->{EntriesPerPage};

  my @all_entries   = $self->GetItemList();
  my $entries_count = scalar(@all_entries);

# print "Seeking entries on page# $page_number. There are $entries_count entries in all.\n";

  $self->{EntriesCount} = $entries_count;
  my $page_count =
  int( ( $entries_count + $entries_per_page - 1 ) / $entries_per_page );

  # print "\tThere are $page_count pages in all\n";
  if ( $page_number >= $page_count ) {

    # Not enough pages! page numbers start at 0.
    if ($page_count) {
      $self->{PageNumber} = $page_count - 1;    # Last page.
           # print "Pagecount for $self reset to $self->{PageNumber}\n";
      $self->{EntriesShownFrom} = $entries_per_page * ( $page_count - 1 );
      $self->{EntriesShownTo} = $entries_count - 1;
      return
      @all_entries[ $self->{EntriesShownFrom} .. $self->{EntriesShownTo} ];
    }
    else {
      $self->{EntriesShownFrom} = '--';
      $self->{EntriesShownTo}   = '--';
      return ();
    }
  }
  else {
    $self->{EntriesShownFrom} = $entries_per_page * $page_number;
    $self->{EntriesShownTo} =
    min( $entries_per_page * ( $page_number + 1 ) - 1, $entries_count - 1 );
    return @all_entries[ $self->{EntriesShownFrom} .. $self->{EntriesShownTo} ];
  }
}

sub ProcessClickOnItem {
  my ( $self, $item ) = @_;
  my $popup = ( $self->{POPUP_WIDGET} ||= $self->CreatePopupWidget() );
  print "WILL PROCESS >>$item<<\n";
  $self->{SelectedItem} = $item;
  $popup->deiconify();
  $Global::Break_Loop = 1;
  $popup->waitVisibility();
}

sub CreatePopupWidget {
  my ($self) = @_;
  return $self->{POPUP_WIDGET} if $self->{POPUP_WIDGET};
  my $Toplevel =
  $self->{Canvas}->Toplevel( -title => 'Actions for ' . ref($self) );
  $Toplevel->protocol(
    'WM_DELETE_WINDOW',
    sub {
      $Toplevel->withdraw;
    }
  );
  while ( my ( $k, $v ) = each %{ $self->{ActionButtons} ||= {} } ) {
    $Toplevel->Button(
      -text    => $k,
      -command => sub {
        $v->( $self->{SelectedItem} );
        Tk::Seqsee::Update();
        $Toplevel->withdraw;
      }
    )->pack( -side => 'top' );
  }

  $Toplevel->withdraw;
  return $Toplevel;
}

sub Clear {
  my ($self) = @_;
  $self->{Canvas}->delete($self);
}

sub PrepareForDrawing {

}

1;
