package SGUI::List::Rules;
use strict;
our @ISA = qw{SGUI::List};

sub new {
  my ($package) = @_;
  my $self = bless { HeightPerRow => 15 }, $package;
  return $self;
}

sub GetItemList {
  return ( SRule->GetListOfSimpleRules(), SRule->GetListOfCompoundRules() );
}

sub DrawOneItem {
  my ( $self, $Canvas, $left, $top, $item ) = @_;
  my @item_ids;
  push @item_ids,
  $Canvas->createText(
    $left, $top,
    -anchor => 'nw',
    -text   => $item->as_text(),
  );

  return @item_ids;
}

1;
