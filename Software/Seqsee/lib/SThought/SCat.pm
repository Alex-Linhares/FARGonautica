package SThought::SCat;
use Moose;
extends 'SThought';
use Carp;
use Smart::Comments;
use English qw(-no_match_vars);
use Class::Multimethods;
use List::Util qw{min max};

our @actions_ret;
our $NAME = 'Focusing on a Category';

sub get_fringe {
  my ($self) = @_;
  my $core = $self->core();
  return [ [ $self->core(), 100 ] ];
}

sub get_actions {
  my ($self) = @_;
  my $core = $self->core();
  our @actions_ret = ();
  my $cat = $self->core();
  return if $cat->isa('SCategory::Interlaced');

  my @objects_of_cat = SWorkspace::__GetObjectsBelongingToCategory($cat);
  my @overlapping_sets =
  SWorkspace::__FindSetsOfObjectsWithOverlappingSubgroups(@objects_of_cat)
  or return;

  for my $set (@overlapping_sets) {
    my @part_names = map { $_->as_text } @$set;
    push @actions_ret,
    SCodelet->new( "MergeGroups", 100, { a => $set->[0], b => $set->[1] } );

    # main::message( "I should perhaps merge @part_names ", 1);
  }

  # main::message( "Just testing! thinking about $cat");
  return @actions_ret;
}

sub as_text {
  my $self = shift;
  return "Category " . $self->core()->as_text();
}

1;
