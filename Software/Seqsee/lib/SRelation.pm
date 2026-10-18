package SRelation;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;
use Try::Tiny;

use Class::Multimethods;
multimethod 'FindMapping';

has strength => (
  is       => 'rw',
  reader   => 'get_strength',
  writer   => 'set_strength',
  init_arg => 'strength',
  default  => 0,
);

has first => (
  is       => 'rw',
  isa      => 'Seqsee::Object',
  reader   => 'get_first',
  writer   => 'set_first',
  required => 1,
  init_arg => 'first',
);

has second => (
  is       => 'rw',
  isa      => 'Seqsee::Object',
  reader   => 'get_second',
  writer   => 'set_second',
  required => 1,
  init_arg => 'second',
);

has type => (
  is       => 'rw',
  isa      => 'Mapping',
  reader   => 'get_type',
  writer   => 'set_type',
  required => 1,
  weak_ref => 0,
);

has history_object => (
  is      => 'rw',
  isa     => 'SHistory',
  handles => [
    qw{ set_history get_history AddHistory search_history UnchangedSince GetAge history_as_text}
  ]
);

has direction_reln => (
  is     => 'rw',
  reader => 'get_direction_reln',
  writer => 'set_direction_reln',
);

has holeyness => (
  is     => 'rw',
  isa    => 'Bool',
  reader => 'get_holeyness',
  writer => 'set_holeyness',
);

sub BUILD {
  my $self = shift;
  my ( $f, $s ) = $self->get_ends();

# $self->set_direction_reln(FindMapping( $f->get_direction, $s->get_direction() ));
  $self->set_holeyness( SWorkspace->are_there_holes_here( $f, $s ) );
  $self->history_object( SHistory->new() );
}

sub get_ends {
  my ($self) = @_;
  return ( $self->get_first(), $self->get_second() );
}

sub get_extent {
  my ($self) = @_;
  return ( $self->get_first()->get_left_edge(),
    $self->get_second()->get_right_edge() );
}

sub are_ends_contiguous {
  my ($self) = @_;
  return $self->get_first()->get_right_edge() + 1 ==
  $self->get_second()->get_left_edge() ? 1 :0;
}

sub insert {
  my ($self) = @_;

  my ( $f, $s ) = $self->get_ends;
  my $reln = $f->get_relation($s);
  $reln->uninsert() if $reln;

  my $add_success;
  try { $add_success = SWorkspace->AddRelation($self) }
  catch { confess "Relation insertion error: $_ " };

  if ($add_success) {
    for ( $f, $s ) {
      $_->AddRelation($self);
    }
  }

  $self->UpdateStrength();
}

sub uninsert {
  my ($self) = @_;
  SWorkspace->RemoveRelation($self);
  for ( $self->get_ends ) {
    $_->RemoveRelation($self);
  }
}

sub get_direction {
  my ($self) = @_;
  my ( $la, $lb ) = map { $_->get_left_edge } $self->get_ends;
  if ( $la < $lb ) {
    return DIR::RIGHT();
  }
  elsif ( $lb < $la ) {
    return DIR::LEFT();
  }
  else {
    return DIR::UNKNOWN();
  }
}

sub get_span {
  my ($self) = @_;
  my ( $l, $r ) = $self->get_extent;
  return $r - $l + 1;
}

sub get_pure {
  my ($self) = @_;
  return $self->get_type;
}

sub SuggestCategory {
  my ($self) = @_;
  my $category = $self->get_type()->get_category();
  if ( $category eq $S::NUMBER ) {
    my $str = $self->get_type()->get_name();
    if ( $str eq "same" ) {
      return $S::SAMENESS;
    }
    elsif ( $str eq "succ" ) {
      return $S::ASCENDING;
    }
    elsif ( $str eq "pred" ) {
      return $S::DESCENDING;
    }
  }
  else {
    return SCategory::MappingBased->Create( $self->get_type() );
  }
}

sub SuggestCategoryForEnds {
  return;
}

sub UpdateStrength {
  my ($self) = @_;
  my $strength = 20 * SLTM::GetRealActivationsForOneConcept( $self->get_type );

  # Holeyness penalty
  $strength *= 0.8 if $self->get_holeyness;

  $strength = 100 if $strength > 100;
  $self->set_strength($strength);
}

sub as_text {
  my ($self)          = @_;
  my $first_location  = $self->get_first()->get_bounds_string();
  my $second_location = $self->get_second()->get_bounds_string();
  return "$first_location --> $second_location: " . $self->get_type()->as_text;
}

sub FlippedVersion {
  my ($self) = @_;
  my $flipped_type = $self->get_type()->FlippedVersion() // return;
  return SRelation->new(
    {
      first  => $self->get_second(),
      second => $self->get_first(),
      type   => $flipped_type,
    }
  );
}

__PACKAGE__->meta->make_immutable;
1;

