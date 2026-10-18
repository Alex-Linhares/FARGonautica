package SRule;
use 5.10.0;
use strict;
use Carp;
use Class::Std;
use Class::Multimethods;
use Smart::Comments;
use English qw(-no_match_vars);

multimethod 'ApplyMapping';
multimethod 'FindMapping';

my %Mapping_of : ATTR(:name<transform>);
my %Flipped_Mapping_of : ATTR(:name<flipped_transform>);

sub create {
  my ($package) = shift;
  createRule(@_);
}

multimethod createRule => qw(SRelation) => sub {
  my ($rel) = @_;
  return createRule( $rel->get_type() );
};

multimethod createRule => qw(Mapping) => sub {
  my ($transform) = @_;
  state %MEMO;
  my $flipped_transform = $transform->FlippedVersion() or return;
  confess unless $flipped_transform->CheckSanity();
  return $MEMO{$transform} ||= SRule->new(
    {
      transform         => $transform,
      flipped_transform => $flipped_transform,
    }
  );
};

sub CreateApplication {
  my ( $self, $opts_ref ) = @_;
  my $id        = ident $self;
  my $start     = $opts_ref->{start} or confess "need start";
  my $direction = $opts_ref->{direction} or confess "need direction";

  return SRuleApp->new(
    {
      rule      => $self,
      items     => [$start],
      direction => $direction,
    }
  );
}

sub CheckApplicability {
  my ( $self, $opts_ref ) = @_;
  my $id = ident $self;
  my $objects_ref = $opts_ref->{objects} or confess "need objects";

  my @objects_to_account_for = @$objects_ref;
  my @accounted_for          = shift(@objects_to_account_for);

  my $transform = $Mapping_of{$id};
  while (@objects_to_account_for) {
    my $last_accounted_for_object = $accounted_for[-1]->GetEffectiveObject;
    my $expected_next = ApplyMapping( $transform, $last_accounted_for_object )
    or return;
    my $actual_next = shift(@objects_to_account_for);
    return
    unless $expected_next->get_structure_string() eq
      $actual_next->GetEffectiveObject()->get_structure_string();
    push @accounted_for, $actual_next;
  }

  my $direction = SWorkspace::__FindObjectSetDirection(@accounted_for);
  return unless $direction->IsLeftOrRight();

  return SRuleApp->new(
    {
      rule      => $self,
      items     => \@accounted_for,
      direction => $direction
    }
  );
}

sub as_text {
  my ($self) = @_;
  my $id = ident $self;
  return "Rule: " . $Mapping_of{$id}->as_text();
}
1;
