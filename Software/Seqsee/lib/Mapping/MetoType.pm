package Mapping::MetoType;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;
use Carp;
use Class::Multimethods;

has category => (
  is       => 'rw',
  reader   => 'get_category',
  writer   => 'set_category',
  init_arg => 'category',
  required => 1,
  weak_ref => 0,
);

has name => (
  is       => 'rw',
  isa      => 'Str',
  reader   => 'get_name',
  writer   => 'set_name',
  init_arg => 'name',
  required => 0,
  weak_ref => 0,
);

# How the info lost is changing: key -> reln
has change_ref => (
  is       => 'rw',
  reader   => 'get_change_ref',
  writer   => 'set_change_ref',
  init_arg => 'change_ref',
  required => 1,
  weak_ref => 0,
);

sub create {
  my ( $package, $opts_ref ) = @_;
  my $string = join( ';',
    $opts_ref->{category}, $opts_ref->{name}, %{ $opts_ref->{change_ref} } );
  state %MEMO;
  return $MEMO{$string} ||= $package->new($opts_ref);
}

sub FlippedVersion {
  my ($self) = @_;
  my %new_change;
  while ( my ( $k, $v ) = each %{ $self->get_change_ref() } ) {
    $new_change{$k} = $v->FlippedVersion;
  }
  my $name = $self->get_name;
  my $new_name =
  ( $name =~ m#^flipped_# ) ? substr( $name, 8 ) :"flipped_$name";
  return Mapping::MetoType->create(
    {
      category   => $self->get_category(),
      name       => $new_name,
      change_ref => \%new_change
    }
  );
}

multimethod FindMapping => qw(SMetonymType SMetonymType) => sub {
  my ( $m1, $m2 ) = @_;
  my $cat1 = $m1->get_category;
  return unless $m2->get_category() eq $cat1;

  my $name1 = $m1->get_name;
  return unless $m2->get_name() eq $name1;

  # Now the meat: the info lost
  my $info_loss1 = $m1->get_info_loss;
  my $info_loss2 = $m2->get_info_loss;

  return unless scalar( keys %$info_loss1 ) == scalar( keys %$info_loss2 );
  my $change_ref = {};
  while ( my ( $k, $v ) = each %$info_loss1 ) {
    return unless exists $info_loss2->{$k};
    my $v2 = $info_loss2->{$k};
    my $rel = FindMapping( $v, $v2 ) or return;
    $change_ref->{$k} = $rel;
  }
  return Mapping::MetoType->create(
    {
      category   => $cat1,
      name       => $name1,
      change_ref => $change_ref,
    }
  );

};

multimethod ApplyMapping => qw(Mapping::MetoType SMetonymType) => sub {
  my ( $rel, $meto ) = @_;
  my $meto_info_loss = $meto->get_info_loss;

  my $rel_change_ref = $rel->get_change_ref;

  my $new_loss = {};
  while ( my ( $k, $v ) = each %$meto_info_loss ) {
    if ( not( exists $rel_change_ref->{$k} ) ) {
      $new_loss->{$k} = $v;
      next;
    }
    my $v2 = ApplyMapping( $rel_change_ref->{$k}, $v );
    $new_loss->{$k} = $v2;
  }
  return SMetonymType->new(
    {
      info_loss => $new_loss,
      name      => $meto->get_name,
      category  => $meto->get_category,
    }
  );

};

sub get_memory_dependencies {
  my ($self) = @_;
  return
  grep { ref($_) }
  ( $self->get_category(), values %{ $self->get_change_ref() } );
}

sub serialize {
  my ($self) = @_;
  return SLTM::encode( $self->get_category(), $self->get_name(),
    $self->get_change_ref() );
}

sub deserialize {
  my ( $package, $str ) = @_;
  my %opts;
  @opts{qw(category name change_ref)} = SLTM::decode($str);
  return $package->create( \%opts );
}

sub as_text {
  my ($self)   = @_;
  my $change   = SUtil::StringifyForCarp( $self->get_change_ref() );
  my $category = SUtil::StringifyForCarp( $self->get_category() );
  return "Mapping::MetoType(change=>$change, category=>$category)";
}

sub get_pure {
  return $_[0];
}

sub IsEffectivelyASamenessRelation {
  my ($self) = @_;
  while ( my ( $k, $v ) = each %{ $self->get_change_ref() } ) {
    return unless $v->IsEffectivelyASamenessRelation;
  }
  return 1;
}

__PACKAGE__->meta->make_immutable;
1;
