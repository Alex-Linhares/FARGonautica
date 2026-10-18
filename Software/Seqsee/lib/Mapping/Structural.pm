package Mapping::Structural;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Carp;
use Smart::Comments;

use Memoize;
extends 'Mapping';

has category => (
    is         => 'rw',
    reader     => 'get_category',
    writer     => 'set_category',
    init_arg   => 'category',
    required   => 1,
    weak_ref   => 0,
);

has meto_mode => (
    is         => 'rw',
    reader     => 'get_meto_mode',
    writer     => 'set_meto_mode',
    init_arg   => 'meto_mode',
    required   => 1,
    weak_ref   => 0,
);

has position_reln => (
    is         => 'rw',
    reader     => 'get_position_reln',
    writer     => 'set_position_reln',
    init_arg   => 'position_reln',
    required   => 1,
    weak_ref   => 0,
);

has metonymy_reln => (
    is         => 'rw',
    reader     => 'get_metonymy_reln',
    writer     => 'set_metonymy_reln',
    init_arg   => 'metonymy_reln',
    required   => 1,
    weak_ref   => 0,
);

has direction_reln => (
    is         => 'rw',
    reader     => 'get_direction_reln',
    writer     => 'set_direction_reln',
    init_arg   => 'direction_reln',
    required   => 1,
    weak_ref   => 0,
);

has slippages => (
  # Key: new attribute, Val: old attribute.
  traits => ['Hash'],
  is        => 'ro',
  isa       => 'HashRef',
  default   => sub { {} },
  reader => 'get_slippages',
  handles => {

  }
);

has changed_bindings => (
  # Key: new attribute. Val: the transform.
  traits => ['Hash'],
  is        => 'ro',
  isa       => 'HashRef',
  default   => sub { {} },
  reader  => 'get_changed_bindings',
  handles => {

  }
);

sub create {
  my ( $package, $opts_ref ) = @_;
  my $meto_mode = $opts_ref->{meto_mode} or confess "need meto_mode";
  if ( not $meto_mode->is_metonymy_present() ) {
    $opts_ref->{metonymy_reln} = 'x';
  }
  if ( not $meto_mode->is_position_relevant() ) {
    $opts_ref->{position_reln} = 'x';
  }

  my $string = join(
    '#',
    (
      map { $opts_ref->{$_} }
      qw(category meto_mode metonymy_reln position_reln direction_reln)
    ),
    join( ';',
      SUtil::hash_sorted_as_array( %{ $opts_ref->{changed_bindings} } ) ),
    join( ';', SUtil::hash_sorted_as_array( %{ $opts_ref->{slippages} } ) ),
  );
  state %MEMO;
  return $MEMO{$string} ||= $package->new($opts_ref);
}

sub FlippedVersion {
  my ($self) = @_;

  my $new_slippages = _FlipSlippages( $self->get_slippages() ) // return;
  my $new_bindings_change =
  _FlipChangedBindings( $self->get_changed_bindings(), $self->get_slippages() )
  // return;

  ## new_slippages: $new_slippages
  ## new_bindings_change: $new_bindings_change

  my ( $new_position_reln, $new_metonymy_reln, $new_direction_reln );
  $new_position_reln = $self->get_position_reln()->FlippedVersion()
  if ref( $self->get_position_reln() );
  $new_metonymy_reln = $self->get_metonymy_reln()->FlippedVersion()
  if ref( $self->get_metonymy_reln() );
  $new_direction_reln = $self->get_direction_reln()->FlippedVersion()
  if ref( $self->get_direction_reln() );

  my $flipped = Mapping::Structural->create(
    {
      category         => $self->get_category(),
      meto_mode        => $self->get_meto_mode(),
      position_reln    => $new_position_reln,
      metonymy_reln    => $new_metonymy_reln,
      direction_reln   => $new_direction_reln,
      changed_bindings => $new_bindings_change,
      slippages        => $new_slippages,
    }
  );
  $flipped->CheckSanity()
  or main::message( "Flip problematic!" . join( ';', %$new_bindings_change ) );
  return $flipped;
}

memoize('FlippedVersion');

sub _FlipChangedBindings {
  my ( $old_bindings, $slippages ) = @_;
  my %new_bindings;
  my %old_bindings = %$old_bindings;
  while ( my ( $k, $v ) = each %old_bindings ) {
    my $new_v = $v->FlippedVersion() // return;
    my $new_k;
    if ( exists $slippages->{$k} ) {
      $new_k = $slippages->{$k};
    }
    else {
      $new_k = $k;
    }
    $new_bindings{$new_k} = $new_v;
  }
  return \%new_bindings;
}

sub _FlipSlippages {
  my ($old_slippages) = @_;
  my %new_slippages;
  my %keys_seen;
  my %old_slippages = %$old_slippages;
  while ( my ( $k, $v ) = each %old_slippages ) {
    return if $keys_seen{$v}++;
    $new_slippages{$v} = $k;
  }
  return \%new_slippages;
}

sub get_pure {
  return $_[0];
}

sub IsEffectivelyASamenessRelation {
  my ($self) = @_;
  while ( my ( $k, $v ) = each %{ $self->get_slippages() } ) {
    return unless $k eq $v;
  }
  while ( my ( $k, $v ) = each %{ $self->get_changed_bindings() } ) {
    return unless $v->IsEffectivelyASamenessRelation();
  }
  if ( $self->get_meto_mode()->is_metonymy_present() ) {
    return unless $self->get_metonymy_reln()->IsEffectivelyASamenessRelation();
    return unless $self->get_direction_reln()->IsEffectivelyASamenessRelation();
    if ( $self->get_meto_mode()->is_position_relevant() ) {
      return unless $self->get_position_reln()->IsEffectivelyASamenessRelation();
    }
  }

  return 1;
}

sub get_memory_dependencies {
  my ($self) = @_;

  return grep { ref($_) } (
    $self->get_category(),       $self->get_meto_mode(),
    $self->get_position_reln(),  $self->get_metonymy_reln(),
    $self->get_direction_reln(), values %{ $self->get_changed_bindings() }
  );
}

sub as_text {
  my ($self)           = @_;
  my $cat_name         = $self->get_category()->get_name();
  my $changed_bindings = $self->get_changed_bindings();
  my $changed_bindings_string;
  my $metonymy_presence = $self->get_meto_mode()->is_metonymy_present() ? '*' :'';
  my %slippages = %{ $self->get_slippages() };
  if (%slippages) {

    while ( my ( $new, $old ) = each %slippages ) {
      my $reln_for_this_attribute = $changed_bindings->{$new};
      if ($reln_for_this_attribute) {
        $changed_bindings_string .=
        "($new => " . $reln_for_this_attribute->as_text();
        $changed_bindings_string .= " (of $old)";
        $changed_bindings_string .= ');';
      }
      else {
        if ( $old ne $new ) {
          $changed_bindings_string .= "new $new is the earlier $old;";
        }
      }
    }
  }
  else {
    while ( my ( $k, $v ) = each %$changed_bindings ) {
      $changed_bindings_string .= "$k => " . $v->as_text() . ";";
    }
  }
  chop($changed_bindings_string);
  return "[$cat_name$metonymy_presence] $changed_bindings_string";
}

sub serialize {
  my ($self) = @_;
  return SLTM::encode(
    $self->get_category(),      $self->get_meto_mode(),
    $self->get_metonymy_reln(), $self->get_direction_reln(),
    $self->get_position_reln(), $self->get_changed_bindings(),
    $self->get_slippages()
  );
}

sub deserialize {
  my ( $package, $string ) = @_;
  my %opts;
  @opts{
    qw{category meto_mode metonymy_reln direction_reln position_reln changed_bindings slippages}
  } = SLTM::decode($string);
  $package->create( \%opts );
}

sub get_complexity {
  my ($self) = @_;

  my $complexity_of_category;
  given ( $self->get_category() ) {
    when ( [ $S::ASCENDING, $S::DESCENDING, $S::SAMENESS ] ) {
      $complexity_of_category = 0.1
    }
    when ( $_->isa('SCategory::Interlaced') ) {
      $complexity_of_category = 0.1 * $_->get_parts_count();
    }
    default { $complexity_of_category = 0.3; }
  }

  my $total_complexity = $complexity_of_category;
  given ( $self->get_meto_mode() ) {
    when ($METO_MODE::NONE) { }
    default { $total_complexity += 0.2; }
  }

  for ( values %{ $self->get_changed_bindings() } ) {
    $total_complexity += $_->get_complexity();
  }

  my %slippages = %{ $self->get_slippages() };
  while ( my ( $k, $v ) = each %slippages ) {
    $total_complexity += 0.2 unless $k eq $v;
  }

  $total_complexity = 0.9 if $total_complexity > 0.9;
  return $total_complexity;
}
__PACKAGE__->meta->make_immutable;
1;

