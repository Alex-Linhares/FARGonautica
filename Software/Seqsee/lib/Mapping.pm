package Mapping;
use 5.10.0;
use Class::Multimethods;
use strict;
use Carp;

multimethod FindMapping => ( '*', '*', '*' ) => sub {
  *__ANON__ = "((__ANON__ FindMapping ***))";
  my ( $a, $b, $cat ) = @_;
  $cat->FindMappingForCat( $a, $b );
};

{
  my $numeric_FindTransorm = sub {
    *__ANON__ = "((__ANON__ FindMapping SInt/Seqsee::Element SInt/Seqsee::Element))";
    my ( $a, $b ) = @_;
    my @common_categories = $a->get_common_categories($b) or confess;
    if ( grep { not defined $_ } @common_categories ) {
      confess
      "undef in common_categories FindMapping SInt/Seqsee::Element SInt/Seqsee::Element:"
      . join( ', ', @common_categories );
    }
    my $cat = SLTM::SpikeAndChoose( 0, @common_categories ) // $S::NUMBER;
    if ( $cat->IsNumeric() ) {
      $cat->FindMappingForCat( $a->get_mag(), $b->get_mag() );
    }
    else {
      $cat->FindMappingForCat( $a, $b );
    }
  };
  multimethod FindMapping => qw{SInt SInt}         => $numeric_FindTransorm;
  multimethod FindMapping => qw{Seqsee::Element Seqsee::Element} => $numeric_FindTransorm;
}

multimethod FindMapping => qw(# #) => sub {
  *__ANON__ = "((__ANON__ FindMapping ##))";
  my ( $a, $b ) = @_;
  $S::NUMBER->FindMappingForCat( $a, $b );
};

multimethod FindMapping => qw(Seqsee::Anchored Seqsee::Anchored) => sub {
  *__ANON__ = "((__ANON__ FindMapping Seqsee::Anchored Seqsee::Anchored))";
  my ( $a, $b ) = @_;
  my @common_categories = $a->get_common_categories($b) or return;
  my $cat = SLTM::SpikeAndChoose( 10, @common_categories ) or return;
  $cat->FindMappingForCat( $a, $b );
};

# More FindMapping in Mapping::Dir

multimethod ApplyMapping => qw(Mapping::Numeric #) => sub {
  *__ANON__ = "((__ANON__ ApplyMapping Mapping::Numeric #))";
  my ( $transform, $num ) = @_;
  $transform->get_category()->ApplyMappingForCat( $transform, $num );
};

multimethod ApplyMapping => qw(Mapping::Numeric SInt) => sub {
  *__ANON__ = "((__ANON__ ApplyMapping Mapping::Numeric SInt))";
  my ( $transform, $num ) = @_;
  my $new_mag =
  $transform->get_category()->ApplyMappingForCat( $transform, $num->get_mag() )
  // return;
  SInt->new($new_mag);
};

multimethod ApplyMapping => qw(Mapping::Numeric Seqsee::Element) => sub {
  *__ANON__ = "((__ANON__ ApplyMapping Mapping::Numeric Seqsee::Element))";
  my ( $transform, $num ) = @_;
  my $new_mag =
  $transform->get_category()->ApplyMappingForCat( $transform, $num->get_mag() )
  // return;
  Seqsee::Element->create( $new_mag, -1 );
};

multimethod ApplyMapping => qw(Mapping::Structural Seqsee::Object) => sub {
  my ( $transform, $object ) = @_;
  $transform->get_category()->ApplyMappingForCat( $transform, $object );
};

{
  my $Fail = sub {
    return;
  };
  multimethod FindMapping  => qw{SInt Seqsee::Element}              => $Fail;
  multimethod FindMapping  => qw{Seqsee::Element SInt}              => $Fail;
  multimethod FindMapping  => qw{Seqsee::Anchored SInt}             => $Fail;
  multimethod FindMapping  => qw{SInt Seqsee::Anchored}             => $Fail;
  multimethod ApplyMapping => qw{Mapping::Numeric Seqsee::Anchored} => $Fail;
}

sub CheckSanity {
  my ($self) = @_;
  return 1 unless $self->isa('Mapping::Structural');
  my $cat  = $self->get_category();
  my @atts = keys %{ $self->get_changed_bindings };
  unless ( $cat->AreAttributesSufficientToBuild(@atts) ) {
    my $cat_name = $cat->as_text();
    main::message("This transform is bogus! CAT=$cat_name ATTS=@atts");

    # die("This transform is bogus! CAT=$cat_name ATTS=@atts");
    return;
  }
  return 1;
}

1;
