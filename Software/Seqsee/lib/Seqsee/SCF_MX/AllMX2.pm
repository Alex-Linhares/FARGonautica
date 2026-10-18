package Seqsee::SCF::AttemptExtensionOfGroup;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;

use Class::Multimethods;
multimethod('SanityCheck');

Codelet_Family(
  attributes => [ object => {}, direction => {} ],
  body       => sub {
    my ( $object, $direction ) = @_;
    SWorkspace::__CheckLiveness($object) or return;
    my $underlying_reln = $object->get_underlying_reln();
    if ($underlying_reln) {
      SanityCheck( $object, $underlying_reln,
        "In AttemptExtensionOfGroup pre" );
    }
    my $extension = $object->FindExtension( $direction, 0 ) or return;
    if ($underlying_reln) {
      SanityCheck( $object, $underlying_reln,
        "In AttemptExtensionOfGroup post" );
    }

    my $add_to_end_p = ( $direction eq $DIR::RIGHT ) ? 1 :0;
    my $extend_success  = $object->SafeExtend( $extension, $add_to_end_p ) or return;

    if ( SUtil::toss( $object->get_strength() / 100 ) ) {
      SCodelet->new( 'AreWeDone', 100, { group => $object } )->schedule();
    }
    if ( $underlying_reln and not $object->get_underlying_reln ) {
      confess "underlying_reln lost!";
    }
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::TryToSquint;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;

Codelet_Family(
  attributes => [ actual => {}, intended => {} ],
  body       => sub {
    my ( $actual, $intended ) = @_;
    my @potential_squints = $actual->CheckSquintability($intended) or return;

    #main::message("potential_squints: @potential_squints");
    my $chosen_squint = SLTM::SpikeAndChoose( 100, @potential_squints )
    or return;

    #main::message("chosen_squint: $chosen_squint");

    my ( $cat, $name ) = $chosen_squint->GetCatAndName;

    #main::message("CAT/NAME: $cat, $name");
    $actual->AnnotateWithMetonym( $cat, $name );
    $actual->SetMetonymActiveness(1);
  }
);

__PACKAGE__->meta->make_immutable;

