package Seqsee::SCF::CheckIfInstance;
use 5.010;
use MooseX::SCF;
Codelet_Family(
  attributes => [ obj => {}, cat => {} ],
  body       => sub {
    my ( $obj, $cat ) = @_;
    if ( $obj->describe_as($cat) and $Global::Feature{LTM} ) {
      $cat->SpikeBy( 10 );
      $obj->InsertISALink( $cat )->Spike(5);
    }
  }
);

package Seqsee::SCF::FocusOn;
use MooseX::SCF;
use Seqsee::SCF;
Codelet_Family(
  attributes => [ what => { optional => 1 } ],
  body       => sub {
    my ($what) = @_;
    if ($what) {
      ContinueWith( SThought->create($what) );
    } else {
      # Equivalent to Reader
      if ( SUtil::toss(0.3) ) {
        SWorkspace::__CreateSamenessGroupAround($SWorkspace::ReadHead);
        return;
      }
      my $object = SWorkspace::__ReadObjectOrRelation() // return;
      main::message( "Focusing on: " . $object->as_text() ) if $Global::debugMAX;
      ContinueWith( SThought->create($object) );
    }
  }
);

package Seqsee::SCF::ActOnOverlappingThoughts;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;

use Class::Multimethods;

# By default, do nothing.
multimethod ActionForThoughtTypes => ('*', '*') => sub { return; };

multimethod ActionForThoughtTypes => ('SRelation', 'SRelation') => sub {
  my ($a_core, $b_core) = @_;
  ACTION(
    100,
    FindIfRelatedRelations => {
      a => $a_core,
      b => $b_core
    }
  );
};

multimethod ActionForThoughtTypes => ('Seqsee::Object', 'Seqsee::Object') => sub {
  my ($a_core, $b_core) = @_;
  ACTION(
    100,
    FindIfRelated => {
      a => $a_core,
      b => $b_core
    }
  );
};

Codelet_Family(
  attributes => [ a => {}, b => {} ],
  body       => sub {
    my ( $a, $b ) = @_;
    my $a_core = $a->can('core') ? $a->core() :undef;
    my $b_core = $b->can('core') ? $b->core() :undef;
    ActionForThoughtTypes($a_core, $b_core);
  }
);

package Seqsee::SCF::AreTheseGroupable;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;

Codelet_Family(
  attributes => [ items => {}, reln => {} ],
  body       => sub {
    my ( $items, $reln ) = @_;

    my @items =  SWorkspace::__SortLtoRByLeftEdge(@{$items}) or return;

    # Create potential group first:
    my @concrete_items = map { $_->GetConcreteObject() } @$items;
    ### require: SWorkspace::__CheckLivenessAtSomePoint(@concrete_items)
    SWorkspace::__CheckLiveness(@concrete_items) or return;   # dead objects.

    my $new_group = Seqsee::Anchored->create(@concrete_items) or return; # Failed _CheckValidity!

    my $conflicts = SWorkspace::__FindGroupsConflictingWith($new_group);

    if ($conflicts->exact_conflict) {
      return; # Already exists!
    }

    if ($conflicts->has_overlapping_conflicts() ) {
      # Should I fight?
      if (SUtil::toss(0.2)) {
        $conflicts->Resolve() or return;
      }
    }

    # If we get here, all conflicting incumbents are dead.
    $new_group->set_underlying_ruleapp($reln);
      
    SWorkspace->add_group($new_group);
    my $reln_type = $reln->get_type();
    if ( $reln_type->isa('Mapping::Structural')
         or $reln_type->get_category() ne $S::NUMBER )
    {
      $new_group->describe_as( SCategory::MappingBased->Create($reln_type) )
      || main::message( "Unable to describe "
                        . $new_group->as_text()
                        . "  as based on "
                        . $reln_type->as_text );
    }
    else {
      state $map = {
        same => $S::SAMENESS,
        succ => $S::ASCENDING,
        pred => $S::DESCENDING
      };
      $new_group->describe_as( $map->{ $reln_type->get_name() }
      || confess "Should not be here ($reln_type)" );
    }
  }
);

package Seqsee::SCF::AreWeDone;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;

my $LastSolutionDescriptionTime;

sub BelieveDone {
  my ($group) = @_;
  if ($Global::TestingMode) {

    # Currently assume belief always right.
    SErr::FinishedTest->new( got_it => 1 )->throw();
  }
  return
  if (  $LastSolutionDescriptionTime
    and $LastSolutionDescriptionTime > $Global::TimeOfLastNewElement );

  $LastSolutionDescriptionTime = $Global::Steps_Finished;
  main::message( "I believe I got it", 1 );
  ACTION( 100, DescribeSolution => { group => $group } );
}

Codelet_Family(
  attributes => [ group => {} ],
  body       => sub {
    my ($group)     = @_;
    my $gp          = $group;
    my $span        = $gp->get_span;
    my $total_count = $SWorkspace::ElementCount;
    my $left_edge   = $gp->get_left_edge();
    ## $span, $total_count
    #main::message( $right_extendibility);

    my $underlying_rule_app = $gp->get_underlying_reln();

    if ( $span / $total_count > 0.5 ) {
      Global::SetRuleAppAsRecent($underlying_rule_app) if $underlying_rule_app;
    }

    if ( $Global::AtLeastOneUserVerification
      and ( $span / $total_count ) > 0.8 )
    {
      if ( $left_edge == 0 ) {
        if ( $span == $total_count ) {

          #Bingo!
          Global::ClearHilit();
          Global::Hilit( 2, @$gp );
          main::update_display();
          BelieveDone($group);
        }
        else {
          ACTION(
            80,
            AttemptExtensionOfGroup => {
              object    => $gp,
              direction => DIR::RIGHT()
            }
          );
        }
      }
    }

  }
);

package Seqsee::SCF::ConvulseEnd;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
multimethod 'SanityCheck';
    
Codelet_Family(
  attributes => [ object => {}, direction => {} ],
  body       => sub {
    my ( $object, $direction ) = @_;
    unless ( SWorkspace::__CheckLiveness($object) ) {
      return;    # main::message("Seqsee::SCF::ConvulseEnd: " . $object->as_text());
    }
    my $change_at_end_p = ( $direction eq $DIR::RIGHT ) ? 1 :0;
    my @object_parts = $object->get_items_array;
    my $ejected_object;
    if ($change_at_end_p) {
      $ejected_object = pop(@object_parts);
    }
    else {
      $ejected_object = shift(@object_parts);
    }

    my $underlying_reln = $object->get_underlying_reln();
    if ($underlying_reln) {
      SanityCheck( $object, $underlying_reln, "Pre-extension" );
    }

    my $new_extension = $object->FindExtension( $direction, 1 ) or return;
    $new_extension = $new_extension->GetConcreteObject();

    if ( $new_extension ne $ejected_object ) {
      if ($underlying_reln) {
        SanityCheck( $object, $underlying_reln, "post-extension" );
      }

      my $structure_string_before_ejection = $object->as_text();
      if ($change_at_end_p) {
        $ejected_object = pop(@$object);
      }
      else {
        $ejected_object = shift(@$object);
      }
      SWorkspace::__RemoveFromSupergroups_of( $ejected_object, $object );
      $object->recalculate_edges();

      my $extended = eval {  $object->Extend( $new_extension, $change_at_end_p ) };
      
      my $e;
      if ( $e = Exception::Class->caught('SErr::CouldNotCreateExtendedGroup') ) {
        print STDERR
        "(structure before ejection): $structure_string_before_ejection\n";
        print STDERR "Extending group: ", $object->as_text(), "\n";
        print STDERR "(But effectively): ",
        $object->GetEffectiveStructureString();
        print STDERR "Ejected object: ",
        $ejected_object->get_structure_string(), "\n";
        print STDERR "(But effectively): ",
        $ejected_object->GetEffectiveStructureString();
        print STDERR "New object: ", $new_extension->get_structure_string(),
        "\n";
        print STDERR "(But effectively): ",
        $new_extension->GetEffectiveStructureString();
        confess "Unable to extend group!";     
      }
      elsif($e = Exception::Class->caught()) {
        ref $e ? $e->rethrow : die $e;
      }

      unless ($extended) {

        # main::message("Failed to extend, and no deaths!");
        if ($change_at_end_p) {
          push( @$object, $ejected_object );
        }
        else {
          unshift( @$object, $ejected_object );
        }
        $object->recalculate_edges();
      }
    }
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::CheckProgress;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [],
  body       => sub {
    state $last_time_progresschecker_run = 0;
    my $time_since_last_addn =
    $Global::Steps_Finished - $Global::TimeOfNewStructure;
    my $time_since_new_elements =
    $Global::Steps_Finished - $Global::TimeOfLastNewElement;
    my $time_since_codelet_run =
    $Global::Steps_Finished - $last_time_progresschecker_run;

    # Don't run too frequently
    return if $time_since_codelet_run < 100;
    $last_time_progresschecker_run = $Global::Steps_Finished;

    my $desperation =
    CalculateDesperation( $time_since_last_addn, $time_since_new_elements );

    my $chooser_on_inv_strength =
    SChoose->create( { map => q{100 - $_->get_strength()} } );
    if ( $desperation > 50 ) {
      main::ask_for_more_terms();
    }
    elsif ( $desperation > 30 ) {

      # XXX(Board-it-up): [2007/02/14] should be biased by 100 - strength?
      # my $gp = SChoose->uniform([SWorkspace::GetGroups()]);
      my $gp = $chooser_on_inv_strength->( [ SWorkspace::GetGroups() ] );
      if ($gp) {

        # main::message("Deleting group $gp: " . $gp->get_structure_string());
        SWorkspace->remove_gp($gp);
      }
    }
    elsif ( $desperation > 10 ) {
      for ( values %SWorkspace::relations ) {
        my $age = $_->GetAge();
        if (  SUtil::toss( ( 100 - $_->get_strength() ) / 200 )
          and SUtil::toss( $age / 400 ) )
        {
          $_->uninsert();
        }
      }
    }
  }
);

my @Cutoffs =
( [ 1500, 0, 80 ], [ 800, 2500, 80 ], [ 500, 0, 40 ], [ 200, 0, 20 ], );

sub CalculateDesperation {
  my ( $time_since_last_addn, $time_since_new_elements ) = @_;
  for (@Cutoffs) {
    my ( $a, $b, $c ) = @$_;
    return $c
    if (  $time_since_last_addn >= $a
      and $time_since_new_elements >= $b );
  }
  return 0;
}

__PACKAGE__->meta->make_immutable;

1;
