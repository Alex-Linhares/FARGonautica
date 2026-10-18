package Seqsee::SCF::LargeGroup;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [group => {}],
  body => sub {
    my ($group) = @_;
    my $flush_right = $group->IsFlushRight();
    my $flush_left  = $group->IsFlushLeft();

    if ( $flush_right and $flush_left ) {
      SCodelet->new('AreWeDone', 100, { group => $group } )->schedule();
    }
    elsif ( $Global::AtLeastOneUserVerification
      and $flush_right
      and!$flush_left )
    {
      SCodelet->new('MaybeStartBlemish', 100, { group => $group } )->schedule();
    }
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::MaybeStartBlemish;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [group => {}],
  body => sub {
    my ($group) = @_;
    my $flush_right = $group->IsFlushRight();
    my $flush_left  = $group->IsFlushLeft();
    if ( !$flush_left ) {
      my $extension = $group->FindExtension( $DIR::LEFT, 0 );
      if ($extension) {
        $group->Extend( $extension, 0 );
      }
      else {

        # So there *is* a blemish!
        #main::message("Start Blemish?");
        my $underlying_ruleapp = $group->get_underlying_reln() or return;
        my $underlying_rule    = $underlying_ruleapp->get_rule();
        my $transform          = $underlying_rule->get_transform();

        if ( $transform->isa("Mapping::Structural") ) {
          my $cat = $transform->get_category();

          #main::message($cat->get_name());
          if ( $cat->get_name() =~ m#^Interlaced_(.*)#o ) {
            SCodelet->new('InterlacedInitialBlemish', 100,
                          {
                            count => $1,
                            group => $group,
                            cat   => $cat,
                          })->schedule();
            return;
          }
        }

        # So: either statecount > 1, or not interlaced.
        if ($flush_right) {
          SCodelet->new('ArbitraryInitialBlemish', 100,  { group => $group } )->schedule();
        }
      }
    }
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::InterlacedInitialBlemish;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;
multimethod 'FindMapping';

Codelet_Family(
  attributes => [count => {}, group => {}, cat => {}],
  body => sub {
    my ($count, $group, $cat) = @_;
    return unless SWorkspace::__CheckLiveness($group);
    my @parts = @$group;
    Global::Hilit( 1, @parts );
    main::message(
      "I realize that there are $count interlaced groups in the sequence, and I have started on the wrong foot. I will shift the big group one unit, and see if that helps!!"
    );
    Global::ClearHilit();
    my @subparts = map { @$_ } @parts;
    SWorkspace::__DeleteGroup($group);
    SWorkspace::__DeleteGroup($_) for @parts;

    # Also delete other interlaced groups of this category.
    for my $object ( SWorkspace::__GetObjectsBelongingToCategory($cat) ) {
      next unless SWorkspace::__CheckLiveness($object);

      # main::message("Shifting, so Deleting " . $object->as_text());
      SWorkspace::__DeleteGroup($object);
    }

    shift(@subparts);
    my @newparts;
    while ( @subparts >= $count ) {
      my @new_part;
      for ( 1 .. $count ) {
        push @new_part, shift(@subparts);
      }
      my $newpart = Seqsee::Anchored->create(@new_part);
      $newpart->describe_as($cat);
      SWorkspace->add_group($newpart) or return;
      push @newparts, $newpart;
    }
    if ( @newparts > 1 ) {
      my $transform = FindMapping( @newparts[ 0, 1 ] ) or return;
      my $new_gp = Seqsee::Anchored->create(@newparts);
      $new_gp->describe_as( SCategory::MappingBased->Create($transform) );
      SWorkspace->add_group($new_gp);
      ContinueWith( SThought->create($new_gp) );
    }
  }
);

__PACKAGE__->meta->make_immutable;

package Seqsee::SCF::ArbitraryInitialBlemish;
use 5.010;
use MooseX::SCF;
use English qw(-no_match_vars);
use Seqsee::SCF;
use Class::Multimethods;

Codelet_Family(
  attributes => [group => {}],
  body => sub {
    my ($group) = @_;
    SErr::FinishedTestBlemished->throw() if $Global::TestingMode;
    ACTION(100, 'DescribeSolution', { group => $group });
  }
);

__PACKAGE__->meta->make_immutable;

