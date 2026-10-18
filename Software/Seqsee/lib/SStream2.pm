package SStream2;
use strict;
use Carp;
use Smart::Comments '###';
use Scalar::Util qw(blessed reftype);

############# Class ######################
#  Uses Class::Std       : No
#  Non-std constructor   : CreateNew
#  Constructor Memoized? : Yes
#  Storable in LTM       : No

my %MEMO;

############# Method ######################
#  Name             : CreateNew
#  Returns          : a Stream object
#  Parameters       : (Str $name, { $DiscountFactor, $MaxOlderThoughts })
#  Params via href  : second
#  Purpose          : Creates a new instance and initializes it.
##
#  Multi?           : No
#  Usage            : SStream2->CreateNew(...)
#  Memoized         : Yes, but only using the name.
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

sub CreateNew {
  my ( $package, $name, $opts_ref ) = @_;
  confess "Missing name!" unless $name;

  return $MEMO{$name} if $MEMO{$name};
  my $self = bless {}, $package;
  $MEMO{$name} = $self;

  $self->{Name}                  = $name;
  $self->{DiscountFactor}        = $opts_ref->{DiscountFactor} || 0.8;
  $self->{MaxOlderThoughts}      = $opts_ref->{MaxOlderThoughts} || 10;
  $self->{OlderThoughtCount}     = 0;
  $self->{OlderThoughts}         = [];
  $self->{ThoughtsSet}           = {};
  $self->{ComponentStrength}     = {};
  $self->{ComponentOwnership_of} = {};
  $self->{CurrentThought}        = '';
  $self->{vivify}                = {};
  $self->{hit_intensity}         = {};
  $self->{thought_hit_intensity} = {};

  return $self;
}

sub clear {
  my ($self) = @_;
  $self->{OlderThoughtCount}     = 0;
  $self->{OlderThoughts}         = [];
  $self->{ThoughtsSet}           = {};
  $self->{ComponentStrength}     = {};
  $self->{ComponentOwnership_of} = {};
  $self->{CurrentThought}        = '';
  $self->{vivify}                = {};
}

############# Method ######################
#  Name             : add_thought
#  Returns          : -
#  Parameters       : the new thought
#  Params via href  : No
#  Purpose          : add a new thought
##
#  Multi?           : No
#  Usage            : $obj->add_thought(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

#  If this is the current thought, nothing happens.
#  If, instead, this is a recent thought, it is made the current thought and
#  removed from the list of older thoughts. Otherwise, it is simply made the
#  current thought.

#  The method C<_think_the_current_thought> is then called, and it does all
#  the hard work.

sub add_thought {
  @_ == 2 or confess "new thought takes two arguments";
  my ( $self, $thought ) = @_;
  return unless $thought->core();

  if ($Global::debugMAX) {
    main::message( "Added thought: " . SUtil::StringifyForCarp($thought) );
  }
  if ( $Global::Feature{CodeletTree} ) {
    print {$Global::CodeletTreeLogHandle} "Chose $thought\n";
  }

  return if $thought eq $self->{CurrentThought};

  if ( exists $self->{ThoughtsSet}{$thought} )
  {    #okay, so this is an older thought
    unshift( @{ $self->{OlderThoughts} }, $self->{CurrentThought} )
    if $self->{CurrentThought};
    @{ $self->{OlderThoughts} } =
    grep { $_ ne $thought } @{ $self->{OlderThoughts} };
    $self->{CurrentThought} = $thought;
    _recalculate_Compstrength();
    $self->{OlderThoughtCount} = scalar( @{ $self->{OlderThoughts} } );
  }

  else {
    $self->antiquate_current_thought() if $self->{CurrentThought};
    $self->{CurrentThought} = $thought;
    $self->{ThoughtsSet}{$thought} = $thought;
    $self->_maybe_expell_thoughts();
  }
  $self->_think_the_current_thought();

}

############# Method ######################
#  Name             : _think_the_current_thought
#  Returns          : -
#  Parameters       : -
#  Params via href  : No
#  Purpose          : Takes some action based on similarity to recent thoughts
#                     and other actions suggested by the thought.
##
#  Multi?           : No
#  Usage            : $obj->_think_the_current_thought(...)
#  Memoized         : No
#  Throws           : no exceptions
#  Comments         :
#  See Also         : n/a

#  The method C<_is_there_a_hit> is used to determine similarity to earlier
#  thoughts.
sub _think_the_current_thought {
  my ($self) = @_;
  my $thought = $self->{CurrentThought};
  return unless $thought;

  $Global::CurrentCodelet       = $thought;
  $Global::CurrentCodeletFamily = ref($thought);

  my $fringe = $thought->get_fringe();
  ## $fringe
  $thought->stored_fringe($fringe);

  my $hit_with = $self->_is_there_a_hit($fringe);
  ## $hit_with

  if ($hit_with) {
    my $new_thought = SCodelet->new(
      'ActOnOverlappingThoughts',
      100,
      {
        a => $hit_with,
        b => $thought
      }
    )->schedule();
  }

  my @codelets;
  for my $x ( $thought->get_actions() ) {
    my $x_type = ref $x;
    if ( $x_type eq "SCodelet" ) {
      push @codelets, $x;
    }
    elsif ( $x_type eq "SAction" ) {

      # print "Action of family ", $x->get_family(), " to be run\n";
      # main::message("Action of family ", $x->get_family());
      if ( $Global::Feature{CodeletTree} ) {
        my $family      = $x->get_family;
        my $probability = $x->get_urgency;
        print {$Global::CodeletTreeLogHandle} "\t$x\t$family\t$probability\n";
      }
      $x->conditionally_run();
    }
    else {
      confess "Huh? non-codelet '$x' returned by get_actions";
    }
  }

  my @choose2 =
  scalar(@codelets) > 2
  ? SChoose->choose_a_few_nonzero( 2, [ map { $_->[1] } @codelets ],
    \@codelets )
  :@codelets;
  for (@choose2) {
    main::message(
      [
        $_->[0], ['codelet_family'],
        " codelet added by thought: " . SUtil::StringifyForCarp($_)
      ]
    ) if $Global::debugMAX;
    SCoderack->add_codelet($_);
  }
}

# method: _maybe_expell_thoughts
# Expells thoughts if $self->{MaxOlderThoughts} exceeded
#

sub _maybe_expell_thoughts {
  my ($self) = @_;
  return unless $self->{OlderThoughtCount} > $self->{MaxOlderThoughts};
  for ( 1 .. $self->{OlderThoughtCount} - $self->{MaxOlderThoughts} ) {
    delete $self->{ThoughtsSet}{ pop @{ $self->{OlderThoughts} } };
  }
  $self->{OlderThoughtCount} = $self->{MaxOlderThoughts};
  $self->_recalculate_Compstrength();
}

#method: _recalculate_Compstrength
# Recalculates the strength of components from scratch
sub _recalculate_Compstrength {
  my ($self)                = @_;
  my $vivify                = $self->{vivify};
  my $ComponentOwnership_of = $self->{ComponentOwnership_of};
  %{$ComponentOwnership_of} = ();
  %{$vivify}                = ();
  for my $t ( @{ $self->{OlderThoughts} } ) {
    my $fringe = $t->stored_fringe();
    for my $comp_act (@$fringe) {
      my ( $comp, $act ) = @$comp_act;
      $vivify->{$comp} = $comp;
      $ComponentOwnership_of->{$comp}{$t} = $act;
    }
  }
}

# method: init
# Does nothing.
#
#    Here for symmetry with similarly named methods in Coderack etc

sub init {
}

# method: antiquate_current_thought
# Makes the current thought the first old thought
#

sub antiquate_current_thought {
  my $self = shift;
  unshift( @{ $self->{OlderThoughts} }, $self->{CurrentThought} );
  $self->{CurrentThought} = '';
  $self->{OlderThoughtCount}++;
  $self->_recalculate_Compstrength();
}

# method: _is_there_a_hit
# Is there another thought with a common fringe?
sub _is_there_a_hit {
  my ( $self, $fringe_ref ) = @_;
  ## $fringe_ref
  my %components_hit;    # keys values same
  my $hit_intensity = $self->{hit_intensity};
  %$hit_intensity = ();    # keys are components, values numbers

  my $ComponentOwnership_of = $self->{ComponentOwnership_of};

  for my $in_fringe (@$fringe_ref) {
    my ( $comp, $intensity ) = @$in_fringe;
    next unless exists $ComponentOwnership_of->{$comp};
    $components_hit{$comp} = $comp;
    $hit_intensity->{$comp} = $intensity;
  }

  # Now get a list of which thoughts are hit.
  my $thought_hit_intensity = $self->{thought_hit_intensity};
  %$thought_hit_intensity = ();    # keys are thoughts, values intensity

  for my $comp ( values %components_hit ) {
    next unless exists $ComponentOwnership_of->{$comp};
    my $owner_ref = $ComponentOwnership_of->{$comp};
    my $intensity = $hit_intensity->{$comp};
    for my $tht ( keys %$owner_ref ) {
      $thought_hit_intensity->{$tht} += $owner_ref->{$tht} * $intensity;
    }
  }

  return unless %$thought_hit_intensity;

  # Dampen their effect...
  my $dampen_by = 1;
  for my $i ( 0 .. $self->{OlderThoughtCount} - 1 ) {
    $dampen_by *= $self->{DiscountFactor};
    my $thought = $self->{OlderThoughts}[$i];
    next unless exists $thought_hit_intensity->{$thought};
    $thought_hit_intensity->{$thought} *= $dampen_by;
    $thought_hit_intensity->{$thought} *=
    $self->thoughtTypeMatch( $thought, $self->{CurrentThought} );
  }

  my $chosen_thought = SChoose->choose( [ values %$thought_hit_intensity ],
    [ keys %$thought_hit_intensity ] );
  return $self->{ThoughtsSet}{$chosen_thought};
}

{
  my %Mapping = (
    'Seqsee::Element:Seqsee::Anchored' => 0.9,
    'Seqsee::Anchored:Seqsee::Element' => 0.9,
  );

  sub thoughtTypeMatch {
    my ( $self, $othertht, $cur_tht ) = @_;
    my ( $type1, $type2 ) = map { blessed($_) } ( $othertht, $cur_tht );

    #main::message("$type1 and $type2");
    return 1 if $type1 eq $type2;
    my $str = "$type1;$type2";
    return $Mapping{$str} if exists $Mapping{$str};

    #main::message("$str barely match!");
    return 0;
  }
}

1;

__END__

=head1 NAME

SStream2 - A class for managing streams of thought
           
=head1 VERSION

This document describes SStream2 for revision $Revision$

=head1 SYNOPSIS
  
=head1 DESCRIPTION

Each instance of this class manages a single stream of thought. Seqsee
currently only ever uses a single stream, but subsequent extensions
are likely to require multiple streams. The single stream that Seqsee
has is stored in the variable C< $Global::MainStream >.

=head2 The purpose of a stream

This is best described in chapter 5 of the dissertation, which deals with context. The last few thoughts (currently 10, as stored in the attribute C<MaxOlderThoughts>) form the context in which subsequent thoughts are understood.

Ideas relevant to this discussion include the notions of the fringe and the action fringe. A far more detailed description of these ideas can again be found in chapter 5.

=head2 How it works

Each entry in the stream satisfies the interface C<Thought>.

=head1 INTERFACE 

=head1 DIAGNOSTICS

=head1 CONFIGURATION AND ENVIRONMENT

SStream2 requires no configuration files or environment variables.

=head1 DEPENDENCIES

None.

=head1 INCOMPATIBILITIES

None reported.

=head1 BUGS AND LIMITATIONS

This module will probably undergo the greatest amounts of change when I start working on the next version of Seqsee. Here are some of the changes to be expected:

=over

=item _is_there_a_hit currently returns a single matching thought. If there is any match whatsoever, something is returned. With this return value, the codelet ActOnOverlappingThoughts is always launched. _is_there_a_hit should instead return a smart value which will be sensitive to the type of the match and the level of the match. Currently, the type of the match is very crudely approximated by the method thoughtTypeMatch.

=back

 
=head1 AUTHOR

Abhijit Mahabal  C<< amahabal@gmail.com >>

=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
