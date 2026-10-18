package S;
use 5.10.0;
use strict;
use warnings;

use Global;

use List::Util;
use Scalar::Util;

use SHistory;

use SErr;
use UserInteraction;
use SChoose;
use SBindings;
use SPos;
use SFasc;

use SCodelet;
use SAction;
use SCoderack;

use SMetonym;
use SMetonymType;

use SCategory::Load;

use Seqsee::Object;
use Seqsee::Anchored;
use Seqsee::Element;
use SInt;
use SWorkspace;

use Seqsee::ResultOfCanBeSeenAs;

use Mapping;
use Mapping::Numeric;
use Mapping::Structural;
use Mapping::Position;
use Mapping::MetoType;
use Mapping::Dir;
use SRelation;
use SRelation::Structural;

use SThought;
use SStream2;
use SWorkspace;

use Seqsee::SCF_MX::Load;
use SThought::Load;
use Seqsee::Scripts::Load;

use SLinkActivation;
use SNodeActivation;
use SLTM;

use SRule;
use SRuleApp;

use PositionStructure;

# use SUtil;

# our $ASCENDING  = $SCat::ascending::ascending;
our $ASCENDING  = SCategory::Ascending->new;
our $DESCENDING = SCategory::Descending->new;
our $MOUNTAIN   = SCategory::Mountain->new;

#our $number     = $SCat::number::number;
our $SAMENESS = SCategory::Sameness->new;
our $AD_HOC   = $SCat::ad_hoc::AD_HOC;
our $NUMBER   = SCategory::Number->new;
our $PRIME    = SCategory::Prime->new;
our $ODD      = SCategory::Odd->new;
our $EVEN     = SCategory::Even->new;

our $DOUBLE = SMetonymType->new(
  {
    category  => $S::SAMENESS,
    name      => "each",
    info_loss => { length => 2 },
  }
);

package DIR;
use strict;
use warnings;
use Carp;

use overload
'~~'     => 'literal_comparison_hack_for_smart_match',
fallback => 1;

sub literal_comparison_hack_for_smart_match {
  return $_[0] eq $_[1];
}

our $LEFT    = bless { text => 'left' },    'DIR';
our $RIGHT   = bless { text => 'right' },   'DIR';
our $UNKNOWN = bless { text => 'unknown' }, 'DIR';
our $NEITHER = bless { text => 'neither' }, 'DIR';

sub LEFT    { $LEFT }
sub RIGHT   { $RIGHT }
sub UNKNOWN { $UNKNOWN }
sub NEITHER { $NEITHER }

sub Flip {
  my ($self) = @_;
  return $LEFT  if $self eq $RIGHT;
  return $RIGHT if $self eq $LEFT;
  confess "Flip called on weird direction";
}

sub PotentiallyExtendible {
  my ($self) = @_;
  return ( $self eq $LEFT or $self eq $RIGHT );
}

sub IsLeftOrRight {
  my ($self) = @_;
  return ( $self eq $LEFT or $self eq $RIGHT );
}

sub as_text {
  my ($self) = @_;
  return $self->{text};
}

package POS_MODE;
our $FORWARD  = bless { mode => 'FORWARD' },  'POS_MODE';
our $BACKWARD = bless { mode => 'BACKWARD' }, 'POS_MODE';

sub FORWARD  { $FORWARD }
sub BACKWARD { $BACKWARD }

sub as_text {
  my ($self) = @_;
  return $self->{mode};
}

sub get_memory_dependencies { return; }

sub serialize {
  my ($self) = @_;
  return $self->{mode};
}

sub deserialize {
  my ( $package, $str ) = @_;
  no strict 'refs';
  return ${$str};
}

package METO_MODE;
use strict;
use Carp;
use warnings;

use overload
'~~'     => 'literal_comparison_hack_for_smart_match',
fallback => 1;

sub literal_comparison_hack_for_smart_match {
  return $_[0] eq $_[1];
}

our $NONE      = bless { mode => 'NONE' },      'METO_MODE';
our $SINGLE    = bless { mode => 'SINGLE' },    'METO_MODE';
our $ALLBUTONE = bless { mode => 'ALLBUTONE' }, 'METO_MODE';
our $ALL       = bless { mode => 'ALL' },       'METO_MODE';
our $OTHER     = bless { mode => 'OTHER' },     'METO_MODE';
sub NONE      { $NONE }
sub SINGLE    { $SINGLE }
sub ALLBUTONE { $ALLBUTONE }
sub ALL       { $ALL }
sub OTHER     { $OTHER }

sub as_text {
  my ($self) = @_;
  return $self->{mode};
}

sub is_position_relevant {
  my ($self) = @_;
  if ( $self eq $SINGLE or $self eq $ALLBUTONE ) {
    return 1;
  }
  else {
    return 0;
  }
}

sub is_metonymy_present {
  my ($self) = @_;
  return ( $self eq $NONE ) ? 0 :1;
}

sub get_memory_dependencies { return; }

sub serialize {
  my ($self) = @_;
  return $self->{mode};
}

sub deserialize {
  my ( $package, $str ) = @_;
  no strict 'refs';
  given ($str) {
    when ('NONE')      { return $NONE }
    when ('SINGLE')    { return $SINGLE }
    when ('ALLBUTONE') { return $ALLBUTONE }
    when ('ALL')       { return $ALL }
    when ('OTHER')     { return $OTHER }
    confess "Unknown!";
  }
}

sub get_pure {
  return $_[0];
}

package EXTENDIBILE;

# XXX(Board-it-up): [2007/04/09] Keeping in case I bring extendibility of relations back.

use strict;
use warnings;
our $NO      = bless { mode => 'NO' },      'EXTENDIBILE';
our $PERHAPS = bless { mode => 'PERHAPS' }, 'EXTENDIBILE';
our $UNKNOWN = bless { mode => 'UNKNOWN' }, 'EXTENDIBILE';
sub NO      { $NO }
sub PERHAPS { $PERHAPS }
sub UNKNOWN { $UNKNOWN }

use overload (
  q{bool} => sub {
    my ($self) = @_;
    return ( $self->{mode} eq 'NO' ) ? 0 :1;
  },
  fallback => 1,
);

package RELN_SCHEME;
use strict;
use warnings;
our $NONE = 0;
our $CHAIN = bless { type => 'CHAIN' }, 'RELN_SCHEME';
sub NONE  { $NONE }
sub CHAIN { $CHAIN }

package DISTANCE_MODE;
use strict;
use warnings;
our $GROUP   = bless { mode => 'group' },   'DISTANCE_MODE';
our $ELEMENT = bless { mode => 'element' }, 'DISTANCE_MODE';

sub GROUP   { $GROUP }
sub ELEMENT { $ELEMENT }

# Can/should be influenced by activations.
sub PickOne {
  if ( SUtil::toss(0.25) ) {
    return $GROUP;
  }
  else {
    return $ELEMENT;
  }
}

sub IsUnitGroups {
  my ($mode) = @_;
  return ( $mode eq $GROUP ) ? 1 :0;
}

package DISTANCE;
use strict;
use warnings;

sub InElements {
  my ($distance) = @_;
  bless [ $distance, $DISTANCE_MODE::ELEMENT ], 'DISTANCE';
}

sub InGroups {
  my ($distance) = @_;
  bless [ $distance, $DISTANCE_MODE::GROUP ], 'DISTANCE';
}

sub Zero {
  bless [ 0, $DISTANCE_MODE::GROUP ], 'DISTANCE';
}

sub IsNonZero {
  return $_[0]->[0];
}

sub IsUnitGroups {
  my ($self) = @_;
  return $self->[1]->IsUnitGroups();
}

sub GetMagnitude {
  my ($self) = @_;
  return $self->[0];
}

sub as_text {
  my ($self) = @_;
  return "$self->[0] " . $self->[1]->{mode};
}

1;
