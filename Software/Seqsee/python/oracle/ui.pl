# Oracle for lib/UserInteraction.pm and Seqsee/SCF_MX/UI.pm (item 044):
# SErr::ElementsBeyondKnownSought's asking methods, RulesAskedSoFar, SolutionConfirmation,
# and the codelet families AskIfThisIsTheContinuation, MaybeAskTheseTerms,
# MaybeAskUsingThisGoodRule and DoTheAsking. Also Seqsee::already_rejected_by_user and
# Test::Seqsee's main::ask_user_extension (the testing-mode answerer).
# Output: tests/golden/ui.json
#
# Same op-driven style (and helpers) as scf_allmx2.pl; tests/test_ui.py replays the
# scenarios op for op. e0, e1, ... are the elements of the last init.
#
# The GUI's $SGUI::Commentary is replaced (op `commentary`) by a fake whose
# MessageRequiringBooleanResponse records its arguments (and the hilit objects at that
# moment) and returns scripted answers. Op `no_commentary` restores the headless undef.
use strict;
use warnings;
no warnings 'uninitialized', 'numeric', 'redefine', 'once';
use Oracle;
use S;
use PadWalker qw(closed_over peek_sub);

BEGIN {
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  require Test::Seqsee;
  open( STDOUT, '>&', $saved ) or die;
}

my %h1 = %{ closed_over( \&SWorkspace::__UpdateGroup ) };
my %h2 = %{ closed_over( \&SWorkspace::__DoGroupAddBookkeeping ) };
my ( $LEFT, $RIGHT ) = @h1{qw(%LeftEdge_of %RightEdge_of)};
my ($OBJECTS) = @h2{qw(%Objects)};
die "PadWalker" unless $LEFT and $OBJECTS;

# AreWeDone's file lexical $LastSolutionDescriptionTime, and CheckProgress's state var.
my $LAST_SOLUTION = closed_over( \&Seqsee::SCF::AreWeDone::BelieveDone )->{'$LastSolutionDescriptionTime'};
my $progress_body = closed_over( \&Seqsee::SCF::CheckProgress::run )->{'%options'}{body};
my $LAST_PROGRESS = peek_sub($progress_body)->{'$last_time_progresschecker_run'};
die "PadWalker 2" unless $LAST_SOLUTION and $LAST_PROGRESS;

# The UI hooks (Test::Seqsee installs them only in INITIALIZE_for_testing).
my @PROBES;
sub main::message            { }
sub main::update_display     { }
sub main::ask_for_more_terms { push @PROBES, ['ask_for_more_terms'] }

# Test::Seqsee's hooks, including main::ask_user_extension (RealSequence-based answers).
{
  open( my $saved, '>&', \*STDOUT ) or die;
  open( STDOUT, '>', '/dev/null' ) or die;
  INITIALIZE_for_testing();
  open( STDOUT, '>&', $saved ) or die;
}

# UserInteraction.pm's file lexicals.
my %ras = %{ closed_over( \&RulesAskedSoFar::AddRuleToSuccessList ) };
my $SUCC = $ras{'@SuccessfulRules'};
my $UNSUCC = closed_over( \&RulesAskedSoFar::AddRuleToFailureList )->{'@UnsuccessfulRules'};
my $ACCEPTED = closed_over( \&RulesAskedSoFar::MarkRuleAsConfirmed )->{'%AcceptedRules'};
my $REJECTED = closed_over( \&RulesAskedSoFar::MarkRuleAsRejected )->{'%RejectedRules'};
my $SC_REJ = closed_over( \&SolutionConfirmation::AddRejectedSolution )->{'%Rejected'};
my %sca = %{ closed_over( \&SolutionConfirmation::SetAcceptedSolution ) };
my ( $SC_RULE, $SC_PS ) = @sca{qw($AcceptedRule $AcceptedPositionStructure)};
die "PadWalker 3" unless $SUCC and $UNSUCC and $ACCEPTED and $REJECTED and $SC_REJ and $SC_RULE and $SC_PS;

# The fake commentary.
my ( @ANSWERS, @ASKED );
my %name_of;

package FakeCommentary {
  sub MessageRequiringBooleanResponse {
    my ( $self, @args ) = @_;
    push @ASKED, [
      [ map { ref($_) eq 'ARRAY' ? join( '+', @$_ ) : $_ } @args ],
      [ sort map { $name_of{$_} // '?' } keys %Global::Hilit ],
    ];
    return shift @ANSWERS;
  }
}

my %obj;

sub reg {
  my ( $name, $o ) = @_;
  $obj{$name} = $o;
  $name_of{$o} = $name if ref $o;
}

sub nm {
  my ($o) = @_;
  return undef unless defined $o;
  return $o if !ref $o;
  return $name_of{$o} // ( '?' . noaddr( $o->can('as_text') ? $o->as_text : ref($o) ) );
}

sub noaddr {
  my ($s) = @_;
  $s =~ s/=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)/=REF/g;
  return $s;
}
sub O { map { $obj{$_} } @_ }

sub err {
  my ($e) = @_;
  return undef unless $e;
  if ( ref $e ) {
    return { class => ref($e), message => '' . ( $e->can('message') ? $e->message : $e ) };
  }
  $e =~ s/ at \S+ line \d+\.?\n.*//s;
  $e =~ s/=HASH\(0x[0-9a-f]+\)/=HASH/g;
  return $e;
}

sub num_or_undef { defined $_[0] ? 0 + $_[0] : undef }

sub cat {
  my ($n) = @_;
  return SCategory::Interlaced->Create($1) if $n =~ /^interlaced(\d+)$/;
  my %c = (
    ascending  => $S::ASCENDING,
    descending => $S::DESCENDING,
    sameness   => $S::SAMENESS,
    number     => $S::NUMBER,
    prime      => $S::PRIME,
    odd        => $S::ODD,
    even       => $S::EVEN,
    mountain   => $S::MOUNTAIN,
  );
  return $c{$n} // die "unknown cat $n";
}

sub mapping {
  my ( $name, $catname ) = @_;
  return Mapping::Numeric->create( $name, cat( $catname // 'number' ) );
}

sub argval {
  my ( $kind, @v ) = @{ $_[0] };
  return $obj{ $v[0] }               if $kind eq 'obj';
  return [ O(@v) ]                   if $kind eq 'list';
  return cat( $v[0] )                if $kind eq 'cat';
  return mapping(@v)                 if $kind eq 'map';
  return $v[0] eq 'LEFT' ? $DIR::LEFT : $DIR::RIGHT if $kind eq 'dir';
  return $v[0]                       if $kind eq 'val';
  return undef                       if $kind eq 'undef';
  die "unknown arg kind $kind";
}

sub summarize {
  my ($v) = @_;
  return [ map { nm($_) } @$v ] if ref($v) eq 'ARRAY';
  return nm($v);
}

sub codelet {
  my ($cl) = @_;
  my $args = $cl->[3];
  return [ $cl->[0], 0 + $cl->[1], [ map { [ $_, summarize( $args->{$_} ) ] } sort keys %$args ] ];
}

sub state {
  my @live = sort { nm($a) cmp nm($b) } values %$OBJECTS;
  return [
    map {
      my $ul = $_->get_underlying_reln;
      [ nm($_), num_or_undef( $LEFT->{$_} ), num_or_undef( $RIGHT->{$_} ),
        [ sort map { noaddr( $_->as_text ) } @{ $_->get_categories() } ],
        ( $ul ? $ul->get_rule->get_transform->as_text : undef ),
        [ map { nm($_) } @$_ ],
      ]
    } @live
  ];
}

sub relations {
  return [ sort map { join( ' ', nm( $_->get_first ), nm( $_->get_second ), noaddr( $_->get_type->as_text ) ) }
      values %SWorkspace::relations ];
}

sub coderack {
  return [ sort { join( ',', @$a ) cmp join( ',', @$b ) }
      map { my $c = codelet($_); [ $c->[0], $c->[1], map { join( '=', $_->[0], ref $_->[1] ? join( '+', @{ $_->[1] } ) : $_->[1] ) } @{ $c->[2] } ] }
      @SCoderack::CODELETS ];
}

sub thought_name {
  my ($t) = @_;
  return '' unless $t;
  return ref($t) . ':' . nm( $t->core );
}

my %OPS = (
  init => sub {
    SLTM->Clear();
    SWorkspace->init( { seq => $_[0] } );
    SWorkspace::__ClearBarLines();
    SCoderack->clear;
    $Global::MainStream->clear();
    %Global::Feature = ();
    %Global::Hilit   = ();
    $Global::Steps_Finished             = 0;
    $Global::TimeOfLastNewElement       = 0;
    $Global::TimeOfNewStructure         = 0;
    $Global::AtLeastOneUserVerification = undef;
    $Global::TestingMode                = undef;
    $Global::RecentPromisingRuleApp     = undef;
    $Global::RecentPromisingRule        = undef;
    $Global::CurrentCodelet             = undef;
    $$LAST_SOLUTION                     = undef;
    $$LAST_PROGRESS                     = 0;
    $Global::AcceptableTrustLevel       = 0.5;
    $Global::Break_Loop                 = undef;
    %Global::ExtensionRejectedByUser    = ();
    @Global::RealSequence               = ();
    @$SUCC = @$UNSUCC = ();
    %$ACCEPTED = %$REJECTED = %$SC_REJ = ();
    $$SC_RULE = $$SC_PS = undef;
    ResetFailedRequests();
    $SGUI::Commentary = undef;
    @ANSWERS = @ASKED = ();
    @PROBES = ();
    %obj = %name_of = ();
    srand(1);
    my @e = SWorkspace::GetElements();
    reg( "e$_", $e[$_] ) for 0 .. $#e;
    return scalar(@e);
  },
  srand    => sub { srand( $_[0] ); return undef },
  rand     => sub { return rand() },
  feature  => sub {
    if ( $_[1] eq 'none' ) { delete $Global::Feature{ $_[0] } }
    else                   { $Global::Feature{ $_[0] } = $_[1] }
    return undef;
  },
  global => sub {
    no strict 'refs';
    ${"Global::$_[0]"} = $_[1];
    return undef;
  },
  describe => sub {
    my $r = $obj{ $_[0] }->describe_as( cat( $_[1] ) );
    return defined($r) ? 1 : 0;
  },
  gp => sub {
    my ( $name, @items ) = @_;
    my $g = Seqsee::Anchored->create( O(@items) );
    reg( $name, $g );
    return $g->as_text;
  },
  add    => sub { my @r = SWorkspace->add_group( $obj{ $_[0] } ); return [ map { num_or_undef($_) } @r ] },
  reln   => sub {
    my ( $name, $a, $b, $t, $c ) = @_;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => mapping( $t, $c ) } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  relnf => sub {
    my ( $name, $a, $b ) = @_;
    SLTM::SpikeBy( 100, $_ ) for @{ $obj{$a}->get_categories() };
    srand(1);
    my $t = Seqsee::Object::FindMapping( $obj{$a}, $obj{$b} ) or return undef;
    my $r = SRelation->new( { first => $obj{$a}, second => $obj{$b}, type => $t } );
    reg( $name, $r );
    $r->insert;
    return $r->as_text;
  },
  ruleapp => sub {
    my ( $g, $r ) = @_;
    my $ra = $obj{$g}->set_underlying_ruleapp( $obj{$r} );
    return defined($ra) ? ref($ra) : undef;
  },
  # A rule app with the group's items as they are, or the given items (no consistency check).
  fake_ruleapp => sub {
    my ( $g, $r, @items ) = @_;
    my $ra = SRuleApp->new(
      { rule => SRule->create( $obj{$r} ), items => ( @items ? [ O(@items) ] : [ @{ $obj{$g} } ] ),
        direction => $DIR::RIGHT } );
    $obj{$g}->set_underlying_reln($ra);
    return undef;
  },
  spike => sub {
    SLTM::SpikeBy( $_[1], cat( $_[0] ) );
    return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) );
  },
  activation => sub { return 0 + SLTM::GetRealActivationsForOneConcept( cat( $_[0] ) ) },
  thought => sub {
    my ( $name, $what ) = @_;
    my $t = SThought->create( $obj{$what} );
    reg( $name, $t );
    return ref($t);
  },
  thought_cat => sub {
    my ( $name, $c ) = @_;
    my $t = SThought->create( cat($c) );
    reg( $name, $t );
    return ref($t);
  },
  stream => sub {
    my $s = $Global::MainStream;
    return [ thought_name( $s->{CurrentThought} ), 0 + $s->{OlderThoughtCount},
      [ map { thought_name($_) } @{ $s->{OlderThoughts} } ] ];
  },
  current_codelet => sub { $Global::CurrentCodelet = SCodelet->new( $_[0], 50, {} ); return undef },
  readhead     => sub { return 0 + $SWorkspace::ReadHead },
  set_readhead => sub { $SWorkspace::ReadHead = $_[0]; return undef },
  items        => sub { my $g = $obj{ $_[0] }; return [ [ map { nm($_) } @$g ], $g->get_left_edge, $g->get_right_edge ] },
  supergroups  => sub { return [ sort map { nm($_) } SWorkspace->GetSuperGroups( $obj{ $_[0] } ) ] },
  hilit        => sub { return [ sort { $a->[0] cmp $b->[0] } map { [ $name_of{$_} // '?', $Global::Hilit{$_} ] } keys %Global::Hilit ] },
  recent       => sub {
    return undef unless $Global::RecentPromisingRuleApp;
    return $Global::RecentPromisingRuleApp eq $obj{ $_[0] }->get_underlying_reln ? 1 : 0;
  },
  last_solution     => sub { return num_or_undef($$LAST_SOLUTION) },
  last_progress     => sub { return num_or_undef($$LAST_PROGRESS) },
  set_last_progress => sub { $$LAST_PROGRESS = $_[0]; return undef },
  desperation       => sub { return 0 + Seqsee::SCF::CheckProgress::CalculateDesperation(@_) },
  strength          => sub { return 0 + $obj{ $_[0] }->get_strength },
  # Replace a family's run with a probe that records its arguments.
  probe => sub {
    my ($family) = @_;
    no strict 'refs';
    *{"Seqsee::SCF::${family}::run"} = sub {
      my ( $action, $args ) = @_;
      push @PROBES, [ $family, map { [ $_, summarize( $args->{$_} ) ] } sort keys %$args ];
    };
    return undef;
  },
  probes    => sub { my @p = @PROBES; @PROBES = (); return \@p },
  state     => sub { return state() },
  relations => sub { return relations() },
  coderack  => sub { return coderack() },
  run       => sub {
    my ( $family, @pairs ) = @_;
    my %args = map { ( $_->[0] => argval( $_->[1] ) ) } @pairs;
    no strict 'refs';
    "Seqsee::SCF::${family}::run"->( undef, \%args );
    return undef;
  },
  name_new => sub {
    my ($prefix) = @_;
    my @new = sort { $LEFT->{$a} <=> $LEFT->{$b} or $RIGHT->{$a} <=> $RIGHT->{$b} }
      grep { !exists $name_of{$_} } values %$OBJECTS;
    my $i = 0;
    reg( $prefix . $i++, $_ ) for @new;
    return [ map { $_->as_text } @new ];
  },
  # An object's metonym: [activeness, category, name, starred structure string].
  metonym => sub {
    my $o = $obj{ $_[0] };
    my $m = $o->get_metonym or return [ num_or_undef( $o->get_metonym_activeness ) ];
    return [ num_or_undef( $o->get_metonym_activeness ), $m->get_category->as_text, $m->get_name,
      $m->get_starred->get_structure_string ];
  },
  categories => sub { return [ sort map { noaddr( $_->as_text ) } @{ $obj{ $_[0] }->get_categories() } ] },
  flush      => sub { my $g = $obj{ $_[0] }; return [ $g->IsFlushLeft ? 1 : 0, $g->IsFlushRight ? 1 : 0 ] },
  live       => sub { return SWorkspace::__CheckLiveness( $obj{ $_[0] } ) ? 1 : 0 },
  spike_type => sub {
    SLTM::SpikeBy( $_[1], $obj{ $_[0] }->get_type );
    return 0 + SLTM::GetRealActivationsForOneConcept( $obj{ $_[0] }->get_type );
  },
  name_ruleapp => sub {
    my $ra = $obj{ $_[1] }->get_underlying_reln;
    reg( $_[0], $ra );
    return defined($ra) ? ref($ra) : undef;
  },
  rule => sub { reg( $_[0], SRule->create( $obj{ $_[1] } ) ); return undef },
  element => sub { reg( $_[0], Seqsee::Element->create( $_[1], -1 ) ); return undef },
  # The fake GUI commentary, answering with the given values in turn.
  commentary    => sub { $SGUI::Commentary = bless( {}, 'FakeCommentary' ); @ANSWERS = @_; return undef },
  no_commentary => sub { $SGUI::Commentary = undef; return undef },
  asked         => sub { my @a = @ASKED; @ASKED = (); return \@a },
  real_seq      => sub { @Global::RealSequence = @_; return undef },
  beyond => sub {
    my ( $name, @terms ) = @_;
    reg( $name, SErr::ElementsBeyondKnownSought->new( next_elements => [@terms] ) );
    return undef;
  },
  # Name an argument of the first scheduled codelet of a family.
  name_arg => sub {
    my ( $name, $family, $arg ) = @_;
    my ($cl) = grep { $_->[0] eq $family } @SCoderack::CODELETS;
    die "no $family" unless $cl;
    reg( $name, $cl->[3]{$arg} );
    return undef;
  },
  # Run the first scheduled codelet of a family, after clearing the coderack.
  run_scheduled => sub {
    my ($family) = @_;
    my ($cl) = grep { $_->[0] eq $family } @SCoderack::CODELETS;
    die "no $family" unless $cl;
    SCoderack->clear;
    no strict 'refs';
    "Seqsee::SCF::${family}::run"->( undef, $cl->[3] );
    return undef;
  },
  next_elements => sub { return [ @{ $obj{ $_[0] }->next_elements } ] },
  ask => sub {
    my ( $e, @args ) = @_;
    return scal( scalar( $obj{$e}->Ask(@args) ) );
  },
  ask_relation => sub { return scal( scalar( $obj{ $_[0] }->AskBasedOnRelation( $obj{ $_[1] }, $_[2] ) ) ) },
  ask_ruleapp  => sub { return scal( scalar( $obj{ $_[0] }->AskBasedOnRuleApp( $obj{ $_[1] }, $_[2] ) ) ) },
  ask_group    => sub { return scal( scalar( $obj{ $_[0] }->AskBasedOnGroup( $obj{ $_[1] }, $_[2] ) ) ) },
  bookkeeping  => sub { $obj{ $_[0] }->DoInsertBookKeeping(); return undef },
  rule_app_penetration => sub { return 0 + $obj{ $_[0] }->RuleAppPenetration( $_[1] ) },
  relation_penetration => sub {
    my @list = $obj{ $_[0] }->RelationPenetration( $obj{ $_[1] } );
    my $scalar = $obj{ $_[0] }->RelationPenetration( $obj{ $_[1] } );
    return [ scalar(@list), scal($scalar) ];
  },
  already_rejected => sub { return scal( Seqsee::already_rejected_by_user( [@_] ) ) },
  rejected         => sub { return [ sort keys %Global::ExtensionRejectedByUser ] },
  set_rejected     => sub { $Global::ExtensionRejectedByUser{$_} = 1 for @_; return undef },
  getg => sub {
    no strict 'refs';
    return scal( ${"Global::$_[0]"} );
  },
  ws => sub { return [ 0 + $SWorkspace::ElementCount, [ map { 0 + $_->get_mag } SWorkspace::GetElements() ] ] },
  # Test::Seqsee's main::ask_user_extension.
  user_ext        => sub { return scal( scalar( main::ask_user_extension( [@_] ) ) ) },
  failed_requests => sub { return num_or_undef( GetFailedRequests() ) },
  ras => sub {
    my ( $op, $r ) = @_;
    my %f = (
      most_recent     => \&RulesAskedSoFar::IsMostRecentSuccessfulRule,
      time_success    => \&RulesAskedSoFar::TimeSinceRuleUsedToExtendSuccessfully,
      time_failure    => \&RulesAskedSoFar::TimeSinceRuleUsedToExtendUnsuccessfully,
      add_success     => \&RulesAskedSoFar::AddRuleToSuccessList,
      add_failure     => \&RulesAskedSoFar::AddRuleToFailureList,
      mark_rejected   => \&RulesAskedSoFar::MarkRuleAsRejected,
      mark_confirmed  => \&RulesAskedSoFar::MarkRuleAsConfirmed,
      has_confirmed   => \&RulesAskedSoFar::HasRuleBeenConfirmed,
      has_rejected    => \&RulesAskedSoFar::HasRuleBeenRejected,
    );
    my $v = $f{$op}->( $obj{$r} );
    return $op =~ /^(add|mark)_/ ? undef : scal($v);
  },
  ras_state => sub {
    return [
      [ map { [ nm( $_->[0] ), 0 + $_->[1] ] } @$SUCC ],
      [ map { [ nm( $_->[0] ), 0 + $_->[1] ] } @$UNSUCC ],
      [ sort map { nm($_) } values %$ACCEPTED ],
      [ sort map { nm($_) } values %$REJECTED ],
    ];
  },
  ps => sub {
    my $ps = PositionStructure->Create( $obj{ $_[1] } );
    reg( $_[0], $ps );
    return [@$ps];
  },
  ps_list => sub {
    my ( $name, @positions ) = @_;
    reg( $name, bless( [@positions], 'PositionStructure' ) );
    return undef;
  },
  sc_reject => sub { SolutionConfirmation->AddRejectedSolution( $obj{ $_[0] }, $obj{ $_[1] } ); return undef },
  sc_accept => sub { SolutionConfirmation->SetAcceptedSolution( $obj{ $_[0] }, $obj{ $_[1] } ); return undef },
  sc_has    => sub { return scal( scalar( SolutionConfirmation->HasThisBeenRejected( $obj{ $_[0] }, $obj{ $_[1] } ) ) ) },
  flush_coderack => sub { SCoderack->clear; return undef },
  sc_state  => sub {
    return [
      nm($$SC_RULE), nm($$SC_PS),
      [ sort { $a->[0] cmp $b->[0] } map { [ $name_of{$_} // '?', scalar( @{ $SC_REJ->{$_} } ) ] } keys %$SC_REJ ],
    ];
  },
);

sub scal { my ($v) = @_; return defined($v) ? "$v" : undef }

sub scenario {
  my ( $name, @ops ) = @_;
  my @results;
  for my $op (@ops) {
    my ( $kind, @args ) = @$op;
    my $code = $OPS{$kind} or die "unknown op $kind";
    my $value;
    my $ok = eval { $value = $code->(@args); 1 };
    push @results, $ok ? { value => $value } : { error => err($@) };
  }
  record( scenario => $name, ops => \@ops, results => \@results );
}

my @after = ( ['state'], ['relations'], ['coderack'], ['rand'] );

# --- Seqsee::already_rejected_by_user, Test::Seqsee's main::ask_user_extension ---------------
scenario(
  'already_rejected',
  [ init => [ 1, 2, 3 ] ],
  [ set_rejected => '1, 2', '7' ],
  [ already_rejected => 1 ],
  [ already_rejected => 1, 2 ],
  [ already_rejected => 1, 2, 3 ],
  [ already_rejected => 2 ],
  [ already_rejected => 7, 8 ],
  [ already_rejected => 8, 7 ],
  ['already_rejected'],
);

scenario(
  'testing_user_ext',
  [ init => [ 1, 2, 3 ] ],
  [ real_seq => 1, 2, 3, 4, 5, 6 ],
  [ user_ext => 4 ],
  [ getg => 'AtLeastOneUserVerification' ],
  [ user_ext => 4, 5 ],
  [ user_ext => 5 ],
  ['failed_requests'],
  [ user_ext => 4, 6 ],
  ['failed_requests'],
  [ user_ext => 4, 5, 6 ],
  [ user_ext => 4, 5, 6, 7 ],
  ['user_ext'],
  [ set_rejected => '4' ],
  [ user_ext => 4, 5 ],
  ['rejected'],
  ['failed_requests'],
);

# --- SErr::ElementsBeyondKnownSought::Ask --------------------------------------------------
scenario(
  'ask_gui',
  [ init => [ 1, 2, 3 ] ],
  [ global => 'AcceptableTrustLevel', 0.9 ],
  [ global => 'Steps_Finished', 4 ],
  [ beyond => 'X', 4, 5 ],
  [ next_elements => 'X' ],
  [ commentary => 1 ],
  [ ask => 'X', 'pre: ', ' suf', 'dbg' ],
  ['asked'], ['ws'], ['rejected'],
  [ getg => 'AcceptableTrustLevel' ], [ getg => 'AtLeastOneUserVerification' ],
  [ getg => 'Break_Loop' ], [ getg => 'TimeOfLastNewElement' ], [ getg => 'TimeOfNewStructure' ],
  [ global => 'Break_Loop', undef ],
  [ beyond => 'Y', 6 ],
  [ commentary => 0 ],
  [ ask => 'Y' ],
  ['asked'], ['ws'], ['rejected'], [ getg => 'Break_Loop' ],
  [ commentary => 1 ],
  [ ask => 'Y', 'again' ],
  ['asked'],
  [ beyond => 'Z', 7, 8, 9 ],
  [ feature => 'debug', 1 ],
  [ commentary => '' ],
  [ ask => 'Z', 'p ', ' s', 'd' ],
  ['asked'], ['rejected'],
  [ feature => 'debug', 'none' ],
  [ beyond => 'W', 10 ],
  ['commentary'],
  [ ask => 'W' ],
  ['asked'], ['rejected'],
  ['no_commentary'],
  [ beyond => 'V', 11 ],
  [ ask => 'V' ],
  [ beyond => 'E' ],
  ['commentary'],
  [ ask => 'E' ],
  ['asked'],
);

scenario(
  'ask_testing',
  [ init => [ 1, 2, 3 ] ],
  [ global => 'TestingMode', 1 ],
  [ real_seq => 1, 2, 3, 4, 5 ],
  [ beyond => 'X', 4 ],
  [ ask => 'X', 'ignored' ],
  ['ws'], [ getg => 'AtLeastOneUserVerification' ], [ getg => 'Break_Loop' ], ['rejected'],
  [ beyond => 'Y', 9 ],
  [ ask => 'Y' ],
  ['failed_requests'], ['rejected'],
  [ beyond => 'Z', 4, 5, 6 ],
  [ ask => 'Z' ],
  [ set_rejected => '4' ],
  [ ask => 'X' ],
  ['failed_requests'],
);

scenario(
  'ask_based_on',
  [ init => [ 1, 2, 3, 4, 5 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e1', 'e2', 'e3' ],
  [ add => 'G' ],
  [ describe => 'G', 'ascending' ],
  [ ruleapp => 'G', 'R' ],
  [ name_ruleapp => 'RA', 'G' ],
  [ beyond => 'X1', 6 ],
  [ beyond => 'X2', 7 ],
  [ beyond => 'X3', 8 ],
  [ beyond => 'X4', 9, 10 ],
  [ commentary => 0, 0, 1, 0 ],
  [ ask_relation => 'X1', 'R', 'msg.' ],
  ['asked'], ['hilit'],
  [ ask_group => 'X2', 'G', 'grp.' ],
  ['asked'], ['hilit'],
  [ ask_ruleapp => 'X3', 'RA', 'ra.' ],
  ['asked'], ['hilit'], ['ws'],
  [ ask_relation => 'X4', 'R', undef ],
  ['asked'], ['hilit'], ['rejected'],
  [ rule_app_penetration => 'X1', 2 ],
  [ rule_app_penetration => 'X1', 0 ],
  [ relation_penetration => 'X1', 'R' ],
  [ beyond => 'B', 11, 12 ],
  [ global => 'Steps_Finished', 6 ],
  [ global => 'AcceptableTrustLevel', 0.7 ],
  [ bookkeeping => 'B' ],
  ['ws'],
  [ getg => 'AcceptableTrustLevel' ], [ getg => 'AtLeastOneUserVerification' ],
  [ getg => 'Break_Loop' ], [ getg => 'TimeOfLastNewElement' ],
);

# --- RulesAskedSoFar, SolutionConfirmation -------------------------------------------------
scenario(
  'rules_asked_so_far',
  [ init => [ 1, 2, 3, 4 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R1', 'e0', 'e1', 'succ' ],
  [ reln => 'R2', 'e2', 'e1', 'pred' ],
  [ rule => 'A', 'R1' ],
  [ rule => 'B', 'R2' ],
  [ ras => 'most_recent', 'A' ],
  [ ras => 'time_success', 'A' ],
  [ ras => 'time_failure', 'A' ],
  [ global => 'Steps_Finished', 3 ],
  [ ras => 'add_success', 'A' ],
  [ global => 'Steps_Finished', 7 ],
  [ ras => 'most_recent', 'A' ],
  [ ras => 'most_recent', 'B' ],
  [ ras => 'time_success', 'A' ],
  [ ras => 'add_success', 'B' ],
  [ ras => 'most_recent', 'A' ],
  [ ras => 'most_recent', 'B' ],
  [ ras => 'time_success', 'A' ],
  [ ras => 'time_success', 'B' ],
  [ ras => 'add_success', 'A' ],
  [ ras => 'add_failure', 'A' ],
  [ ras => 'time_failure', 'A' ],
  [ global => 'Steps_Finished', 10 ],
  [ ras => 'time_failure', 'A' ],
  [ ras => 'time_failure', 'B' ],
  [ ras => 'time_success', 'A' ],
  [ ras => 'has_confirmed', 'A' ],
  [ ras => 'has_rejected', 'A' ],
  [ ras => 'mark_rejected', 'A' ],
  [ ras => 'mark_confirmed', 'B' ],
  [ ras => 'mark_confirmed', 'B' ],
  [ ras => 'has_confirmed', 'A' ],
  [ ras => 'has_confirmed', 'B' ],
  [ ras => 'has_rejected', 'A' ],
  [ ras => 'has_rejected', 'B' ],
  ['ras_state'],
);

scenario(
  'solution_confirmation',
  [ init => [ 1, 2, 3, 4, 5, 6 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R1', 'e0', 'e1', 'succ' ],
  [ reln => 'R2', 'e2', 'e1', 'pred' ],
  [ rule => 'A', 'R1' ],
  [ rule => 'B', 'R2' ],
  [ gp => 'G', 'e0', 'e1', 'e2' ],
  [ gp => 'H', 'e1', 'e2', 'e3' ],
  [ gp => 'K', 'e0', 'e1', 'e2', 'e3' ],
  [ gp => 'L', 'e0', 'e1' ],
  [ add => 'L' ],
  [ gp => 'M', 'L', 'e2', 'e3' ],
  [ ps => 'P', 'G' ],
  [ ps => 'Q', 'K' ],
  [ ps => 'S', 'H' ],
  [ ps => 'T', 'M' ],
  [ ps_list => 'U', '[1]', '[2]' ],
  [ sc_has => 'A', 'P' ],
  ['sc_state'],
  [ sc_reject => 'A', 'P' ],
  [ sc_has => 'A', 'P' ],
  [ sc_has => 'A', 'Q' ],
  [ sc_has => 'A', 'S' ],
  [ sc_has => 'A', 'T' ],
  [ sc_has => 'B', 'P' ],
  [ sc_reject => 'A', 'U' ],
  [ sc_has => 'A', 'S' ],
  [ sc_has => 'A', 'T' ],
  [ sc_reject => 'B', 'T' ],
  [ sc_has => 'B', 'T' ],
  [ sc_has => 'B', 'Q' ],
  [ sc_accept => 'B', 'Q' ],
  ['sc_state'],
);

# --- AskIfThisIsTheContinuation ------------------------------------------------------------
# Scheduled by AttemptExtensionOfRelation beyond the known elements (seed 2 asks).
my @aer_setup = (
  [ init => [ 1, 2, 3 ] ],
  [ reln => 'R12', 'e1', 'e2', 'succ' ],
  [ spike_type => 'R12', 100 ],
  [ spike_type => 'R12', 100 ],
  [ srand => 2 ],
  [ run => 'AttemptExtensionOfRelation', [ core => [ obj => 'R12' ] ], [ direction => [ dir => 'RIGHT' ] ] ],
  ['coderack'],
  [ name_arg => 'EX', 'AskIfThisIsTheContinuation', 'exception' ],
  [ name_arg => 'OBJ', 'AskIfThisIsTheContinuation', 'expected_object' ],
  [ next_elements => 'EX' ],
);

for my $answer ( 1, 0 ) {
  scenario(
    "ask_continuation_relation_$answer",
    @aer_setup,
    [ commentary => $answer ],
    [ run_scheduled => 'AskIfThisIsTheContinuation' ],
    ['asked'], ['ws'], ['rejected'], [ getg => 'Break_Loop' ], [ name_new => 'N' ], @after,
  );
}

scenario(
  'ask_continuation_relation_misc',
  @aer_setup,
  ['commentary'],
  # known_term_count out of date: nothing.
  [ run => 'AskIfThisIsTheContinuation', [ relation => [ obj => 'R12' ] ], [ exception => [ obj => 'EX' ] ],
    [ expected_object => [ obj => 'OBJ' ] ], [ start_position => [ val => 3 ] ], [ known_term_count => [ val => 2 ] ] ],
  ['asked'],
  # neither relation nor group.
  [ run => 'AskIfThisIsTheContinuation', [ exception => [ obj => 'EX' ] ],
    [ expected_object => [ obj => 'OBJ' ] ], [ start_position => [ val => 3 ] ], [ known_term_count => [ val => 3 ] ] ],
  [ run => 'AskIfThisIsTheContinuation', [ relation => [ obj => 'R12' ] ], [ exception => [ obj => 'EX' ] ] ],
  # testing mode: a yes inserts nothing, so the plonk fails.
  [ global => 'TestingMode', 1 ],
  [ real_seq => 1, 2, 3, 4 ],
  [ run => 'AskIfThisIsTheContinuation', [ relation => [ obj => 'R12' ] ], [ exception => [ obj => 'EX' ] ],
    [ expected_object => [ obj => 'OBJ' ] ], [ start_position => [ val => 3 ] ], [ known_term_count => [ val => 3 ] ] ],
  ['asked'], ['ws'], [ getg => 'AtLeastOneUserVerification' ], @after,
);

for my $variant ( 'ruleapp', 'bare', 'no' ) {
  scenario(
    "ask_continuation_group_$variant",
    [ init => [ 1, 2, 3, 4 ] ],
    [ spike => 'number', 10 ],
    [ reln => 'R', 'e0', 'e1', 'succ' ],
    [ gp => 'G', 'e1', 'e2', 'e3' ],
    [ add => 'G' ],
    [ describe => 'G', 'ascending' ],
    ( $variant eq 'bare' ? () : [ ruleapp => 'G', 'R' ] ),
    [ beyond => 'X', 5 ],
    [ element => 'O', 5 ],
    [ commentary => ( $variant eq 'no' ? 0 : 1 ) ],
    [ run => 'AskIfThisIsTheContinuation', [ group => [ obj => 'G' ] ], [ exception => [ obj => 'X' ] ],
      [ expected_object => [ obj => 'O' ] ], [ start_position => [ val => 4 ] ], [ known_term_count => [ val => 4 ] ] ],
    ['asked'], ['ws'], ['rejected'], [ items => 'G' ], [ name_new => 'N' ], @after,
  );
}

# --- MaybeAskTheseTerms, MaybeAskUsingThisGoodRule, DoTheAsking ----------------------------
scenario(
  'maybe_ask_ruleapp',
  [ init => [ 1, 2, 3, 4, 5 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R', 'e0', 'e1', 'succ' ],
  [ gp => 'G', 'e1', 'e2', 'e3', 'e4' ],
  [ add => 'G' ],
  [ describe => 'G', 'ascending' ],
  [ ruleapp => 'G', 'R' ],
  [ name_ruleapp => 'RA', 'G' ],
  [ rule => 'A', 'R' ],
  [ beyond => 'X', 6 ],
  [ run => 'MaybeAskTheseTerms', [ core => [ obj => 'RA' ] ], [ exception => [ obj => 'X' ] ] ],
  ['coderack'], ['ras_state'], ['rand'],
  # DoTheAsking without msg_prefix ("defualt" typo: mandatory).
  [ run_scheduled => 'DoTheAsking' ],
  # success at the same step counts as never.
  [ ras => 'add_success', 'A' ],
  [ run => 'MaybeAskTheseTerms', [ core => [ obj => 'RA' ] ], [ exception => [ obj => 'X' ] ] ],
  ['coderack'], ['ras_state'],
  [ global => 'Steps_Finished', 5 ],
  [ ras => 'add_success', 'A' ],
  [ global => 'Steps_Finished', 8 ],
  ['flush_coderack'],
  [ run => 'MaybeAskTheseTerms', [ core => [ obj => 'RA' ] ], [ exception => [ obj => 'X' ] ] ],
  ['coderack'], ['ras_state'],
  [ run_scheduled => 'MaybeAskUsingThisGoodRule' ],
  ['coderack'],
  [ commentary => 1 ],
  [ run_scheduled => 'DoTheAsking' ],
  ['asked'], ['ws'], ['ras_state'], @after,
);

for my $seed ( 1 .. 4 ) {
  scenario(
    "maybe_ask_relation_$seed",
    [ init => [ 1, 2, 3 ] ],
    [ reln => 'R', 'e1', 'e2', 'succ' ],
    [ rule => 'A', 'R' ],
    [ beyond => 'X', 4 ],
    [ strength => 'R' ],
    [ srand => $seed ],
    [ run => 'MaybeAskTheseTerms', [ core => [ obj => 'R' ] ], [ exception => [ obj => 'X' ] ] ],
    [ spike_type => 'R', 0 ],
    ['ras_state'], @after,
    [ run => 'MaybeAskTheseTerms', [ core => [ obj => 'R' ] ], [ exception => [ obj => 'X' ] ] ],
    [ spike_type => 'R', 0 ],
    ['ras_state'], @after,
  );
}

scenario(
  'do_the_asking',
  [ init => [ 1, 2, 3 ] ],
  [ reln => 'R', 'e1', 'e2', 'succ' ],
  [ rule => 'A', 'R' ],
  [ beyond => 'X', 4 ],
  [ beyond => 'Y', 5 ],
  [ commentary => 0, 1 ],
  [ run => 'DoTheAsking', [ core => [ obj => 'R' ] ], [ exception => [ obj => 'Y' ] ], [ msg_prefix => [ val => 'Hi.' ] ] ],
  ['asked'], ['ras_state'], ['rejected'],
  [ global => 'Steps_Finished', 2 ],
  [ run => 'DoTheAsking', [ core => [ obj => 'R' ] ], [ exception => [ obj => 'X' ] ], [ msg_prefix => [ val => '' ] ] ],
  ['asked'], ['ras_state'], ['ws'], ['rejected'], @after,
  [ run => 'DoTheAsking', [ core => [ val => 'foo' ] ], [ exception => [ obj => 'X' ] ], [ msg_prefix => [ val => '' ] ] ],
  [ run => 'MaybeAskTheseTerms', [ core => [ val => 'foo' ] ], [ exception => [ obj => 'X' ] ] ],
  [ run => 'MaybeAskTheseTerms', [ core => [ obj => 'R' ] ] ],
  [ run => 'MaybeAskTheseTerms', [ exception => [ obj => 'X' ] ] ],
  [ run => 'MaybeAskUsingThisGoodRule', [ core => [ obj => 'R' ] ], [ exception => [ obj => 'X' ] ] ],
  [ run => 'DoTheAsking', [ core => [ obj => 'R' ] ], [ exception => [ obj => 'X' ] ] ],
);

emit();

