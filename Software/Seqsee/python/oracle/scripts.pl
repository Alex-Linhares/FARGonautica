# Oracle for lib/Seqsee/Scripts.pm and lib/Seqsee/Scripts/DescribeSolution.pm (item 045):
# the script runner (Seqsee::Scripts::run, RETURN, SCRIPT) and the script families
# DescribeSolution, DescribeInitialBlemish, DescribeBlocks, DescribeRule, DescribeMapping,
# DescribeRelationSimple, DescribeRelationCompound, DescribeRelnCategory,
# DescribeInterlacedCategory, Describe2InterlacedCategory, DescribeMultipleInterlacedCategory
# and DescribeRelnMetoMode.
# Output: tests/golden/scripts.json
#
# The harness (helpers and ops) is a copy of ui.pl's; tests/test_scripts.py replays the
# scenarios op for op with test_ui's interpreter plus the ops added at the end. The script
# runner prints to STDOUT, so the scenarios run with a /dev/null handle selected.
# main::message / main::debug_message are recorded (op `messages`), SLTM->Dump is recorded
# instead of writing memory_dump.dat (op `dumps`), and the fake $SGUI::Commentary also
# answers MessageRequiringAResponse (op `responses`).
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


# --- Seqsee/Scripts.pm additions ------------------------------------------------------------
my ( @MESSAGES, @RESPONSES, @DUMPS );
*main::message       = sub { my ( $m, $l ) = @_; push @MESSAGES, [ $m, $l ] };
*main::debug_message = sub { my ( $m, $l ) = @_; push @MESSAGES, [ 'DEBUG', noaddr($m), $l ] };
*SLTM::Dump          = sub { push @DUMPS, $_[1] };

sub FakeCommentary::MessageRequiringAResponse {
  my ( $self, $choices, $question ) = @_;
  push @RESPONSES, [ join( '+', @$choices ), $question ];
  return shift @ANSWERS;
}

sub args_summary {
  my ($h) = @_;
  return nm($h) unless ref($h) eq 'HASH';
  return [ map { [ $_, summarize( $h->{$_} ) ] } sort keys %$h ];
}

# Each scheduled codelet: [family, urgency, step, args, stack]; a stack frame is
# [step_no, family, args].
sub scripts {
  return [
    map {
      my $a = $_->[3];
      [ $_->[0], 0 + $_->[1], $a->{__S_T_E_P__}, args_summary( $a->{__A_R_G_S__} ),
        [ map { [ $_->[0], $_->[2], args_summary( $_->[1] ) ] } @{ $a->{__S_T_A_C_K__} } ] ]
    } @SCoderack::CODELETS
  ];
}

my %EXTRA = (
  messages  => sub { my @m = @MESSAGES; @MESSAGES = (); return \@m },
  responses => sub { my @r = @RESPONSES; @RESPONSES = (); return \@r },
  dumps     => sub { my @d = @DUMPS; @DUMPS = (); return \@d },
  scripts   => sub { return scripts() },
  # Run a family's run sub with a codelet of that family as the action object.
  script => sub {
    my ( $family, @pairs ) = @_;
    my %args = map { ( $_->[0] => argval( $_->[1] ) ) } @pairs;
    my $cl = SCodelet->new( $family, 50, \%args );
    no strict 'refs';
    "Seqsee::SCF::${family}::run"->( $cl, \%args );
    return undef;
  },
  # Resume a script at a step (the args a scheduled script codelet carries; empty stack).
  script_resume => sub {
    my ( $family, $step, @pairs ) = @_;
    my %args = map { ( $_->[0] => argval( $_->[1] ) ) } @pairs;
    my $full = { __S_T_E_P__ => $step, __A_R_G_S__ => \%args, __S_T_A_C_K__ => [] };
    my $cl = SCodelet->new( $family, 50, $full );
    no strict 'refs';
    "Seqsee::SCF::${family}::run"->( $cl, $full );
    return undef;
  },
  # The same with a raw args value; a false $action passes an undef action object.
  script_raw => sub {
    my ( $family, $action, $args ) = @_;
    $action = $action ? SCodelet->new( $family, 50, {} ) : undef;
    no strict 'refs';
    "Seqsee::SCF::${family}::run"->( $action, $args );
    return undef;
  },
  # Take the only scheduled codelet off the coderack and run it (SCodelet::run).
  run_next => sub {
    die "coderack has " . scalar(@SCoderack::CODELETS) . "\n" unless @SCoderack::CODELETS == 1;
    my ($cl) = @SCoderack::CODELETS;
    SCoderack->clear;
    $cl->run;
    return $cl->[0];
  },
  meta => sub {
    my ($family) = @_;
    my $pkg = "Seqsee::SCF::$family";
    return [ 0 + $pkg->number_of_steps, [ $pkg->expected_attributes ] ];
  },
  name_rule => sub { reg( $_[0], $obj{ $_[1] }->get_underlying_reln->get_rule ); return undef },
  name_type => sub { reg( $_[0], $obj{ $_[1] }->get_type ); return undef },
  best      => sub { return [ nm($Global::BestRuleApp), nm($Global::BestRule) ] },
  sc_full   => sub {
    return [
      nm($$SC_RULE), ( $$SC_PS ? [@$$SC_PS] : undef ),
      [ sort { $a->[0] cmp $b->[0] }
        map { [ $name_of{$_} // '?', [ map { [@$_] } @{ $SC_REJ->{$_} } ] ] } keys %$SC_REJ ],
    ];
  },
);
$OPS{$_} = $EXTRA{$_} for keys %EXTRA;

# MooseX::Params::Validate caches each call site's spec (keyed by the calling sub, here
# Seqsee::Scripts::run) on first use. init clears that cache so scenarios are independent;
# `clear_spec_cache` clears it mid-scenario.
my $SPEC_CACHE = closed_over( \&MooseX::Params::Validate::validated_list )->{'%CACHED_SPECS'};
die "PadWalker 4" unless $SPEC_CACHE;
$OPS{clear_spec_cache} = sub { %$SPEC_CACHE = (); return undef };
{
  my $init = $OPS{init};
  $OPS{init} = sub { %$SPEC_CACHE = (); return $init->(@_) };
}
{
  my $old = \&argval;
  *argval = sub {
    my ( $kind, @v ) = @{ $_[0] };
    no strict 'refs';
    return ${"METO_MODE::$v[0]"} if $kind eq 'meto';
    return $old->( $_[0] );
  };
}

open( my $NULL, '>', '/dev/null' ) or die;
select($NULL);

my @after = ( ['state'], ['scripts'], ['messages'], ['rand'] );

# --- The families' steps and attributes ----------------------------------------------------
scenario(
  'meta',
  map { [ meta => $_ ] }
    qw(DescribeSolution DescribeInitialBlemish DescribeBlocks DescribeRule DescribeMapping
    DescribeRelationSimple DescribeRelationCompound DescribeRelnCategory DescribeInterlacedCategory
    Describe2InterlacedCategory DescribeMultipleInterlacedCategory DescribeRelnMetoMode),
);

# --- DescribeSolution, the whole chain ----------------------------------------------------
my @solution_world = (
  [ init => [ 7, 1, 2, 3, 4 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R', 'e1', 'e2', 'succ' ],
  [ gp => 'G', 'e1', 'e2', 'e3', 'e4' ],
  [ add => 'G' ],
  [ describe => 'G', 'ascending' ],
  [ ruleapp => 'G', 'R' ],
  [ name_rule => 'A', 'G' ],
);

for my $answer ( 'Yes', 'No', 'headless', 'ltm' ) {
  scenario(
    "describe_solution_$answer",
    @solution_world,
    # A group inconsistent with the rule app (overlaps it).
    [ gp => 'H', 'e0', 'e1' ],
    [ add => 'H' ],
    ( $answer eq 'headless' ? () : [ commentary => ( $answer eq 'ltm' ? 'Yes' : $answer ) ] ),
    ( $answer eq 'ltm' ? [ feature => 'LTM', 1 ] : () ),
    [ script => 'DescribeSolution', [ group => [ obj => 'G' ] ] ],
    @after,
    # DescribeInitialBlemish
    ['run_next'], @after,
    # DescribeSolution, step 2
    ['run_next'], @after,
    # DescribeBlocks
    ['run_next'], @after,
    # DescribeSolution, steps 3-6
    ['run_next'], @after,
    ['responses'], ['dumps'], ['sc_full'], ['best'],
    # Again: a rejected solution stops at step 0.
    ( $answer eq 'headless' ? () : [ commentary => 'Yes' ] ),
    [ script => 'DescribeSolution', [ group => [ obj => 'G' ] ] ],
    @after,
  );
}

scenario(
  'describe_solution_no_ruleapp',
  [ init => [ 1, 2, 3 ] ],
  [ gp => 'G', 'e0', 'e1', 'e2' ],
  [ add => 'G' ],
  [ script => 'DescribeSolution', [ group => [ obj => 'G' ] ] ],
  @after,
);

scenario(
  'describe_solution_rejected_before',
  @solution_world,
  [ ps => 'P', 'G' ],
  [ sc_reject => 'A', 'P' ],
  [ script => 'DescribeSolution', [ group => [ obj => 'G' ] ] ],
  @after,
  ['sc_full'],
);

scenario(
  'describe_solution_args',
  [ init => [ 1, 2, 3 ] ],
  [ gp => 'G', 'e0', 'e1', 'e2' ],
  [ add => 'G' ],
  [ script => 'DescribeSolution' ],
  [ script => 'DescribeSolution', [ group => [ obj => 'G' ] ], [ other => [ val => 1 ] ] ],
  [ script => 'DescribeBlocks' ],
  [ script_raw => 'DescribeSolution', 0, {} ],
  [ script_raw => 'DescribeBlocks', 1, undef ],
  [ script_raw => 'DescribeSolution', 1, { group => undef } ],
  ['clear_spec_cache'],
  [ script => 'DescribeRule', [ rule => [ val => 1 ] ] ],
  [ script => 'DescribeRule', [ rule => [ val => 1 ] ], [ ruleapp => [ val => 2 ] ], [ zzz => [ val => 3 ] ],
    [ aaa => [ val => 3 ] ] ],
  [ script => 'DescribeRule' ],
  ['clear_spec_cache'],
  [ script => 'DescribeRelnMetoMode', [ ruleapp => [ val => 2 ] ] ],
  @after,
);

# PERL-QUIRK: the first script validated fixes the spec (and argument order) for all.
scenario(
  'spec_cached_by_first_script',
  [ init => [ 1, 2, 3, 4 ] ],
  [ spike => 'number', 10 ],
  [ reln => 'R', 'e1', 'e2', 'succ' ],
  [ name_type => 'T', 'R' ],
  [ gp => 'G', 'e1', 'e2', 'e3' ],
  [ add => 'G' ],
  [ script => 'DescribeBlocks', [ group => [ obj => 'G' ] ] ],
  [ script => 'DescribeRule', [ rule => [ val => 1 ] ], [ ruleapp => [ val => 2 ] ] ],
  [ script => 'DescribeRelationSimple', [ reln => [ obj => 'T' ] ] ],
  [ script => 'DescribeInitialBlemish', [ group => [ obj => 'G' ] ] ],
  [ script => 'DescribeSolution', [ group => [ obj => 'G' ] ] ],
  @after,
  ['clear_spec_cache'],
  # DescribeMapping's spec: reln, then ruleapp (default 0).
  [ script => 'DescribeMapping', [ reln => [ obj => 'T' ] ] ],
  @after,
  [ script => 'DescribeBlocks', [ group => [ obj => 'G' ] ] ],
  ['flush_coderack'],
  [ script => 'DescribeRelationSimple', [ reln => [ obj => 'T' ] ] ],
  [ script => 'DescribeRelnCategory', [ reln => [ cat => 'ascending' ] ], [ ruleapp => [ val => 0 ] ] ],
  @after,
  ['clear_spec_cache'],
  # DescribeRelnCategory's spec, cat then ruleapp; DescribeRelationCompound gets (cat, ruleapp).
  [ script => 'DescribeRelnCategory', [ cat => [ cat => 'ascending' ] ], [ ruleapp => [ val => 0 ] ] ],
  [ script => 'DescribeRelationCompound', [ cat => [ obj => 'T' ] ], [ ruleapp => [ val => 0 ] ] ],
  @after,
);

# --- DescribeInitialBlemish, DescribeBlocks ---------------------------------------------------
scenario(
  'initial_blemish_and_blocks',
  [ init => [ 7, 8, 1, 2, 3, 4, 5 ] ],
  [ gp => 'G', 'e2', 'e3', 'e4' ],
  [ gp => 'K', 'e1', 'e2', 'e3' ],
  [ gp => 'L', 'e0', 'e1' ],
  [ add => 'L' ],
  [ gp => 'B', 'e2', 'e3' ],
  [ add => 'B' ],
  [ gp => 'M', 'L', 'B', 'e4' ],
  [ script => 'DescribeInitialBlemish', [ group => [ obj => 'G' ] ] ],
  [ script => 'DescribeInitialBlemish', [ group => [ obj => 'K' ] ] ],
  [ script => 'DescribeInitialBlemish', [ group => [ obj => 'M' ] ] ],
  [ script => 'DescribeBlocks', [ group => [ obj => 'G' ] ] ],
  [ script => 'DescribeBlocks', [ group => [ obj => 'M' ] ] ],
  @after,
);

# --- DescribeRule → DescribeMapping → DescribeRelationSimple ---------------------------------
my %rule_seq = ( succ => [ 1, 2, 3, 4 ], pred => [ 4, 3, 2, 1 ], same => [ 5, 5, 5, 5 ] );
for my $name ( 'succ', 'pred', 'same' ) {
  scenario(
    "describe_rule_$name",
    [ init => $rule_seq{$name} ],
    [ spike => 'number', 10 ],
    [ reln => 'R', 'e1', 'e2', $name ],
    [ gp => 'G', 'e1', 'e2', 'e3' ],
    [ add => 'G' ],
    [ ruleapp => 'G', 'R' ],
    [ name_rule => 'A', 'G' ],
    [ name_ruleapp => 'RA', 'G' ],
    [ name_type => 'T', 'R' ],
    [ script => 'DescribeRule', [ rule => [ obj => 'A' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
    @after, ['best'],
    # DescribeMapping
    ['clear_spec_cache'], ['run_next'], @after,
    # DescribeRelationSimple (never returns to DescribeMapping/DescribeRule)
    ['clear_spec_cache'], ['run_next'], @after, ['best'],
    ['clear_spec_cache'],
    [ script => 'DescribeRelationSimple', [ reln => [ obj => 'T' ] ] ],
    ['clear_spec_cache'],
    [ script => 'DescribeMapping', [ reln => [ obj => 'T' ] ] ],
    @after,
    [ script => 'DescribeMapping', [ reln => [ obj => 'G' ] ] ],
    [ script => 'DescribeMapping', [ reln => [ val => 'x' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
    @after,
  );
}

# --- Structural mappings: DescribeRelationCompound and the category scripts ------------------
my @interlaced_world = (
  [ init => [ 1, 6, 2, 7, 3, 8 ] ],
  [ spike => 'number', 10 ],
  [ gp => 'P1', 'e0', 'e1' ],
  [ add => 'P1' ],
  [ describe => 'P1', 'interlaced2' ],
  [ gp => 'P2', 'e2', 'e3' ],
  [ add => 'P2' ],
  [ describe => 'P2', 'interlaced2' ],
  [ gp => 'P3', 'e4', 'e5' ],
  [ add => 'P3' ],
  [ describe => 'P3', 'interlaced2' ],
  [ relnf => 'R', 'P1', 'P2' ],
  [ gp => 'G', 'P1', 'P2', 'P3' ],
  [ add => 'G' ],
  [ ruleapp => 'G', 'R' ],
  [ name_ruleapp => 'RA', 'G' ],
  [ name_type => 'T', 'R' ],
);

scenario(
  'describe_mapping_structural',
  @interlaced_world,
  [ script => 'DescribeMapping', [ reln => [ obj => 'T' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  # DescribeRelationCompound (validated with DescribeMapping's cached spec)
  ['run_next'], @after,
  # DescribeRelnCategory: its cat argument is not in the cached spec.
  ['run_next'], @after,
  # Again, clearing the spec cache before each script.
  ['clear_spec_cache'],
  [ script => 'DescribeMapping', [ reln => [ obj => 'T' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  ['clear_spec_cache'], ['run_next'], @after,
  # DescribeRelnCategory
  ['clear_spec_cache'], ['run_next'], @after,
  # DescribeInterlacedCategory
  ['clear_spec_cache'], ['run_next'], @after,
  # Describe2InterlacedCategory
  ['clear_spec_cache'], ['run_next'], @after,
  # No ruleapp given: the default 0 is passed on.
  ['clear_spec_cache'],
  [ script => 'DescribeMapping', [ reln => [ obj => 'T' ] ] ],
  @after,
  ['clear_spec_cache'], ['run_next'], @after,
);

scenario(
  'describe_categories',
  @interlaced_world,
  [ script => 'DescribeRelnCategory', [ cat => [ cat => 'ascending' ] ], [ ruleapp => [ val => 0 ] ] ],
  [ script => 'DescribeRelnCategory', [ cat => [ cat => 'interlaced2' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  ['flush_coderack'],
  ['clear_spec_cache'],
  [ script => 'DescribeInterlacedCategory', [ cat => [ cat => 'interlaced3' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  ['clear_spec_cache'], ['run_next'], @after,
  ['clear_spec_cache'],
  [ script => 'Describe2InterlacedCategory', [ cat => [ cat => 'interlaced2' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  ['clear_spec_cache'],
  [ script => 'DescribeRelnMetoMode', [ meto_mode => [ meto => 'NONE' ] ], [ meto_reln => [ val => 0 ] ],
    [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  [ script => 'DescribeRelnMetoMode', [ meto_mode => [ meto => 'SINGLE' ] ], [ meto_reln => [ val => 0 ] ],
    [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  ['clear_spec_cache'],
  [ script => 'DescribeRelationCompound', [ reln => [ obj => 'T' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  # Step 1 of DescribeRelationCompound is only reached by resuming at step 1.
  ['flush_coderack'],
  ['clear_spec_cache'],
  [ script_resume => 'DescribeRelationCompound', 1, [ reln => [ obj => 'T' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
  # DescribeRelnMetoMode (no metonymy: RETURN to DescribeRelationCompound, step 2)
  ['clear_spec_cache'], ['run_next'], @after,
  # Past the last step: nothing.
  ['clear_spec_cache'], ['run_next'], @after,
);

# A longer interlaced group (only three items are described).
scenario(
  'describe_interlaced_long',
  [ init => [ 1, 6, 2, 7, 3, 8, 4, 9 ] ],
  [ spike => 'number', 10 ],
  ( map {
      my $i = $_;
      ( [ gp => "P$i", 'e' . ( 2 * $i - 2 ), 'e' . ( 2 * $i - 1 ) ], [ add => "P$i" ],
        [ describe => "P$i", 'interlaced2' ] )
    } 1 .. 4 ),
  [ relnf => 'R', 'P1', 'P2' ],
  [ gp => 'G', 'P1', 'P2', 'P3', 'P4' ],
  [ add => 'G' ],
  [ ruleapp => 'G', 'R' ],
  [ name_ruleapp => 'RA', 'G' ],
  [ script => 'Describe2InterlacedCategory', [ cat => [ cat => 'interlaced2' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  [ script => 'DescribeMultipleInterlacedCategory', [ cat => [ cat => 'interlaced4' ] ], [ ruleapp => [ obj => 'RA' ] ] ],
  ['clear_spec_cache'],
  [ script => 'DescribeRelnMetoMode', [ meto_mode => [ meto => 'ALL' ] ], [ meto_reln => [ val => 0 ] ],
    [ ruleapp => [ obj => 'RA' ] ] ],
  @after,
);

# Last (Mapping::Numeric's state %MEMO survives SLTM->Clear): non-NUMBER relations.
scenario(
  'describe_relation_even',
  [ init => [ 2, 4, 6, 8 ] ],
  ( map { [ spike => $_, 1 ] } qw(prime odd mountain sameness descending ascending even) ),
  [ reln => 'R', 'e0', 'e1', 'succ', 'even' ],
  [ reln => 'S', 'e1', 'e2', 'same', 'even' ],
  [ name_type => 'T', 'R' ],
  [ name_type => 'U', 'S' ],
  [ script => 'DescribeRelationSimple', [ reln => [ obj => 'T' ] ] ],
  [ script => 'DescribeRelationSimple', [ reln => [ obj => 'U' ] ] ],
  @after,
  ['clear_spec_cache'],
  [ rule => 'A', 'R' ],
  [ script => 'DescribeRule', [ rule => [ obj => 'A' ] ], [ ruleapp => [ val => 0 ] ] ],
  @after,
  ['clear_spec_cache'], ['run_next'], @after,
  ['clear_spec_cache'], ['run_next'], @after,
);

select(STDOUT);
emit();
