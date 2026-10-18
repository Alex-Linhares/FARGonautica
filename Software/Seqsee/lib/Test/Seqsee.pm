use 5.10.0;
use strict;
use Test::More;
use Test::Exception;
use Test::Deep;
use Test::Stochastic qw(stochastic_all_seen_ok stochastic_all_seen_nok
stochastic_all_and_only_ok
stochastic_all_and_only_nok
);
use English qw(-no_match_vars);
use Carp;
use Sub::Installer;

use S;
use Seqsee;
use Smart::Comments;
use Config::Std;
use List::Util qw(sum);

## useful to turn a few features off...
$Global::TestingMode           = 1;
$Global::CurrentRunnableString = "";

{
  my $failed_requests;

  sub ResetFailedRequests {
    $failed_requests = 0;
  }

  sub IncrementFailedRequests {
    $failed_requests++;
  }

  sub GetFailedRequests {
    return $failed_requests;
  }
}

sub undef_ok {
  my ( $what, $msg ) = @_;
  if ( not( defined $what ) ) {
    $msg ||= "is undefined";
    ok( 1, $msg );
  }
  else {
    $msg ||= "expected undef, got $what";
    ok( 0, $msg );
  }
}

sub instance_of_cat_ok {
  my ( $what, $cat, $msg ) = @_;
  no warnings;
  $msg ||= "$what is an instance of $cat";
  ok( $what->instance_of_cat($cat), $msg );
}

sub throws_thought_ok {
  my ( $cl, $type ) = @_;

  my @types = ( ref($type) eq "ARRAY" ) ? @$type :($type);
  @types = map { /^SThought::/ ? $_ :"SThought::$_" } @types;

  eval { $cl->run; };
  my $e;
  unless ( $e = $EVAL_ERROR ) {
    ok( 0, "No thought returned" );
    return;
  }
  my $payload = $e->payload;

  unless ($payload) {
    ok( 0, "Died without payload" );
    return;
  }

  for (@types) {
    if ( $payload->isa($_) ) {
      ok( 1, "$payload returned" );
      return $payload;
    }
  }

  ok( 0, "Wrong type: $payload. Expected one of: " . join( ", ", @types ) );
}

sub throws_no_thought_ok {
  my ($cl) = @_;

  eval { $cl->run; };
  my $e;
  if ( $e = $EVAL_ERROR ) {
    ok( 0, "Should return no thought! $e" );
    return;
  }
  ok( 1, "Lived Ok" );
}

sub _wrap_to_get_payload_type {
  my ( $subr, $check_sub ) = @_;

  # check_sub runs only if no eval error.
  return sub {
    eval { $subr->() };
    if ( my $e = $EVAL_ERROR ) {
      if ( UNIVERSAL::can( $e, 'payload' ) ) {
        my $type = ref( $e->payload );
        if ( $type =~ /^(Seqsee::SCF|SThought)::(.*)/ ) {
          return $2;
        }
      }
      die $e;
    }

    # We reach here only if no exception was thrown.
    # Beyond here, if $check_sub->() fails(or returns false),
    # we should die. If it returns true, return "".
    if ($check_sub) {
      my $check_value = $check_sub->();
      unless ($check_value) {

        # No exception, and yet the check sub did not deliver.
        confess "Failed Check in check_sub";
      }
    }
    return "";
  };
}

sub code_throws_stochastic_ok {
  my ( $subr, $arr_ref, $check_sub ) = @_;
  my $new_sub = _wrap_to_get_payload_type( $subr, $check_sub );
  stochastic_all_seen_ok $new_sub, $arr_ref;
}

sub code_throws_stochastic_nok {
  my ( $subr, $arr_ref ) = @_;
  my $new_sub = _wrap_to_get_payload_type($subr);
  stochastic_all_seen_nok $new_sub, $arr_ref;
}

sub code_throws_stochastic_all_and_only_ok {
  my ( $subr, $arr_ref, $check_sub ) = @_;
  my $new_sub = _wrap_to_get_payload_type( $subr, $check_sub );
  stochastic_all_and_only_ok $new_sub, $arr_ref, $check_sub;
}

sub code_throws_stochastic_all_and_only_nok {
  my ( $subr, $arr_ref ) = @_;
  my $new_sub = _wrap_to_get_payload_type($subr);
  stochastic_all_and_only_nok $new_sub, $arr_ref;
}

sub INITIALIZE_for_testing {
  $Global::TestingOptionsRef     = Seqsee::_read_config( seq => '0' );  # Random
  $Global::Steps_Finished        = 0;
  $Global::CurrentRunnableString = "";

  "main"->install_sub(
    {
      message => sub {
      }
    }
  );
  "main"->install_sub(
    {
      debug_message => sub {
      }
    }
  );
  "main"->install_sub(
    {
      update_display => sub {
      }
    }
  );
  "main"->install_sub(
    {
      default_error_handler => sub {
        die $_[0];
      }
    }
  );

  "main"->install_sub(
    {
      ask_user_extension => sub {
        my ($arr_ref) = @_;
        return if Seqsee::already_rejected_by_user($arr_ref);
        my $ws_count        = $SWorkspace::ElementCount;
        my $ask_terms_count = scalar(@$arr_ref);
        unless ($ask_terms_count) {
          die "ask_user_extension called with 0 terms!";
        }
        my $known_elements_count = scalar(@Global::RealSequence);
        unless ( $ws_count + $ask_terms_count <= $known_elements_count ) {
          SErr::NotClairvoyant->new()->throw();
        }
        for my $i ( 0 .. $ask_terms_count - 1 ) {
          unless ( $Global::RealSequence[ $ws_count + $i ] == $arr_ref->[$i] ) {
            IncrementFailedRequests();
            return;
          }
        }
        $Global::AtLeastOneUserVerification = 1;
        return 1;
      }
    }
  );

  # XXX(Board-it-up): [2007/02/14] Will need modification for correct testing!
  "main"->install_sub(
    {
      ask_for_more_terms => sub { }
    }
  );
}

sub stochastic_test_codelet {
  my (%opts_ref) = @_;
  my ( $setup_sub, $expected_throws, $check_sub, $codefamily ) =
  @opts_ref{qw(setup throws post_run codefamily)};

#    if ($check_sub) {
#         confess ' defining a check_sub when there can be exceptions is useless..  here, we are expecting' . "@$expected_throws" unless List::MoreUtils::all { $_ eq '' } @$expected_throws;
#}

  code_throws_stochastic_ok sub {
    SUtil::clear_all();
    my $opts_ref = $setup_sub->();
    my $cl = new SCodelet( $codefamily, 100, $opts_ref );
    ## $cl
    $cl->run;
  }, $expected_throws;
  if ($check_sub) {
    ok( $check_sub->(), 'checking the after effects' );
  }
  else {
    ok( 1, 'No check_sub: nothing to check' );
  }

}

sub output_contains {
  my ( $subr, %scope ) = @_;
  my $msg = delete( $scope{msg} ) || "output_contains";
  my %seen;
  my $times = 5;
  for ( 1 .. $times ) {
    my $ret = $subr->();
    my %seen_here;
    for (@$ret) {
      ## $_
      $seen_here{$_}++;
    }
    for ( keys %seen_here ) {
      $seen{$_}++;
    }
  }

  my $problems_found = 0;
  LOOP: while ( my ( $k, $v ) = each %scope ) {
    if ( $k eq 'always' ) {
      foreach (@$v) {
        $seen{$_} ||= 0;
        unless ( $seen{$_} == $times ) {
          $problems_found = 1;
          $msg .= "$_ was not always seen. Seen $seen{$_} times out of $times";
          last LOOP;
        }
      }
    }
    elsif ( $k eq 'never' ) {
      foreach (@$v) {
        $seen{$_} ||= 0;
        unless ( $seen{$_} == 0 ) {
          $problems_found = 1;
          $msg .= "$_ was not never seen. Seen $seen{$_} times out of $times";
          last LOOP;
        }
      }
    }
    elsif ( $k eq 'sometimes' ) {
      foreach (@$v) {
        $seen{$_} ||= 0;
        unless ( $seen{$_} > 0 ) {
          $problems_found = 1;
          $msg .= "$_ was not seen anytime. Seen $seen{$_} times out of $times";
          last LOOP;
        }
      }

    }
    elsif ( $k eq 'sometimes_but_not_always' ) {
      foreach (@$v) {
        $seen{$_} ||= 0;
        unless ( $seen{$_} > 0 and $seen{$_} < $times ) {
          $problems_found = 1;
          $msg .= "Expected to see $_ sometimes but not always, but it was ";
          $msg .= ( $seen{$_} ? 'always' :'never' );
          $msg .= ' seen';
          last LOOP;
        }
      }

    }
    else {
      confess "unknown quantifier $k";
    }
  }
  ok( 1 - $problems_found, $msg );
}

sub output_always_contains {
  my ( $subr, $arg ) = @_;
  $arg = [$arg] unless ref($arg) eq "ARRAY";
  output_contains $subr, always => $arg;
}

sub output_never_contains {
  my ( $subr, $arg ) = @_;
  $arg = [$arg] unless ref($arg) eq "ARRAY";
  output_contains $subr, never => $arg;
}

sub output_sometimes_contains {
  my ( $subr, $arg ) = @_;
  $arg = [$arg] unless ref($arg) eq "ARRAY";
  output_contains $subr, sometimes => $arg;
}

sub output_sometimes_but_not_always_contains {
  my ( $subr, $arg ) = @_;
  $arg = [$arg] unless ref($arg) eq "ARRAY";
  output_contains $subr, sometimes_but_not_always => $arg;
}

sub fringe_contains {
  my ( $self, %options ) = @_;
  my $setup_sub;

  if ( ref($self) eq "CODE" ) {
    $setup_sub = $self;
  }

  my $subr;
  if ($setup_sub) {
    $subr = sub {
      SUtil::clear_all();
      $self = $setup_sub->();
      return [ map { $_->[0] } @{ $self->get_fringe() } ];
    };

  }
  else {
    $subr = sub {
      return [ map { $_->[0] } @{ $self->get_fringe() } ];
    };
  }
  output_contains( $subr, msg => "fringe_contains  ", %options );
}

sub extended_fringe_contains {
  my ( $self, %options ) = @_;
  my $setup_sub;

  if ( ref($self) eq "CODE" ) {
    $setup_sub = $self;
  }

  my $subr;
  if ($setup_sub) {
    $subr = sub {
      SUtil::clear_all();
      $self = $setup_sub->();
      return [ map { $_->[0] } @{ $self->get_extended_fringe() } ];
    };

  }
  else {
    $subr = sub {
      return [ map { $_->[0] } @{ $self->get_extended_fringe() } ];
    };
  }
  output_contains( $subr, msg => "extended_fringe_contains  ", %options );
}

sub action_contains {
  my ( $self, %options ) = @_;
  my $setup_sub;

  if ( ref($self) eq "CODE" ) {
    $setup_sub = $self;
  }

  my $subr;
  if ($setup_sub) {
    $subr = sub {
      SUtil::clear_all();
      $self = $setup_sub->();
      return [ map { ref($_) } $self->get_actions() ];
    };

  }
  else {
    $subr = sub {
      return [ map { ref($_) } $self->get_actions() ];
    };
  }
  output_contains( $subr, msg => "action_contains  ", %options );
}

sub RegTestHelper {
  my ($opts_ref) = @_;
  for (qw(seq continuation max_false max_steps min_extension)) {
    confess "Missing option $_" unless exists $opts_ref->{$_};
  }
  my $seq                     = $opts_ref->{seq};
  my $continuation            = $opts_ref->{continuation};
  my $max_false_continuations = $opts_ref->{max_false};
  my $max_steps               = $opts_ref->{max_steps};
  my $min_extension           = $opts_ref->{min_extension};

  ResetFailedRequests();
  SWorkspace->init( { %{$Global::TestingOptionsRef}, seq => $seq } );
  Global->SetFutureTerms(@$continuation);
  SCoderack->init($Global::TestingOptionsRef);
  $Global::MainStream->init($Global::TestingOptionsRef);

  # XXX(Board-it-up): [2006/10/23] Init memory here
  $SWorkspace::ReadHead = 0;
  Global->clear();

  print STDERR "\n****** BEGIN ANOTHER RUN: ";

  eval { 
    while (
      !Seqsee::Interaction_step_n(
        {
          n            => $max_steps,
          max_steps    => $max_steps,
          update_after => $max_steps,
        }
      )
    ) {}
  };
  
  my $e;
  if ( $e = Exception::Class->caught('SErr::FinishedTest') ) {
    if ( $e->got_it() ) {
      print STDERR "+GOT IT\n";
      return ( "GotIt", $Global::Steps_Finished );
    }
    else {
      $e->rethrow;
    }
  }
  elsif ( $e = Exception::Class->caught('SErr::NotClairvoyant') ) {
    print STDERR "+EXTENDED (NO MORE KNOWN TERMS)\n";
    return ( "Extended", $Global::Steps_Finished );
  }
  elsif ( $e = Exception::Class->caught('SErr::FinishedTestBlemished') ) {
    print STDERR "+BLEMISHED GOT IT\n";
    return ( "BlemishedGotIt", $Global::Steps_Finished );
  }
  elsif($e = Exception::Class->caught()) {
    my $failed_requests = GetFailedRequests();
    if ( $failed_requests > $max_false_continuations ) {
      print STDERR
      "+TOO MANY FAILED QUERIES ($failed_requests > $max_false_continuations)!\n";
      print STDERR join( '; ', keys %Global::ExtensionRejectedByUser ), "\n";
      return ( "TooManyFalseQueries", 0 );
    }

    ref $e ? $e->rethrow : die $e;
  }
  else {

    # Natural end?
    if ( $SWorkspace::ElementCount - scalar(@$seq) > $min_extension ) {
      print STDERR "+EXTENDED A BIT\n";
      return ( "ExtendedWithoutGettingIt", $Global::Steps_Finished );
    }
    else {

      # print "Steps finished : $Global::Steps_Finished\n";
      print STDERR "+NOT EVEN EXTENDED ONCE\n";
      return ( "NotEvenExtended", $Global::Steps_Finished );
    }
  }
}

{
  my $tmp_file = "foo";

  sub RegStat {
    open TEMP, "<", $tmp_file;
    my $opts_ref;
    my $str = join( "\n", <TEMP> );
    eval $str;
    close TEMP;
    ## $opts_ref
    my %outputs;
    my %errors;
    my @successful_codelet_count;
    my @RESULTS;
    for ( 1 .. 10 ) {    ## Trials===[%]      Done
      my $out;
      my $step_count;
      eval { ( $out, $step_count ) = RegTestHelper($opts_ref); };
      if ( $out eq "GotIt" or $out eq "Extended" ) {
        push @successful_codelet_count, $step_count;
        push @RESULTS,                  "SUCCESS: $out\t$step_count";
        $outputs{$out}++;
        next;
      }

      if ($EVAL_ERROR) {
        push @RESULTS, "Error: $EVAL_ERROR";
        $errors{$EVAL_ERROR}++;
        $outputs{UnnaturalDeath}++;
        next;
      }

      if ( $out =~ m/^UnnaturalDeath:\s*(.*)$/ ) {
        $errors{$1}++;
        push @RESULTS, "Error: $1";
        $outputs{UnnaturalDeath}++;
        print STDERR "\nERROR:\n$1\n=================\n";
        next;
      }

      push @RESULTS, "Different result!";

    }

    print "============\n";
    while ( my ( $k, $v ) = each %$opts_ref ) {
      my $v2 = ( ref $v ) eq "ARRAY" ? join( ", ", @$v ) :$v;
      print "$k\t=>  $v2\n";
    }

    print "============\n";
    while ( my ( $k, $v ) = each %outputs ) {
      $k = substr( $k, 0, 50 );
      print "$v\t times: $k\n";
    }

    $outputs{RESULTS} = \@RESULTS;
    if (@successful_codelet_count) {
      $outputs{avgcc} =
      List::Util::sum(@successful_codelet_count) /
      scalar(@successful_codelet_count);
    }
    use Data::Dumper;
    open OUT, ">", $tmp_file;
    print OUT Data::Dumper->Dump( [ \%outputs ] );
    close OUT;
    return \%outputs;
  }

  sub RegStatShell {
    my ($opts_ref) = @_;
    open TEMP, ">", $tmp_file;
    print TEMP Data::Dumper->Dump( [$opts_ref], ["opts_ref"] );
    close TEMP;
    my $FeatureSetCommand =
    join( ";", map { "\$Global::Feature{$_} = 1" } ( keys %Global::Feature ) );
    system
    "perl  -e \"use lib 'lib';use Test::Seqsee; use warnings; $FeatureSetCommand; RegStat();\""
    and die "The subcommand was cancelled. exiting";
    open REG, "<", $tmp_file;
    my $VAR1;
    my $reg_out = join( "\n", <REG> );
    close REG;
    ## $reg_out
    eval $reg_out;
    return $VAR1;
  }
}

my %TooLow = (
  0  => -1,
  1  => 0,
  2  => 0,
  3  => 1,
  4  => 2,
  5  => 3,
  6  => 4,
  7  => 5,
  8  => 6,
  9  => 7,
  10 => 9
);
my %PleasantlyHigh = (
  0  => 1,
  1  => 3,
  2  => 4,
  3  => 5,
  4  => 6,
  5  => 7,
  6  => 8,
  7  => 9,
  8  => 9,
  9  => 10,
  10 => 11
);

sub RegHarness {
  my $arg1 = shift;
  my %opts;
  my ( @improved, @became_worse, @Results );

  if ( ref($arg1) eq 'HASH' ) {
    %opts = %$arg1;
  }
  else {
    my $file = shift;
    read_config $file => %opts;
    %opts = %{ $opts{''} };
  }

  ( $opts{seq}, $opts{continuation} ) = ParseSeq_( $opts{seq} );
  $opts{max_false}     ||= 10;
  $opts{max_steps}     ||= 10000;
  $opts{min_extension} ||= 2;
  my $start_time = time();
  my $output     = RegStatShell( \%opts );
  ## output: $output
  $output->{avgcc} ||= 0;
  push @Results, @{ $output->{RESULTS} };

  #return unless sum( values %$output ) == 10 + $output->{avgcc};
  my $total_time = time() - $start_time;
  print "Processing time: $total_time\n";
  my $current_GotIt = $output->{GotIt} ||= 0;

  my $earlier_GotIt = 0;
  my $last_res_file = $_ . ".last_res";
  my $log_file      = $_ . ".log_res";
  if ( -e $last_res_file ) {
    my %out;
    eval { read_config $last_res_file => %out; };
    $earlier_GotIt = $EVAL_ERROR ? 0 :$out{''}->{GotIt};
  }

  open LOG,     ">>", $log_file;
  open CURRENT, ">",  $last_res_file;
  print LOG "[", sprintf( time() ), "]\n";
  while ( my ( $k, $v ) = each %$output ) {
    $k =~ s#\W##g;
    print LOG "$k = $v\n";
    print CURRENT "$k = $v\n";
  }
  close LOG;
  close CURRENT;

  if ( $current_GotIt <= $TooLow{$earlier_GotIt} ) {
    print "##########\n# PERFORMANCE WORSE!\n Had Got It ",
    "$earlier_GotIt times, now just $current_GotIt";
    push @became_worse, [ $opts{seq}, $earlier_GotIt, $current_GotIt ];
  }
  elsif ( $current_GotIt >= $PleasantlyHigh{$earlier_GotIt} ) {
    print "!!!!!!!!!\n# PERFORMANCE BETTER!\n Had Got It ",
    "$earlier_GotIt times, now it is $current_GotIt";
    push @improved, [ $opts{seq}, $earlier_GotIt, $current_GotIt ];
  }

  my @to_ret = ( \@improved, \@became_worse, \@Results, \%opts );
  use Smart::Comments;
  ## to return: @to_ret
  return @to_ret;
}

sub ParseSeq_ {
  my ($seq) = @_;
  my ( $s, $c ) = split( /\|/, $seq );
  for ( $s, $c ) {
    s/^\s*//;
    s/\s*$//;
  }
  return ( [ split( /\s+/, $s ) ], [ split( /\s+/, $c ) ] );
}

use Seqsee::ResultOfTestRun;

sub RunSeqsee {
  my ( $seq, $continuation, $max_steps, $max_false, $min_extension ) = @_;
  ResetFailedRequests();
  SWorkspace->init( { %{$Global::TestingOptionsRef}, seq => $seq } );
  Global->SetFutureTerms(@$continuation);
  SCoderack->init($Global::TestingOptionsRef);
  $Global::MainStream->init($Global::TestingOptionsRef);

  if ( $Global::Feature{LTM} ) {
    eval { SLTM->Load('memory_dump.dat') };
    if ($EVAL_ERROR) {
      return Seqsee::ResultOfTestRun->new(
        {
          status => $TestOutputStatus::Crashed,
          steps  => $Global::Steps_Finished,
          error  => "Unable to load memory file! " . $EVAL_ERROR,
        }
      );
    }
  }
  else {
    say "LTM not passed in as an option.";
  }
  SLTM->init();

  # XXX(Board-it-up): [2006/10/23] Init memory here
  $SWorkspace::ReadHead = 0;
  Global->clear();

  my $return;
  
       eval { 
    while (
      not Seqsee::Interaction_step_n(
        {
          n            => $max_steps,
          max_steps    => $max_steps,
          update_after => $max_steps,
        }
      )
    )
    {
    }
   };
       if (my $err = $EVAL_ERROR) {
          CATCH_BLOCK: { if (UNIVERSAL::isa($err, 'SErr::FinishedTest')) { 
      $return = Seqsee::ResultOfTestRun->new(
        {
          status => $TestOutputStatus::Successful,
          steps  => $Global::Steps_Finished,
          error  => undef,
        }
      ) if $err->got_it();
      confess "A SErr::FinishedTest thrown without getting it. Bad."
      unless $err->got_it();
    ; last CATCH_BLOCK; }if (UNIVERSAL::isa($err, 'SErr::NotClairvoyant')) { 
      $return = Seqsee::ResultOfTestRun->new(
        {
          status => $TestOutputStatus::RanOutOfTerms,
          steps  => $Global::Steps_Finished,
          error  => undef,
        }
      );
    ; last CATCH_BLOCK; }if (UNIVERSAL::isa($err, 'SErr::FinishedTestBlemished')) { 
      $return = Seqsee::ResultOfTestRun->new(
        {
          status => $TestOutputStatus::InitialBlemish,
          steps  => $Global::Steps_Finished,
          error  => undef,
        }
      );
    ; last CATCH_BLOCK; } 
      $return = Seqsee::ResultOfTestRun->new(
        {
          status => $TestOutputStatus::Crashed,
          steps  => $Global::Steps_Finished,
          error  => "Crashed!\n$err",
        }
      );
    ; last CATCH_BLOCK; die $err }
       }
    

  if ( $Global::Feature{LTM} ) {
    SLTM->Dump('memory_dump.dat');
  }
  else {
    say "LTM not passed in as an option, so no need to save.";
  }
  if ($return) {
    return $return;
  }

  # So did not die.
  if ( $SWorkspace::ElementCount - scalar(@$seq) > $min_extension ) {
    return Seqsee::ResultOfTestRun->new(
      {
        status => $TestOutputStatus::ExtendedABit,
        steps  => $Global::Steps_Finished,
        error  => undef,
      }
    );
  }
  else {
    return Seqsee::ResultOfTestRun->new(
      {
        status => $TestOutputStatus::NotEvenExtended,
        steps  => $Global::Steps_Finished,
        error  => undef,
      }
    );
  }
}

INITIALIZE_for_testing();

1;
