package SLTM;
use 5.10.0;
use strict;
use warnings;
use Class::Multimethods;
use File::Slurp;
use Carp;
use Smart::Comments;
use English qw(-no_match_vars);
use Set::Weighted;
use SLTM::Platonic;

multimethod 'ApplyMapping';
use constant {
  LTM_FOLLOWS        => 1,    # Link of type A often follows B in sequences
  LTM_IS             => 2,    # A is an instance of B
  LTM_CAN_BE_SEEN_AS => 3,    # A has been squinted as B
  LTM_TYPE_COUNT     => 3,    #
};

our %LinkType2Str = ( 1 => 'FOLLOWS', 2 => 'IS', 3 => 'CAN_BE_SEEN_AS' );

our @PRECALCULATED = @SLinkActivation::PRECALCULATED;
confess "Load order issues" unless @PRECALCULATED;
use SNodeActivation;

our %MEMORY;        # Just has the index into @MEMORY.
our @MEMORY;        # Is 1-based, so that I can say $MEMORY{$x} || ...
our @ACTIVATIONS;   # Also 1-based, an array of SNodeActivation objects.
our @LINKS;         # List of all links, for decay purposes.
our @OUT_LINKS;     # Also 1-based; Outgoing links from given node.
our $NodeCount;     # Number of nodes.
our
%_PURE_CLASSES_;    # List of pure classes: those that can be stored in the LTM.
our %CurrentlyInstalling
;    # We are currently installing these. Needed to detect cycles.

our %MEMORIZED_SERIALIZED;    # This is a hack to cover-up for a strange bug:

# Sometimes while dumping there are multiple copies of the same concept (so far, always instances of
# SRelnType::Compound). And a node's serialized version can refer to nodes with a higher index.
# This hash freezes references.

%_PURE_CLASSES_ = map { $_ => 1 } qw(SCat::OfObj::Std
SCat::OfObj::RelationTypeBased
SCat::OfObj::Interlaced
SCat::OfObj::Assuming
SCat::OfObj::Alternating
SLTM::Platonic
METO_MODE POS_MODE
Mapping::Numeric Mapping::Structural Mapping::Position Mapping::MetoType
Mapping::Dir
SMetonymType
);

sub init {
  print "Initializing SLTM...\n";
  if ( $Global::Feature{LogActivations} ) {
    print "\tActivation logging requested.\n";
    unless ($Global::ActivationsLogHandle) {
      print "\tOpening file for write: $Global::ActivationsLogfile\n";
      open my $handle, '>', $Global::ActivationsLogfile;
      select($handle);
      $| = 1;
      select(*STDOUT);
      $|                            = 1;
      $Global::ActivationsLogHandle = $handle;
    }
  }
}

Clear();

# method Clear( $package:  )
sub Clear {
  %MEMORY      = ();
  $NodeCount   = 0;
  @MEMORY      = ('!!!');                      # Remember, its 1-based
  @ACTIVATIONS = ( SNodeActivation->new() );   # Remember, this, too, is 1-based
  @LINKS       = ();
  @OUT_LINKS   = ('!!!');
}

# method GetNodeCount( $package:  ) returns int
sub GetNodeCount { return $NodeCount; }

# Always call as: SLTM::GetMemoryIndex($x), not SLTM->GetMemoryIndex($x)
sub GetMemoryIndex {

  # say "Inside GetMemoryIndex";
  ### ensure: $_[0] ne "SLTM"
  ## GetMemoryIndex called on: $_[0]
  my $pure = ( ref( $_[0] ) ~~ %_PURE_CLASSES_ ? $_[0] :$_[0]->get_pure() )
  or confess "No pure version of $_[0] available!";
  ## pure: $pure
  # say "\t\tpure=$pure";
  return $MEMORY{$pure} ||= InsertNode($pure);
}

*InsertUnlessPresent = *GetMemoryIndex;

sub InsertNode {
  ### ensure: $_[0] ne "SLTM"
  confess "Attempt to insert bogus object: '$_[0]'" unless ref( $_[0] );
  ### ensure: ref($_[0])
  ### ensure: $_[0] and ($_PURE_CLASSES_{ref($_[0])} or $_[0]->is_pure())
  my ($pure) = @_;

  ## Currently installing: %CurrentlyInstalling, $pure
  confess if $CurrentlyInstalling{$pure}++;
  for ( $pure->get_memory_dependencies() ) {
    $MEMORY{$_} or InsertNode($_);
  }

  $NodeCount++;
  push @MEMORY, $pure;
  ## ACTIVATIONS: @ACTIVATIONS
  push @ACTIVATIONS, SNodeActivation->new();
  ## ACTIVATIONS NOW: @ACTIVATIONS
  push @OUT_LINKS, [];
  $MEMORY{$pure} = $NodeCount;

  ## Finished installing: $pure
  delete $CurrentlyInstalling{$pure};
  return $NodeCount;
}

sub __InsertLinkUnlessPresent {
  my ( $from_index, $to_index, $modifier_index, $type ) = @_;
  my $outgoing_links_ref = ( $OUT_LINKS[$from_index][$type] ||= {} );

  if ( my $link = $outgoing_links_ref->{$to_index} ) {
    return $link;
  }
  else {
    my $new_link = SLinkActivation->new($modifier_index);
    $outgoing_links_ref->{$to_index} = $new_link;
    push @LINKS, $new_link;
    return $new_link;
  }
}

sub InsertFollowsLink {
  my ( $category, $relation ) = @_;
  __InsertLinkUnlessPresent(
    GetMemoryIndex($category),
    GetMemoryIndex($relation),
    0, LTM_FOLLOWS,
  );
}

sub InsertISALink {
  my ( $from, $to ) = @_;
  __InsertLinkUnlessPresent( GetMemoryIndex($from), GetMemoryIndex($to), 0,
    LTM_IS );
}

sub StrengthenLinkGivenIndex {
  my ( $from, $to, $type, $amount ) = @_;
  my $outgoing_links_ref = ( $OUT_LINKS[$from][$type] ||= {} );
  ### require: exists($outgoing_links_ref->{$to})
  $outgoing_links_ref->{$to}->Spike($amount);
}

sub StrengthenLinkGivenNodes {
  my ( $from, $to, $type, $amount ) = @_;
  StrengthenLinkGivenIndex( GetMemoryIndex($from), GetMemoryIndex($to), $type,
    $amount, );
}

sub SpreadActivationFrom {
  my ($root_index) = @_;
  my $root_name = $MEMORY[$root_index]->as_text();
  my %nodes_at_distance_below_1 = ( $root_index, 0 );    # Keys are nodes.
        # values are amount of activation pumped into them.

  my $activation =
  $ACTIVATIONS[$root_index][ SNodeActivation::REAL_ACTIVATION() ]
  ;     # is fn faster?
  for my $link_set ( @{ $OUT_LINKS[$root_index] } ) {
    while ( my ( $target_index, $link ) = each %$link_set ) {
      my $amount_to_spread = $link->AmountToSpread($activation);
      SNodeActivation::SpikeSeveral( int($amount_to_spread),
        $ACTIVATIONS[$target_index] );
      $nodes_at_distance_below_1{$target_index} += $amount_to_spread;
      my $node_name = $MEMORY[$target_index]->as_text();
      main::debug_message(
        "distance = 1 [$target_index] >$node_name< got an extra $amount_to_spread from >$root_name<",
        1, 1
      );
    }
  }

  # Now to nodes at distance 2.
  while ( my ( $node, $amount_spiked_by ) = each %nodes_at_distance_below_1 ) {
    next unless $amount_spiked_by > 5;
    for my $link_set ( @{ $OUT_LINKS[$node] } ) {
      while ( my ( $target_index, $link ) = each %$link_set ) {
        next if exists $nodes_at_distance_below_1{$target_index};
        my $amount_to_spread = $link->AmountToSpread($activation);
        $amount_to_spread *= 0.3;
        SNodeActivation::SpikeSeveral( int($amount_to_spread),
          $ACTIVATIONS[$target_index] );
        my $node_name = $MEMORY[$target_index]->as_text();
        main::debug_message(
          "distance = 2 [$target_index] >$node_name< got an extra $amount_to_spread from >$root_name<",
          1, 1
        );
      }
    }
  }
}

# method Dump( $package: Str $filename )
sub Dump {
  my ( $package, $file ) = @_;
  say "Dumping LTM to file $file";
  my $filehandle;

  if ( my $type = ref $file ) {
    if ( $type eq q{File::Temp} ) {
      $filehandle = $file;
    }
    else {
      confess
      "Dump must be called either with an unblessed filename or a File::Temp object";
    }
  }
  else {
    open $filehandle, ">", $file;
  }

  for my $index ( 1 .. $NodeCount ) {
    my ( $pure, $activation ) = ( $MEMORY[$index], $ACTIVATIONS[$index] );
    my ($depth_reciprocal) =
    ( $activation->[ SNodeActivation::DEPTH_RECIPROCAL() ], );
    print {$filehandle} "=== $index: ", ref($pure), " $depth_reciprocal\n",

    #$MEMORIZED_SERIALIZED{$pure} ||
    $pure->serialize(), "\n";
  }

  # Links.
  print {$filehandle} "#####\n";
  for my $from_node ( 1 .. $NodeCount ) {
    my $links_ref = $OUT_LINKS[$from_node];
    for my $type ( 1 .. LTM_TYPE_COUNT ) {
      my $links_of_this_type = $links_ref->[$type];
      while ( my ( $to_node, $link ) = each %$links_of_this_type ) {
        my $modifier_index = $link->[SLinkActivation::MODIFIER_NODE_INDEX] || 0;
        my ( $significance, $stability ) = (
          $link->[SLinkActivation::RAW_SIGNIFICANCE],
          $link->[SLinkActivation::STABILITY_RECIPROCAL]
        );
        print {$filehandle} sprintf( "%4s %4s %2s %4s %7.4f %7.5f\n",
          $from_node, $to_node, $type, $modifier_index, $significance,
          $stability );
      }
    }
  }

  close $filehandle;
}

# method Load( $package: Str $filename )
sub Load { # Safe, non-throwing.
  my ( $package, $filename ) = @_;
  eval { $package->Load_Helper($filename) };

  my $e;
  if ( $e = Exception::Class->caught('SErr::LTM_LoadFailure') ) {
      warn "Failure loading LTM: ", $e->what, "\n", $e->trace->as_string, "\n";
      exit;
  }
  else {
      $e = Exception::Class->caught();
      ref $e ? $e->rethrow : die $e;
  }
}

sub Load_Helper { # May throw SErr::LTM_LoadFailure
  my ( $package, $filename ) = @_;

  say "Loading LTM from $filename";
  Clear();
  my $string = read_file($filename);
  my ( $nodes, $links ) = split( q{#####}, $string );
  ## nodes: $nodes
  ## links: $links

  my @nodes = split( qr{=== \d+:}, $nodes );
  my $nodes_added = 0;
  for (@nodes) {
    s#^\s*##;
    s#\s*$##;
    next if m#^$#;
    my ( $type_and_sig, $val ) = split( /\n/, $_, 2 );
    my ( $type, $depth_reciprocal ) = split( /\s/, $type_and_sig, 2 );
    ## type, val: $type, $val
    my $pure;

    # print "$val...";
    eval { $pure = $type->deserialize($val) };
    if ($EVAL_ERROR) {
      my $msg = "Unable to deserialize >>$val<< of type >>$type<<\n";
      $msg .= $EVAL_ERROR;
      $msg .= "\nNodes inserted so far: $NodeCount.";
      SErr::LTM_LoadFailure->throw( what => $msg );
    }

    ## pure: $pure
    SErr::LTM_LoadFailure->throw(
      what => qq{Could not find pure: type='$type', val='$val'} )
    unless defined($pure);

    # say "Done. $pure:" . $pure->as_text;
    my $index = InsertUnlessPresent($pure);

    # say "\t\tStill here!";
    #$MEMORIZED_SERIALIZED{$pure} = $val;
    $nodes_added++;

    # say "\t$nodes_added, $NodeCount";
    unless ( $nodes_added == $NodeCount ) {
      SErr::LTM_LoadFailure->throw( what =>
        "Should have only added $nodes_added nodes by now, but looks like $NodeCount. Was trying to add '$pure'. Its index is now $index."
      );
    }
    SetDepthReciprocalForIndex( $index, $depth_reciprocal );
  }
  ## nodes: @nodes

  my @links = split( /\n+/, $links );
  ## links split: @links
  for (@links) {
    s#^\s*##;
    s#\s*$##;
    next if m#^$#;
    my ( $from, $to, $type, $modifier_index, $significance, $stability ) =
    split( /\s+/, $_ );
    my $activation =
    __InsertLinkUnlessPresent( $from, $to, $modifier_index, $type );
    $activation->[ SLinkActivation::RAW_SIGNIFICANCE() ]     = $significance;
    $activation->[ SLinkActivation::STABILITY_RECIPROCAL() ] = $stability;
  }

  ## links: $links

  # print "Would have loaded the file\n";
}

{
  my ( $sep1, $sep2, $char1, $char2, $char3 ) = map { chr($_) } ( 129 .. 133 );
  my $rx1 = qr{^$char1(.*)};
  my $rx2 = qr{^$char2(.*)};
  my $rx3 = qr{^$char3(.*)};

  sub encode {
    return join(
      $sep1,
      map {
        my $class = ref($_);
         $class eq 'HASH' ? encode_hash($_)
        :$class eq 'SInt' ? $char3 . $_->[0]
        :$class ? $char1 . $MEMORY{$_}
        :$_
      } @_
    );
  }

  sub encode_hash {
    my ($hash_ref) = @_;
    return $char2 . join(
      $sep2,
      map {
        my $class = ref($_);
        $class  eq 'HASH' ? confess('Recursive hash cannot be encoded')
        :$class eq 'SInt' ? $char3 . $_->[0]
        :$class
        ? $char1 . ( $MEMORY{$_} || confess "unrecognized reference to '$_'" )
        :$_
      } %$hash_ref
    );
  }

  sub decode {
    my ($str) = @_;
    return map {
       $_ =~ $rx1 ? $MEMORY[$1]
      :$_ =~ $rx2 ? { decode_hash($1) }
      :$_ =~ $rx3 ? SInt->new($1)
      :$_
    }
    split( $sep1, $str );
  }

  sub decode_hash {
    my ($str) = @_;
    ## decode_hash called on: $str
    $str =~ s#$sep2#$sep1#g;
    ## string now: $str
    return decode($str);
  }
}

sub SetSignificanceAndStabilityForIndex {
  my ( $index, $significance, $stability ) = @_;
  my $activation_object = $ACTIVATIONS[$index];

# The / 5 in next line: too many concepts end up hyperactive o/w. This limits their
# influence at load time, yet biases a little bit towards faster activation.
# Also, now stability rises only if *for several problems* significance is high.
  $activation_object->[SLinkActivation::RAW_SIGNIFICANCE] =
  int( $significance / 5 );
  $activation_object->[SLinkActivation::STABILITY_RECIPROCAL] = $stability;
}

sub SetDepthReciprocalForIndex {
  my ( $index, $depth_reciprocal ) = @_;
  $ACTIVATIONS[$index][ SNodeActivation::DEPTH_RECIPROCAL() ] =
  $depth_reciprocal;
}

sub SetRawActivationForIndex {
  my ( $index, $activation ) = @_;
  $ACTIVATIONS[$index]->[ SLinkActivation::RAW_ACTIVATION() ] = $activation;
}

sub SpikeBy {
  my ( $amount, @concepts ) = @_;
  ## Mem index: GetMemoryIndex($concept)
  ## @ACTIVATIONS: @ACTIVATIONS
  # main::message("Spiking @concepts", 1);
  my @indices = map { GetMemoryIndex($_) } grep { ref($_) } @concepts;
  SNodeActivation::SpikeSeveral( $amount, @ACTIVATIONS[@indices] );
}

sub WeakenBy {
  my ( $amount, @concepts ) = @_;
  ## Mem index: GetMemoryIndex($concept)
  ## @ACTIVATIONS: @ACTIVATIONS
  my @indices = map { GetMemoryIndex($_) } grep { ref($_) } @concepts;
  SNodeActivation::WeakenSeveral( $amount, @ACTIVATIONS[@indices] );
}

sub SpikeAndChoose {
  my ( $amount, @concepts ) = @_;

  return unless @concepts;
  if ( grep { not defined $_ } @concepts ) {
    confess "undef was one argument to SpikeAndChoose";
  }

  #main::message("SpikeAndChoose: $amount, @concepts");
  my @indices = map { GetMemoryIndex($_) } (@concepts);

  #main::message("indices: @indices");
  my @relevant_activations = @ACTIVATIONS[@indices];

  #main::message("relevant_activations: @relevant_activations");
  SNodeActivation::SpikeSeveral( $amount, @relevant_activations );
  return SChoose->choose_if_non_zero(
    [
      map { my $a = $_->[SNodeActivation::REAL_ACTIVATION]; $a > 0.02 ? $a :0 }
      @relevant_activations
    ],
    \@concepts
  );
}

my $DecayString = qq{
    sub {
        SNodeActivation::DecayManyTimes(1, \@ACTIVATIONS);
        for ( \@LINKS ) {
            $SLinkActivation::DECAY_CODE;
        }
    }
};

*DecayAll = eval $DecayString;

sub GetRawActivationsForIndices {
  my ($index_ref) = @_;
  return [ map { $ACTIVATIONS[$_]->[SNodeActivation::RAW_ACTIVATION] }
    @$index_ref ];
}

sub GetRealActivationsForIndices {
  my ($index_ref) = @_;
  return [ map { $ACTIVATIONS[$_]->[SNodeActivation::REAL_ACTIVATION] }
    @$index_ref ];
}

sub GetRealActivationsForConcepts {
  my ($index_ref) = @_;
  return [
    map {
      $ACTIVATIONS[ GetMemoryIndex($_) ]->[SNodeActivation::REAL_ACTIVATION]
    } @$index_ref
  ];
}

sub GetRealActivationsForOneConcept {
  my ($concept) = @_;
  return $ACTIVATIONS[ GetMemoryIndex( $_[0] ) ]
  ->[SNodeActivation::REAL_ACTIVATION];
}

{
  my $chooser_given_indices = SChoose->create(
    { map => q{$SLTM::ACTIVATIONS[$_]->[SNodeActivation::REAL_ACTIVATION]} } );
  my $chooser_given_concepts = SChoose->create(
    {
      map =>
      q{$SLTM::ACTIVATIONS[$SLTM::MEMORY{$_}]->[SNodeActivation::REAL_ACTIVATION]}
    }
  );

  sub ChooseIndexGivenIndex {
    return $chooser_given_indices->( $_[0] );
  }

  sub ChooseConceptGivenIndex {
    return $MEMORY[ $chooser_given_indices->( $_[0] ) ];
  }

  sub ChooseIndexGivenConcept {
    return $MEMORY{ $chooser_given_concepts->( $_[0] ) };
  }

  sub ChooseConceptGivenConcept {
    return $chooser_given_concepts->( $_[0] );
  }
}

# method GetRelated( $package: SNode $node ) returns @LTMNodes
# method WhoGotExcited( $package: LTMNode @nodes ) returns @LTMNodes

# proto method GetMemoryActions (...) returns @SAction
# multi method GetMemoryActions( Seqsee::Element $e )
# multi method GetMemoryActions( Seqsee::Anchored $o )
# multi method GetMemoryActions( SReln $r )

# XXX(Board-it-up): [2006/11/15] dummy function
sub GetTopConcepts {
  my ($N) = @_;
  return map {
    [
      $MEMORY[$_],
      $ACTIVATIONS[$_]->[SNodeActivation::REAL_ACTIVATION],
      $ACTIVATIONS[$_]->[SNodeActivation::RAW_ACTIVATION],
    ]
  } ( 1 .. $NodeCount );
}

sub FindActiveFollowers {
  my ($concept) = @_;

  my @categories = @{ $concept->get_categories() };
  my $ret        = Set::Weighted->new();
  for my $cat (@categories) {
    my $node_id = GetMemoryIndex($cat);
    my $follows_links_ref = ( $OUT_LINKS[$node_id][LTM_FOLLOWS] ||= {} );

    while ( my ( $relation_type_index, $link ) = each %{$follows_links_ref} ) {
      my $relation_type = $MEMORY[$relation_type_index];

      my $possible_next_object =
      eval { ApplyMapping( $relation_type, $concept ) }
      or next;
      $ret->insert( [ $possible_next_object, 1 ] )
      ;    #XXX a dummy value currently.
    }
  }
  $ret->merge_keys();
  return $ret;
}

sub FindActiveCategories {
  my ($concept)          = @_;
  my @current_categories = @{ $concept->get_categories() };
  my $ret                = Set::Weighted->new();
  my $isa_links_ref = ( $OUT_LINKS[ GetMemoryIndex($concept) ][LTM_IS] ||= {} );
  while ( my ( $category_index, $link ) = each %{$isa_links_ref} ) {
    my $category = $MEMORY[$category_index];
    next if $category ~~ @current_categories;
    $ret->insert(
      [
        $category,
        $ACTIVATIONS[$category_index][SNodeActivation::REAL_ACTIVATION]
      ]
    );
  }
  return $ret;
}
{
  my %NodesAlreadyPrinted;

  sub LogActivations {

    # Called only when $Global::Feature{LogActivations} is on.
    my @to_print = ($Global::Steps_Finished);
    for ( 1 .. $NodeCount ) {
      my $activation = $ACTIVATIONS[$_]->[SNodeActivation::REAL_ACTIVATION];
      next unless $activation > 0.01;
      unless ( $NodesAlreadyPrinted{$_} ) {
        PrintNode( $_, $MEMORY[$_]->as_text );
        $NodesAlreadyPrinted{$_} = 1;
      }
      push @to_print, $_, $activation;
    }
    print {$Global::ActivationsLogHandle} join( ' ', @to_print ), "\n";
  }

  sub PrintNode {
    my ( $id, $name ) = @_;
    print {$Global::ActivationsLogHandle} "NewNode\t$id\t$name\n";
  }

}

sub Print {
  for my $index ( 1 .. $NodeCount ) {
    my ( $pure, $activation ) = ( $MEMORY[$index], $ACTIVATIONS[$index] );
    my ($depth_reciprocal) =
    ( $activation->[ SNodeActivation::DEPTH_RECIPROCAL() ], );
    print "=== $index: ", ref($pure), " $depth_reciprocal\n",
    $pure->as_text(),
    "\n";
    my $links_ref = $OUT_LINKS[$index];
    for my $type ( 1 .. LTM_TYPE_COUNT ) {
      my $links_of_this_type = $links_ref->[$type] || {};
      next unless %$links_of_this_type;
      say "\t$LinkType2Str{$type}";
      while ( my ( $to_node, $link ) = each %$links_of_this_type ) {
        my $modifier_index = $link->[SLinkActivation::MODIFIER_NODE_INDEX];
        my ( $significance, $stability ) = (
          $link->[SLinkActivation::RAW_SIGNIFICANCE],
          $link->[SLinkActivation::STABILITY_RECIPROCAL]
        );

        my $modifier_name = '';
        if ($modifier_index) {
          $modifier_name = $MEMORY[$modifier_index]->as_text();
        }
        my $to_name = $MEMORY[$to_node]->as_text();
        say "\t\tTo: $to_name\n\t\tModifier: $modifier_name";
        say "\t\tSig: $significance, \tStab: $stability";
      }
    }
  }
}

1;
