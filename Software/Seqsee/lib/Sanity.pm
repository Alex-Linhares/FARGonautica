package Sanity;
use strict;
use Carp;
use Class::Multimethods;
multimethod SanityFail => ('$') => sub {
  my ($m) = @_;
  my $msg =
  "Entered inconsistent state after a $Global::CurrentRunnableString.($Global::Steps_Finished)\n$m";
  $msg .= "The codelet was: " . $Global::CurrentCodelet->as_text;
  main::message($msg);
  confess "Sanity failed... exiting! $msg";
};

multimethod SanityCheck => () => sub {
  for my $gp ( SWorkspace::GetGroups() ) {
    SanityCheck($gp);
  }
  for my $rel ( values %SWorkspace::relations ) {
    SanityCheck($rel);
  }
};

multimethod SanityCheck => qw(Seqsee::Element) => sub {
  my ($gp) = @_;
  for my $cat ( @{ $gp->get_categories } ) {
    my $bindings = $gp->GetBindingForCategory($cat)
    or SanityFail("No bindings?");
    while ( my ( $k, $v ) = each %{ $bindings->get_bindings_ref() } ) {
      ref($v)
      or SanityFail( "Non-ref in bindings: $k => $v for " . $cat->get_name );
    }
  }
};

multimethod SanityCheck => qw(Seqsee::Anchored) => sub {
  my ($gp) = @_;
  if ( my $underlying_ruleapp = $gp->get_underlying_reln() ) {
    SanityCheck( $gp, $underlying_ruleapp );
  }
  my ( $l, $r ) = $gp->get_edges();
  0 <= $l  or SanityFail("Edge problem: left $l");
  $l <= $r or SanityFail("Edge problem: $l $r");
  $r < $SWorkspace::ElementCount
  or
  SanityFail("Edge problem: right $r; Workspace has $SWorkspace::ElementCount");

  my @parts = @$gp;

  SWorkspace->are_there_holes_here(@parts) and SanityFail("Holes in group!");

  for my $part (@parts) {
    $part->isa('Seqsee::Anchored') or SanityFail("Unanchored part!");
    $part->get_is_a_metonym() and SanityFail("Group has metonym as part");
  }

  my @cat = @{ $gp->get_categories() };
  unless (@cat) {
    my $hist = join( "\n", @{ $gp->get_history } );
    for my $subgp (@$gp) {
      $hist .= "\n-------- " . $subgp->as_text . "\n";
      $hist .= join( "\n", @{ $subgp->get_history } );
    }
    SanityFail(
      "Group without any category:" . $gp->as_text . "\nhist:\n$hist" );
  }
  for my $cat (@cat) {
    my $bindings = $gp->GetBindingForCategory($cat)
    or SanityFail("No bindings?");
    while ( my ( $k, $v ) = each %{ $bindings->get_bindings_ref() } ) {
      ref($v)
      or SanityFail( "Non-ref in bindings: $k => $v for " . $cat->get_name );
    }
  }
};

multimethod SanityCheck => qw(Seqsee::Anchored SRuleApp) => sub {
  SanityCheck( @_, '' );
};

multimethod SanityCheck => qw(Seqsee::Anchored SRuleApp $) => sub {
  my ( $gp, $ra, $m ) = @_;
  $m = $m ? "($m) " :"";
  my @gp_parts = @$gp;
  my @ra_items = @{ $ra->get_items() };
  my $count    = scalar(@gp_parts);
  unless ( scalar(@ra_items) == $count ) {
    my $msg =
      "Group: "
    . $gp->as_text()
    . " has $count elements: @gp_parts, whereas ruleapp only has @ra_items";
    SanityFail("$m Gp/Ruleapp out of sync! $msg");
  }
  for my $i ( 0 .. $count - 1 ) {
    my $gp_part = $gp_parts[$i];
    my $ra_part = $ra_items[$i];
    if ( $gp_part->get_metonym_activeness() ) {

# $gp_part->GetEffectiveObject() eq $ra_part or SanityFail("Metonym'd object had ruleapp with unmetonymd part or different part " . join(";", "Group", $gp, $gp->as_text(), "Part: ", $gp_part, $gp_part->as_text(), $gp_part->GetEffectiveObject(), $ra_part, $ra_part->as_text()));
    }
    else {
      $gp_part eq $ra_part or SanityFail("$m Gp/Ruleapp item out of sync!");
    }
  }
};

multimethod SanityCheck => qw(SRelation) => sub {
  my ($rel)  = @_;
  my (@ends) = $rel->get_ends();
  if ( $ends[0]->get_left_edge() > $ends[1]->get_left_edge() ) {
    SanityFail( "Leftward relation " . $rel->as_text . '!' );
  }
  for (@ends) {
    SanityFail("End of a relation is a metonymed object")
    if $_->IsThisAMetonymedObject();
  }
};
1;
