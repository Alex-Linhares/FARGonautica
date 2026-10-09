# Oracle for Global.pm. Output: tests/golden/global.json
use strict;
use Oracle;
use S;

# --- load-time values -------------------------------------------------------
sub d { defined $_[0] ? $_[0] : undef }
record(op => 'initial',
       Steps_Finished => $Global::Steps_Finished,
       Break_Loop => d($Global::Break_Loop),
       AtLeastOneUserVerification => d($Global::AtLeastOneUserVerification),
       LogString => $Global::LogString,
       PossibleFeatures => [ sort keys %Global::PossibleFeatures ],
       Feature => [ sort keys %Global::Feature ],
       RealSequence => [@Global::RealSequence],
       TimeOfLastNewElement => $Global::TimeOfLastNewElement,
       TimeOfNewStructure => $Global::TimeOfNewStructure,
       InterstepSleep => $Global::InterstepSleep,
       Sanity => $Global::Sanity,
       AcceptableTrustLevel => $Global::AcceptableTrustLevel,
       CodeletTreeLogfile => $Global::CodeletTreeLogfile,
       ActivationsLogfile => $Global::ActivationsLogfile,
       MainStream_class => ref($Global::MainStream),
       MainStream_name => $Global::MainStream->{Name},
       MainStream_DiscountFactor => $Global::MainStream->{DiscountFactor},
       MainStream_MaxOlderThoughts => $Global::MainStream->{MaxOlderThoughts},
       MainStream_memo => (SStream2->CreateNew('MainStream') == $Global::MainStream) ? 1 : 0,
       BestRule => d($Global::BestRule),
       InitialTermCount => d($Global::InitialTermCount),
       debugMAX => d($Global::debugMAX),
      );

# --- fake rule / ruleapp ------------------------------------------------------
package FakeObj;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
package FakeRuleApp;
sub new { my ($c, $rule, @items) = @_; bless { rule => $rule, items => [@items] }, $c }
sub get_rule  { $_[0]{rule} }
sub get_items { $_[0]{items} }
package main;

my @o = map { FakeObj->new($_) } 0 .. 4;
my %name_of = map { ($o[$_] => "o$_") } 0 .. 4;
sub gsbc {
  my %h;
  $h{ $name_of{$_} } = $Global::GroupStrengthByConsistency{$_} for keys %Global::GroupStrengthByConsistency;
  \%h;
}
sub hilit {
  my %h;
  $h{ $name_of{$_} } = $Global::Hilit{$_} for keys %Global::Hilit;
  \%h;
}

# --- Hilit / ClearHilit ------------------------------------------------------
Global::Hilit(1, @o[0, 1]);
Global::Hilit(2, @o[1, 2]);
record(op => 'hilit', after => 'two calls', hilit => hilit());
Global::Hilit(1);
record(op => 'hilit', after => 'no objects', hilit => hilit());
Global::ClearHilit();
record(op => 'hilit', after => 'ClearHilit', hilit => hilit());

# --- SetRuleAppAsBest / SetRuleAppAsRecent ------------------------------------
my $ra1 = FakeRuleApp->new('rule1', @o[0, 1, 2]);
my $ra2 = FakeRuleApp->new('rule2', @o[2, 3, 2]);
Global::SetRuleAppAsBest($ra1);
record(op => 'rules', after => 'best ra1', BestRule => $Global::BestRule,
       RecentPromisingRule => d($Global::RecentPromisingRule), gsbc => gsbc());
Global::SetRuleAppAsRecent($ra2);
record(op => 'rules', after => 'recent ra2', BestRule => $Global::BestRule,
       RecentPromisingRule => $Global::RecentPromisingRule, gsbc => gsbc());
Global::SetRuleAppAsBest($ra2);
record(op => 'rules', after => 'best ra2', BestRule => $Global::BestRule,
       RecentPromisingRule => $Global::RecentPromisingRule, gsbc => gsbc());

# --- clear --------------------------------------------------------------------
$Global::Steps_Finished = 17;
$Global::AtLeastOneUserVerification = 1;
%Global::ExtensionRejectedByUser = ('1, 2' => 1);
$Global::LogString = 'abc';
Global::Hilit(1, $o[4]);
$Global::TimeOfLastNewElement = 9;
Global::clear();
record(op => 'clear',
       Steps_Finished => $Global::Steps_Finished,
       AtLeastOneUserVerification => $Global::AtLeastOneUserVerification,
       ExtensionRejectedByUser => [ keys %Global::ExtensionRejectedByUser ],
       LogString => $Global::LogString,
       hilit => hilit(),
       BestRule => d($Global::BestRule),
       RecentPromisingRule => d($Global::RecentPromisingRule),
       BestRuleApp_defined => defined($Global::BestRuleApp) ? 1 : 0,
       RecentPromisingRuleApp_defined => defined($Global::RecentPromisingRuleApp) ? 1 : 0,
       gsbc => gsbc(),
       TimeOfLastNewElement => $Global::TimeOfLastNewElement);
# After clear, the ruleapps survive, so the next update brings the strengths back.
Global::UpdateGroupStrengthByConsistency();
record(op => 'update_after_clear', gsbc => gsbc());

# --- SetFutureTerms -------------------------------------------------------------
@Global::RealSequence = (1, 2);
Global->SetFutureTerms(3, 4);
Global->SetFutureTerms();
Global->SetFutureTerms(5);
record(op => 'set_future_terms', RealSequence => [@Global::RealSequence]);

# --- UpdateExtensionsRejectedByUser -------------------------------------------------
my @keysets = (
  ['1, 2, 3', '1, 2', '1, 2, 3, 4', '2, 3', '1, 23, 4', '1, 2, 5'],
  ['7', '7, 8', '8'],
  ['1, 2, 3', ', 1', 'x'],
  ['10, 11', '1, 11'],
  ['1. 2, 3', '1, 2, 3'],
  [],
);
my @prefixes = ([1, 2], [7], [], [1], [10], ['1.']);
for my $keys (@keysets) {
  for my $prefix (@prefixes) {
    %Global::ExtensionRejectedByUser = map { $_ => 1 } @$keys;
    Global::UpdateExtensionsRejectedByUser(@$prefix);
    record(op => 'update_rejected', keys => $keys, prefix => $prefix,
           result => [ sort keys %Global::ExtensionRejectedByUser ],
           values => [ map { $Global::ExtensionRejectedByUser{$_} } sort keys %Global::ExtensionRejectedByUser ]);
  }
}

emit();
