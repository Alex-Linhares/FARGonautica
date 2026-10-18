# This represents one person's response to one sequence.
package Experiment::Encounter;
use strict;
use Class::Std;
use Memoize;

my %Participant_of :ATTR(:name<participant>);
my %Sequence_of :ATTR(:name<sequence>);

my %PresentedTerms_of : ATTR(:name<presented_terms>);
my %PresentedTermsString_of :ATTR(:name<presented_terms_string>);
my %ExtesionDone_of : ATTR(:name<extension_by_user>);
my %TimeToUnderstand_of : ATTR(:name<time_to_understand>);
my %TypingTimes_of :ATTR(:name<typing_times>);

# 0: not outlier; 1: Participant outlier; 2: this specific encounter
my %IsOutlier_of :ATTR(:get<is_outlier>, :set<is_outlier>); 
my %IsExtensionCorrect_of :ATTR(:get<is_extension_correct> :set<is_extension_correct>);

sub IsExtensionCorrect {
    my ( $self ) = @_;
    return $self->get_sequence()->IsExtensionCorrect($self->get_extension_by_user());
}
memoize('IsExtensionCorrect');


1;
