# This track's each participant, known only by time.
package Experiment::Participant;
use strict;
use Class::Std;
use List::Util qw(sum);
use Memoize;

my %ExtensionEncounters_of :ATTR(:get<extension_encounters>);

sub BUILD {
    my ( $self, $id, $opts_ref ) = @_;
    $ExtensionEncounters_of{$id} = {};
}

sub add_extension_encounter {
    my ( $self, $encounter ) = @_;
    $ExtensionEncounters_of{ident $self}{$encounter->get_presented_terms_string} = $encounter;
}

sub AverageTimeToUnderstand {
    my ( $self ) = @_;
    my @encounters = values %{$self->get_extension_encounters()};
    return 0 unless @encounters;
    return sum(map {$_->get_time_to_understand()} @encounters) / scalar(@encounters);
}
memoize('AverageTimeToUnderstand');

sub AverageTimeToUnderstandWhenCorrect {
    my ( $self ) = @_;
    my @encounters = grep { $_->get_is_extension_correct() > 0 } values %{$self->get_extension_encounters};
    return 0 unless @encounters;
    return sum(map {$_->get_time_to_understand()} @encounters) / scalar(@encounters);
}
memoize('AverageTimeToUnderstandWhenCorrect');

sub PercentCorrect {
    my ( $self ) = @_;
    my @encounters = grep { $_->get_is_extension_correct() >= 0 } values %{$self->get_extension_encounters};
    my @correct_encounters = grep { $_->get_is_extension_correct() > 0 } values %{$self->get_extension_encounters};
    return 0 unless @encounters;
    return 100*scalar(@correct_encounters)/scalar(@encounters);
}
memoize('PercentCorrect');

sub MarkAllEncountersAsOutliers {
    my ( $self ) = @_;
    for my $encounter (values %{$self->get_extension_encounters}) {
        $encounter->set_is_outlier(1);
    }
}

1;
