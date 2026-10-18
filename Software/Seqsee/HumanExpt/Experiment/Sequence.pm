package Experiment::Sequence;
use strict;
use Class::Std;
use List::Util qw{min};
use Memoize;
use Smart::Comments;

my %PresentedTerms_of : ATTR(:name<presented_terms>);
my %AcceptableExtensions_of : ATTR(:name<acceptable_extensions>);
my %Encounters_of : ATTR(:get<encounters>);

my %InlierStat_of :ATTR(:get<inlier_stat>, :set<inlier_stat>);
my %InlierCorrectStat_of :ATTR(:get<inlier_correct_stat>, :set<inlier_correct_stat>);

{
    my %MEMO;

    sub FetchOrCreate {
        my ( $package, $opts_ref ) = @_;
        my $key = array_to_string( $opts_ref->{presented_terms} );
        if ( exists $MEMO{$key} ) {
            return $MEMO{$key};
        }
        return ( $MEMO{$key} = $package->new($opts_ref) );
    }

    sub FetchGivenPresentedTerms {
        my ( $package, $presented_ref ) = @_;
        my $key = array_to_string($presented_ref);
        return $MEMO{$key} || die "Sequence fetch attempted for unseen sequence '$key'";
    }
}

sub array_to_string {
    my ($array_ref) = @_;
    join( ", ", @$array_ref );
}

sub HasAnyCorrectExtension {
    my ($self) = @_;
    my $id = ident $self;
    if ( @{ $AcceptableExtensions_of{$id} } ) {
        return 1;
    }
    else {
        return 0;
    }
}
memoize('HasAnyCorrectExtension');

sub IsExtensionCorrect {
    my ( $self, $extension ) = @_;
    my @extension      = @$extension;
    my $extension_size = scalar(@extension);

    my @acceptable_extensions = @{ $self->get_acceptable_extensions() };
    ## extension, acceptable_extension: @extension, @acceptable_extensions
    if ( not(@acceptable_extensions) ) {

        # print "*";
        return -1;
    }

    my $idx = 0;
OUTER: for my $ae_ref (@acceptable_extensions) {
        $idx++;
        my @acceptable_extension      = @$ae_ref;
        my $acceptable_extension_size = scalar(@acceptable_extension);
    INNER: for ( 0 .. min( $extension_size, $acceptable_extension_size ) - 1 ) {
            next OUTER if $extension[$_] != $acceptable_extension[$_];
        }

        # print $idx;
        return $idx;
    }

    # print 'x';
    return 0;
}

sub add_encounter {
    my ( $self, $encounter ) = @_;
    push @{ $Encounters_of{ ident $self} }, $encounter;
}

sub GetEncountersForInlierParticipants {
    my ($self) = @_;
    my @ret;
    for my $encounter ( @{ $self->get_encounters() } ) {
        push( @ret, $encounter ) if $encounter->get_is_outlier() != 1;
    }
    return @ret;
}

sub GetInlierEncounters {
    my ($self) = @_;
    return grep { not( $_->get_is_outlier ) } @{ $self->get_encounters };
}

sub GetCorrectEncountersForInlierParticipants {
    my ($self) = @_;
    return grep { $_->get_is_extension_correct() > 0 } $self->GetEncountersForInlierParticipants();
}

sub GetCorrectInlierEncounters {
    my ($self) = @_;
    return grep { $_->get_is_extension_correct() > 0 } $self->GetInlierEncounters;
}

sub PercentCorrectForInlierParticipants {
    my ($self) = @_;
    return 0 unless $self->HasAnyCorrectExtension();
    my @inlier_participants = $self->GetEncountersForInlierParticipants();
    my $inlier_count        = scalar(@inlier_participants);
    return "---" unless $inlier_count;

    return 100 * scalar( $self->GetCorrectEncountersForInlierParticipants ) / $inlier_count;
}

sub PercentCorrectForInliers {
    my ($self) = @_;
    return 0 unless $self->HasAnyCorrectExtension();
    my @inlier_encounters = $self->GetInlierEncounters;
    return '---' unless @inlier_encounters;
    return 100 * scalar( $self->GetCorrectInlierEncounters ) / scalar(@inlier_encounters);
}

sub UnderstandingTimeForInlierParticipants {
    my ($self) = @_;
    map { $_->get_time_to_understand } $self->GetEncountersForInlierParticipants;
}

sub UnderstandingTimeWhenCorrectForInlierParticipants {
    my ($self) = @_;
    map { $_->get_time_to_understand } $self->GetCorrectEncountersForInlierParticipants;
}

sub SetAsOutliers {
    my ( $self, $fn, $min, $max ) = @_;
    for my $encounter ( $self->GetEncountersForInlierParticipants() ) {
        my $value = $encounter->$fn();
        if ( $value < $min or $value > $max ) {
            $encounter->set_is_outlier(2);
            print "\t\t****Marked $value as outlier\n";
        }
    }
}

sub as_text {
    my ($self) = @_;
    return array_to_string( $self->get_presented_terms );
}

sub CreateTypingTimeProfile {
    my ( $self ) = @_;
    my @array_of_stats;
    for (0..9) {
        push @array_of_stats, Statistics::Descriptive::Full->new();
    }
    for my $encounter ($self->GetCorrectInlierEncounters) {
        my @typing_times = @{$encounter->get_typing_times};
        for (0..9) {
            last if $typing_times[$_] eq '?';
            $array_of_stats[$_]->add_data($typing_times[$_]);
        }
    }
    return map { $_->mean() } @array_of_stats;
}


1;
