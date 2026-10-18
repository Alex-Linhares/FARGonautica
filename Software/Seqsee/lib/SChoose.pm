#####################################################
#
#    Package: SChoose
#
#####################################################
#   Creating custom choosers
#####################################################

package SChoose;
use strict;
use Carp;
use Class::Std;
use base qw{};
use Smart::Comments;
use List::Util qw(sum);
use English qw(-no_match_vars );

sub create {
  my ( $package, $opts_ref ) = @_;

  my ( $map_needed,  $grep_needed );
  my ( $map_closure, $grep_closure );

  my $map_fn = $opts_ref->{map};
  if ( defined $map_fn ) {
    $map_needed = 1;
    if ( UNIVERSAL::isa( $map_fn, 'CODE' ) ) {
      $map_closure = $map_fn;
      $map_fn      = q{$map_closure->($_)};
    }
  }

  my $grep_fn = $opts_ref->{grep};
  if ( defined $grep_fn ) {
    $grep_needed = 1;
    if ( UNIVERSAL::isa( $grep_fn, 'CODE' ) ) {
      $grep_closure = $grep_fn;
      $grep_fn      = q{$grep_closure->($_)};
    }
  }

  ## map_fn, grep_fn: $map_fn, $grep_fn

  my $choosing_sub = q{ sub {
        my ( $objects_ref ) = @_;
        return unless @$objects_ref;

        my $likelihood;
        my ( $likelihood_sum, @likelihood_parial_sums) = (0);

        CHANGEABLE_PART;

        my $idx;
        if ($likelihood_sum) {
            my $random = rand() * $likelihood_sum;
            $idx = -1;
            for (@likelihood_parial_sums) {
                $idx++;
                return $objects_ref->[$idx] if $_ >= $random;
            }
        }
        else {
            $idx = int( rand() * scalar(@likelihood_parial_sums) );
            return $objects_ref->[$idx];
        }
    }; };

  my $GREP_PREAMBLE  = q{ my($grep_pass_count, @grep_pass_array) = (0); };
  my $GREP_POSTAMBLE = q{
    if ( $grep_pass_count and not $likelihood_sum ) {
        my $random = rand() * $grep_pass_count;
        my $idx = -1;
        for (@grep_pass_array) {
            $idx++;
            return $objects_ref->[$idx] if $_ >= $random;
        }
    } elsif (not $grep_pass_count) {
       return;
    }
    };

  my $CHANGEABLE_PART;
  if ( $map_needed and $grep_needed ) {
    $CHANGEABLE_PART = $GREP_PREAMBLE . q{
          for (@$objects_ref) {
            $likelihood = MAP_CODE;
            my $passed_grep = GREP_CODE;
            if ($passed_grep) {
                $grep_pass_count++;
            }
            else {
                $likelihood = 0;
            }
            push @grep_pass_array, $grep_pass_count;
            $likelihood_sum += $likelihood;
            push @likelihood_parial_sums, $likelihood_sum;
        }
      } . $GREP_POSTAMBLE;
  }
  elsif ( $map_needed and not $grep_needed ) {
    $CHANGEABLE_PART = q{
         for (@$objects_ref) {
            $likelihood = MAP_CODE;
            $likelihood_sum += $likelihood;
            push @likelihood_parial_sums, $likelihood_sum;
        }};
  }
  elsif ( $grep_needed and not $map_needed ) {
    $CHANGEABLE_PART = $GREP_PREAMBLE . q{
          for (@$objects_ref) {
            $likelihood = $_;
            my $passed_grep = GREP_CODE;
            if ($passed_grep) {
                $grep_pass_count++;
            }
            else {
                $likelihood = 0;
            }
            push @grep_pass_array, $grep_pass_count;
            $likelihood_sum += $likelihood;
            push @likelihood_parial_sums, $likelihood_sum;
        }
      } . $GREP_POSTAMBLE;
  }
  else {    # neither map nor grep
    $CHANGEABLE_PART = q{
         for (@$objects_ref) {
            $likelihood = $_;
            $likelihood_sum += $likelihood;
            push @likelihood_parial_sums, $likelihood_sum;
        }};
  }

  if ($grep_needed) {
    $CHANGEABLE_PART =~ s#GREP_CODE#$grep_fn#g;
  }
  if ($map_needed) {
    $CHANGEABLE_PART =~ s#MAP_CODE#$map_fn#g;
  }

  ## CHANGEABLE_PART: $CHANGEABLE_PART

  $choosing_sub =~ s#CHANGEABLE_PART;#$CHANGEABLE_PART#g;
  ## choosing_sub: $choosing_sub
  my $ret = eval $choosing_sub;
  ## ret: $ret
  if ($EVAL_ERROR) {
    confess $EVAL_ERROR;
  }
  return $ret;
}

sub choose {
  my ( $package, $number_ref, $name_ref ) = @_;
  return unless @$number_ref;
  $name_ref ||= $number_ref;

  my $random = rand() * sum(@$number_ref);
  my $idx    = -1;
  for (@$number_ref) {
    $idx++;
    last if $_ > $random;
    $random -= $_;
  }
  return $name_ref->[$idx];
}

sub choose_a_few_nonzero {
  my ( $package, $how_many, $number_ref, $name_ref ) = @_;
  my @numbers = @$number_ref;
  my @names   = @{ $name_ref // $number_ref };
  my $sum     = sum(@numbers);
  my @chosen;
  my $still_to_choose = $how_many;

  while ( $still_to_choose and $sum > 0 ) {
    my $random = rand() * $sum;
    my $idx    = -1;
    for (@numbers) {
      $idx++;
      last if $_ > $random;
      $random -= $_;
    }
    push @chosen, $names[$idx];
    $sum -= $numbers[$idx];
    $numbers[$idx] = 0;
    $still_to_choose--;
  }

  return @chosen;
}

sub choose_if_non_zero {
  my ( $package, $number_ref, $name_ref ) = @_;
  return unless @$number_ref;
  $name_ref ||= $number_ref;

  my $sum = sum(@$number_ref);
  return unless $sum;

  my $random = rand() * $sum;
  my $idx    = -1;
  for (@$number_ref) {
    $idx++;
    last if $_ > $random;
    $random -= $_;
  }
  return $name_ref->[$idx];
}

sub using_fascination {
  my ( $package, $array_ref, $fasc ) = @_;
  my @imp = map { $_->get_fascination($fasc) } @$array_ref;
  $package->choose( $array_ref, \@imp );
}

sub uniform {
  my ( $package, $arr_ref ) = @_;
  return $arr_ref->[ int( rand() * scalar(@$arr_ref) ) ];
}

1;
