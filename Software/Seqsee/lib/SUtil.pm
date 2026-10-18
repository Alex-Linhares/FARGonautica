package SUtil;
use 5.10.0;
use strict;
use warnings;
use SPos;
use Smart::Comments;

#use SCat;
# use SBlemishType;

#use SBlemish::double;
#use SBlemish::triple;
use Carp;

our @EXPORT = qw{uniq equal_when_flattened generate_blemished};
use base qw{Exporter};

sub uniq {
  my %hash;
  for (@_) {
    $hash{$_} = $_;
  }
  values %hash;
}

sub compare_deep {
  ( @_ == 2 ) or confess;
  my ( $deep_list1, $deep_list2 ) = @_;
  my $is_ref1 = ref $deep_list1;
  my $is_ref2 = ref $deep_list2;
  ## compare_deep: @_
  if ( !$is_ref1 and!$is_ref2 ) {
    return ( $deep_list1 == $deep_list2 );
  }
  if ( $is_ref1 and $is_ref2 ) {
    confess unless ( $is_ref1 eq 'ARRAY' and $is_ref2 eq 'ARRAY' );
    return unless @$deep_list1 == @$deep_list2;
    for ( my $i = 0 ; $i < @$deep_list1 ; $i++ ) {
      return
      unless compare_deep( $deep_list1->[$i], $deep_list2->[$i] );
    }
    return 1;
  }
  return;
}

sub equal_when_flattened {
  my ( $obj1, $obj2 ) = @_;
  unless ( ref $obj1 ) {
    if ( ref $obj2 ) {
      return undef;
    }
    else {
      return $obj1 == $obj2;
    }
  }
  return undef unless ref $obj2;
  my @flattened1 = $obj1->flatten;
  my @flattened2 = $obj2->flatten;
  return undef unless scalar(@flattened1) == scalar(@flattened2);
  for my $i ( 0 .. scalar(@flattened1) - 1 ) {
    return undef unless $flattened1[$i] == $flattened2[$i];
  }
  return 1;
}

sub generate_blemished {
  my (%args)  = @_;
  my $cat     = delete $args{cat};
  my $blemish = delete $args{blemish};
  my $pos     = delete $args{pos};
  my $bo = $cat->build( {%args} );
  my $blemished = $bo->apply_blemish_at( $blemish, $pos );
  return $blemished;
}

sub oddman {

  # use Smart::Comments;
  my (@objects) = @_;
  for (@objects) {
    $_->seek_blemishes(
      [ $SBlemishType::triple::triple, $SBlemishType::double::double ] );
  }
  for my $cat ( $SCat::ascending::ascending, $SCat::mountain::mountain ) {
    my @bindings = map { $cat->is_instance($_) } @objects;
    ## @bindings
    my @definedness = map { defined($_) ? 1 :0 } @bindings;
    ## definedness for: $cat->get_name(), @definedness
    my $odd_position;
    $odd_position = odd_position(@definedness);
    if ( defined $odd_position ) {

      # Cool, we have a solution!
      ## position of odd: $odd_position
      ## Found odd man: $objects[$odd_position]->show
      my $msg;
      if ( $definedness[$odd_position] ) {
        $msg = "The only " . $cat->get_name() . ":";
      }
      else {
        $msg = "Everything else is a " . $cat->get_name() . ":";
      }
      return join( " ", $msg, $objects[$odd_position]->flatten );
    }
    if ( myall(@definedness) ) {
      return "All are instances of " . $cat->get_name();
    }

    # print "Press Enter"; <STDIN>;
  }

}

sub myall {
  for (@_) {
    return unless $_;
  }
  return 1;
}

sub odd_position {
  my @input = @_;
  croak "need at least three arguments" unless @input >= 3;
  my ( $odd_pos, $odd_value, $repeated_value );
  if ( $input[0] eq $input[1] ) {

    # odd isn't first or second!
    $repeated_value = $input[0];
    for ( my $i = 2 ; $i < @input ; $i++ ) {
      next if $input[$i] eq $input[0];
      $odd_pos   = $i;
      $odd_value = $input[$i];
      last;
    }
  }
  else {    # first or second is odd
    if ( $input[0] eq $input[2] ) {
      $odd_pos        = 1;
      $odd_value      = $input[1];
      $repeated_value = $input[0];
    }
    else {
      $odd_pos        = 0;
      $odd_value      = $input[0];
      $repeated_value = $input[1];
    }
  }

  return unless defined $odd_pos;

  # So: a problematic position is guesses, along with value
  for ( my $i = 0 ; $i < @input ; $i++ ) {
    if ( $i == $odd_pos ) {
      next if $input[$i] eq $odd_value;
      return;
    }
    else {
      next if $input[$i] eq $repeated_value;
      return;
    }
  }
  return $odd_pos;
}

sub naive_brittle_chunking {
  my $array_ref = shift;
  my @items     = @$array_ref;
  my @ret;
  while ( @items > 1 ) {
    my $next_item     = shift(@items);
    my @next_item_arr = ($next_item);
    while ( @items and $items[0] == $next_item ) {
      push @next_item_arr, shift(@items);
    }
    if ( @next_item_arr > 1 ) {
      push @ret, [@next_item_arr];
    }
    else {
      push @ret, $next_item;
    }
  }
  push @ret, @items;    # at most one left
  return @ret;
}

sub next_available_file_number {

  # use Smart::Comments;
  my $dir = shift;
  ## $dir
  my @filelist = glob("$dir/*");
  ## @filelist
  my @numbers = grep { $_ } map { s/\D//g; $_ } @filelist;
  ## @numbers
  return 1 unless @numbers;
  return 1 + List::Util::max(@numbers);
}

sub toss {
  my ($prob) = @_;
  confess "uninitialized prob as argument!" unless defined $prob;
  rand() <= $prob ? 1 :0;
}

sub clear_all {
  SWorkspace->clear();
  clear_all_but_workspace();
}

sub clear_all_but_workspace {
  $Global::MainStream->clear;
  SCoderack->clear;
}

sub minmax {
  die "undefined!" unless @_;
  return ( $_[0], $_[0] ) if @_ == 1;
  my $min = shift;
  my $max = $min;
  while (@_) {
    my $x = shift;
    $min = $x if $min > $x;
    $max = $x if $max < $x;
  }
  return ( $min, $max );

}

sub all (&@) {
  my ( $code, @rest ) = @_;
  for (@rest) {
    return unless $code->($_);
  }
  return 1;
}

sub significant {
  my ($x) = @_;

  # XXX(Board-it-up): [2006/11/16] should be configurable
  return ( $x > 0.7 ) ? 1 :0;
}

sub StructureToString {
  my ($structure) = @_;
  if ( ref $structure ) {
    return '[' . join( ', ', map { StructureToString($_) } @$structure ) . ']';
  }
  else {
    return $structure;
  }
}

sub trim {
  for (@_) {
    s#^\s*##;
    s#\s*$##;
    s#\s+# #;
  }
}

sub StringifyForCarp {
  my $arg = shift;

  # temporarily replaced line by next...
  $arg = '«' . $arg->as_text() . '»'
  if ( ref($arg) and UNIVERSAL::can( $arg, 'as_text' ) );

  # $arg = '«' . $arg . '»' if UNIVERSAL::can( $arg, 'as_text' );

  my $reftype = ref($arg);
  if ($reftype) {
    if ( $reftype eq 'HASH' ) {
      my %arg = %$arg;
      $arg = '{ ';
      while ( my ( $k, $v ) = each %arg ) {
        $v //= 'undef';
        $arg .= "$k => ";
        if ( UNIVERSAL::can( $v, 'as_text' ) ) {
          $arg .= '«' . $v->as_text() . '»';
        }
        elsif ( ref($v) eq 'ARRAY' ) {
          $arg .= '[ '
          . join( ', ',
            map { UNIVERSAL::can( $_, 'as_text' ) ? $_->as_text() :$_ } @{$v} )
          . ' ]';
        }
        else {
          $arg .= "$v";
        }
        $arg .= ", ";
      }
      $arg .= ' }';
    }
    elsif ( $reftype eq 'ARRAY' ) {
      $arg = join( ', ',
        map { UNIVERSAL::can( $_, 'as_text' ) ? $_->as_text :"$_" } @$arg );
    }
    else {
      $arg = "reftype=$reftype";
    }
  }
  elsif ( not defined($arg) ) {
    $arg = 'undef';
  }
  return $arg;
}

sub hash_sorted_as_array {
  my (%hash) = @_;
  my @keys = sort keys %hash;
  return map { ( $_, $hash{$_} ) } @keys;
}

sub StringifyDeepArray {
  my ($item) = @_;
  if ( ref($item) ) {
    return '[' . join( ', ', map StringifyDeepArray($_), @$item ) . ']';
  }
  return $item;
}

1;
