# Theme golden (loop0002 item 002): lib/SColor.pm (HSV2RGB, HSV2Color) and every Style::*
# function of lib/Themes/Std2.pm, called over a grid of arguments, including out-of-range
# values (v >= 100, negative v, h >= 360) whose Perl results are quirky.
# Style results are recorded as ordered [key, value] pairs (undef -> null).
# Needs no canvas; it is named gui_* only because it belongs to the GUI port.
use strict;
use Oracle;
use Themes::Std2;

$SIG{__WARN__} = sub { };    # undef hash lookups etc. in the out-of-range cases

sub pairs {
  my @kv = @_;
  my @out;
  while (@kv) { my $k = shift @kv; my $v = shift @kv; push @out, [ $k, $v ]; }
  return \@out;
}

# --- SColor ---
my @H = ( ( map { $_ * 20 } 0 .. 17 ), 59.9, 60, 100, 160, 190, 250, 300, 359, 359.9, 360, 400, -10, -60.5 );
my @S = ( 0, 20, 30, 40, 50, 60, 70, 90, 100, -5 );
my @V = ( 0, 2, 5, 12.3, 20, 50, 60, 80, 90, 99, 99.6, 100, 120, -1, -10, -25.5 );
my @hsv;
for my $h (@H) {
  for my $s (@S) {
    for my $v (@V) {
      my @rgb = SColor::HSV2RGB( $h, $s, $v );
      push @hsv, { args => [ $h, $s, $v ], rgb => [ map { "$_" } @rgb ],
        color => SColor::HSV2Color( $h, $s, $v ) };
    }
  }
}
record( name => 'hsv', calls => \@hsv );

# --- Style::* ---
my @calls;
sub call {
  my ( $fn, @args ) = @_;
  no strict 'refs';
  my @res = &{"Style::$fn"}(@args);
  push @calls, { fn => $fn, args => [@args], result => pairs(@res) };
}

my @ATT = ( -0.1, 0, 0.001, 0.01, 0.0333, 0.05, 0.1, 0.123, 0.2, 0.2475, 0.25, 0.3, 1 );

call( 'Element', $_ ) for 0, 1, 2, 3;
call('Starred');
for my $st ( 0, 10, 33.3, 50, 77.7, 100 ) { call( 'Relation', $st, $_ ) for 0, 1 }
for my $meto ( 0, 1, 0.5 ) {
  for my $st ( 0, 10, 12.5, 33.3, 50, 77.7, 100, 120 ) { call( 'Group', $meto, $st, $_ ) for 0, 1 }
}
for my $meto ( 0, 1 ) {
  for my $cat ( 'ascending', 'descending', 'sameness', 'other', '', '0', undef ) {
    call( 'Group2', $meto, $cat, $_ ) for 0, 1;
  }
}
call( 'GroupBorder', $_ ) for 0, 1, 2, 3;
call( 'ElementAttention', $_ ) for @ATT;
call( 'GroupAttention', $_ ) for @ATT;
call('GroupBorderAttention');
call( 'RelationAttention', $_ ) for @ATT;
call( 'NetActivation', $_ ) for 0, 1, 5, 10, 33, 50, 77, 99, 100, 101, 102, 103, 110, -12;
for my $hit ( 0, 1, 100, 333, 1000, 1999, 2000, 2500, -100 ) { call( 'ThoughtBox', $hit, $_ ) for 0, 1 }
call( 'ThoughtComponent', 0, 0 );
call( 'ThoughtComponent', 0.7, 55 );
call('ThoughtHead');
record( name => 'styles', calls => \@calls );

# Wrong argument counts confess.
record( name => 'arity', dies => [
  dies( sub { Style::Element() } ),
  dies( sub { Style::Starred(1) } ),
  dies( sub { Style::Relation(1) } ),
  dies( sub { Style::Group( 1, 2 ) } ),
  dies( sub { Style::NoSuchStyle() } ),
] );

emit();
