# Smoke golden for the GUI test infrastructure (item 000): an empty canvas, one item of each
# type with Tk defaults, one item of each type with the options the Seqsee GUI uses
# (lib/Themes/Std2.pm, lib/SGUI/*.pm), and Tk's RGB values for the X11 colour names in
# seqsee/gui/draw/colors.py. The 'styled' canvas is saved as docs/gui/perl/smoke.png.
use strict;
use warnings;
use Tk;
use Oracle;
use CanvasDump;

my $mw = MainWindow->new;

sub canvas_case {
  my ( $name, $w, $h, $draw, $png ) = @_;
  my $c = $mw->Canvas( -width => $w, -height => $h, -background => '#FFFFFF' )->pack;
  $draw->($c);
  record( name => $name, width => $w, height => $h, items => dump_canvas($c) );
  save_png( $mw, $c, perl_screen_path($png) ) if $png;
  $c->destroy;
}

canvas_case( 'empty', 300, 200, sub { } );

canvas_case(
  'defaults', 300, 200,
  sub {
    my ($c) = @_;
    $c->createLine( 10, 10, 60, 40 );
    $c->createOval( 70, 10, 110, 40 );
    $c->createRectangle( 120, 10, 160, 40 );
    $c->createPolygon( 170, 40, 190, 10, 210, 40 );
    $c->createArc( 220, 10, 280, 60 );
    $c->createText( 150, 100, -text => 'defaults' );
  }
);

canvas_case(
  'styled', 400, 260,
  sub {
    my ($c) = @_;
    $c->createRectangle( 5, 5, 395, 255, -fill => '#EEEEEE', -outline => '', -tags => ['bg'] );
    $c->createLine( 20, 200, 80, 120, 140, 200, -fill => 'red', -width => 3, -smooth => 1,
      -arrow => 'last', -arrowshape => [ 8, 12, 10 ], -tags => [ 'reln', 'r1' ] );
    $c->createLine( 160, 30, 380, 30, -fill => '#0000FF', -width => 2, -dash => '---' );
    $c->createLine( 160, 50, 380, 50, -dash => [ 6, 4 ], -arrow => 'both', -capstyle => 'round' );
    $c->createOval( 160, 70, 220, 130, -fill => '#CCFFDD', -outline => 'navy blue', -width => 0 );
    $c->createRectangle( 240, 70, 300, 130, -fill => 'gray75', -outline => '#000000',
      -width => 4, -stipple => 'gray75' );
    $c->createPolygon( 320, 130, 350, 70, 380, 130, -fill => '', -outline => 'DarkGreen',
      -smooth => 1, -width => 2 );
    $c->createArc( 160, 150, 260, 250, -start => 30, -extent => 120, -style => 'arc',
      -outline => '#FF0000', -width => 2 );
    $c->createArc( 270, 150, 370, 250, -start => 200, -extent => 90, -style => 'chord',
      -fill => 'yellow' );
    $c->createText( 20, 20, -text => 'nw anchor', -anchor => 'nw',
      -font => '-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4',
      -fill => '#FF0000', -tags => ['label'] );
    $c->createText( 80, 240, -text => "two\nlines", -justify => 'center',
      -font => '-adobe-helvetica-bold-r-normal--10-140-100-100-p-105-iso8859-4' );
  },
  'smoke'
);

# The base names of seqsee/gui/draw/colors.py, some spelling variants, grayN and hex forms.
my @base = qw(
  aliceblue antiquewhite aquamarine azure beige bisque black blanchedalmond blue blueviolet
  brown burlywood cadetblue chartreuse chocolate coral cornflowerblue cornsilk cyan darkblue
  darkcyan darkgoldenrod darkgray darkgreen darkkhaki darkmagenta darkolivegreen darkorange
  darkorchid darkred darksalmon darkseagreen darkslateblue darkslategray darkturquoise
  darkviolet deeppink deepskyblue dimgray dodgerblue firebrick floralwhite forestgreen
  gainsboro ghostwhite gold goldenrod gray green greenyellow honeydew hotpink indianred ivory
  khaki lavender lavenderblush lawngreen lemonchiffon lightblue lightcoral lightcyan
  lightgoldenrod lightgoldenrodyellow lightgray lightgreen lightpink lightsalmon lightseagreen
  lightskyblue lightslateblue lightslategray lightsteelblue lightyellow limegreen linen magenta
  maroon mediumaquamarine mediumblue mediumorchid mediumpurple mediumseagreen mediumslateblue
  mediumspringgreen mediumturquoise mediumvioletred midnightblue mintcream mistyrose moccasin
  navajowhite navy navyblue oldlace olivedrab orange orangered orchid palegoldenrod palegreen
  paleturquoise palevioletred papayawhip peachpuff peru pink plum powderblue purple red
  rosybrown royalblue saddlebrown salmon sandybrown seagreen seashell sienna skyblue slateblue
  slategray snow springgreen steelblue tan thistle tomato turquoise violet violetred wheat
  white whitesmoke yellow yellowgreen
);
my @names = (
  @base, 'navy blue', 'NavyBlue', 'DarkGreen', 'dark green', 'LightGrey', 'grey', 'DimGrey',
  ( map { "gray$_" } 0 .. 100 ), 'grey75', 'Grey33',
  '#abc', '#AABBCC', '#aaaabbbbcccc', '#aabbcc',
);
record( name => 'colours', rgb => { map { $_ => normalise_colour( $mw, $_ ) } @names } );

emit();
