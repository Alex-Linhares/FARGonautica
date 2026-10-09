package CanvasDump;
# Helpers for GUI oracle scripts (run with oracle/run_perl_gui.sh): dump every item of a
# Tk canvas as plain data, and save the canvas as a PNG screenshot.
#
# dump_canvas($c) returns one hash per item, in display-list (draw) order:
#   { type => 'line', coords => [x0, y0, ...] (rounded to 0.1),
#     opts => { only the options whose value differs from Tk's default },
#     tags => [ ... ] }
# dump_canvas($c, \%tag_map) replaces each tag found in %tag_map (e.g. a stringified object
# ref -> 'obj3'); a tag that still looks like a ref ('=HASH(0x...)') makes it die, so no
# address is ever recorded.
# Normalisation: colours become '#RRGGBB'; numbers become numbers; list-valued options
# (arrowshape) become arrays; -smooth becomes 1; -font becomes its name; '' counts as unset.
# Active*/disabled* options, -offset/-outlineoffset (when default), -updatecommand and the
# 'current' tag are skipped.
#
# Perl/Tk 804 reads string -dash patterns ('---', '.') back as garbage, so -dash is taken
# from the value passed to create/itemconfigure, which this module records (load it before
# drawing). save_png($mw, $c, $path) writes an EPS with $c->postscript and converts it with gs.
use strict;
use warnings;
use Tk;
use Tk::Canvas;
use File::Basename qw(dirname);
use File::Path qw(make_path);
use File::Spec;
use File::Temp qw(tempdir);
use Exporter 'import';
our @EXPORT = qw(dump_canvas save_png perl_screen_path normalise_colour);

my %Dash;    # "$canvas:$id" -> -dash value as passed in

sub _opts_from_args {
  my @args = @_;
  my $i = 0;
  $i++ while $i < @args && !( defined $args[$i] && !ref $args[$i] && $args[$i] =~ /^-[a-z]/ );
  return @args[ $i .. $#args ];
}

{
  no warnings 'redefine';
  my $create = \&Tk::Canvas::create;
  *Tk::Canvas::create = sub {
    my ( $c, $type, @rest ) = @_;
    my $id = $create->( $c, $type, @rest );
    my %o = _opts_from_args(@rest);
    $Dash{"$c:$id"} = $o{-dash} if exists $o{-dash};
    return $id;
  };
  my $itemconfigure = \&Tk::Canvas::itemconfigure;
  *Tk::Canvas::itemconfigure = sub {
    my ( $c, $tag, @rest ) = @_;
    if ( @rest >= 2 ) {
      my %o = @rest;
      if ( exists $o{-dash} ) {
        $Dash{"$c:$_"} = $o{-dash} for $c->find( 'withtag', $tag );
      }
    }
    return $itemconfigure->( $c, $tag, @rest );
  };
}

my %COLOUR_OPTS = map { $_ => 1 } qw(-fill -outline);
my %NUMERIC_OPTS = map { $_ => 1 } qw(-width -start -extent -splinesteps -dashoffset);
my %SKIP = map { $_ => 1 } qw(-updatecommand -tags -dash);
my %IGNORED_DEFAULTS = ( '-style' => 'pieslice' );    # Tk reports no default for arc -style

sub _num { my ($v) = @_; my $r = sprintf( "%.1f", $v ); return 0 + $r; }

sub normalise_colour {
  my ( $w, $colour ) = @_;
  return undef if !defined $colour || $colour eq '';
  my @rgb = $w->rgb($colour);
  return sprintf( "#%02X%02X%02X", map { int( $_ / 257 + 0.5 ) } @rgb );
}

sub _value {
  my ( $c, $name, $v ) = @_;
  return undef if !defined $v;
  if ( ref $v eq 'ARRAY' ) {
    return undef unless @$v;
    return [ map { /^-?[\d.]+$/ ? _num($_) : $_ } @$v ];
  }
  if ( ref $v && $v->isa('Tk::Font') ) { return $$v; }
  return undef if $v eq '';
  return normalise_colour( $c, $v ) if $COLOUR_OPTS{$name};
  return ( $v eq '0' ? undef : 1 ) if $name eq '-smooth';
  return _num($v) if $NUMERIC_OPTS{$name} && $v =~ /^-?[\d.]+$/;
  return $v;
}

sub _same {
  my ( $a, $b ) = @_;
  return 1 if !defined $a && !defined $b;
  return 0 if !defined $a || !defined $b;
  $a = join( ' ', @$a ) if ref $a eq 'ARRAY';
  $b = join( ' ', @$b ) if ref $b eq 'ARRAY';
  return $a == $b if $a =~ /^-?[\d.]+$/ && $b =~ /^-?[\d.]+$/;
  return $a eq $b;
}

sub _tag {
  my ( $tag_map, $t ) = @_;
  $t = $tag_map->{$t} if $tag_map && exists $tag_map->{$t};
  die "dump_canvas: unmapped object tag $t\n" if $t =~ /=[A-Z]+\(0x[0-9a-f]+\)/;
  return $t;
}

sub dump_canvas {
  my ( $c, $tag_map ) = @_;
  my @items;
  for my $id ( $c->find('all') ) {
    my %opts;
    for my $spec ( $c->itemconfigure($id) ) {
      my ( $name, undef, undef, $default, $current ) = @$spec;
      next if $SKIP{$name} || $name =~ /^-(active|disabled)/;
      my $cur = _value( $c, $name, $current );
      my $def = exists $IGNORED_DEFAULTS{$name} ? $IGNORED_DEFAULTS{$name} : _value( $c, $name, $default );
      next if _same( $cur, $def );
      next if ( $name eq '-offset' || $name eq '-outlineoffset' ) && _same( $cur, '0 0' );
      ( my $key = $name ) =~ s/^-//;
      $opts{$key} = $cur;
    }
    my $dash = $Dash{"$c:$id"};
    if ( defined $dash && ( ref $dash ? @$dash : $dash ne '' ) ) {
      $opts{dash} = ref $dash ? [ map { 0 + $_ } @$dash ] : $dash;
    }
    push @items, {
      type   => $c->type($id),
      coords => [ map { _num($_) } $c->coords($id) ],
      opts   => \%opts,
      tags   => [ map { _tag( $tag_map, $_ ) } grep { $_ ne 'current' } $c->gettags($id) ],
    };
  }
  return \@items;
}

# python/docs/gui/perl/NAME.png
sub perl_screen_path {
  my ($name) = @_;
  my $here = dirname( File::Spec->rel2abs(__FILE__) );
  return File::Spec->catfile( $here, '..', 'docs', 'gui', 'perl', "$name.png" );
}

sub save_png {
  my ( $mw, $c, $path ) = @_;
  make_path( dirname($path) );
  $mw->update;
  my $eps = File::Spec->catfile( tempdir( CLEANUP => 1 ), 'canvas.eps' );
  $c->postscript( -file => $eps );
  system( 'gs', '-q', '-dSAFER', '-dBATCH', '-dNOPAUSE', '-sDEVICE=png16m', '-dEPSCrop',
    '-r96', "-sOutputFile=$path", $eps ) == 0
    or die "gs failed for $path\n";
  return $path;
}

1;
