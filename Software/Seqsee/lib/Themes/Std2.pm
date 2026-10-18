package Style;
use Carp;

sub AUTOLOAD {
  our $AUTOLOAD;
  confess "Unknown method $AUTOLOAD called. Have you defined this style?";
}

package Themes::Std;
use Memoize;
use SColor;
use Carp;

*HSV = \&SColor::HSV2Color;

 
{
    sub Style::Element{ scalar(@_)==1 or confess "Needed exactly 1 arguments!";
my ($hilit)=@_;
; return (-fill => do {
    if ( $hilit == 1 ) {
      '#00FF00';
    }
    elsif ( $hilit == 2 ) {
      '#0000FF';
    }
    else {
      HSV( 160, 20, 20 );
    }
  },
-anchor => do { 'center' },
-font => do {
    $hilit
    ? '-adobe-helvetica-bold-r-normal--28-140-100-100-p-105-iso8859-4'
    :'-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4';
  },
);
 }; memoize('Style::Element');
}


 
{
    sub Style::Starred{ scalar(@_)==0 or confess "Expected no arguments!";
; return (-fill => do { HSV( 240, 50, 50 ) },
-anchor => do { 'center' },
-font => do { '-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4' },
);
 }; memoize('Style::Starred');
}


 
{
    sub Style::Relation{ scalar(@_)==2 or confess "Needed exactly 2 arguments!";
my ($strength, $hilit)=@_;
; return (-width => do { $hilit ? 5 :3 },
-fill => do { $hilit ? "#00FF00" :"#777777" },
-smooth => do { 1 },
-arrow => do { 'last' },
);
 }; memoize('Style::Relation');
}


 
{
    sub Style::Group{ scalar(@_)==3 or confess "Needed exactly 3 arguments!";
my ($meto, $strength, $is_largest)=@_;
; return (-width => do { 0 },
-fill => do {
    my ( $s, $v ) = ( 40, 90 - 0.4 * $strength );
    $meto
    ? HSV( 240 - 20 * $is_largest, $s, $v )
    :HSV( 160 - 20 * $is_largest, $s, $v );
  },
);
 }; memoize('Style::Group');
}


 
{
    sub Style::Group2{ scalar(@_)==3 or confess "Needed exactly 3 arguments!";
my ($meto, $category_name, $is_largest)=@_;
; return (-width => do { 0 },
-fill => do {
    if ($meto) {
      HSV( 240 - 20 * $is_largest, 40, 60 );
    }
    else {
      my $h;
      if ( $category_name eq 'ascending' ) {
        HSV( 180, 60, 80 );
      }
      elsif ( $category_name eq 'descending' ) {
        HSV( 180, 60, 80 );
      }
      elsif ( $category_name eq 'sameness' ) {
        HSV( 240, 40, 60 );
      }
      else {
        HSV( 120, 40, 60 + 20 * $is_largest );
      }
    }
  },
-stipple => do {
    if ($is_largest) {
      undef;
    }
    elsif ($category_name) {
      undef;
    }
    else {
      'gray75';
    }
  },
);
 }; memoize('Style::Group2');
}


 
{
    sub Style::GroupBorder{ scalar(@_)==1 or confess "Needed exactly 1 arguments!";
my ($hilit)=@_;
; return (-width => do { 2 + 2 * $hilit },
-dash => do {
    if ( $hilit == 1 ) {
      '---';
    }
    else {
      undef;
    }
  },
-outline => do {
    if ( $hilit == 1 ) {
      '#000000';
    }
    elsif ( $hilit == 2 ) {
      '#0000FF';
    }
    else { HSV( 240, 70, 70 ) }
  },
);
 }; memoize('Style::GroupBorder');
}


 
{
    sub Style::ElementAttention{ scalar(@_)==1 or confess "Needed exactly 1 arguments!";
my ($attention)=@_;
; return (-fill => do {
    my ( $s, $v ) = ( 40, 400 * $attention );
    $v = 0  if $v < 0;
    $v = 99 if $v > 99;
    HSV( 300, $s, $v );
  },
-font => do {
    '-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4';
  },
);
 }; memoize('Style::ElementAttention');
}


 
{
    sub Style::GroupAttention{ scalar(@_)==1 or confess "Needed exactly 1 arguments!";
my ($attention)=@_;
; return (-fill => do {
    my ( $s, $v ) = ( 40, 400 * $attention );
    $v = 0  if $v < 0;
    $v = 99 if $v > 99;
    HSV( 160, $s, $v );
  },
);
 }; memoize('Style::GroupAttention');
}


 
{
    sub Style::GroupBorderAttention{ scalar(@_)==0 or confess "Expected no arguments!";
; return (-outline => do { HSV( 180, 40, 5 ) },
);
 }; memoize('Style::GroupBorderAttention');
}


 
{
    sub Style::RelationAttention{ scalar(@_)==1 or confess "Needed exactly 1 arguments!";
my ($attention)=@_;
; return (-width => do { 4 },
-fill => do {
    my ( $s, $v ) = ( 40, 400 * $attention );
    $v = 0  if $v < 0;
    $v = 99 if $v > 99;
    HSV( 190, $s, $v );
  },
-smooth => do { 1 },
-arrow => do { 'last' },
);
 }; memoize('Style::RelationAttention');
}


 
{
    sub Style::NetActivation{ scalar(@_)==1 or confess "Needed exactly 1 arguments!";
my ($raw_significance)=@_;
; return (-fill => do { HSV( 240, 30, 90 - 0.88 * $raw_significance ) },
);
 }; memoize('Style::NetActivation');
}


 
{
    sub Style::ThoughtBox{ scalar(@_)==2 or confess "Needed exactly 2 arguments!";
my ($hit_intensity, $is_current)=@_;
; return (-width => do {
    $is_current ? 3 :1;
  },
-fill => do {
    $hit_intensity = 2000 if $hit_intensity > 2000;
    my ( $s, $v ) = ( 40, 90 - 0.02 * $hit_intensity );

    # print "$hit_intensity => $v\n";
    $is_current ? HSV( 120, $s, $v ) :HSV( 100, $s, $v );
  },
);
 }; memoize('Style::ThoughtBox');
}


 
{
    sub Style::ThoughtComponent{ scalar(@_)==2 or confess "Needed exactly 2 arguments!";
my ($presence_level, $component_importance)=@_;
; return (-fill => do {

    # my ( $s, $v ) = ( 90, 80 - 0.5 * $component_importance );
    # my ( $s, $v ) = ( 90, 80 );
    # print "$component_importance => $v\n";
    HSV( 250, 90, 80 );
  },
-font => do { '-adobe-helvetica-bold-r-normal--10-140-100-100-p-105-iso8859-4' },
);
 }; memoize('Style::ThoughtComponent');
}


 
{
    sub Style::ThoughtHead{ scalar(@_)==0 or confess "Expected no arguments!";
; return (-font => do { '-adobe-helvetica-bold-r-normal--14-140-100-100-p-105-iso8859-4' },
);
 }; memoize('Style::ThoughtHead');
}


1;
