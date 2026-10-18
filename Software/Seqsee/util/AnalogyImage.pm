use 5.010;
use MooseX::Declare;
use MooseX::AttributeHelpers;
use Config::Std;

use version; our $VERSION = qv('0.01');

class AnalogyArrow {
    has 'left_attribute' => (
        is       => 'ro',
        isa      => 'Any',
        required => 1,
    );
    has 'right_attribute' => (
        is       => 'ro',
        isa      => 'Any',
        required => 1,
    );
    has 'label' => (
        is       => 'ro',
        isa      => 'Any',
        required => 1,
    );

};

class AnalogyImage {
    use Config::Std;
    has 'is_concrete' => (
        is       => 'rw',
        isa      => 'Bool',
        required => 0,
    );

    has 'left_values' => (
        is       => 'rw',
        isa      => 'HashRef',
        required => 0,
    );

    has 'right_values' => (
        is       => 'rw',
        isa      => 'HashRef',
        required => 0,
    );

    has 'left_object' =>
      ( is   => 'rw',
        isa  => 'Any',
        required => 0,
        );

    has 'right_object' =>
      ( is   => 'rw',
        isa  => 'Any',
        required => 0,
        );

    has 'attributes' => (
        metaclass => 'Collection::Array',
        is        => 'rw',
        isa       => 'ArrayRef[Str]',
        default   => sub { [] },
        provides  => { push => 'push_attribute', }
    );

    has 'left_category' => (
        is       => 'rw',
        isa      => 'Str',
        required => 0,
    );

    has 'right_category' => (
        is       => 'rw',
        isa      => 'Str',
        required => 0,
    );

    has 'arrows' => (
        metaclass => 'Collection::Array',
        is        => 'rw',
        isa       => 'ArrayRef[AnalogyArrow]',
        default   => sub { [] },
        provides  => { push => 'push_arrow' }
    );

    method load_from_file($filename) {
          read_config $filename => my %config;
          say keys %config;
          $self->left_category( $config{''}{left_category} );
          $self->right_category( $config{''}{right_category} );
          $self->left_object($config{''}{left_object});
          $self->right_object($config{''}{right_object});
        $self->is_concrete( $config{''}{is_concrete} // ($config{left_values} ? 1 : 0) );
        $self->left_values( $config{left_values} )
            if $config{left_values};
        $self->right_values( $config{right_values} )
            if $config{right_values};
        $config{''}{attributes} = [$config{''}{attributes}]
            unless ref $config{''}{attributes};
        for ( @{ $config{''}{attributes} } ) {
            $self->push_attribute($_);
        }

        my $arrow_count = $config{''}{arrow_count};
          for ( 1 .. $arrow_count ) {
            my $arrow = AnalogyArrow->new( $config{ 'arrow_' . $_ } );
            $self->push_arrow($arrow);
        }
    };

    method find_index_of_attribute( Str $attribute_name) {
        my $i = 0;
          for ( @{ $self->attributes() } ) {
            return $i if $attribute_name eq $_;
            $i++;
        }
        die "Unknown attribute '$attribute_name'";
    };

    method calculate_arrow_positions( AnalogyArrow $arrow) {
        return (
            $self->find_index_of_attribute( $arrow->left_attribute() ),
            $self->find_index_of_attribute( $arrow->right_attribute() )
        );
    };

    method draw( $canvas, Num $height) {
        $canvas->delete('all');
        my $attribute_count   = scalar( @{ $self->{attributes} } );
        my $y_margin        = 30;
        my $y_bottom_margin = 15;
        my $y_per_attribute = ( $height - $y_margin - $y_bottom_margin ) / $attribute_count;
        my @rectangle_options = $self->is_concrete ?
            (-fill => '#DDDDDD' ) : (-fill => '#FCFCFC', -width => 2, -dash => '-');
        $canvas->createRectangle(30, $y_bottom_margin, 125, $height - $y_bottom_margin,
                                 @rectangle_options, -outline => '#BBBBBB');
        $canvas->createRectangle(375, $y_bottom_margin, 470, $height - $y_bottom_margin,
                                 @rectangle_options, -outline => '#BBBBBB');
        $canvas->createText(78, $y_margin - 2, -text => $self->left_category,
                                -anchor => 's', -fill => 'blue', -font => 'Lucida 12');
        $canvas->createText(422, $y_margin - 2, -text => $self->right_category,
                                -anchor => 's', -fill => 'blue', -font => 'Lucida 12');
        $self->draw_labels( $canvas, $y_margin, $y_per_attribute );
        $self->draw_arrows( $canvas, $y_margin, $y_per_attribute );
        
        if ($self->is_concrete) {
            $canvas->createText(78, $height - $y_bottom_margin + 4,
                                    -text => $self->left_object, -fill => 'blue',
                                    -anchor => 'n', -font => 'Lucida 8');
            $canvas->createText(422, $height - $y_bottom_margin + 4,
                                -text => $self->right_object, -fill => 'blue',
                                -anchor => 'n', -font => 'Lucida 8');
        }

    };

    method draw_labels( $canvas, Num $y_margin, Num $y_per_attribute) {
        my $index = 0;
          my ( $is_concrete, $left_values, $right_values );
        if ( $self->is_concrete ) {
            $is_concrete  = 1;
            $left_values  = $self->left_values;
            $right_values = $self->right_values;
        }
        else {
            $is_concrete = 0;
        }

        for my $attribute_name ( @{ $self->attributes } ){
            my $y = $y_margin + ( $index + 0.5 ) * $y_per_attribute;

              my ( $left_text, $right_text );
              if ($is_concrete) {
                $left_text =
                  "$attribute_name = " . $left_values->{$attribute_name};
                $right_text =
                  "$attribute_name = " . $right_values->{$attribute_name};
            }
            else {
                $left_text = $right_text = $attribute_name;
            }
            $canvas->createText(
                78, $y,
                -text   => $left_text,
                -anchor => 'c',
                -font   => 'Lucida 10',
            );
              $canvas->createText(
                  422, $y,
                -text   => $right_text,
                -anchor => 'c',
                -font   => 'Lucida 10',
              );

              $index++;
          }
      }

      method draw_arrows( $canvas, Num $y_margin, Num $y_per_attribute) {
        for my $arrow ( @{ $self->arrows } ){
            my ( $y1, $y2 ) =
              map { $y_margin + ( $_ + 0.5 ) * $y_per_attribute }
              $self->calculate_arrow_positions($arrow);
              $canvas->createLine( 140, $y1, 360, $y2, -arrow => 'last' );
              my ( $label_x, $label_y, $label_anchor );
              if ( $y1 == $y2 ) {
                $label_x      = 250;
                $label_anchor = 's';
                $label_y      = $y1 - 2;
            }
            elsif ( $y1 < $y2 ) {    # sloping down
                $label_x      = 250 + 2;
                $label_y      = -2 + ( $y1 + $y2 ) / 2;
                $label_anchor = 'sw';
            }
            else {
                $label_x      = 250 - 2;
                $label_y      = -2 + ( $y1 + $y2 ) / 2;
                $label_anchor = 'se';
            }
            $canvas->createText(
                $label_x, $label_y,
                -text   => $arrow->label,
                -anchor => $label_anchor,
                -font   => 'Lucida 8',
            );
          }
      };
};

1;
