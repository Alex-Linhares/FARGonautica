package Win32::Excel::Util;
use base qw(Exporter);
our @EXPORT
    = qw{integer_to_column cell_to_range_string enter_value_in_cell enter_formula_in_cell add_comment_to_cell};

sub integer_to_column {
    my ($int) = @_;
    $int ||= 26;
    return ( 'A' .. 'Z' )[ $int - 1 ] if $int <= 26;
    return integer_to_column( int( ( $int - 1 ) / 26 ) ) . integer_to_column( $int % 26 );
}

sub cell_to_range_string {
    my ( $col, $row ) = @_;
    return integer_to_column($col) . $row;
}

sub enter_value_in_cell {
    my ( $sheet, $col, $row, $value ) = @_;
    my $range = $sheet->Range( cell_to_range_string( $col, $row ) );
    $range->{Value} = $value;
}

sub enter_formula_in_cell {
    my ( $sheet, $col, $row, $value ) = @_;
    my $range = $sheet->Range( cell_to_range_string( $col, $row ) );
    $range->{Formula} = $value;
}

sub add_comment_to_cell {
    my ( $sheet, $col, $row, $comment ) = @_;
    my $range = $sheet->Range( cell_to_range_string( $col, $row ) );
    $range->AddComment($comment);
}

1;
