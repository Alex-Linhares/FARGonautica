use strict;
use Config::Std;
use Carp;
use List::Util qw(sum);
use Smart::Comments;
my $GroupSize = 10;
my $outfile   = "reg_rep.tex";

my $output_core;
my $file_number = 1;
{
    my @files = glob("Reg/*.reg.last_res");
    while (@files) {
        my @top_few = splice( @files, 0, $GroupSize );
        process_set_of_last_res( \@top_few, \$output_core, $file_number++ );
    }
}

{
    my @files = glob("Reg/*.reg.log_res");
    for (@files) {
        process_single_seq( $_, \$output_core, $file_number++ );
    }

}

sub process_set_of_last_res {
    my ( $files, $output_core, $file_number ) = @_;
    my @list;

    my $out_str_table
        = '\begin{tabular}{l|p{2in}|p{2in}}\\ \textbf{name}&\textbf{seq}&\textbf{extension} \\\\';
    for my $file (@$files) {
        read_config $file => my %opts;
        %opts = %{ $opts{''} };
        my @result = (
            $opts{GotIt}           ||= 0,
            $opts{BlemishedGotIt}  ||= 0,
            $opts{Extended}        ||= 0,
            $opts{NotEvenExtended} ||= 0
        );
        push @result, 10 - sum(@result);
        $file =~ m#(\w+).reg.last_res$# or confess "Huh?";
        my $seq_name = $1;
        $seq_name =~ s#_# #g;

        my $seq_file = $file;
        $seq_file =~ s#.last_res$##;
        read_config $seq_file => my %opts2;
        %opts2 = %{ $opts2{''} };
        my ( $seq, $ext ) = split( /\|/, $opts2{seq} );
        $out_str_table .= "$seq_name & $seq & $ext \\\\\n";

        unshift @result, $seq;
        push @list, \@result;
    }
    $out_str_table .= '\end{tabular}';
    open OUT, ">", "tmpfile$file_number.csv";
    for my $pos ( 0 .. 5 ) {
        print OUT join( ", ", map { $_->[$pos] } @list ), "\n";
    }
    close OUT;

    my $out_str_graph = '\begin{pspicture}(-4,-1)(5,10)\readpsbardata{\data}{'
        . "tmpfile$file_number.csv"
        . '}\psbarchart[orientation=horizontal,chartstyle=stack,barstyle={blue,yellow,green,white,red}]{\data}\end{pspicture}';
    $$output_core .= "$out_str_table\n$out_str_graph\n\\newpage\n";
}

sub process_single_seq {
    my ( $file, $output_core, $file_number ) = @_;
    my @list;

    $file =~ m#(\w+).reg.log_res$# or confess "Huh?";
    my $seq_name = $1;
    $seq_name =~ s#_# #g;

    my $seq_file = $file;
    $seq_file =~ s#.log_res$##;
    read_config $seq_file => my %opts;
    %opts = %{ $opts{''} };
    my ( $seq, $ext ) = split( /\|/, $opts{seq} );
    @opts{'seq', 'ext'} = ($seq, $ext);

    my $out_str_table
        = '\begin{tabular}{l|p{2in}}\\ \textbf{option}&\textbf{value} \\\\';
    while (my($k, $v) = each %opts) {
        $k =~ s#_# #g;
        $out_str_table .= "$k & $v \\\\\n";
    }
    $out_str_table .= '\end{tabular}';

    read_config $file => my %opts2;
    delete $opts2{''};
    my @times = sort { $b <=> $a }keys %opts2;
    @times = splice(@times, 0, $GroupSize);
    for my $time (@times) {
        my %opts = %{$opts2{$time}};
        my @result = (
            $opts{GotIt}           ||= 0,
            $opts{BlemishedGotIt}  ||= 0,
            $opts{Extended}        ||= 0,
            $opts{NotEvenExtended} ||= 0
        );
        push @result, 10 - sum(@result);
        unshift @result, timestring($time);
        $seq_name =~ s#_# #g;
        push @list, \@result;
    }

    ### @list
    open OUT, ">", "tmpfile$file_number.csv";
    for my $pos ( 0 .. 5 ) {
        print OUT join( ", ", map { $_->[$pos] } @list ), "\n";
    }
    close OUT;

    my $out_str_graph = '\begin{pspicture}(-4,-1)(5,10)\readpsbardata{\data}{'
        . "tmpfile$file_number.csv"
        . '}\psbarchart[orientation=horizontal,chartstyle=stack,barstyle={blue,yellow,green,white,red}]{\data}\end{pspicture}';
    $$output_core .= "$out_str_table\n\n$out_str_graph\n\\newpage\n";
}

sub timestring{
    my ( $time ) = @_;
    my $string = localtime($time);
    return $string;
}


open OUT, ">", $outfile;
print OUT <<'PREAMBLE';
\documentclass{article}
\usepackage{pstricks}
\usepackage{pst-bar}
\begin{document}

\newpsbarstyle{yellow}{fillcolor=yellow,fillstyle=solid}
\newpsbarstyle{green}{fillcolor=green,fillstyle=solid}
\newpsbarstyle{white}{fillcolor=white,fillstyle=solid}
\psset{unit=0.3in}

PREAMBLE

print OUT $output_core;

print OUT '\end{document}';
close OUT;
system "lmake reg_rep";
