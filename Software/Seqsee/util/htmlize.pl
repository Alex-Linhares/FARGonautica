use CGI qw{:standard};
use Smart::Comments;
open OUT, ">/u/amahabal/.hyplan/src/index.html";

print OUT start_html();
print OUT "<ul>\n";


sub process{
    my $fname = shift;
    my $suffix = shift;
    $fname =~ s#\.$suffix$##;
    my $usepod;
    if ($suffix eq 't') {
        system "cp $fname.t $fname.t.tdy";
        $usepod = '';
    } else {
        system "perltidy -l=200 $fname.$suffix";
        $usepod = "-pod ";
    }
    
    my $cmd = "perltidy -html -nnn -l=200 $usepod $fname.$suffix.tdy -o /u/amahabal/.hyplan/src/$fname.html";
    # print $cmd, "\n";
    system $cmd;
    system "rm $fname.$suffix.tdy";
    print OUT "\t<li><a href=\"$fname.html\"> $fname.$suffix </a>\n";  
}

for my $fname (<lib/*.pm lib/*/*.pm lib/*/*/*.pm>) { ### libs  @@@>              done[%]
    process($fname, "pm");
}

for my $fname (<t/*.t t/*/*.t t/*/*/*.t>) { ### tests @@@>               done[%]
    process($fname, "t");
}


print OUT "</ul>";
print OUT end_html();
