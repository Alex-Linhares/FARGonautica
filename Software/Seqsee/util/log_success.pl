#!/usr/bin/perl -w
use IO::Prompt;
use Smart::Comments;

my $line =  ` grep 'Input' log/latest`;
my $lastcl =  ` grep '^=== ' log/latest | tail -n 1`;

$line =~ s#^\D*##;
chomp($line);

$lastcl =~ m/^===\s*(\d+)/;
$lastclnumber = $1;

my $time = localtime();
### $time

if (prompt "Should I log $line as a success story(in $lastclnumber steps)?" , "-yn") {
    open my $SUCC_LOG, ">>success_log";
    print {$SUCC_LOG} "$line\n\tSteps: $lastclnumber\n\tDate: $time\n";
}

