use CPAN;
my @reqs = qw{
YAML
version
Config::Std
Smart::Comments
};

for my $mod (@reqs) {
    my $obj = CPAN::Shell->expand('Module',$mod);
    $obj->install;
}

