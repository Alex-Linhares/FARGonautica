use 5.10.0;
use CPAN;

my @reqs = qw{
File::Slurp
Smart::Comments
Parse::RecDescent
Class::Std
Perl::Tidy
Tk
Tk::ComboEntry
Tk::StatusBar
Config::Std
Sort::Key
# Needs FORCE: Tk::Carp
UNIVERSAL::require
Sub::Installer
Exception::Class
Class::Multimethods
Carp::Source
Text::Diff::Parser
};

my @forced_reqs = qw(
Tk::Carp
);

for my $mod (@reqs) {
    my $obj = CPAN::Shell->expand('Module',$mod) // next;
    $obj->install;
}

for my $mod (@forced_reqs) {
    my $obj = CPAN::Shell->expand('Module',$mod);
    $obj->force('install');
}
