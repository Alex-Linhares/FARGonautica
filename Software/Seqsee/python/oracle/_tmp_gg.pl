use strict; use warnings; no warnings 'once';
BEGIN { open( my $saved, '>&', \*STDOUT ); open( STDOUT, '>', '/dev/null' ); require Test::Seqsee; open( STDOUT, '>&', $saved ); }
use GuiRecipes;
build_recipe('groups_many');
my @a = map { $_->get_bounds_string } SWorkspace::GetGroups();
my @b = map { $_->get_bounds_string } SWorkspace::GetGroups();
print join(",", @a[0..8]), "\n", join(",", @b[0..8]), "\n";
print "same\n" if "@a" eq "@b";
