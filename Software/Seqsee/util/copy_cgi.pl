my $path = "/l/cgi/amahabal/site_perl/lib/site_perl/5.8.5";
my $cgi_path = '/l/cgi/amahabal/cgi-pub';

for (<lib/*.pm lib/*/*.pm lib/*/*/*.pm>) {
  s#^lib/##;
  system "cp lib/$_ $path/$_\n";
}

system "cp Apps/oddman.* $cgi_path/";

