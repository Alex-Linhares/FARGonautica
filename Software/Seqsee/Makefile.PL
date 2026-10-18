use ExtUtils::MakeMaker;

sub MY::postamble{
  return <<"MAKE_FRAG";
smoke:
\t\tmake
\t\tperl util/run-smoke.pl . smoke.html
\t\tcp smoke.html /u/amahabal/.hyplan/smoke.html

test_cover:
\t\tmake
\t\tcover -delete
\t\tHARNESS_PERL_SWITCHES=-MDevel::Cover make test
\t\tcover

docs:
\t\tcd NaturalDocs; ./NaturalDocs -i /u/amahabal/SeqseeTree/summer05/lib -i /u/amahabal/SeqseeTree/summer05/Apps -i /u/amahabal/SeqseeTree/summer05/t -i /u/amahabal/SeqseeTree/summer05/util -i /u/amahabal/SeqseeTree/summer05/pod -o HTML /u/amahabal/.hyplan/ND/ -p Proj 

pdfdocs:
\t\tcd NaturalDocs; \\rm -rf pdfout/* ; \\rm -rf pdfproj/* ; ./NaturalDocs -i /u/amahabal/SeqseeTree/summer05/lib -i /u/amahabal/SeqseeTree/summer05/Apps -i /u/amahabal/SeqseeTree/summer05/t -i /u/amahabal/SeqseeTree/summer05/util -o PDF pdfout -p pdfproj ; cd pdfout ; lmake --indices=Functions --indices=Classes merged ; lmake merged

MAKE_FRAG
}

WriteMakefile(
    NAME              => 'Seqsee',
    VERSION_FROM      => 'lib/Seqsee.pm', # finds $VERSION
    PREREQ_PM         => {
        Class::Multimethods => 0,
        Class::Std => 0,
        Config::Std => 0,
        Exception::Class => 0,
        List::Util => 0,
        Log::Log4perl => 0,
        Perl6::Form => 0,
        Scalar::Util => 0,
        Smart::Comments => 0,
        Sub::Installer => 0,
        Test::Builder => 0,
        Test::Deep => 0,
        Test::Exception => 0,
        Test::Stochastic => 0,
        UNIVERSAL::require => 0,
            }, # e.g., Module::Name => 1.1
    test => { TESTS             => 't/*.t t/*/*.t t/*/*/*.t'},	      
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (
       AUTHOR         => 'Abhijit Mahabal <amahabal@cs.indiana.edu>',
      ) : ()),

);
