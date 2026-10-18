use strict; use warnings;
no warnings 'redefine','once';
use Tk;
use S;
use Test::Seqsee;
use Themes::Std2;
use SGUI::Workspace;
use SGUI::Slipnet;
use SGUI::Coderack;
srand(3);
INITIALIZE_for_testing();
SWorkspace->init({ %{$Global::TestingOptionsRef}, seq=>[1,1,2,1,2,3]});
Global->SetFutureTerms(1,2,3,4);
SCoderack->init($Global::TestingOptionsRef);
$Global::MainStream->init($Global::TestingOptionsRef);
SLTM->init(); $SWorkspace::ReadHead = 0; Global->clear();
eval { Seqsee::Interaction_step_n({n=>400, max_steps=>400, update_after=>400}) ; 1 } or print STDERR "run: $@\n";
my $mw = MainWindow->new;
my $c = $mw->Canvas(-width=>780,-height=>450)->pack;
SGUI::Workspace->Setup($c, 0, 0, 780, 450);
SGUI::Workspace->DrawIt();
my %n;
for my $id ($c->find('all')) {
  my $t = $c->type($id); $n{$t}++;
}
print "steps=$Global::Steps_Finished elements=$SWorkspace::ElementCount groups=".scalar(SWorkspace::GetGroups())."\n";
print join(", ", map {"$_=$n{$_}"} sort keys %n), "\n";
for my $id (($c->find('all'))[0..4]) {
  my $t=$c->type($id); my @o = $t eq 'text' ? (text=>$c->itemcget($id,'-text')) : (fill=>$c->itemcget($id,'-fill')//'');
  printf "%s [%s] %s\n", $t, join(",", map {sprintf "%.1f",$_} $c->coords($id)), "@o";
}
$mw->update; $c->postscript(-file=>'/tmp/guiproto/ws.eps');
