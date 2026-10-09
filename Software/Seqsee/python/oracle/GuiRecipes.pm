package GuiRecipes;
# Named, deterministic workspace states for the GUI drawing oracles (loop0002). The Python
# twin is python/tests/gui_recipes.py: each recipe there builds the same state under the same
# name. build($name) resets the workspace and returns a hashref of the named objects.
# object_tags() maps each stringified workspace object to the tag the Python draw code uses
# for it ('obj<oid>', with oids as in seqsee/gui/snapshot.py: elements first, left to right).
use strict;
use warnings;
no warnings 'once';
use Exporter 'import';
our @EXPORT = qw(build_recipe object_tags);

sub _init {
  my @seq = @_;
  %Global::Feature               = ();
  $Global::Steps_Finished        = 0;
  $Global::CurrentRunnableString = '';
  Global::ClearHilit();
  SLTM->Clear();
  SWorkspace->init( { seq => [@seq] } );
  SWorkspace::__ClearBarLines();
  SCoderack::clear();
  $Global::MainStream->clear();
  %{ $Global::MainStream->{hit_intensity} }         = ();
  %{ $Global::MainStream->{thought_hit_intensity} } = ();
  return [ SWorkspace::GetElements() ];
}

my %RECIPES = (
  empty => sub { _init(); return {}; },
  one_element     => sub { return { e => _init(7) } },
  six_elements    => sub { return { e => _init( 1, 1, 2, 1, 2, 3 ) } },
  twenty_elements => sub { return { e => _init( 1 .. 20 ) } },

  # Six elements and bar lines, added out of order (__AddBarLines sorts them); 6 is after the
  # last element.
  bar_lines => sub {
    my $e = _init( 1, 1, 2, 1, 2, 3 );
    SWorkspace::__AddBarLines( 3, 0 );
    SWorkspace::__AddBarLines(6);
    return { e => $e };
  },

  # Highlighted elements (Hilit 1, 2 and 3), the debug feature (index labels), steps and a
  # codelet as the last runnable (its family has no $NAME, so the label is empty).
  hilit_debug => sub {
    my $e = _init( 1, 1, 2, 1, 2, 3 );
    $Global::Feature{debug} = 1;
    Global::Hilit( 1, $e->[1] );
    Global::Hilit( 2, $e->[3] );
    Global::Hilit( 3, $e->[5] );
    $Global::Steps_Finished        = 42;
    $Global::CurrentRunnableString = 'Seqsee::SCF::FocusOn';
    return { e => $e };
  },

  # A last-runnable string whose package does define $NAME (an SThought class), and one bar line.
  runnable_thought => sub {
    my $e = _init( 4, 5 );
    $Global::CurrentRunnableString = 'SThought::Seqsee::Element';
    SWorkspace::__AddBarLines(1);
    return { e => $e };
  },

  # 1 2 3 2 2 2 4 5: an ascending group inside a larger group, a sameness group with an
  # active "each" metonym, relations inside/outside groups, a highlighted group and relation.
  groups_relations => sub {
    my $e    = _init( 1, 2, 3, 2, 2, 2, 4, 5 );
    my $asc  = _group( $S::ASCENDING, @$e[ 0, 1, 2 ] );
    my $same = _group( $S::SAMENESS, @$e[ 3, 4, 5 ] );
    $same->AnnotateWithMetonym( $S::SAMENESS, 'each' );
    $same->set_metonym_activeness(1);
    my $big   = _group( undef, $asc, $same );
    my $r_in  = _reln( $e->[0], $e->[1], 'succ' );
    my $r_out = _reln( $e->[6], $e->[7], 'succ' );
    my $r_hi  = _reln( $e->[1], $e->[2], 'succ' );
    SWorkspace::__AddBarLines( 0, 6 );
    Global::Hilit( 1, $asc );
    Global::Hilit( 2, $r_hi );
    $Global::Steps_Finished        = 42;
    $Global::CurrentRunnableString = 'Seqsee::SCF::FocusOn';
    return { e => $e, asc => $asc, same => $same, big => $big, r_in => $r_in,
      r_out => $r_out, r_hi => $r_hi };
  },

  # 30 elements 1 2 3 1 2 3 ...: a group per triple (equal spans: GetGroups' order is hash
  # order), relations inside the triples, bar lines.
  large => sub {
    my $e = _init( (1, 2, 3) x 10 );
    _group( $S::ASCENDING, @$e[ $_ .. $_ + 2 ] ) for grep { $_ % 3 == 0 } 0 .. 29;
    _reln( $e->[$_], $e->[ $_ + 1 ], 'succ' ) for grep { $_ % 3 == 0 } 0 .. 29;
    _reln( $e->[ $_ + 1 ], $e->[ $_ + 2 ], 'succ' ) for grep { $_ % 3 == 0 } 0 .. 29;
    SWorkspace::__AddBarLines( grep { $_ % 3 == 0 } 0 .. 29 );
    return { e => $e };
  },

  # 1 2 1 2 3 1 2 3 4: groups A, B, C; S1 = (A B); S2 = (S1 C) (distinct spans). Hilit 1 on
  # B, 3 on S2, 2 on the hidden relation S1->C.
  nested_groups => sub {
    my $e  = _init( 1, 2, 1, 2, 3, 1, 2, 3, 4 );
    my $a  = _group( $S::ASCENDING, @$e[ 0, 1 ] );
    my $b  = _group( $S::ASCENDING, @$e[ 2, 3, 4 ] );
    my $c  = _group( $S::ASCENDING, @$e[ 5, 6, 7, 8 ] );
    my $s1 = _group( undef, $a, $b );
    my $s2 = _group( undef, $s1, $c );
    _reln( $a, $b, 'succ' );
    _reln( $e->[0], $e->[1], 'succ' );
    my $s1c = _reln( $s1, $c, 'succ' );
    _reln( $b, $c, 'succ' );
    _reln( $a, $c, 'succ' );
    _reln( $e->[1], $e->[2], 'pred' );
    Global::Hilit( 1, $b );
    Global::Hilit( 3, $s2 );
    Global::Hilit( 2, $s1c );
    return { e => $e };
  },

  # 2 2 2 2 3 3 5 3 2 1: M (largest) with an active metonym, P with an inactive one,
  # element 6 with an active metonym and squinted (group_p), element 5 squinted, D descending.
  metonyms => sub {
    my $e = _init( 2, 2, 2, 2, 3, 3, 5, 3, 2, 1 );
    my $m = _group( $S::SAMENESS, @$e[ 0 .. 3 ] );
    $m->AnnotateWithMetonym( $S::SAMENESS, 'each' );
    $m->set_metonym_activeness(1);
    my $p = _group( $S::SAMENESS, @$e[ 4, 5 ] );
    $p->AnnotateWithMetonym( $S::SAMENESS, 'each' );
    _group( $S::DESCENDING, @$e[ 7, 8, 9 ] );
    $e->[6]->AnnotateWithMetonym( $S::SAMENESS, 'each' );
    $e->[6]->set_metonym_activeness(1);
    $e->[6]->set_group_p(1);
    $e->[5]->set_group_p(1);
    _reln( $m, $p, 'succ' );
    _reln( $e->[6], $e->[7], 'pred' );
    _reln( $e->[5], $e->[6], 'succ' );
    return { e => $e };
  },

  # 1 2 3 4 5 6 7, group G (1 2 3), overlapping relations, one to an element outside the
  # workspace. Bar lines 3, 7.
  overlapping_relations => sub {
    my $e = _init( 1, 2, 3, 4, 5, 6, 7 );
    my $g = _group( $S::ASCENDING, @$e[ 0, 1, 2 ] );
    my $outside = Seqsee::Element->create( 9, 9 );
    _reln( $e->[0], $e->[1], 'succ' );
    _reln( $e->[1], $e->[2], 'succ' );
    _reln( $e->[2], $e->[3], 'succ' );
    _reln( $e->[3], $e->[4], 'succ' );
    _reln( $e->[2], $e->[4], 'succ' );
    _reln( $e->[0], $e->[6], 'succ' );
    _reln( $e->[6], $e->[5], 'pred' );
    _reln( $g, $e->[5], 'succ' );
    _reln( $e->[4], $outside, 'succ' );
    SWorkspace::__AddBarLines( 3, 7 );
    return { e => $e };
  },

  # 4 4 4: a squinted element and one with an active metonym, but no groups.
  squint_no_groups => sub {
    my $e = _init( 4, 4, 4 );
    $e->[1]->set_group_p(1);
    $e->[2]->AnnotateWithMetonym( $S::SAMENESS, 'each' );
    $e->[2]->set_metonym_activeness(1);
    _reln( $e->[0], $e->[1], 'succ' );
    return { e => $e };
  },

  # Attention (SGUI::Workspace_Attention): codelets whose arguments are elements (and a
  # string), urgencies summing to 100: e0 0.8 (clamped), e1 0.2, e2 0.05, e4 0.1, e3 and e5 0.
  # Hilit 1 on e1, the debug feature, bar lines 0 and 3, a current runnable.
  attention_elements => sub {
    my $e = _init( 1, 1, 2, 1, 2, 3 );
    _codelet( 'AreRelated', 20, a    => $e->[0], b => $e->[1] );
    _codelet( 'Reader',     5,  core => $e->[2] );
    _codelet( 'Bigger',     60, core => $e->[0] );
    _codelet( 'Mid',        10, core => $e->[4] );
    _codelet( 'Str',        5,  s    => 'x' );
    $Global::Feature{debug} = 1;
    Global::Hilit( 1, $e->[1] );
    SWorkspace::__AddBarLines( 0, 3 );
    $Global::Steps_Finished        = 7;
    $Global::CurrentRunnableString = 'Seqsee::SCF::FocusOn';
    return { e => $e };
  },

  # 1 2 1 2 3 1 2 3 4 5: groups A, B, C, S1 = (A B), S2 = (S1 C) (distinct spans), element 9
  # squinted with an active metonym, no relations. A FocusOn codelet spreads attention by the
  # reader's distribution; the others point at B, S2 and element 9. Hilit 1 on B, 3 on S2.
  attention_groups => sub {
    my $e  = _init( 1, 2, 1, 2, 3, 1, 2, 3, 4, 5 );
    my $a  = _group( $S::ASCENDING, @$e[ 0, 1 ] );
    my $b  = _group( $S::ASCENDING, @$e[ 2, 3, 4 ] );
    my $c  = _group( $S::ASCENDING, @$e[ 5, 6, 7, 8 ] );
    my $s1 = _group( undef, $a, $b );
    my $s2 = _group( undef, $s1, $c );
    $e->[9]->AnnotateWithMetonym( $S::SAMENESS, 'each' );
    $e->[9]->set_metonym_activeness(1);
    $e->[9]->set_group_p(1);
    _codelet( 'FocusOn', 40 );
    _codelet( 'G',       30, core => $b );
    _codelet( 'H',       30, core => $s2, other => $e->[9] );
    Global::Hilit( 1, $b );
    Global::Hilit( 3, $s2 );
    return { e => $e };
  },

  # groups_relations plus codelets on the reader, two elements and a relation:
  # Workspace_Attention dies on the first relation (SReln::draw_attention is not SRelation's).
  attention_relations => sub {
    my $r = build_recipe('groups_relations');
    _codelet( 'FocusOn',    50 );
    _codelet( 'AreRelated', 25, a    => $r->{e}[0], b => $r->{e}[3] );
    _codelet( 'Rel',        25, core => $r->{r_out} );
    return $r;
  },

  # Slipnet (SGUI::Slipnet): elements 1 2 3 (nodes 1-3), then ascending, sameness, the succ
  # mapping (with its dependencies) and descending. Real activations set directly: plat1 0.9,
  # plat2 exactly 0.01 (not shown: the test is >), plat3 0.5, ascending 1, sameness left at the
  # initial 0.003 (not shown), succ 0.0101, descending 0.25; number spiked by 40 (raw 10, 0.0145).
  slipnet_small => sub {
    my $e    = _init( 1, 2, 3 );
    my $asc  = SLTM::GetMemoryIndex($S::ASCENDING);
    my $same = SLTM::GetMemoryIndex($S::SAMENESS);
    my $succ = SLTM::GetMemoryIndex( Mapping::Numeric->create( 'succ', $S::NUMBER ) );
    my $desc = SLTM::GetMemoryIndex($S::DESCENDING);
    _activate( 1, 0.9 );
    _activate( 2, 0.01 );
    _activate( 3, 0.5 );
    _activate( $asc,  1 );
    _activate( $succ, 0.0101 );
    _activate( $desc, 0.25 );
    SLTM::SpikeBy( 40, $S::NUMBER );
    return { e => $e };
  },

  # 40 nodes (elements 1..40); node i has activation (i % 7 + 1) / 8, except every fifth,
  # 0.005 (not shown). 32 are shown: the 31st lands in a fourth column (col 3), then DrawIt stops.
  slipnet_full => sub {
    my $e = _init( 1 .. 40 );
    _activate( $_, $_ % 5 ? ( $_ % 7 + 1 ) / 8 : 0.005 ) for 1 .. $SLTM::NodeCount;
    return { e => $e };
  },

  # Long concept texts (cut to MaxTextWidth = 30): 1..12, and the ascending groups (1..6) and
  # (7..12) inside a larger group. Node i has activation 0.02 + (i % 4) * 0.3.
  slipnet_long_text => sub {
    my $e   = _init( 1 .. 12 );
    my $g1 = _group( $S::ASCENDING, @$e[ 0 .. 5 ] );
    my $g2 = _group( $S::ASCENDING, @$e[ 6 .. 11 ] );
    _group( undef, $g1, $g2 );
    _activate( $_, 0.02 + ( $_ % 4 ) * 0.3 ) for 1 .. $SLTM::NodeCount;
    return { e => $e };
  },

  # Mapping::Dir nodes, which have no as_text: elements 4 5, then Dir Same (0.005, not shown)
  # and Dir Different (0.8, shown). Activations: plat4 0.6, plat5 0.3.
  slipnet_dir => sub {
    my $e    = _init( 4, 5 );
    my $same = SLTM::GetMemoryIndex($Mapping::Dir::Same);
    my $diff = SLTM::GetMemoryIndex($Mapping::Dir::Different);
    _activate( 1,     0.6 );
    _activate( 2,     0.3 );
    _activate( $same, 0.005 );
    _activate( $diff, 0.8 );
    return { e => $e };
  },

  # Coderack (SGUI::Coderack): five codelets of four families (urgencies sum 120) and a
  # history of runs: Reader 12, FocusOn 6 and AttemptExtension 2 (not on the rack).
  # AreRelated and Bigger are on the rack but not in the history (DrawIt adds them with 0).
  coderack_small => sub {
    my $e = _init( 1, 2, 3 );
    _codelet( 'Reader',     30, core => $e->[0] );
    _codelet( 'Reader',     10 );
    _codelet( 'AreRelated', 20, a => $e->[0], b => $e->[1] );
    _codelet( 'FocusOn',    15 );
    _codelet( 'Bigger',     45, core => $e->[2] );
    _history( Reader => 12, FocusOn => 6, AttemptExtension => 2 );
    return { e => $e };
  },

  # Codelets whose urgencies are all 0 (URGENCIES_SUM 0: the sums become '---'), no history
  # (no run so far: no red bars).
  coderack_zero_urgency => sub {
    _init();
    _codelet( 'Reader',  0 );
    _codelet( 'FocusOn', 0 );
    _codelet( 'Reader',  0 );
    return {};
  },

  # No codelet on the rack, only a history (one family with a count of 0).
  coderack_history_only => sub {
    _init();
    _history( Reader => 3, FocusOn => 1, AreRelated => 0 );
    return {};
  },

  # 55 families: 25 codelets F00..F24 (urgency i + 1; the rack's maximum), and a history of
  # F00..F04 (5 - i) and H00..H29 (i + 1). Three columns of 16 rows are drawn, then DrawIt stops.
  coderack_many => sub {
    _init();
    _codelet( sprintf( 'F%02d', $_ ), $_ + 1 ) for 0 .. 24;
    _history( map { ( sprintf( 'F%02d', $_ ), 5 - $_ ) } 0 .. 4 );
    _history( map { ( sprintf( 'H%02d', $_ ), $_ + 1 ) } 0 .. 29 );
    return {};
  },

  # Stream (SGUI::Stream): only a current thought, with its real fringe (get_fringe of an
  # element: literal platonics and absolute positions) and no hit intensity (undef).
  stream_current => sub {
    my $e = _init( 3, 5, 7 );
    my $t = SThought::Seqsee::Element->new( core => $e->[1] );
    $t->stored_fringe( $t->get_fringe() );
    _stream($t);
    return { e => $e, t => $t };
  },

  # A current thought and four older ones of the four thought classes, with hand-made fringes:
  # 4 components (3 drawn: a string, a number, an SInt; then a category), 1, none ([]), undef
  # (never thought), 2 (a float and a string). Thought hit intensities 100, 500, 2500 (clamped
  # to 2000), none (undef) and 0; component hit intensities for two components.
  stream_small => sub {
    my $e   = _init( 1, 2, 3, 4 );
    my $g   = _group( $S::ASCENDING, @$e[ 0, 1, 2 ] );
    my $r   = _reln( $e->[2], $e->[3], 'succ' );
    my $cur = SThought::Seqsee::Element->new( core => $e->[3] );
    $cur->stored_fringe(
      [ [ 'absolute_position_3', 80 ], [ 4, 100 ], [ SInt->new(5), 30 ], [ $S::ASCENDING, 50 ] ] );
    my $tg = SThought::Seqsee::Anchored->new( core => $g );
    $tg->stored_fringe( [ [ $S::ASCENDING, 100 ] ] );
    my $tr = SThought::SRelation->new( core => $r );
    $tr->stored_fringe( [] );
    my $tc = SThought::SCat->new( core => $S::ASCENDING );
    my $te = SThought::Seqsee::Element->new( core => $e->[0] );
    $te->stored_fringe( [ [ 0.5, 20 ], [ 'x', 10 ] ] );
    _stream( $cur, $tg, $tr, $tc, $te );
    _thought_hits( [ $cur, 100 ], [ $tg, 500 ], [ $tr, 2500 ], [ $te, 0 ] );
    _component_hits( absolute_position_3 => 80, 4 => 100 );
    return { e => $e, g => $g, r => $r, cur => $cur, tg => $tg, tr => $tr, tc => $tc, te => $te };
  },

  # No current thought; 12 older element thoughts and a '' (skipped; antiquating with no
  # current thought leaves one): rows 1, 2, then 0..2 per column; the 12th is in a fifth
  # column, right of the rectangle. Hit intensities 150 * i; i % 5 components each.
  stream_full => sub {
    my $e = _init( 1 .. 12 );
    my @t;
    for my $i ( 0 .. 11 ) {
      my $t = SThought::Seqsee::Element->new( core => $e->[$i] );
      $t->stored_fringe( [ map { [ "c${i}_$_", 10 * $_ ] } 1 .. ( $i % 5 ) ] );
      push @t, $t;
    }
    _stream( '', @t[ 0 .. 4 ], '', @t[ 5 .. 11 ] );
    _thought_hits( map { [ $t[$_], 150 * $_ ] } 0 .. 11 );
    return { e => $e, t => \@t };
  },

  # Real fringes (get_fringe) of a group, a relation, a category and an element: most
  # components are objects (platonics, categories, mappings, elements), which Perl draws as
  # stringified refs.
  stream_real => sub {
    my $e   = _init( 1, 2, 3, 2, 2, 2 );
    my $asc = _group( $S::ASCENDING, @$e[ 0, 1, 2 ] );
    my $r   = _reln( $e->[0], $e->[1], 'succ' );
    my $cur = SThought::Seqsee::Anchored->new( core => $asc );
    my $tr  = SThought::SRelation->new( core => $r );
    my $tc  = SThought::SCat->new( core => $S::SAMENESS );
    my $te  = SThought::Seqsee::Element->new( core => $e->[4] );
    $_->stored_fringe( $_->get_fringe() ) for $cur, $tr, $tc, $te;
    _stream( $cur, $tr, $tc, $te );
    _thought_hits( [ $tr, 40 ], [ $te, 1999 ] );
    return { e => $e, asc => $asc, r => $r };
  },

  # 1 2 3 4 5 6 7 8, groups A (1 2 3) and B (4 5 6): relations of every kind the Relations
  # pane shows, with strengths set after insert (0, 5.555, 100, 123456.789, -3.14159, 0.005,
  # 42): succ, pred and same on NUMBER (complexity 0.1 / 0.1 / 0), succ on EVEN (0.4), a
  # SRelation::Structural between the groups, a group to an element, and one to an element
  # outside the workspace.
  relations_pane => sub {
    my $e = _init( 1, 2, 3, 4, 5, 6, 7, 8 );
    my $a = _group( $S::ASCENDING, @$e[ 0, 1, 2 ] );
    my $b = _group( $S::ASCENDING, @$e[ 3, 4, 5 ] );
    my $outside = Seqsee::Element->create( 9, 9 );
    my @r = (
      _reln( $e->[0], $e->[1], 'succ' ),
      _reln( $e->[2], $e->[1], 'pred' ),
      _reln( $e->[6], $e->[7], 'same' ),
      _reln( $e->[6], $e->[0], 'succ', $S::EVEN ),
      _struct_reln( $a, $b ),
      _reln( $a, $e->[6], 'succ' ),
      _reln( $e->[7], $outside, 'succ' ),
    );
    my @strengths = ( 0, 5.555, 100, 123456.789, -3.14159, 0.005, 42 );
    $r[$_]->set_strength( $strengths[$_] ) for 0 .. $#r;
    return { e => $e, a => $a, b => $b, r => [@r] };
  },

  # 27 elements, 26 relations e_i -> e_(i+1) (more than RowCount = 22 rows), strength 3.7 i.
  relations_many => sub {
    my $e = _init( map { 1 + $_ % 9 } 0 .. 26 );
    my @r = map { _reln( $e->[$_], $e->[ $_ + 1 ], 'succ' ) } 0 .. 25;
    $r[$_]->set_strength( 3.7 * $_ ) for 0 .. 25;
    return { e => $e, r => [@r] };
  },

  # 1 2 3 4 5 6 7 7 7 8: groups for the Groups list. A = ascending (1 2 3), locked, strength
  # 55.555; B = ascending (4 5 6) with a second category (EVEN, empty bindings), strength 0;
  # C = sameness (7 7 7), locked, strength 100; D = (A B), no category, strength 99.999.
  groups_list => sub {
    my $e = _init( 1, 2, 3, 4, 5, 6, 7, 7, 7, 8 );
    my $a = _group( $S::ASCENDING, @$e[ 0, 1, 2 ] );
    my $b = _group( $S::ASCENDING, @$e[ 3, 4, 5 ] );
    $b->add_category( $S::EVEN, SBindings->create( {}, {}, $b ) );
    my $c = _group( $S::SAMENESS, @$e[ 6, 7, 8 ] );
    my $d = _group( undef, $a, $b );
    $a->set_is_locked_against_deletion(1);
    $c->set_is_locked_against_deletion(1);
    $a->set_strength(55.555);
    $b->set_strength(0);
    $c->set_strength(100);
    $d->set_strength(99.999);
    return { e => $e, a => $a, b => $b, c => $c, d => $d };
  },

  # 60 elements, 30 disjoint pair groups (no category; equal spans, so hash order), strength
  # 3.3 i, every 7th locked: more groups than one page holds.
  groups_many => sub {
    my $e = _init( map { 1 + $_ % 5 } 0 .. 59 );
    my @g = map { _group( undef, @$e[ 2 * $_, 2 * $_ + 1 ] ) } 0 .. 29;
    $g[$_]->set_strength( 3.3 * $_ ) for 0 .. 29;
    $g[$_]->set_is_locked_against_deletion(1) for grep { $_ % 7 == 0 } 0 .. 29;
    return { e => $e, g => [@g] };
  },

  # Categories list (SGUI::List::Categories): 28 elements (category number), 14 disjoint pair
  # groups G0..G13, Gi with the i-th of ascending, descending, mountain, sameness, prime, odd,
  # even, interlaced 2..8 (added with empty bindings), G0 also descending, and BIG = (G0 G1)
  # ascending: 15 categories, more than a 780x450 page holds (10).
  categories_many => sub {
    my $e    = _init( map { 1 + $_ % 9 } 0 .. 27 );
    my @cats = ( $S::ASCENDING, $S::DESCENDING, $S::MOUNTAIN, $S::SAMENESS, $S::PRIME,
      $S::ODD, $S::EVEN, map { SCategory::Interlaced->Create($_) } 2 .. 8 );
    my @g = map { _group( undef, @$e[ 2 * $_, 2 * $_ + 1 ] ) } 0 .. 13;
    _add_category( $g[$_], $cats[$_] ) for 0 .. 13;
    _add_category( $g[0], $S::DESCENDING );
    my $big = _group( undef, @g[ 0, 1 ] );
    _add_category( $big, $S::ASCENDING );
    return { e => $e, g => [@g], big => $big };
  },

  # Stream list (SGUI::List::Stream): a current thought with four components (activations
  # 1, 3, 2, 3: sorted 3, 3, 2, 1, stable) and 14 older element thoughts Ti (i = 1..14): no
  # stored fringe when i % 4 == 0, an empty one when i % 4 == 1, else [["f<i>", i], ["g<i>",
  # 0.5]]. Hit intensities: none when i % 3 == 0, 2.5 for T7, else 10 * (i % 5) (ties).
  stream_list_many => sub {
    my $e   = _init( 1 .. 15 );
    my $cur = SThought::Seqsee::Element->new( core => $e->[0] );
    $cur->stored_fringe( [ [ 'a', 1 ], [ 'b', 3 ], [ 'c', 2 ], [ SInt->new(5), 3 ] ] );
    my @t;
    for my $i ( 1 .. 14 ) {
      my $t = SThought::Seqsee::Element->new( core => $e->[$i] );
      if ( $i % 4 == 1 ) { $t->stored_fringe( [] ) }
      elsif ( $i % 4 ) { $t->stored_fringe( [ [ "f$i", $i ], [ "g$i", 0.5 ] ] ) }
      push @t, $t;
    }
    _stream( $cur, @t );
    _thought_hits( map { [ $t[ $_ - 1 ], $_ == 7 ? 2.5 : 10 * ( $_ % 5 ) ] }
        grep { $_ % 3 } 1 .. 14 );
    return { e => $e, cur => $cur, t => [@t] };
  },

  # A current thought, then older T1 (hit 7), a '' and T2 (no hit): the '' sorts between them
  # and its row dies in DrawOneItem (as_text on '').
  stream_hole => sub {
    my $e   = _init( 1, 2, 3 );
    my @t   = map { SThought::Seqsee::Element->new( core => $_ ) } @$e;
    $_->stored_fringe( [ [ 'x', 1 ] ] ) for @t;
    _stream( $t[0], $t[1], '', $t[2] );
    _thought_hits( [ $t[1], 7 ] );
    return { e => $e, t => [@t] };
  },

  # The end of a solved run on 1 1 2 1 2 3 (item 022; modelled on the Python GUI run with seed
  # 7): 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5, ascending blocks A (1 2), B (1 2 3), C (1 2 3 4),
  # D (1 2 3 4 5), the first element squinted, BIG = (e0 A B C D) with a mapping-based category
  # (strength set: 74.4, blocks 56.5 .. 68.5); no relations (DescribeSolution deleted them);
  # Hilit 2 on C and D. Slipnet: succ 0.956, ascending 0.95, number 0.166, mountain 0.079.
  # Coderack: 7 codelets (one on D), a history of 8 families. Stream: the current thought on
  # BIG (hit 22500), older thoughts on D (hit 500), e9 (no hit) and ascending (no fringe).
  # 677 steps; the last runnable was DescribeSolution.
  solution => sub {
    my $e   = _init( 1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5 );
    my $a   = _group( $S::ASCENDING, @$e[ 1, 2 ] );
    my $b   = _group( $S::ASCENDING, @$e[ 3 .. 5 ] );
    my $c   = _group( $S::ASCENDING, @$e[ 6 .. 9 ] );
    my $d   = _group( $S::ASCENDING, @$e[ 10 .. 14 ] );
    $e->[0]->set_group_p(1);
    my $big = _group( undef, $e->[0], $a, $b, $c, $d );
    my $type = Mapping::Structural->create(
      { category         => $S::ASCENDING,
        meto_mode        => $METO_MODE::NONE,
        direction_reln   => Mapping::Dir->create('Same'),
        changed_bindings => { end => Mapping::Numeric->create( 'succ', $S::NUMBER ) },
        slippages        => {}
      }
    );
    _add_category( $big, SCategory::MappingBased->Create($type) );
    my @strengths = ( 56.5, 60.5, 64.5, 68.5, 74.4 );
    ( $a, $b, $c, $d, $big )[$_]->set_strength( $strengths[$_] ) for 0 .. 4;
    Global::Hilit( 2, $c );
    Global::Hilit( 2, $d );
    _activate( SLTM::GetMemoryIndex( Mapping::Numeric->create( 'succ', $S::NUMBER ) ), 0.956 );
    _activate( SLTM::GetMemoryIndex($S::ASCENDING), 0.95 );
    _activate( SLTM::GetMemoryIndex($S::NUMBER),    0.166 );
    _activate( SLTM::GetMemoryIndex($S::MOUNTAIN),  0.079 );
    _codelet( 'FocusOn',                  50 );
    _codelet( 'ActOnOverlappingThoughts', 100 );
    _codelet( 'ConvulseEnd',              10 );
    _codelet( 'FocusOn',                  50 );
    _codelet( 'AttemptExtensionOfGroup',  80, core => $d );
    _codelet( 'CheckProgress',            100 );
    _codelet( 'FocusOn',                  50 );
    _history( FocusOn => 219, ActOnOverlappingThoughts => 134,
      AttemptExtensionOfRelation => 117, AttemptExtensionOfGroup => 81, MergeGroups => 33,
      CheckProgress => 27, CreateGroup => 14, DescribeSolution => 2 );
    my $cur = SThought::Seqsee::Anchored->new( core => $big );
    $cur->stored_fringe( [ [ 'plat[1, [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]', 100 ],
        [ '[ascending] end => succ', 50 ], [ 'Gp based on [ascending]', 100 ] ] );
    my $td = SThought::Seqsee::Anchored->new( core => $d );
    $td->stored_fringe( [ [ 'plat[1, 2, 3, 4, 5]', 100 ], [ 'ascending', 100 ] ] );
    my $te = SThought::Seqsee::Element->new( core => $e->[9] );
    $te->stored_fringe( [ [ 'plat4', 100 ], [ 'absolute_position_9', 80 ] ] );
    my $tc = SThought::SCat->new( core => $S::ASCENDING );
    _stream( $cur, $td, $te, $tc );
    _thought_hits( [ $cur, 22500 ], [ $td, 500 ] );
    _component_hits( 'ascending' => 100 );
    $Global::Steps_Finished        = 677;
    $Global::CurrentRunnableString = 'Seqsee::SCF::DescribeSolution';
    return { e => $e, a => $a, b => $b, c => $c, d => $d, big => $big };
  },
);

sub _add_category {
  my ( $obj, $cat ) = @_;
  $obj->add_category( $cat, SBindings->create( {}, {}, $obj ) );
}

sub _struct_reln {
  my ( $first, $second ) = @_;
  my $type = Mapping::Structural->create(
    { category       => $S::ASCENDING,
      meto_mode      => $METO_MODE::NONE,
      direction_reln => Mapping::Dir->create('Same'),
      changed_bindings => { start => Mapping::Numeric->create( 'succ', $S::NUMBER ) },
      slippages        => {}
    }
  );
  my $r = SRelation::Structural->new( { first => $first, second => $second, type => $type } );
  $r->insert;
  return $r;
}

# Sets the stream's thoughts directly (add_thought would run get_actions and choose at random).
sub _stream {
  my ( $current, @older ) = @_;
  my $stream = $Global::MainStream;
  $stream->{CurrentThought}    = $current;
  $stream->{OlderThoughts}     = [@older];
  $stream->{OlderThoughtCount} = scalar(@older);
  $stream->{ThoughtsSet}{$_}   = $_ for grep { $_ } $current, @older;
}

sub _thought_hits {
  $Global::MainStream->{thought_hit_intensity}{ $_->[0] } = $_->[1] for @_;
}

sub _component_hits {
  my (%hits) = @_;
  $Global::MainStream->{hit_intensity}{$_} = $hits{$_} for keys %hits;
}

sub _history {
  my (%counts) = @_;
  $SCoderack::HistoryOfRunnable{"Seqsee::SCF::$_"} = $counts{$_} for keys %counts;
}

sub _activate {
  my ( $index, $value ) = @_;
  $SLTM::ACTIVATIONS[$index][ SNodeActivation::REAL_ACTIVATION() ] = $value;
}

sub _codelet {
  my ( $family, $urgency, %args ) = @_;
  SCoderack->add_codelet( SCodelet->new( $family, $urgency, {%args} ) );
}

sub _group {
  my ( $cat, @items ) = @_;
  my $g = Seqsee::Anchored->create(@items);
  $g->describe_as($cat) if defined $cat;
  SWorkspace->add_group($g);
  return $g;
}

sub _reln {
  my ( $first, $second, $name, $cat ) = @_;
  $cat //= $S::NUMBER;
  my $r = SRelation->new(
    { first => $first, second => $second, type => Mapping::Numeric->create( $name, $cat ) } );
  $r->insert;
  return $r;
}

sub build_recipe {
  my ($name) = @_;
  my $r = $RECIPES{$name} or die "no recipe $name\n";
  return $r->();
}

sub object_tags {
  my %map;
  my $oid = 0;
  $map{"$_"} = 'obj' . $oid++ for SWorkspace::GetElements();
  # Groups and relations are never tags in the Workspace view; they get no oid here because
  # Perl's GetGroups/relations order (hash order) differs from Python's.
  return \%map;
}

1;
