# Oracle for SBindings.pm. Output: tests/golden/sbindings.json
use strict;
use Oracle;
use S;

# Fake metonyms: SMetonym->intersection only needs ->get_type, and the
# delegated get_metonymy_cat/get_metonymy_name only need the type's
# get_category/get_name. Types are compared with `eq` (refs, i.e. identity).
package FakeCat;
sub new { my ($c, $n) = @_; bless { n => $n }, $c }
sub get_name { $_[0]{n} }

package FakeType;
sub new { my ($c, $n, $cat) = @_; bless { n => $n, cat => $cat }, $c }
sub get_name     { $_[0]{n} }
sub get_category { $_[0]{cat} }

package FakeMeto;
sub new { my ($c, $t) = @_; bless { t => $t }, $c }
sub get_type { $_[0]{t} }

package main;

my $cat_a = FakeCat->new('catA');
my $cat_b = FakeCat->new('catB');
my $type1 = FakeType->new('type1', $cat_a);
my $type2 = FakeType->new('type2', $cat_b);
my %types = (type1 => $type1, type2 => $type2);

sub mode_text { my ($m) = @_; defined($m) ? $m->as_text : undef }

sub describe {
  my ($b) = @_;
  my $pos = $b->get_position;
  my $type = $b->get_metonymy_type;
  my $cat_name;
  my $name;
  my $cat_dies = dies(sub { $cat_name = $b->get_metonymy_cat->get_name });
  my $name_dies = dies(sub { $name = $b->get_metonymy_name });
  return (
    slippages_count => $b->slippages_count,
    slippage_positions => [ sort $b->slippage_positions ],
    all_slippage_types => [ sort map { $_->get_type->get_name } $b->all_slippages ],
    metonymy_mode => mode_text($b->get_metonymy_mode),
    position_mode => mode_text($b->get_position_mode),
    position => defined($pos) ? $pos->position : undef,
    metonymy_type => defined($type) ? $type->get_name : undef,
    metonymy_cat => $cat_name, metonymy_cat_dies => $cat_dies,
    metonymy_name => $name, metonymy_name_dies => $name_dies,
    binding_keys => [ sort keys %{ $b->get_bindings_ref } ],
  );
}

# --- create() with slippage sets: {position => type name} ------------------
my @slippage_sets = (
  ['empty', {}],
  ['one at 0', { 0 => 'type1' }],
  ['one at 2', { 2 => 'type1' }],
  ['one at "5"', { '5' => 'type2' }],
  ['one at -2', { -2 => 'type1' }],
  ['one at "abc"', { abc => 'type1' }],
  ['one at "2.0"', { '2.0' => 'type1' }],
  ['one at -1', { -1 => 'type1' }],
  ['one at "2.5"', { '2.5' => 'type1' }],
  ['two same type', { 0 => 'type1', 3 => 'type1' }],
  ['two different types', { 0 => 'type1', 3 => 'type2' }],
  ['three same type', { 1 => 'type2', 2 => 'type2', 4 => 'type2' }],
);
for my $set (@slippage_sets) {
  my ($label, $spec) = @$set;
  my %raw = map { $_ => FakeMeto->new($types{ $spec->{$_} }) } keys %$spec;
  my $b;
  my $d = dies(sub { $b = SBindings->create(\%raw, { a => 1 }) });
  record(op => 'create', label => $label, slippages => $spec, dies => $d,
         ($d ? () : describe($b)));
}

# --- bindings ---------------------------------------------------------------
{
  my %bdg = (a => 1, b => 'x', length => 3);
  my $b = SBindings->create({}, \%bdg, 'ignored third arg');
  record(op => 'bindings',
         a => $b->GetBindingForAttribute('a'),
         b => $b->GetBindingForAttribute('b'),
         length => $b->GetBindingForAttribute('length'),
         missing => $b->GetBindingForAttribute('missing'),
         same_ref => ($b->get_bindings_ref == \%bdg) ? 1 : 0);
  $bdg{c} = 7;  # the hash is shared, not copied
  record(op => 'bindings_live', c => $b->GetBindingForAttribute('c'));
  my %raw = (1 => FakeMeto->new($type1));
  my $b2 = SBindings->create(\%raw, {});
  record(op => 'squinting_raw_shared', same_ref => ($b2->get_squinting_raw == \%raw) ? 1 : 0);
}

# --- argument checking --------------------------------------------------------
record(op => 'create_undef_slippages', dies => dies(sub { SBindings->create(undef, {}) }));
record(op => 'create_undef_bindings', dies => dies(sub { SBindings->create({}, undef) }));
record(op => 'create_no_args', dies => dies(sub { SBindings->create() }));
record(op => 'create_array_bindings', dies => dies(sub { SBindings->create({}, [1]) }));
record(op => 'create_array_slippages', dies => dies(sub { SBindings->create([1], {}) }));
record(op => 'new_missing_bindings', dies => dies(sub { SBindings->new(raw_slippages => {}) }));
record(op => 'new_missing_slippages', dies => dies(sub { SBindings->new(bindings => {}) }));
{
  # Unknown init args (MappingBased passes `object`) are ignored.
  my $b;
  my $d = dies(sub { $b = SBindings->new({ raw_slippages => {}, bindings => { x => 1 }, object => 'o' }) });
  record(op => 'new_hashref_extra_arg', dies => $d, x => $b->GetBindingForAttribute('x'));
}
{
  # Constructor-supplied modes: BUILD overrides them for 0/1 slippages only.
  my $b0 = SBindings->new(raw_slippages => {}, bindings => {},
                          metonymy_mode => METO_MODE::ALL(), position_mode => POS_MODE::BACKWARD());
  record(op => 'new_modes_empty', describe($b0));
  my $b1 = SBindings->new(raw_slippages => { 4 => FakeMeto->new($type1) }, bindings => {},
                          metonymy_mode => METO_MODE::ALL(), position_mode => POS_MODE::BACKWARD(),
                          position => SPos->new(1));
  record(op => 'new_modes_one', describe($b1));
  my $b2 = SBindings->new(raw_slippages => { 4 => FakeMeto->new($type1), 5 => FakeMeto->new($type1) },
                          bindings => {}, metonymy_mode => METO_MODE::ALL(),
                          position_mode => POS_MODE::BACKWARD(), position => SPos->new(2),
                          metonymy_type => $type2);
  record(op => 'new_modes_two_same', describe($b2));
  my $b3 = SBindings->new(raw_slippages => { 4 => FakeMeto->new($type1), 5 => FakeMeto->new($type2) },
                          bindings => {}, metonymy_type => $type2);
  record(op => 'new_type_two_different', describe($b3));
}

# --- setters --------------------------------------------------------------------
{
  my $b = SBindings->create({}, {});
  record(op => 'set_meto_mode', dies => dies(sub { $b->metonymy_mode(METO_MODE::ALLBUTONE()) }),
         value => mode_text($b->get_metonymy_mode));
  record(op => 'set_meto_mode_bad', dies => dies(sub { $b->metonymy_mode('ALL') }));
  record(op => 'set_meto_mode_pos_mode', dies => dies(sub { $b->metonymy_mode(POS_MODE::FORWARD()) }));
  record(op => 'set_meto_mode_undef', dies => dies(sub { $b->metonymy_mode(undef) }));
  record(op => 'set_pos_mode', dies => dies(sub { $b->position_mode(POS_MODE::BACKWARD()) }),
         value => mode_text($b->get_position_mode));
  record(op => 'set_pos_mode_bad', dies => dies(sub { $b->position_mode(METO_MODE::ALL()) }));
  record(op => 'set_position', dies => dies(sub { $b->position(SPos->new(-1)) }),
         value => $b->get_position->position);
  record(op => 'set_position_bad', dies => dies(sub { $b->position(3) }));
  record(op => 'set_meto_type', dies => dies(sub { $b->metonymy_type($type2) }),
         name => $b->get_metonymy_name, cat => $b->get_metonymy_cat->get_name);
  record(op => 'set_meto_type_any', dies => dies(sub { $b->metonymy_type(42) }),
         name_dies => dies(sub { $b->get_metonymy_name }));
  record(op => 'tell_stories',
         dies => dies(sub { $b->TellDirectedStory(1, 2); $b->tell_backward_story; $b->tell_forward_story }));
}

emit();
