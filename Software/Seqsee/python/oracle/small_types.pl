# Oracle for SInt.pm, SFasc.pm and SHistory.pm. Output: tests/golden/small_types.json
use strict;
use Oracle;
use S;

sub cat_names { [ map { $_->get_name } @{ $_[0]->get_categories } ] }

# --- SInt construction under the Primes/Parity features -------------------
for my $features ([], ['Primes'], ['Parity'], ['Primes', 'Parity']) {
  %Global::Feature = map { $_ => 1 } @$features;
  for my $mag (0, 1, 2, 3, 4, 7, 9, 97, 101, -3, -4) {
    my $s = SInt->new($mag);
    record(op => 'new', features => $features, mag => $mag,
           get_mag => $s->get_mag, as_text => $s->as_text, str => "$s",
           categories => cat_names($s));
  }
}
# IsPrime is a string-key lookup: 2.0 stringifies as "2", the string "2.0" does not.
%Global::Feature = (Primes => 1);
for my $pair (['float 2.0', 2.0], ['string 2.0', '2.0'], ['string 2', '2']) {
  record(op => 'new_prime_key', label => $pair->[0], categories => cat_names(SInt->new($pair->[1])));
}
%Global::Feature = ();

# --- arithmetic overloads ---------------------------------------------------
for my $pair ([5, 3], [3, 5], [0, 0], [-2, 7]) {
  my ($a, $b) = @$pair;
  my ($sa, $sb) = (SInt->new($a), SInt->new($b));
  record(op => 'arith', a => $a, b => $b,
         add_sint => ($sa + $sb)->get_mag,
         add_num => ($sa + $b)->get_mag,
         radd_num => ($a + $sb)->get_mag,
         sub_sint => ($sa - $sb)->get_mag,
         sub_num => ($sa - $b)->get_mag,
         rsub_num => ($a - $sb)->get_mag,
         result_class => ref($sa + $sb),
         eq_sint => ($sa eq $sb) ? 1 : 0,
         ne_sint => ($sa ne $sb) ? 1 : 0,
         eq_num => ($sa eq $b) ? 1 : 0,
         ne_num => ($sa ne $b) ? 1 : 0,
         req_num => ($a eq $sb) ? 1 : 0);
}
{
  my $s = SInt->new(4);
  record(op => 'eq_numeric_string', eq => ($s eq '4.0') ? 1 : 0, ne => ($s ne '4.0') ? 1 : 0);
  record(op => 'num_eq_dies', dies => dies(sub { my $x = ($s == $s) }));
  record(op => 'direction', dir => $s->get_direction eq DIR::RIGHT() ? 'RIGHT' : 'other');
}

# --- categories ---------------------------------------------------------------
{
  my $s = SInt->new(4);
  $s->add_category($S::PRIME);
  $s->add_category($S::PRIME);
  $s->add_category($S::NUMBER);
  $s->add_category($S::EVEN);
  record(op => 'add_category', categories => cat_names($s));

  %Global::Feature = (Primes => 1, Parity => 1);
  my @sints = map { SInt->new($_) } @_ = (3, 5, 7);
  record(op => 'common', mags => [3, 5, 7],
         common => [ sort map { $_->get_name } SInt::get_common_categories(@sints) ]);
  @sints = map { SInt->new($_) } (2, 4);
  record(op => 'common', mags => [2, 4],
         common => [ sort map { $_->get_name } SInt::get_common_categories(@sints) ]);
  @sints = map { SInt->new($_) } (2, 3);
  record(op => 'common', mags => [2, 3],
         common => [ sort map { $_->get_name } SInt::get_common_categories(@sints) ]);
  @sints = map { SInt->new($_) } (9);
  record(op => 'common', mags => [9],
         common => [ sort map { $_->get_name } SInt::get_common_categories(@sints) ]);
  record(op => 'common', mags => [],
         common => [ sort map { $_->get_name } SInt::get_common_categories() ]);
  %Global::Feature = ();
}

# --- SFasc --------------------------------------------------------------------
for my $args ([], [ { strength => 40 } ], [ { strength => 0 } ], [ { strength => '' } ],
              [ { strength => 75.5 } ]) {
  my $f = SFasc->new(@$args);
  my $label = @$args ? (defined $args->[0]{strength} ? "$args->[0]{strength}" : 'undef') : 'none';
  record(op => 'fasc', arg => $label, strength => $f->get_strength);
}
{
  my $f = SFasc->new({ strength => 10 });
  $f->set_strength(60);
  record(op => 'fasc_set', strength => $f->get_strength);
}

# --- SHistory -----------------------------------------------------------------
{
  $Global::Steps_Finished = 0;
  $Global::CurrentRunnableString = '';
  # get_history returns the live array, so copy it before recording.
  my $h = SHistory->new();
  record(op => 'hist_new', history => [ @{ $h->get_history } ], age => $h->GetAge,
         unchanged_since_0 => $h->UnchangedSince(0),
         unchanged_since_neg => $h->UnchangedSince(-1));

  $Global::Steps_Finished = 5;
  $Global::CurrentRunnableString = 'Codelet(foo)';
  $h->AddHistory("Added category ascending");
  record(op => 'hist_add', history => [ @{ $h->get_history } ], age => $h->GetAge,
         unchanged_since_4 => $h->UnchangedSince(4),
         unchanged_since_5 => $h->UnchangedSince(5),
         unchanged_since_6 => $h->UnchangedSince(6));

  $Global::Steps_Finished = 12;
  $Global::CurrentRunnableString = '';
  $h->AddHistory("Removed category ascending");
  record(op => 'hist_add2', history => [ @{ $h->get_history } ], age => $h->GetAge,
         as_text => $h->history_as_text,
         search_qr => [ $h->search_history(qr/Removed/) ],
         search_str => [ $h->search_history('nomatch') ],
         search_empty => [ $h->search_history('') ],
         search_zero => [ $h->search_history('0') ]);

  # Born later: dob is the step count at creation.
  my $h2 = SHistory->new();
  $Global::Steps_Finished = 20;
  record(op => 'hist_dob', history => $h2->get_history, age => $h2->GetAge);

  # Steps_Finished undef / '' prints as 0 in the message; age arithmetic uses 0.
  $Global::Steps_Finished = undef;
  $Global::CurrentRunnableString = 'X';
  my $h3 = SHistory->new();
  $Global::Steps_Finished = 3;
  record(op => 'hist_undef_steps', history => $h3->get_history, age => $h3->GetAge);
}
emit();
