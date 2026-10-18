package SRelation::Structural;
use 5.010;
use Moose;
use English qw( -no_match_vars );
use Smart::Comments;

extends 'SRelation';

has unchanged_bindings => (
  traits  => ['Hash'],
  is      => 'rw',
  isa     => 'HashRef',
  default => sub { {} },
  reader  => 'get_unchanged_bindings',
  writer  => 'set_unchanged_bindings',
  handles => { 'no_unchanged_bindings' => 'is_empty', }
);

__PACKAGE__->meta->make_immutable;
1;
