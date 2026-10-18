package Memory::Storable;
use 5.010;
use Moose::Role;

# Role satisfied by anything that can be stored as-is into memory.

with 'Memory::Insertible';

sub GetNormalizedForMemory {
  return $_[0];
}

requires 'GetMemoryDependencies';
requires 'Serialize';
requires 'Deserialize';
1;
