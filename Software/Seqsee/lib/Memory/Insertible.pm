package Memory::Insertible;
use 5.010;
use Moose::Role;

# Role filled by anything that can be inserted into memory, perhaps in a
# normalized form.

requires 'GetNormalizedForMemory';

1;
