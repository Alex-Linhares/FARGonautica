package LTMStorable;
use Moose::Role;

requires 'get_pure';
requires 'get_memory_dependencies';
requires 'serialize';
requires 'deserialize';

sub SpikeBy {
  my ($self, $amount) = @_;
  SLTM::SpikeBy($amount, $self);
}

sub InsertISALink {
  my ($self, $cat) = @_;
  SLTM::InsertISALink($self, $cat);
}
1;
