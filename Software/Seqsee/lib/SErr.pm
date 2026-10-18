# I am perhaps erring on the side of using too many..
use strict;
use Exception::Class (
  #Generic error
  'SErr'                     => {},

  # Long-term Memory loading issues; caught by SLTM::Load. 
  'SErr::LTM_LoadFailure'    => { fields => ['what'] },

  #  Attempted meto app not feasible. Trapped by MaybeAnnotateWithMetonym
  'SErr::MetonymNotAppicable' => {},


  'SErr::FinishedTest'          => { fields => [qw( got_it)] },
  'SErr::FinishedTestBlemished' => {},
  'SErr::NotClairvoyant'        => {},

  'SErr::CouldNotCreateExtendedGroup' => {},

  'SErr::AskUser' => {
    fields => [
      qw{already_matched
      next_elements
      object
      from_position
      direction
      }
    ]
  },

);
1;

