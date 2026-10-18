"""The v2 defaults as a `Config` (D-114): `standard` as it stood before the optimized
defaults, every field the v3 change touched set back to its a-priori value
(`tabletop.config.V2_DEFAULTS`), and the other three profiles' overrides on top of it, as
`PROFILES` puts them on top of `standard`. The unit tests of a mechanism check it at the
values its decision states (D-004 to D-100), and the GUI's tests of what a pane shows read
the runs those values make; the tests of the defaults themselves, the goldens, the lock and
the documents read `PROFILES`. Not a test file: the test files import it."""

import dataclasses

from tabletop.config import CALIBRATION_KNOBS, FAULT_KNOBS, PROFILES, V2_DEFAULTS, Config

V2 = dataclasses.replace(Config(), **dict(V2_DEFAULTS))
# Each profile's own knobs, read from the profile (the calibrations) or from FAULT_KNOBS (the
# seven faults, one of which, `push_reset_set`, v3's default already equals).
_OWN = {name: {knob: getattr(PROFILES[name], knob)} for name, knob in CALIBRATION_KNOBS}
_OWN["french-faults"] = dict(FAULT_KNOBS)
V2_PROFILES = {name: dataclasses.replace(V2, profile=name, **_OWN.get(name, {}))
               for name in PROFILES}


def v2(**changes) -> Config:
    """`Config(**changes)` on the v2 defaults: the mechanism tests' own configurations."""
    return dataclasses.replace(V2, **changes)
