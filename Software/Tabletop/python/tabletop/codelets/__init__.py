"""The codelets: the catalogue of the 23 types the screen's legend names [F95 p. 96 (img)]
(D-051) and the role codelets after them (D-081), with one function per type and its
follow-ups (D-052, D-056).

"Codelets are small pieces of code that perform certain simple actions. In Tabletop, there
are approximately thirty different codelet types." [F95 p. 63] The catalogue
(`tabletop.codelets.catalogue`) records, for each type, its strip label, its band (which
sets its raw urgency, D-052), its job, and whether the job is described in the text,
inferred from the legend's name (D-051), or the re-interpretation's (D-081).
"""

from tabletop.codelets.catalogue import (BANDS, CATALOGUE, CodeletType, entry, names, of_band,
                                         role_codelets)

__all__ = ["BANDS", "CATALOGUE", "CodeletType", "entry", "names", "of_band", "role_codelets"]
