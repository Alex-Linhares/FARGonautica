#!/usr/bin/env bash
# Run RhythmCat/Musicat built by build-linux.sh.
#   bash run-musicat.sh                      # the GUI: pick a melody, press Restart, then Go
#   bash run-musicat.sh noGUI "input=QE QE QE QG | Q.G EF HE" png=out.png seed=1   # headless
B="${MUSICAT_BUILD:-$HOME/dev/musicat-build}"
export PATH="$B/mono-env/bin:$PATH" LD_LIBRARY_PATH="$B/mono-env/lib"
cd "$B/src/RhythmCat/bin/Release" && exec mono RhythmCat.exe "$@"
