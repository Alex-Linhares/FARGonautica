#!/usr/bin/env bash
# Build RhythmCat/Musicat (Nichols, .NET 4.0 WinForms) on Linux with Mono, without root.
# Everything goes into a build folder outside the repository (default ~/dev/musicat-build);
# the archived source in Software/Musicat/RhythmCat/ is copied, never modified.
#
#   bash Software/Musicat/linux/build-linux.sh [BUILD_DIR]
#
# Needs conda (for Mono and libgdiplus's build dependencies), git, network access.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
SRC="$HERE/../RhythmCat"
B="${1:-$HOME/dev/musicat-build}"
ENV="$B/mono-env"
mkdir -p "$B"

# 1. Mono 6.12 (mono, mcs, csc, xbuild) and libgdiplus's build dependencies, from conda-forge.
if [ ! -x "$ENV/bin/mono" ]; then
  conda create -y -q -p "$ENV" -c conda-forge mono
fi
conda install -y -q -p "$ENV" -c conda-forge cairo glib libpng libjpeg-turbo libtiff giflib \
  libexif fontconfig freetype expat zlib pango xorg-xorgproto pkg-config autoconf automake \
  libtool make c-compiler cxx-compiler
export PATH="$ENV/bin:$PATH"
export PKG_CONFIG_PATH="$ENV/lib/pkgconfig:$ENV/share/pkgconfig"

# 2. libgdiplus (System.Drawing for WinForms), built from source with X11 support. Only the
#    library is built: its own tests need a googletest too old for current CMake.
if [ ! -e "$ENV/lib/libgdiplus.so.0" ]; then
  rm -rf "$B/libgdiplus"
  git clone -q --depth 1 https://github.com/mono/libgdiplus.git "$B/libgdiplus"
  ( cd "$B/libgdiplus"
    CFLAGS="-I$ENV/include" LDFLAGS="-L$ENV/lib -Wl,-rpath,$ENV/lib" ./autogen.sh --prefix="$ENV"
    make -j4 -C src && make -C src install )
fi

# 3. A copy of the source with the Windows paths replaced (linux-paths.patch), built with xbuild.
rm -rf "$B/src"
cp -r "$SRC" "$B/src"
( cd "$B/src" && patch -p1 < "$HERE/linux-paths.patch" )
( cd "$B/src" && xbuild /p:Configuration=Release RhythmCat/RhythmCat.csproj )
OUT="$B/src/RhythmCat/bin/Release"
cp "$B/src/RhythmCat/melodies.txt" "$OUT/"
mkdir -p "$OUT/RhythmcatTesting" "$OUT/RhythmcatScreenshots"
echo "built: $OUT/RhythmCat.exe — run it with Software/Musicat/linux/run-musicat.sh"
