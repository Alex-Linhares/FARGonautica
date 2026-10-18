# cython: language_level=3
# cython: boundscheck=False, wraparound=False, cdivision=False
"""The Coderack's chances in C (D-112): `tabletop/temperature.py`'s `chances_of`, the one
numeric loop of the run loop that the profile after the native Slipnet (D-111) still
showed, mirrored with the Python's floating-point operations in the Python's order, so
that the compiled engine's draws are the pure engine's bit for bit. The mechanism is
specified by `temperature.py` and `coderack.py` (D-052, D-060), not repeated here.

What differs from the Python, and why: the softened weights are kept in a C array instead
of a list, and the total is summed by the compensated (Neumaier) summation that Python's
`sum` uses for floats, written out here, so that the total, and so every chance, is the
one `sum` returns. `tests/test_native_coderack.py` checks the two functions agree exactly.

The build embeds its stamp, flags, compiler and Cython version as C string macros (set by
`setup.py`; D-110), read below into module attributes for `tabletop.accel`.
"""

from libc.math cimport fabs, isinf, pow
from libc.stdlib cimport free, malloc

cdef extern from *:
    """
    #ifndef TABLETOP_BUILD_STAMP
    #define TABLETOP_BUILD_STAMP "unstamped"
    #endif
    #ifndef TABLETOP_BUILD_FLAGS
    #define TABLETOP_BUILD_FLAGS ""
    #endif
    #ifndef TABLETOP_BUILD_COMPILER
    #define TABLETOP_BUILD_COMPILER ""
    #endif
    #ifndef TABLETOP_CYTHON_VERSION
    #define TABLETOP_CYTHON_VERSION ""
    #endif
    """
    const char *TABLETOP_BUILD_STAMP
    const char *TABLETOP_BUILD_FLAGS
    const char *TABLETOP_BUILD_COMPILER
    const char *TABLETOP_CYTHON_VERSION

cdef const char *_stamp = TABLETOP_BUILD_STAMP
cdef const char *_flags = TABLETOP_BUILD_FLAGS
cdef const char *_compiler = TABLETOP_BUILD_COMPILER
cdef const char *_cython = TABLETOP_CYTHON_VERSION
BUILD_STAMP = _stamp.decode("utf-8")
BUILD_FLAGS = _flags.decode("utf-8")
BUILD_COMPILER = _compiler.decode("utf-8")
CYTHON_VERSION = _cython.decode("utf-8")


cdef double neumaier_sum(const double *values, Py_ssize_t n) noexcept:
    # The total as Python's `sum` of a list of floats computes it (CPython 3.12,
    # bltinmodule.c: the first float added to the integer 0 exactly, then Neumaier's
    # compensated summation, the compensation added at the end unless it is not finite);
    # mirrors the `sum(softened)` of temperature.py, chances_of.
    cdef double total, c, x, t
    cdef Py_ssize_t i
    if n == 0:
        return 0.0
    total = values[0]
    c = 0.0
    for i in range(1, n):
        x = values[i]
        t = total + x
        if fabs(total) >= fabs(x):
            c += (total - t) + x
        else:
            c += (x - t) + total
        total = t
    if c != 0.0 and not isinf(c) and c == c:
        total += c
    return total


def softened_weights(weights, double exponent):
    """The weights raised to the exponent a (D-060) [F95 p. 93]. Mirrors temperature.py,
    softened_weights: w ** a for w > 0 (libm's pow, as Python's float power), 0 for the
    rest, and when a power overflows every weight is first divided by the largest; an
    infinite weight takes the limit, the infinite ones sharing the chance (D-060, D-114)."""
    cdef Py_ssize_t n = len(weights), i
    cdef double *out = <double *> malloc(max(n, 1) * sizeof(double))
    if out == NULL:
        raise MemoryError()
    try:
        _soften(weights, n, exponent, out)
        return [out[i] for i in range(n)]
    finally:
        free(out)


cdef int _soften(object weights, Py_ssize_t n, double exponent, double *out) except -1:
    # temperature.py, softened_weights, into a C array: the try (every power) and the
    # except OverflowError (the weights over the largest) of the Python
    cdef Py_ssize_t i
    cdef double w, top
    cdef bint overflow = False
    for i in range(n):
        w = weights[i]
        if w > 0:
            out[i] = pow(w, exponent)
            if isinf(out[i]):
                overflow = True
                break
        else:
            out[i] = 0.0
    if not overflow:
        return 0
    top = weights[0]
    for i in range(1, n):          # max(weights), the first of equals
        w = weights[i]
        if w > top:
            top = w
    if isinf(top):                 # an infinite weight: the limit, the infinite ones share
        for i in range(n):
            out[i] = 1.0 if weights[i] == top else 0.0
        return 0
    for i in range(n):
        w = weights[i]
        out[i] = pow(w / top, exponent) if w > 0 else 0.0
    return 0


def chances_of(weights, double exponent):
    """The chance of each weight at the exponent a: w^a over the total of w^a [F95 p. 92
    (img)] (D-060). Mirrors temperature.py, chances_of: the same softened weights, the
    total as Python's `sum` computes it, equal chances when the total is 0, none for no
    weights."""
    cdef Py_ssize_t n = len(weights), i
    cdef double total
    if n == 0:
        return []
    cdef double *softened = <double *> malloc(n * sizeof(double))
    if softened == NULL:
        raise MemoryError()
    try:
        _soften(weights, n, exponent, softened)
        total = neumaier_sum(softened, n)
        if total <= 0:
            return [1.0 / n] * n
        return [softened[i] / total for i in range(n)]
    finally:
        free(softened)
