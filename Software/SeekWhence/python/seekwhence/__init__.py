"""seekwhence: Seek-Whence (Meredith, 1986) reconstructed from the specification.

The thesis is the specification [M86 pp. 1-189]; its distillation is SPEC.md, the choices
made where it is silent are DECISIONS.md. The package's conventions: standard library only,
one seeded generator per run (`seekwhence.rng.Rng`), one frozen dataclass of knobs
(`seekwhence.config.Config`).
"""

__all__ = ["__version__"]
__version__ = "0.0.1"
