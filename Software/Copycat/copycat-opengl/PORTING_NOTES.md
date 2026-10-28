# Porting notes: Hager's Copycat on Python 3.12 and pyglet 2

Joseph Aaron Hager translated Melanie Mitchell's Copycat from Lisp to Python between 2008 and
2010 (Python 2) and moved it to Python 3 in April 2017 (https://github.com/ajhager/copycat,
commit `6ede75b`, the last one). Run on Python 3.12 in October 2026, the engine crashed on
every problem with a snag and the GUI did not open. This file lists every change made to his
code here. Each one is marked in the source with the tag given in its heading; nothing else was
touched (no reformatting, no restructuring), and `README-upstream.md` is his README as found.

The engine's answers were not tuned: no sequence, seed or problem is special-cased, and the
behavioural fixes below only remove crashes and nondeterminism. How the answers compare with
Mitchell's program is in `README.md` ("How it behaves").

## `PY3-FIX`: Python 2 idioms that broke in Python 3

1. **`copycat/toolbox.py`, `flatten`.** Only recursed into `list`s. `dict.values()` is a list
   in Python 2 and a view in Python 3, so `workspace.proposed_bonds()` and its siblings, which
   flatten `string.proposed_bonds.values()`, returned the view itself as one "bond". The first
   codelet to touch it was `AnswerBuilder` on a snag (`bond.string` on a `dict_values`), so every
   problem with a snag (`abc abd xyz`, the famous one) died. Fix: tuples and dict views are
   flattened too.
2. **`copycat/coderack/codelets/description.py`, `DescriptionTopDownScout`.** Built its
   weights with `map(...)`, a list in Python 2 and a one-shot iterator in Python 3.
   `weighted_select` sums the weights, then iterates them again to pick; the second pass saw
   nothing, `weighted_index` fell off its loop and returned `None`, and `items[None]` raised
   `TypeError`. Fix: `list(map(...))`.
3. **`clients/opengl/__init__.py`.** The step counter printed `coderack.time / timestep` as
   `7.133333333333334`; true division replaced integer division. Fix: `//`.

Checked and left alone: `round()` (banker's rounding and an `int` result in Python 3; the
differences are in the last unit of activations and urgencies and do not change the program's
character), `(110 - temperature) / 15` in `Bin.urgency` (Python 3's true division matches
Mitchell's Lisp, where `/` on integers is a rational; Python 2 truncated), and the
`sys.path.insert(0, "lib")` in `Copycat.py`, which is now a no-op because `lib/` (the bundled
pyglet 1.2.2, which imports the removed module `imp`) is gone.

## `BUG-FIX`: bugs independent of the Python version

4. **`copycat/run.py`, `run_codelet`.** `deleted.agruments` (a typo for `arguments`): when
   the coderack was full and posting a codelet pushed another one out, the run died with
   `AttributeError`. Reached on long runs (several thousand codelets).
5. **`copycat/toolbox.py`, `average`.** `average()` of nothing divided by zero. It is
   reached from `propose_correspondence` when the correspondence scout flips a string-spanning
   group (the `xyz`-style opposite-direction case) and the new concept mappings include no
   distinguishing one: the urgency is then the average of an empty list. Seen once in 550
   headless runs (`abc abd iijjkk`). Fix: an empty average is 0, so the strength tester is
   posted with the lowest urgency.
6. **`copycat/workspace/__init__.py`, `post_codelet_number`.** The number of replacement
   finders to post was computed only `if self.rule`, the inverse of Mitchell's `formulas.l`
   (zero once there is a rule, otherwise by the rough number of unreplaced objects). So after
   the initial batch no replacement finder was ever re-posted; when the batch happened to miss
   a letter (the finder picks letters at random and fizzles on one already replaced), that
   letter never got its replacement, `RuleScout` fizzled on `has_null_replacement()` for ever,
   and the run never ended: 4 of 650 sweep runs were still going at 40,000 codelets (`abc abd
   xyd` seed 8, `aabc aabd ijkk` seeds 13, 28 and 48). Fix: `and not self.rule`. This changes
   the random draws of every run, so the step counts quoted anywhere are those of the fixed
   engine.

## `DETERMINISM-FIX`: the same seed now gives the same run

7. **`toolbox.unique`** replaces eight `list(set(...))` de-duplications (`workspace/string.py`
   ×3, `workspace/correspondence.py`, `workspace/group.py`, `workspace/bond.py`,
   `workspace/__init__.py` ×2). A `set` of workspace objects is ordered by their `id()`, that
   is by memory address, which differs from one process to the next; the lists fed
   `weighted_select` and the breaker, so `--seed 8` gave `xyd` in 4,832 steps in one process
   and `wyz` in 3,273 in the next. This was true of Hager's Python 2 code as well. `unique`
   keeps the first occurrence in the original order, so runs are reproducible per seed:
   `tests/test_headless.py::test_deterministic` checks it.

## `PYGLET2-FIX`: the GUI client on pyglet 2 (and 1.5)

Hager bundled pyglet 1.2.2 (2015) in `lib/`. It needs `imp`, removed in Python 3.12, so the
GUI could not even import. pyglet 1.5 loads but wants `libGLU`, which modern distributions no
longer ship; pyglet 2 needs only OpenGL itself (Mesa's software renderer is enough). The client
was made to run on pyglet 2, with changes that also work on 1.5:

8. `pyglet.text.Label("0", "EraserDust", 18, ...)` passed the font as positional arguments;
   pyglet 2 changed their order (`x` is now second). The six calls name them
   (`font_name=`, `font_size=`).
9. `pyglet.clock.ClockDisplay` (removed in pyglet 1.4) became `pyglet.window.FPSDisplay`.
10. Sprite colours must be integers in pyglet 2: two computations (the temperature-tinted
   background in `__init__.py`, the Slipnet squares in `slipnet.py`) are wrapped in `int()`.

## `GUI-2026`: what was added to the display

Hager's workspace view drew the letters and the changed letter in red; its header says
"Add drawing routines for workspace structures". `clients/opengl/workspace.py` now has a
`Structures` layer that draws, after every step and only when something changed:

- **Bonds** as arcs over the letters, labelled `succ`, `pred` or `same`, green, orange and
  blue, with an arrowhead at the object the bond points to; proposed bonds thin and faint,
  built bonds thick and bright.
- **Groups** as boxes around their letters, with the group category (`succgr`, `samegr`, ...)
  and a `<` or `>` for the direction; nested groups get bigger boxes.
- **Correspondences** as curves from an object of the initial string (top left) to one of the
  target string (bottom left), with up to two of their distinguishing concept mappings
  (`rmost->rmost`, `succ->pred`) beside the upper end.
- **The rule** as soon as it is built (Hager only showed it at the end), in the right half
  between the modified string and the answer, and **the translated rule** under the answer.
- **The answer** at the bottom right once it exists (Hager drew it too, once per frame; now
  once).

Also: the window's data folder is located from the module's own path, so the GUI runs from
any directory, and the rule label moved from the centre to the right half so the
correspondences stay legible. The engine is untouched by the GUI: it draws no random number
and changes no structure, so a run with the window is the run `--quiet` prints for the same
seed.
