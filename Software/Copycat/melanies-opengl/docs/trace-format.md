# The trace format

`lisp/trace-emitter.lisp` writes one JSON object per line to standard output while
Mitchell's program runs. Every line is a complete snapshot of the run; nothing has to be
accumulated to draw it. The adapter (`melanies_opengl/trace.py`) reads these lines;
`--record` saves them and `--replay` reads them back.

## Kinds of line

| `k` | When | Extra fields |
|---|---|---|
| `init` | once, before the first codelet (`n` is 0) | `problem`, `seed`, `timestep`, `nodes` |
| `step` | after every codelet, or every `--every` codelets | |
| `final` | once, after `run-ccat` returns; the answer is in `strings.a` | |

## Fields

| Field | Type | Meaning |
|---|---|---|
| `n` | integer | `*codelet-count*`, codelets run so far |
| `temp` | number | `*temperature*` (0–100) |
| `codelet` | string or null | the type of the codelet that just ran, e.g. `"bottom-up-bond-scout"` (Mitchell's names) |
| `snag` | bool | `*snag-condition*` |
| `snags` | integer | `*snag-count*` |
| `found` | bool | `*found-answer*` |
| `problem` | `[initial, modified, target]` | init line only |
| `seed` | integer or null | init line only |
| `timestep` | integer | `%time-step-length%`, 15: codelets between updates (the window's counter divides by it) |
| `nodes` | `[[pname, cm-name], ...]` | init line only; the slipnet's nodes in `*slipnet*` order (59). `cm-name` is Mitchell's label for concept-mapping graphics, null for a few nodes |
| `strings` | object | `i`, `m`, `t`: the initial, modified and target strings; `a`: the answer string, null until found |
| `bonds` | list | built and proposed bonds of both strings (`*workspace* :bond-list` and `:proposed-bond-list`) |
| `groups` | list | built and proposed groups |
| `corrs` | list | built and proposed correspondences |
| `rule` | string or null | `*rule*` as `(rule :print)` prints it |
| `trule` | string or null | `*translated-rule*`, same form |
| `act` | list of numbers | the nodes' activations, in `nodes` order |
| `clamp` | list of bools | whether each node is clamped |
| `rack` | object | codelets waiting on the coderack, counted by type: `{"bond-builder": 3, ...}` |

### A string

A list of letters, each `{"p": "a", "pos": 0, "ch": false, "d": [[type, descriptor], ...]}`:
the print name, the string position, Mitchell's `changed?` flag (the letter of the initial
string that the modified string changes) and the letter's descriptions as pairs of slipnode
print names, e.g. `["string-position-category", "rmost"]`.

### An object reference

`[string, left, right, kind]`: the string's key (`i`, `m`, `t`, `a`), the object's left
and right string positions and `"l"` for a letter or `"g"` for a group. A group is looked
up by its span in the same line's `groups`.

### A bond

```json
{"s": "i", "from": 1, "to": 0, "lvl": 3, "cat": "predecessor", "dir": "left",
 "lo": ["i", 0, 0, "l"], "ro": ["i", 1, 1, "l"]}
```

`s` the string; `from`/`to` the positions of the objects the bond goes from and to;
`lvl` Mitchell's proposal level (1 proposed, 2 strength-tested, 3 built, her `%built%`);
`cat` the bond category's print name (`successor`, `predecessor`, `sameness`); `dir` the
direction category (`left`, `right`, null); `lo`/`ro` the leftmost and rightmost objects.

### A group

```json
{"s": "i", "l": 0, "r": 2, "lvl": 3, "cat": "predgrp", "dir": "left",
 "objs": [["i", 2, 2, "l"], ["i", 1, 1, "l"], ["i", 0, 0, "l"]]}
```

`cat` is `succgrp`, `predgrp` or `samegrp`; `objs` the member objects in Mitchell's
`object-list` order.

### A correspondence

```json
{"o1": ["i", 0, 0, "l"], "o2": ["t", 0, 0, "l"], "lvl": 3,
 "cms": [["lmost", "lmost", true]]}
```

`o1` is in the initial string, `o2` in the target string; each concept mapping is
`[descriptor1, descriptor2, distinguishing?]` (`concept-mapping :distinguishing?`).

## Size and cost

A line is about 3–6 KB; a run of 2,500 codelets is about 14 MB with `--every 1`. The
emitter is a few hundred `format` calls per codelet and adds about a millisecond; SBCL
finishes a run in a second or two whatever the GUI does, so the window can always keep up
by reading faster, and `--speed` sets how many codelets per second it shows.
