"""The results pane's model (PANES.md P8; G-052): a batch of runs of one problem, tallied
as French tallies his, beside his published numbers [F95 pp. 115-139].

French ran each configuration about fifty times [F95 p. 114] and printed, for each, a bar
graph of the percentage of runs giving each answer, the percentage above each black bar
and the answer's label under it, the configuration's title under the graph, and a table
headed "Object Eliza touches", "Average structure", "No. of codelets run"
[F95 p. 118 (img)]. `BatchModel` holds a batch's runs as they come in and gives the same
picture for them: the survey (`bench/harness.py`'s `Survey`, D-072), French's result set
for the problem (`reported.py`), the comparison answer by answer with his labels and
`other` for the answers he prints no bar for (`harness.compare`, as RESULTS.md groups
them), the table's cells and the bar graph's geometry as a pure function (`bars_layout`).

`BatchWorker` runs the batch on its own thread with `run_problem` on the seeds of
`rng.run_seeds(seed, count)`, which is what `run_batch` and `python3 -m tabletop NAME
--batch N --seed S` run, so the answers are theirs run for run (TASK.md's contract 4). It
holds the run controller's engine lock for the batch's duration, so a batch never runs
during a live run and a live run never starts during a batch (G-052). No observer is
attached; the runs draw from the engine's generator alone. Qt-free.
"""

import dataclasses
import threading

from tabletop.bench import stats as ST
from tabletop.bench.harness import OTHER, Survey, compare, tallies_of
from tabletop.config import Config
from tabletop.gui.models import run_controller as rc
from tabletop.problems import Problem
from tabletop.reported import ResultSet, result_sets
from tabletop.accel import run_seeds
from tabletop.run import run_problem

__all__ = ["DEFAULT_COUNT", "MAX_COUNT", "STATES", "HEADINGS", "ResultRow", "Bar",
           "BarsLayout", "BatchModel", "BatchWorker", "bars_layout", "row_text",
           "set_title", "number"]

DEFAULT_COUNT = ST.FRENCH_RUNS      # fifty, French's count [F95 p. 114]
MAX_COUNT = 5000
STATES = ("idle", "running", "finished", "cancelled", "error")
JOIN_TIMEOUT = 30.0
# French's three headings [F95 p. 118 (img)], each with his column and ours, and the z of
# the two frequencies (the bench's, D-072).
HEADINGS = ("Object Eliza touches", "Frequency, French", "Frequency, ours", "z",
            "Average structure, French", "ours", "No. of codelets run, French", "ours")


@dataclasses.dataclass(frozen=True)
class ResultRow:
    """One answer of the table (G-052): French's `label` (`E1`, `E1+E2`, or `other`) and
    what it `names`; his frequency, structure and codelets (None where he prints a dash,
    or when there is no set); our `count`, `frequency`, `structure` and `codelets`; the
    objects behind ours with their counts; the bench's `z` (None without a set)."""
    label: str
    names: str
    french_frequency: float | None
    french_structure: float | None
    french_codelets: float | None
    count: int
    frequency: float
    structure: float | None
    codelets: float | None
    answers: tuple
    z: float | None


@dataclasses.dataclass(frozen=True)
class Bar:
    """One bar of the graph (G-052): the answer's `label`, `who` (`French` or `ours`),
    the `value` in percent (None when he prints no bar), its box in pixels and the text
    printed above it, as the figure prints the percentage [F95 p. 118 (img)]."""
    label: str
    who: str
    value: float | None
    x: float
    y: float
    width: float
    height: float
    text: str


@dataclasses.dataclass(frozen=True)
class BarsLayout:
    """The graph's geometry (G-052): the plot box, the bars in pairs by answer (French's
    then ours), the labels under each pair, the y ticks (percent, y) and the title under
    the graph [F95 p. 118 (img)]."""
    box: tuple
    bars: tuple
    labels: tuple
    ticks: tuple
    title: str


def number(value, digits: int = 0) -> str:
    """A number for a cell; a dash where French prints one [F95 p. 119] (G-052)."""
    if value is None:
        return "-"
    return f"{value:.{digits}f}"


def set_title(rs: ResultSet) -> str:
    """French's heading with its source and page: `Surround 5 [F95 p. 118 (img)]`, with
    FCCA's figure label and the modified program when they apply (G-052)."""
    figure = f" {rs.figure}" if rs.figure else ""
    program = f", {rs.program} program" if rs.program != "original" else ""
    return f"{rs.title}{program} [{rs.source}{figure} p. {rs.page} (img)]"


class BatchModel:
    """A batch of runs of one problem and the picture French prints for it (G-052).
    `start` names the batch; `add` takes each run's `Result` as the worker reports it;
    `finish` ends it. Everything else is a function of what has come in."""

    def __init__(self):
        self.problem = None
        self.config = None
        self.seed = 0
        self.count = DEFAULT_COUNT
        self.results = []
        self.state = "idle"
        self.error = None
        self.set_index = 0
        self.listeners = []

    # -- the batch --------------------------------------------------------------------

    def start(self, problem: Problem, config: Config, seed: int, count: int) -> None:
        """Begin a batch of `count` runs of `problem` under `config` from the batch seed
        `seed` (G-052): the previous batch's runs are forgotten."""
        if count < 1 or count > MAX_COUNT:
            raise ValueError(f"a batch wants 1 to {MAX_COUNT} runs, not {count}")
        self.problem, self.config, self.seed, self.count = problem, config, int(seed), int(count)
        self.results = []
        self.state = "running"
        self.error = None
        self.set_index = 0
        self._changed()

    def add(self, result) -> None:
        """One more run's `Result`, in the batch's order (G-052)."""
        self.results.append(result)
        self._changed()

    def finish(self, cancelled=False, error=None) -> None:
        """The batch ended: whole, cancelled after `done` runs, or with an error
        (G-052)."""
        self.error = error
        self.state = "error" if error is not None else "cancelled" if cancelled else "finished"
        self._changed()

    def clear(self) -> None:
        """Forget the batch (G-052)."""
        self.__init__()

    @property
    def done(self) -> int:
        """The runs that have come in (G-052)."""
        return len(self.results)

    @property
    def running(self) -> bool:
        """Whether the batch is still coming in (G-052)."""
        return self.state == "running"

    def _changed(self):
        for listener in list(self.listeners):
            listener()

    # -- the survey and French's set ------------------------------------------------------

    def survey(self) -> Survey | None:
        """The runs so far as a `Survey` (D-072): the problem, the profile, the batch
        seed as both the bench seed and the batch seed (the GUI's batch takes the seeds
        of `run_seeds(seed, count)` directly, as the CLI's `--batch` does), the tallies
        in the order the answers first came up, the capped count and the mean codelets
        (G-052); None before the first run."""
        results = self.results
        if not results or self.problem is None:
            return None
        return Survey(self.problem.name, self.config.profile, self.seed, self.seed,
                      len(results), tallies_of(results, self.problem),
                      sum(r.capped for r in results),
                      sum(r.codelets for r in results) / len(results),
                      tuple((r.seed, r.answer, r.structure_value, r.codelets, r.capped)
                            for r in results))

    def sets(self) -> list:
        """French's result sets for the problem, the book's first [F95 p. 115], then
        FCCA's (`reported.result_sets`); empty for a problem he prints none for
        (G-052)."""
        if self.problem is None:
            return []
        return result_sets(self.problem.name)

    def french(self) -> ResultSet | None:
        """The result set shown, the `set_index`th of `sets` (G-052); None when there
        is none."""
        sets = self.sets()
        if not sets:
            return None
        return sets[min(self.set_index, len(sets) - 1)]

    def choose_set(self, index: int) -> None:
        """Show another of French's sets for the problem (G-052)."""
        self.set_index = max(0, int(index))
        self._changed()

    def rows(self) -> tuple:
        """The table's rows (G-052): with a set of French's, `harness.compare`'s rows,
        one per label he prints and `other` for the answers he prints no bar for, as
        RESULTS.md groups them; without one, one row per answer in the order it came up,
        with French's cells and the z empty. Empty before the first run."""
        survey = self.survey()
        if survey is None:
            return ()
        rs = self.french()
        if rs is not None:
            return tuple(ResultRow(r.label, r.names, r.french_frequency, r.french_structure,
                                   r.french_codelets, r.count, r.frequency, r.structure,
                                   r.codelets, r.answers, r.z)
                         for r in compare(survey, rs).rows)
        return tuple(ResultRow(t.label or t.answer, t.answer, None, None, None, t.count,
                               t.frequency, t.structure, t.codelets, ((t.answer, t.count),),
                               None)
                     for t in survey.tallies)

    def chi_square(self) -> tuple | None:
        """The chi-square of homogeneity between our counts and French's over the rows,
        with its degrees of freedom and p (D-072); None without a set or a run
                (G-052)."""
        survey, rs = self.survey(), self.french()
        if survey is None or rs is None:
            return None
        c = compare(survey, rs)
        return c.chi_square, c.df, c.p

    # -- the texts ----------------------------------------------------------------------

    def cells(self) -> tuple:
        """The table's rows as strings under HEADINGS, formatted as the bench's
        `markdown_table` formats them (D-072; G-052)."""
        out = []
        for r in self.rows():
            who = r.label if r.label != OTHER else "other (" + " ".join(
                f"{a} {n}" for a, n in r.answers) + ")"
            z = "" if r.z is None else f"{r.z:+.2f}"
            out.append((who, number(r.french_frequency), f"{r.frequency:.1f}", z,
                        number(r.french_structure), number(r.structure, 1),
                        number(r.french_codelets), number(r.codelets, 1)))
        return tuple(out)

    def title(self) -> str:
        """What the batch is: `surround-05 under standard, batch seed 1` (G-052)."""
        if self.problem is None:
            return "no batch yet"
        profile = self.config.profile if self.config.profile else "custom"
        return f"{self.problem.name} under {profile}, batch seed {self.seed}"

    def status(self) -> str:
        """One line on the batch's state and progress (G-052): `running: 12 of 50 runs`,
        `cancelled after 12 of 50 runs`, `50 runs, 0 capped, mean codelets 52.3`."""
        if self.state == "idle":
            return "no batch yet: choose a count and press Run batch"
        done, count = self.done, self.count
        if self.state == "running":
            return f"running: {done} of {count} runs"
        if self.state == "error":
            return f"the batch failed after {done} of {count} runs: {self.error}"
        survey = self.survey()
        head = f"cancelled after {done} of {count} runs" if self.state == "cancelled" \
            else f"{done} runs"
        if survey is None:
            return head
        return f"{head}, {survey.capped} capped, mean codelets {survey.codelets:.1f}"

    def footer(self) -> str:
        """The line under the table (G-052): the runs, the batch seed, the capped runs,
        the chi-square against French's n [F95 p. 114], or that he prints no set."""
        survey = self.survey()
        if survey is None:
            return ""
        chi = self.chi_square()
        if chi is None:
            return (f"{survey.runs} runs of {survey.problem} under {survey.profile} (batch "
                    f"seed {survey.batch_seed}), {survey.capped} capped, mean codelets "
                    f"{survey.codelets:.1f}; French prints no result set for this problem.")
        statistic, df, p = chi
        return (f"{survey.runs} runs of {survey.problem} under {survey.profile} (batch seed "
                f"{survey.batch_seed}), {survey.capped} capped, mean codelets "
                f"{survey.codelets:.1f}; chi-square {statistic:.2f} on {df} df, p = {p:.3f}; "
                f"French's n = {ST.FRENCH_RUNS}.")

    def french_title(self) -> str:
        """French's heading with its page, or that he prints no set (G-052)."""
        rs = self.french()
        if rs is None:
            return "" if self.problem is None else "French prints no result set for this problem"
        return set_title(rs)


def row_text(row: ResultRow) -> str:
    """The hover text of a row or a bar (G-052): the label, what it names, both
    frequencies, the z, both structures and both codelet counts, and the objects behind
    ours."""
    lines = [f"{row.label}: {row.names}",
             f"frequency: French {number(row.french_frequency)} %, ours {row.frequency:.1f} % "
             f"({row.count} runs)" + ("" if row.z is None else f", z {row.z:+.2f}"),
             f"average structure: French {number(row.french_structure)}, ours "
             f"{number(row.structure, 1)}",
             f"codelets run: French {number(row.french_codelets)}, ours "
             f"{number(row.codelets, 1)}"]
    if row.answers:
        lines.append("ours: " + ", ".join(f"{a} {n}" for a, n in row.answers))
    return "\n".join(lines)


def bars_layout(rows, width, height, title="", margin=(44, 18, 10, 34)) -> BarsLayout:
    """The bar graph's geometry for a size, as a pure function (G-052): one pair of bars
    per row, French's then ours, on a 0-100 axis of percent of runs [F95 p. 118 (img)],
    the percentage printed above each bar, the label under the pair and the title under
    the graph; the plot box inside `margin` (left, top, right, bottom)."""
    left, top, right, bottom = margin
    box = (left, top, max(1.0, width - left - right), max(1.0, height - top - bottom))
    bx, by, bw, bh = box
    bars, labels = [], []
    n = max(1, len(rows))
    slot = bw / n
    bar_w = min(28.0, slot * 0.34)
    for i, row in enumerate(rows):
        centre = bx + slot * (i + 0.5)
        for k, (who, value) in enumerate((("French", row.french_frequency), ("ours", row.frequency))):
            x = centre - bar_w + k * bar_w
            if value is None:
                bars.append(Bar(row.label, who, None, x, by + bh, bar_w, 0.0, "-"))
            else:
                h = bh * min(100.0, max(0.0, value)) / 100.0
                text = f"{value:.0f}" if who == "French" else f"{value:.1f}"
                bars.append(Bar(row.label, who, value, x, by + bh - h, bar_w, h, text))
        labels.append((row.label, centre, by + bh + 4))
    ticks = tuple((pct, by + bh - bh * pct / 100.0) for pct in (0, 25, 50, 75, 100))
    return BarsLayout(box, tuple(bars), tuple(labels), ticks, title)


class BatchWorker:
    """The batch on its own thread (G-052; Numbo GUI, `run_controller.py`'s worker):
    `start` runs `count` runs of the problem with `run_problem` on the seeds of
    `run_seeds(seed, count)`, calling `on_result(result)` after each on the worker and
    `on_done(cancelled, error)` at the end; `cancel` stops it between two runs. The
    engine lock is held for the batch's duration, so a live run and a batch exclude each
    other (`EngineBusy`)."""

    def __init__(self, on_result=None, on_done=None):
        self.on_result = on_result
        self.on_done = on_done
        self.thread = None
        self._cancel = threading.Event()

    def is_alive(self) -> bool:
        """Whether the batch's thread is running (G-052)."""
        return self.thread is not None and self.thread.is_alive()

    def start(self, problem: Problem, config: Config, seed: int, count: int) -> None:
        """Start the batch; `EngineBusy` when a batch or a live run holds the engine
        (G-052). The inputs are checked here, on the caller's thread."""
        if not isinstance(problem, Problem) or not isinstance(config, Config):
            raise TypeError("a batch wants a Problem and a Config")
        if not self.wait(JOIN_TIMEOUT):
            raise rc.EngineBusy("the previous batch did not stop")
        seeds = run_seeds(int(seed), int(count))
        lock = rc.engine_lock()
        if not lock.acquire(blocking=False):
            raise rc.EngineBusy("the engine is busy: another run or a batch is running")
        self._cancel.clear()
        self.thread = threading.Thread(target=self._work, name="tabletop-batch",
                                       args=(problem, config, seeds, lock), daemon=True)
        self.thread.start()

    def cancel(self) -> None:
        """Ask the batch to stop after the run in progress (G-052)."""
        self._cancel.set()

    def wait(self, timeout=None) -> bool:
        """Wait for the thread to end; whether it has (G-052)."""
        if self.thread is None or self.thread is threading.current_thread():
            return True
        self.thread.join(timeout)
        return not self.thread.is_alive()

    def stop(self, timeout=JOIN_TIMEOUT) -> bool:
        """Cancel and wait (G-052)."""
        self.cancel()
        return self.wait(timeout)

    def _work(self, problem, config, seeds, lock):
        cancelled, error = False, None
        try:
            for s in seeds:
                if self._cancel.is_set():
                    cancelled = True
                    break
                result = run_problem(problem, config, s)
                if self.on_result is not None:
                    self.on_result(result)
        except Exception as exception:   # a broken input, or a bug: reported, not raised
            error = exception
        finally:
            lock.release()
        if self.on_done is not None:
            self.on_done(cancelled, error)
