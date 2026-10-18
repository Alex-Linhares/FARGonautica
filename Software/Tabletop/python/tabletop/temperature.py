"""Temperature: the one law by which temperature softens every choice of the program
(D-060), and the temperature itself, T = α ΔQ/Q + β Δ_WV − γ T_cp, with its four factors
and the windows of six and ten (D-047 to D-050).

"Four factors determine temperature in Tabletop" [F95 p. 90]: the quality of the Worldview,
"measured as a fraction of an optimally good correspondence" [F95 p. 90]; its average rate of
change, "averaged in with a certain number of past rates of change (generally averaged over
the last six)" [F95 p. 90]; the number of changes of the Worldview, "the more often the
Worldview changes, the higher the temperature" [F95 p. 91]; and the number of competitions,
since "The program, like people, must eventually tire of trying to decide, and that is why
temperature slowly falls as a function of the number of competitions." [F95 p. 91] The
equation is T = α ΔQ/Q + β Δ_WV − γ T_cp [F95 p. 91 (img)], Δ_WV being "the number of
changes in the Worldview in the last ten competitons" [F95 p. 91] and T_cp "the total number
of competitions" [F95 p. 91]. `Temperature` holds the value and its history; the run loop
feeds it the Worldview's quality and the two logs at every update (D-050), so this module
knows nothing of the Workspace.

"temperature affects not only codelet urgencies but all of the probabilistic processes of the
program" [F95 p. 93]: every selection "approaches determinism as the temperature approaches 0,
is completely unbiased (i.e., uniformly distributed random) at 100" [F95 p. 93]. The exponent
a runs from 6 at T = 0 to 0.1 at T = 100 [F95 p. 92], and a weighted choice raises its
weights to the a-th power; a yes-or-no test with chance q is the same law with the two
weights q and 1 - q (D-060). Coins that are perceptual rather than decisions (the region
blur, the proximity link) are not softened; see D-060's list.
"""

from dataclasses import dataclass

_INFINITY = float("inf")


def exponent_at(temperature: float, config) -> float:
    """The exponent a(T): linear from `exponent_at_zero` (6) at T = 0 to
    `exponent_at_hundred` (0.1) at T = 100, the only line through French's two points
    [F95 p. 92] (D-060); T is clipped to [0, 100]."""
    t = min(100.0, max(0.0, temperature))
    a0, a100 = config.exponent_at_zero, config.exponent_at_hundred
    return a0 + (a100 - a0) * t / 100.0


def softened_weights(weights, exponent: float) -> list:
    """The weights of a choice raised to the exponent a (D-060): at a = 6 the largest weight
    is almost certain, at a = 0.1 every item is nearly equally likely [F95 p. 93]. Weights at
    or below 0 stay 0. Where a weight to the exponent would overflow a float (the `rho`
    bench's variant below 1 multiplies a codelet's urgency by its generation's power, the
    urgency explosion of [F95 p. 82]) the weights are first divided by the largest, which
    leaves every chance the same; a run that does not overflow is not touched. An infinite
    weight (that urgency past generation 1,023 at `rho` 0.5) takes the limit of the same
    rule: the infinite weights share all the chance, the rest none (D-060 as revised, D-114)."""
    try:
        softened = [w ** exponent if w > 0 else 0.0 for w in weights]
        if _INFINITY not in softened:
            return softened
    except OverflowError:
        pass
    top = max(weights)
    if top == _INFINITY:
        return [1.0 if w == _INFINITY else 0.0 for w in weights]
    return [(w / top) ** exponent if w > 0 else 0.0 for w in weights]


def chances_of(weights, exponent: float) -> list:
    """The chance of each weight at the exponent a: w^a over the total of w^a [F95 p. 92
    (img)] (D-060), the share of the total at a = 1 [F95 p. 77]; equal chances when the
    total is 0, and none for no weights. The Coderack's draw reads it through the engine
    seam (D-112: the compiled engine has it in C, mirroring this function)."""
    softened = softened_weights(weights, exponent)
    total = sum(softened)
    if total <= 0:
        return [1.0 / len(softened)] * len(softened) if softened else []
    return [w / total for w in softened]


def pass_probability(chance: float, exponent: float) -> float:
    """The probability that a yes-or-no test with chance q passes at exponent a:
    q^a / (q^a + (1 - q)^a) (D-060, the two-outcome form of the law). A chance of 0 or 1 is
    not softened: nothing is certain at any temperature except what is certain at all."""
    if chance <= 0.0:
        return 0.0
    if chance >= 1.0:
        return 1.0
    yes, no = chance ** exponent, (1.0 - chance) ** exponent
    return yes / (yes + no)


def passes(chance: float, exponent: float, rng) -> bool:
    """One coin through the run's generator at `pass_probability` (D-060); no draw is spent
    when the outcome is certain."""
    p = pass_probability(chance, exponent)
    if p >= 1.0:
        return True
    if p <= 0.0:
        return False
    return rng.coin(p)


# ---------------------------------------------------------------------------------------------
# The temperature itself (D-047 to D-050).


def clamp(value: float, low: float = 0.0, high: float = 100.0) -> float:
    """Clip to [0, 100], the thermometer's range [F95 p. 92] (D-049: the equation is not
    clamped on the page; the exponent a is defined for 0-100)."""
    return min(high, max(low, value))


@dataclass(frozen=True)
class TemperatureUpdate:
    """One recomputation of T (D-050; `docs/engine-api.md`, `TemperatureUpdated`): the
    clock, who asked (`start`, `schedule`, `competition`, `change`), the new `temperature`,
    the four inputs `q`, `dq`, `dwv`, `tcp`, the exponent `a` at the new value, and
    `held`, True while T is still held at its initial value (D-049)."""

    clock: int
    by: str
    temperature: float
    q: float
    dq: float
    dwv: int
    tcp: int
    a: float
    held: bool


class Temperature:
    """The computational temperature of a run, which measures how satisfied the program is
    with the representation it has built [F95 pp. 90-91] (D-047 to D-050):
    `value`, T on 0-100; `q`, the Worldview's quality at the last update (D-049); `rates`,
    the rate of change recorded at every update, averaged into `dq` with the weight
    1 / `dq_window` (D-048); `dq`, `dwv`, `tcp`, the three other inputs at the last update; and
    `exponent`, a(T) for the choices of the law (D-060). T starts at
    `initial_temperature` and holds there until the first change of the Worldview, after
    which the formula applies at every update (D-049); `temperature_fixed` >= 0 clamps T
    for the whole run, the lesion of FCCA criterion 11 [FCCA p. 365] (D-047)."""

    def __init__(self, config):
        self.config = config
        self.value = config.initial_temperature
        self.q = config.q_floor
        self.rates: list[float] = []
        self.dq = 0.0
        self.dwv = 0
        self.tcp = 0
        self.updates = 0
        self.last: TemperatureUpdate | None = None
        if config.temperature_fixed >= 0:
            self.value = clamp(config.temperature_fixed)

    @property
    def exponent(self) -> float:
        """a(T) at the current value [F95 p. 92] (D-060)."""
        return exponent_at(self.value, self.config)

    def rate(self, q: float) -> float:
        """The rate of change of quality at this update, "the difference between the quality
        rating of the current Worldview and the rating of the previous Worldview" [F95
        p. 90]: q less the quality at the previous update, absolute unless `dq_signed`
        (D-048); 0 when nothing changed."""
        difference = q - self.q
        return difference if self.config.dq_signed else abs(difference)

    def average_rate(self, rate: float) -> float:
        """ΔQ after this update's rate is "averaged in with a certain number of past rates
        of change (generally averaged over the last six)" [F95 p. 90]: the running average
        ΔQ + (rate − ΔQ) / `dq_window`, so that a change raises ΔQ by a sixth of itself
        and, when nothing follows, ΔQ falls by a sixth at every update, "the value of ΔQ
        will decrease over time" [F95 p. 92 (img)] (D-048)."""
        return self.dq + (rate - self.dq) / self.config.dq_window

    def changes_in_window(self, competition_clocks, change_clocks) -> int:
        """Δ_WV, "the number of changes in the Worldview in the last ten competitons" [F95
        p. 91]: the changes since the competition `wv_window` competitions back; every
        change so far while fewer than that many competitions have been held (D-048)."""
        window = self.config.wv_window
        if len(competition_clocks) <= window:
            return len(change_clocks)
        since = competition_clocks[-window]
        return sum(1 for c in change_clocks if c >= since)

    def formula(self, q: float, dq: float, dwv: int, tcp: int) -> float:
        """T = α ΔQ/Q + β Δ_WV − γ T_cp [F95 p. 91 (img)], clamped to [0, 100] (D-047,
        D-049); Q is floored at `q_floor` so that the first term is finite."""
        cfg = self.config
        q = max(cfg.q_floor, q)
        return clamp(cfg.temperature_alpha * dq / q + cfg.temperature_beta * dwv
                     - cfg.temperature_gamma * tcp)

    def update(self, q: float, competition_clocks, change_clocks, by: str,
               clock: int) -> TemperatureUpdate:
        """Recompute T (D-050) from the Worldview's quality `q` (D-049), the clocks of the
        competitions held and of the Worldview's changes (D-048). One rate is appended to
        the window at every update, so that "If no additional structure is found, even if Q
        is high, the value of ΔQ will decrease over time" [F95 p. 92 (img)]. T is held at
        its initial value until the first change (D-049) and at `temperature_fixed` when
        that lesion is on (D-047)."""
        cfg = self.config
        rate = self.rate(q)
        self.rates.append(rate)
        self.q = max(cfg.q_floor, min(1.0, q))
        self.dq = self.average_rate(rate)
        self.dwv = self.changes_in_window(competition_clocks, change_clocks)
        self.tcp = len(competition_clocks)
        held = not change_clocks
        if cfg.temperature_fixed >= 0:
            self.value = clamp(cfg.temperature_fixed)
        elif held:
            self.value = cfg.initial_temperature
        else:
            self.value = self.formula(self.q, self.dq, self.dwv, self.tcp)
        self.updates += 1
        self.last = TemperatureUpdate(clock, by, self.value, self.q, self.dq, self.dwv,
                                      self.tcp, self.exponent, held)
        return self.last
