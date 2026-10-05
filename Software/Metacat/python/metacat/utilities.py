"""utilities.ss: Metacat's general utilities.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from utilities.ss, function for function and
in the file's order, with racket/utilities.rkt as a worked translation.

Representations (docs/python-translation-plan.md): symbols and strings are str,
proper lists are Python lists (never mutated once built), vectors and tables are
chez.Vector, #f is False.  Only #f is false in Scheme: every test on a value
that may be 0, '() or "" is written `is False` / `is not False`.  Arithmetic
that may meet a flonum or an exact quotient goes through chez.add/mul/div...

The object procedures (base-object, tell, report-error-and-halt, tell-all,
delegate, delegate-to-all) live in objects.py and are re-exported here; a run's
driver replaces objects.report_error_and_halt.  The engine never imports tkinter.
"""
from __future__ import annotations

import sys
import time
from fractions import Fraction

from metacat import chez
from metacat import objects
from metacat.objects import (base_object, delegate, delegate_to_all, procedure_p,  # noqa: F401
                             report_error_and_halt, tell, tell_all)
from metacat.chez import add, sub, mul, div, max_, min_, Vector

_F = Fraction


def _null_p(x) -> bool:
    """Chez: null?"""
    return (type(x) is list or type(x) is tuple) and len(x) == 0


def _list_p(x) -> bool:
    """Chez: list? (a Python list or tuple; not a vector or a dotted pair)"""
    t = type(x)
    return t is list or t is tuple


def _pair_p(x) -> bool:
    """Chez: pair?"""
    t = type(x)
    return ((t is list or t is tuple) and len(x) > 0) or t is chez.Pair


def _car_error(x):
    raise chez.SchemeError("car", "~s is not a pair", x)


def compose(f, *l):
    """utilities.ss: compose"""
    if not l:
        return f
    g = compose(*l)
    return lambda x: f(g(x))


def print_(*l):
    """utilities.ss: print.  Objects print themselves; other values are
    displayed, one per line."""
    for obj in flatten(list(l)):
        if procedure_p(obj):
            tell(obj, "print")
        else:
            chez.printf("~a~%", obj)


def say_object(x):
    """utilities.ss: say-object"""
    return chez.printf("~a", tell(x, "print-name") if procedure_p(x) else x)


def ask(*l):
    """utilities.ss: ask.  Displays l, then reads a line: 'nothing for an empty
    line, else the first datum on it (symbols and numbers; the original's REPL
    prompt, not used by a run)."""
    for x in l:
        chez.display(x)
    sys.stdout.flush()
    line = sys.stdin.readline()
    if line == "" or line[0] == "\n":
        return "nothing"
    token = line.split()[0]
    number = chez.string_to_number(token)
    return token.lower() if number is False else number


def type_tester(type_):
    """utilities.ss: type-tester"""
    def tester(obj):
        """utilities.ss: type-tester (the tester it makes)"""
        return tell(obj, "object-type") == type_
    return tester


bond_p = type_tester("bond")
letter_p = type_tester("letter")
group_p = type_tester("group")
bridge_p = type_tester("bridge")
concept_mapping_p = type_tester("concept-mapping")
rule_p = type_tester("rule")
description_p = type_tester("description")
workspace_string_p = type_tester("workspace-string")
workspace_p = type_tester("workspace")
answer_description_p = type_tester("answer-description")
snag_description_p = type_tester("snag-description")

_EVENT_TYPES = ["generic-event", "answer-event", "clamp-event", "concept-activation-event",
                "group-event", "rule-event", "concept-mapping-event", "snag-event"]


def event_p(obj):
    """utilities.ss: event?  (member's value: the tail of the type list, or #f)"""
    return chez.member(tell(obj, "object-type"), _EVENT_TYPES)


def slipnode_p(obj):
    """utilities.ss: slipnode?"""
    return procedure_p(obj) and tell(obj, "object-type") == "slipnode"


def vertical_bridge_p(obj):
    """utilities.ss: vertical-bridge?"""
    return bridge_p(obj) and tell(obj, "get-orientation") == "vertical"


def horizontal_bridge_p(obj):
    """utilities.ss: horizontal-bridge?"""
    return bridge_p(obj) and tell(obj, "get-orientation") == "horizontal"


def exists_p(x):
    """utilities.ss: exists?  chez: only #f is false (0 and '() exist)."""
    # chez: only #f is false (docs/python-translation-plan.md, "Booleans and truthiness")
    return x is not False


def all_exist_p(l):
    """utilities.ss: all-exist?"""
    return chez.andmap(exists_p, l)


def all_same_p(l):
    """utilities.ss: all-same?  By eq?: two flonums computed apart are not
    the same (fixture all-same)."""
    if _null_p(l):
        return True
    head = l[0]
    # chez: eq? on flonums is identity (fixture all-same)
    return chez.andmap(lambda x: chez.eq_p(x, head), l)


def compress(l):
    """utilities.ss: compress"""
    return filter_(exists_p, l)


def map_compress(proc, l):
    """utilities.ss: map-compress"""
    return map_filter(proc, exists_p, l)


def flatmap(proc, l):
    """utilities.ss: flatmap.  chez: map's order of application."""
    out = []
    # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
    for part in chez.map_(proc, l):
        out.extend(part)
    return out


# The original saves Chez's rounding procedures, then redefines them to return
# exact integers (chez.exact_round rounds half to even, as Chez does).
scheme_truncate = chez.truncate
scheme_ceiling = chez.ceiling
scheme_floor = chez.floor_
scheme_round = chez.round_


def truncate(n):
    """utilities.ss: truncate (exact)"""
    return chez.exact_truncate(n)


def ceiling(n):
    """utilities.ss: ceiling (exact)"""
    return chez.exact_ceiling(n)


def floor(n):
    """utilities.ss: floor (exact)"""
    return chez.exact_floor(n)


def round_(n):
    """utilities.ss: round (exact; half to even)"""
    return chez.exact_round(n)


def round_to_10ths(n):
    """utilities.ss: round-to-10ths"""
    return chez.inexact(div(round_(mul(n, 10)), 10))


def round_to_100ths(n):
    """utilities.ss: round-to-100ths"""
    return chez.inexact(div(round_(mul(n, 100)), 100))


def round_to_1000ths(n):
    """utilities.ss: round-to-1000ths"""
    return chez.inexact(div(round_(mul(n, 1000)), 1000))


def square(x):
    """utilities.ss: ^2"""
    return mul(x, x)


def cube(x):
    """utilities.ss: ^3"""
    return mul(x, x, x)


def sort_wrt_order(l, order):
    """utilities.ss: sort-wrt-order (Chez's sort)"""
    # chez: Chez's sort algorithm and predicate calls (anomalies: "sort, remq, for-each and one-armed if differ")
    return chez.sort(lambda v1, v2: list_index(order, v1) < list_index(order, v2), l)


def sort_by_method(method_name, pred_p, l):
    """utilities.ss: sort-by-method (Chez's sort and its sequence of
    predicate calls; fixture sort-by-method-order).  The two tells are
    arguments of one call: Python makes them left to right, which only matters
    for methods with effects, and the model's sort keys have none."""
    # chez: Chez's sort algorithm and predicate calls (anomalies: "sort, remq, for-each and one-armed if differ")
    # speed (item 12): each element's key is asked once, when the original first
    # asks it, and remembered (the keys are pure); the sort runs on positions
    keys = {}

    def key(i):
        k = keys.get(i, _NO_KEY)
        if k is _NO_KEY:
            k = keys[i] = tell(l[i], method_name)
        return k
    order = chez.sort(lambda i, j: pred_p(key(i), key(j)), list(range(len(l))))
    return [l[i] for i in order]


_NO_KEY = object()


def ascending_index_list(n):
    """utilities.ss: ascending-index-list (0-based indices)"""
    if n <= 0:
        # 1.2: (accumulate (sub1 n) '()) counts down from n - 1 and never
        # reaches zero, so n = 0 loops forever (porting-notes.md, item 03;
        # the Racket port keeps it too).  Reached by for-each-vector-element*
        # on an empty vector.
        i = n - 1
        while True:
            i -= 1
    return list(range(n))


def descending_index_list(n):
    """utilities.ss: descending-index-list"""
    return list(range(n - 1, -1, -1))


def symbol_to_letter_categories(sym):
    """utilities.ss: symbol->letter-categories.  The original evals each
    plato- name, a top-level-value lookup here."""
    return chez.map_(lambda c: chez.top_level_value("plato-" + c), list(sym))


def char_index(char, string):
    """utilities.ss: char-index"""
    return string.find(char)


def _char_upcase(c):
    u = c.upper()
    return u if len(u) == 1 else c


def _char_downcase(c):
    d = c.lower()
    return d if len(d) == 1 else c


def string_upcase(s):
    """utilities.ss: string-upcase (character by character)"""
    return "".join(_char_upcase(c) for c in s)


def string_downcase(s):
    """utilities.ss: string-downcase (character by character)"""
    return "".join(_char_downcase(c) for c in s)


def capitalize_string(s):
    """utilities.ss: capitalize-string"""
    if s == "":
        return ""
    return _char_upcase(s[0]) + s[1:]


def quoted_string(string):
    """utilities.ss: quoted-string"""
    return chez.format_('"~a"', tell(string, "print-name"))


def make_table(row_dim, column_dim, *optional_args):
    """utilities.ss: make-table (a vector of rows; #f unless a value is given)"""
    initial_value = False if not optional_args else optional_args[0]
    return Vector([Vector([initial_value] * column_dim) for _ in range(row_dim)])


def row_dimension(table):
    """utilities.ss: row-dimension"""
    return len(table)


def column_dimension(table):
    """utilities.ss: column-dimension"""
    return len(table[0])


def table_to_list(table):
    """utilities.ss: table->list"""
    return [x for row in table for x in row]


def table_set_bang(table, i, j, value):
    """utilities.ss: table-set!"""
    table[i][j] = value


def table_ref(table, i, j):
    """utilities.ss: table-ref"""
    return table[i][j]


def vector_increment_bang(v, i, value):
    """utilities.ss: vector-increment!"""
    v[i] = add(value, v[i])


def initialize_row_bang(table, i, value):
    """utilities.ss: initialize-row!"""
    row = table[i]
    for j in range(len(row)):
        row[j] = value


def initialize_column_bang(table, j, value):
    """utilities.ss: initialize-column!"""
    for row in table:
        row[j] = value


def get_row(table, i):
    """utilities.ss: get-row"""
    return list(table[i])


def get_column(table, j):
    """utilities.ss: get-column"""
    return [row[j] for row in table]


def copy_vector_contents_bang(source_vector, target_vector):
    """utilities.ss: copy-vector-contents!"""
    for i in range(len(source_vector)):
        target_vector[i] = source_vector[i]


def copy_table_contents_bang(source_table, target_table):
    """utilities.ss: copy-table-contents!"""
    for i in range(len(source_table)):
        for j in range(len(source_table[i])):
            target_table[i][j] = source_table[i][j]


def add_scaled_vector_bang(target, source, scalar):
    """utilities.ss: add-scaled-vector!"""
    for i in range(len(target)):
        target[i] = add(target[i], mul(scalar, source[i]))


def rotate_90_degrees_clockwise(old_table):
    """utilities.ss: rotate-90-degrees-clockwise"""
    row_dim = row_dimension(old_table)
    column_dim = column_dimension(old_table)
    new_table = make_table(column_dim, row_dim)
    for i in range(row_dim):
        for j in range(len(old_table[i])):
            new_table[j][row_dim - i - 1] = old_table[i][j]
    return new_table


def sum_(l):
    """utilities.ss: sum ((apply + l), left to right)"""
    return add(*l)


def product(l):
    """utilities.ss: product"""
    return mul(*l)


def average(*l):
    """utilities.ss: average.  (average list) or (average x ...)"""
    if _null_p(l[0]):
        return 0
    if len(l) == 1 and _list_p(l[0]):
        return div(sum_(l[0]), len(l[0]))
    return div(sum_(l), len(l))


def weighted_average(values, weights):
    """utilities.ss: weighted-average"""
    if chez.zero_p(sum_(weights)):
        return 0
    return div(sum_(chez.map_(mul, weights, values)), sum_(weights))


_LN10 = chez.log(10)


def log10(x):
    """utilities.ss: log10 (nudged by 1e-15 away from zero)"""
    result = div(chez.log(x), _LN10)
    return add(result, mul(sgn(result), 1E-15))


def sgn(x):
    """utilities.ss: sgn (1 for zero and -0.0)"""
    return -1 if chez.negative_p(x) else 1


def list_index(l, x):
    """utilities.ss: list-index (by eq?; an error if x is missing, from car of '())"""
    for i, y in enumerate(l):
        if chez.eq_p(y, x):
            return i
    _car_error([])


def cd(x):
    """utilities.ss: cd"""
    return tell(x, "get-conceptual-depth")


_START = time.monotonic()


def _real_time():
    """Chez: real-time (milliseconds of elapsed real time)"""
    return int((time.monotonic() - _START) * 1000)


def randomize():
    """utilities.ss: randomize (seeds the generator from the clock).
    1.2: a clock value whose 5/3 power is a multiple of 2^32 gives seed 0,
    which random-seed rejects."""
    upper_bound = chez.expt(2, 32)
    return chez.random_seed(chez.modulo(round_(chez.expt(_real_time(), _F(5, 3))), upper_bound))


def prob_p(p):
    """utilities.ss: prob?  One (random 1.0) unless p <= 0 or p >= 1."""
    if p <= 0.0:
        return False
    if p >= 1.0:
        return True
    return p > chez.random(1.0)


def rough(n):
    """utilities.ss: ~  (n plus or minus a random amount up to about sqrt n;
    the size is drawn before the sign)."""
    # chez: the size is drawn (let binding) before the sign (body)
    delta = chez.random(chez.add1(round_(chez.sqrt(n))))
    if prob_p(0.5):
        return add(n, delta)
    return sub(n, delta)


def random_pick(l):
    """utilities.ss: random-pick (#f for '())"""
    if _null_p(l):
        return False
    return nth(chez.random(len(l)), l)


def stochastic_pick(l, weights):
    """utilities.ss: stochastic-pick"""
    weight_sum = sum_(weights)
    if chez.zero_p(weight_sum):
        return random_pick(l)
    return nth(weighted_index(chez.random(chez.inexact(weight_sum)), weights), l)


def stochastic_pick_by_method(object_list, *message):
    """utilities.ss: stochastic-pick-by-method"""
    weights = tell_all(object_list, *message)
    return stochastic_pick(object_list, weights)


def weighted_index(w, weights):
    """utilities.ss: weighted-index.  An error past the end (car of '())."""
    # speed (item 12): an index, not a copy of the rest at each step
    n = len(weights)
    i = 0
    while True:
        if i >= n:
            _car_error([])
        if w < weights[i]:
            return i
        w = sub(w, weights[i])
        i += 1


def stochastic_select(selection_list):
    """utilities.ss: stochastic-select.  selection-list is ((<weight> <obj> ...) ...)."""
    weight_sum = sum_([first(e) for e in selection_list])
    if chez.zero_p(weight_sum):
        return random_pick(selection_list)
    return weighted_select(chez.random(chez.inexact(weight_sum)), selection_list)


def weighted_select(w, selection_list):
    """utilities.ss: weighted-select"""
    while True:
        if not selection_list:
            _car_error(selection_list)
        first_element = selection_list[0]
        if w < first_element[0]:
            return first_element
        w = sub(w, first_element[0])
        selection_list = selection_list[1:]


def stochastic_filter(proc, l):
    """utilities.ss: stochastic-filter (first to last, one prob? each)"""
    return [x for x in l if prob_p(proc(x))]


def hundred_minus(n):
    """utilities.ss: 100-"""
    return sub(100, n)


def ten_minus(x):
    """utilities.ss: 10-"""
    return sub(10, x)


def one_minus(x):
    """utilities.ss: 1-"""
    return sub(1, x)


def times_100(x):
    """utilities.ss: 100*"""
    return round_(mul(100, x))


def percent(n):
    """utilities.ss: %"""
    return div(n, 100)


def percent_20(n):
    """utilities.ss: 20%"""
    return mul(_F(1, 5), n)


def percent_40(n):
    """utilities.ss: 40%"""
    return mul(_F(2, 5), n)


def percent_80(n):
    """utilities.ss: 80%"""
    return mul(_F(4, 5), n)


def sigmoid(beta, m):
    """utilities.ss: sigmoid.  f on 0..100 with range 0..1, f(m) = 0.5, beta
    the steepness at m."""
    def f(x):
        return div(1, add(1, chez.exp(mul(_F(1, 25), beta, sub(m, x)))))
    return f


def clip_function(lower_bound, upper_bound):
    """utilities.ss: clip-function"""
    def clip(x):
        return max_(lower_bound, min_(x, upper_bound))
    return clip


def flatten(l):
    """utilities.ss: flatten (the leaves, in order; '() vanishes, a dotted
    pair is a leaf)"""
    leaves = []

    def walk(l):
        for x in l:
            if _list_p(x):
                walk(x)
            else:
                leaves.append(x)
    walk(l)
    return leaves


def select_longest_list(l):
    """utilities.ss: select-longest-list"""
    result = select_extreme(max_, len, l)
    if exists_p(result):
        return result
    return []


def select_extreme(min_or_max, proc, l):
    """utilities.ss: select-extreme.  chez: map's order for proc; the first
    element whose value is eqv? to the extreme."""
    if _null_p(l):
        return False
    # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
    values = chez.map_(proc, l)
    return chez.assv(min_or_max(*values), [[v, x] for v, x in zip(values, l)])[1]


def maximum(l):
    """utilities.ss: maximum (0 for '())"""
    if _null_p(l):
        return 0
    return max_(*l)


def minimum(l):
    """utilities.ss: minimum (0 for '())"""
    if _null_p(l):
        return 0
    return min_(*l)


def count(predicate_p, l):
    """utilities.ss: count"""
    n = 0
    for x in l:
        if predicate_p(x) is not False:
            n += 1
    return n


def adjacency_map(f, l):
    """utilities.ss: adjacency-map.  chez: two-list map's order."""
    # chez: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
    return chez.map_(f, all_but_last(1, l), l[1:])


def select(predicate_p, l):
    """utilities.ss: select (the first element satisfying predicate?, or #f)"""
    for x in l:
        if predicate_p(x) is not False:
            return x
    return False


def filter_(pred_p, l):
    """utilities.ss: filter"""
    return [x for x in l if pred_p(x) is not False]


def filter_out(pred_p, l):
    """utilities.ss: filter-out"""
    return filter_(lambda x: pred_p(x) is False, l)


def map_leaves(proc, l):
    """utilities.ss: map-leaves (car before cdr; a '() element is a leaf)"""
    out = []
    while True:
        if _null_p(l):
            return out
        if type(l) is chez.Pair:
            head, tail = l.car, l.cdr
        elif _list_p(l):
            head, tail = l[0], l[1:]
        else:
            _car_error(l)
        if _pair_p(head):
            out.append(map_leaves(proc, head))
        else:
            out.append(proc(head))
        l = tail


def filter_map(pred_p, proc, l):
    """utilities.ss: filter-map (filter, then map, element by element)"""
    out = []
    for x in l:
        if pred_p(x) is not False:
            out.append(proc(x))
    return out


def map_filter(proc, pred_p, l):
    """utilities.ss: map-filter (map, then filter, element by element)"""
    out = []
    for x in l:
        value = proc(x)
        if pred_p(value) is not False:
            out.append(value)
    return out


def cross_product(l1, l2):
    """utilities.ss: cross-product"""
    return cross_product_filter_map(lambda x, y: True, lambda x, y: [x, y], l1, l2)


def cross_product_filter(pred_p, l1, l2):
    """utilities.ss: cross-product-filter"""
    return cross_product_filter_map(pred_p, lambda x, y: [x, y], l1, l2)


def cross_product_map(proc, l1, l2):
    """utilities.ss: cross-product-map"""
    return cross_product_filter_map(lambda x, y: True, proc, l1, l2)


def cross_product_filter_map(pred_p, proc, l1, l2):
    """utilities.ss: cross-product-filter-map.  The original's f recurses on
    the rest of l1 before g walks l2, so l1 is processed last to first, l2
    first to last (fixture cross-product-filter-map-order)."""
    result = []
    # chez: f recurses on (rest l1) before g walks l2, so l1 runs last to first
    for x in reversed(l1):
        part = []
        for y in l2:
            if pred_p(x, y) is not False:
                part.append(proc(x, y))
        result = part + result
    return result


def cross_product_map_filter(proc, pred_p, l1, l2):
    """utilities.ss: cross-product-map-filter (l1 last to first, as above)"""
    result = []
    # chez: l1 last to first, as in cross_product_filter_map
    for x in reversed(l1):
        part = []
        for y in l2:
            value = proc(x, y)
            if pred_p(value) is not False:
                part.append(value)
        result = part + result
    return result


def cross_product_ormap(pred_p, l1, l2):
    """utilities.ss: cross-product-ormap"""
    return chez.ormap(lambda x: chez.ormap(lambda y: pred_p(x, y), l2), l1)


def cross_product_andmap(pred_p, l1, l2):
    """utilities.ss: cross-product-andmap"""
    return chez.andmap(lambda x: chez.andmap(lambda y: pred_p(x, y), l2), l1)


def cross_product_for_each(proc, l1, l2):
    """utilities.ss: cross-product-for-each (its value is #f)"""
    def each(x, y):
        proc(x, y)
        return False
    return cross_product_ormap(each, l1, l2)


def make_list_method_procedure(proc):
    """utilities.ss: make-list-method-procedure.
    Example: (filter-meth l 'predicate-method-name? arg1 arg2 ...)"""
    def method_procedure(l, *message):
        """utilities.ss: make-list-method-procedure (the procedure it makes)"""
        return proc(lambda x: tell(x, *message), l)
    return method_procedure


select_meth = make_list_method_procedure(select)
filter_meth = make_list_method_procedure(filter_)
filter_out_meth = make_list_method_procedure(filter_out)
andmap_meth = make_list_method_procedure(chez.andmap)
ormap_meth = make_list_method_procedure(chez.ormap)
count_meth = make_list_method_procedure(count)


def pairwise_map(proc, l):
    """utilities.ss: pairwise-map.  (pairwise-map list '(1 2 3)) ==> ((1 2) (1 3) (2 3))
    chez: in (append (map ...) (pairwise-map (rest l))) the recursive call is
    evaluated first, so the suffixes are mapped from the shortest, each in
    map's order (porting-notes.md; racket/utilities.rkt:720)."""
    parts = []
    # chez: the recursive call before the map (anomalies: "Chez evaluates append's second argument first")
    for i in range(len(l) - 1, -1, -1):
        x = l[i]
        parts.append(chez.map_(lambda y: proc(x, y), l[i + 1:]))
    out = []
    for part in reversed(parts):
        out.extend(part)
    return out


def pairwise_do(proc, l):
    """utilities.ss: pairwise-do (its value is 'done)"""
    for i in range(len(l)):
        for obj in l[i + 1:]:
            proc(l[i], obj)
    return "done"


def pairwise_andmap(pred_p, l):
    """utilities.ss: pairwise-andmap"""
    for i in range(len(l)):
        x = l[i]
        if chez.andmap(lambda y: pred_p(x, y), l[i + 1:]) is False:
            return False
    return True


def intersect(l1, l2):
    """utilities.ss: intersect"""
    return intersect_pred(chez.eq_p, l1, l2)


def intersect_pred(equiv_pred_p, l1, l2):
    """utilities.ss: intersect-pred"""
    return cross_product_filter_map(equiv_pred_p, lambda x, y: x, l1, l2)


def intersect_all(l):
    """utilities.ss: intersect-all"""
    return intersect_all_pred(chez.eq_p, l)


def intersect_all_pred(equiv_pred_p, l):
    """utilities.ss: intersect-all-pred (from the last list backwards)"""
    if _null_p(l):
        return []
    result = l[-1]
    for i in range(len(l) - 2, -1, -1):
        result = intersect_pred(equiv_pred_p, l[i], result)
    return result


def partition(pred_p, l):
    """utilities.ss: partition.  Elements are inserted last to first (the
    recursion on the rest comes first), each into the first class all of whose
    members satisfy (pred? x y), or a new class at the end."""
    classes = []
    # chez: (insert (1st l) (partition (rest l))): the rest is partitioned first
    for x in reversed(l):
        classes = _insert(pred_p, x, classes, None)
    return classes


def _insert(pred_p, x, classes, bound):
    """partition's and bounded-random-partition's insert"""
    for k, cls in enumerate(classes):
        if (bound is None or len(cls) < bound) and \
                chez.andmap(lambda y: pred_p(x, y), cls) is not False:
            return classes[:k] + [[x] + cls] + classes[k + 1:]
    return classes + [[x]]


def bounded_random_partition(pred_p, l, bound):
    """utilities.ss: bounded-random-partition.  Randomly partitions l into
    classes (under pred?) of at most bound elements.  Every element is picked
    first (random-pick, removing it), then they are inserted in the reverse
    order of the picks."""
    picks = []
    while not _null_p(l):
        x = random_pick(l)
        picks.append(x)
        l = _remove_first(x, l)
    classes = []
    # chez: every pick (and draw) happens before the first insert
    for x in reversed(picks):
        classes = _insert(pred_p, x, classes, bound)
    return classes


def _remove_first(x, l):
    for i, y in enumerate(l):
        if chez.eq_p(y, x):
            return l[:i] + l[i + 1:]
    return list(l)


def member_p(a, l):
    """utilities.ss: member? (memq forced to #t or #f)"""
    return chez.memq_p(a, l)


def member_pred_p(pred_p, x, l):
    """utilities.ss: member-pred?"""
    return chez.ormap(lambda y: pred_p(x, y), l)


def member_equal_p(x, l):
    """utilities.ss: member-equal?"""
    return member_pred_p(chez.equal_p, x, l)


def subset_p(set1, set2):
    """utilities.ss: subset?"""
    return chez.andmap(lambda x: member_p(x, set2), set1)


def subset_pred_p(pred_p, set1, set2):
    """utilities.ss: subset-pred?"""
    return chez.andmap(lambda x: member_pred_p(pred_p, x, set2), set1)


def sets_equal_p(set1, set2):
    """utilities.ss: sets-equal?"""
    r = subset_p(set1, set2)
    return r if r is False else subset_p(set2, set1)


def sets_equal_pred_p(pred_p, set1, set2):
    """utilities.ss: sets-equal-pred?"""
    r = subset_pred_p(pred_p, set1, set2)
    return r if r is False else subset_pred_p(pred_p, set2, set1)


def sets_disjoint_p(set1, set2):
    """utilities.ss: sets-disjoint?"""
    return chez.andmap(lambda x: not member_p(x, set2), set1)


def sets_intersect_p(set1, set2):
    """utilities.ss: sets-intersect?"""
    return chez.ormap(lambda x: member_p(x, set2), set1)


def remove_elements_pred(pred_p, elements, l):
    """utilities.ss: remove-elements-pred"""
    return [x for x in l if member_pred_p(pred_p, x, elements) is False]


def remq_elements(elements, l):
    """utilities.ss: remq-elements"""
    return [x for x in l if not member_p(x, elements)]


def remove_elements(elements, l):
    """utilities.ss: remove-elements"""
    return remove_elements_pred(chez.equal_p, elements, l)


def remove_duplicates_pred(pred_p):
    """utilities.ss: remove-duplicates-pred (keeps the last of equivalent elements)"""
    def remove_duplicates(l):
        """utilities.ss: remove-duplicates-pred (the procedure it makes)"""
        return [l[i] for i in range(len(l)) if member_pred_p(pred_p, l[i], l[i + 1:]) is False]
    return remove_duplicates


remq_duplicates = remove_duplicates_pred(chez.eq_p)
remove_duplicates = remove_duplicates_pred(chez.equal_p)


def string_suffix(s, i):
    """utilities.ss: string-suffix"""
    return s[i:]


def reveal(x):
    """utilities.ss: reveal (an object, or the leaves of a list of them, as
    something printable)"""
    if _list_p(x):
        return map_leaves(reveal_obj, x)
    return reveal_obj(x)


def reveal_obj(obj):
    """utilities.ss: reveal-obj.  format-slipnode (rules.ss) is a top-level
    value, as in the original's single top level (anomalies_and_quirks.md,
    "Verbose mode reached an unregistered format-slipnode")."""
    if not procedure_p(obj):
        return obj
    if slipnode_p(obj):
        return string_downcase(chez.top_level_value("format-slipnode")(obj))
    if letter_p(obj) or group_p(obj):
        return tell(obj, "ascii-name")
    return chez.format_("<~a>", tell(obj, "object-type"))


def first(l):
    """utilities.ss: 1st"""
    return l[0]


def second(l):
    """utilities.ss: 2nd"""
    return l[1]


def third(l):
    """utilities.ss: 3rd"""
    return l[2]


def fourth(l):
    """utilities.ss: 4th"""
    return l[3]


def fifth(l):
    """utilities.ss: 5th"""
    return l[4]


def sixth(l):
    """utilities.ss: 6th"""
    return l[5]


def seventh(l):
    """utilities.ss: 7th"""
    return l[6]


def eighth(l):
    """utilities.ss: 8th"""
    return l[7]


def rest(l):
    """utilities.ss: rest (a new list: lists are never mutated)"""
    return l[1:]


def coord(x, y):
    """utilities.ss: coord (make-rectangular)"""
    return chez.make_rectangular(x, y)


def get_first(n, l):
    """utilities.ss: get-first"""
    return list(l[:n])


def sublist(l, m, n):
    """utilities.ss: sublist"""
    return get_first(n - m, l[m:])


def nth(n, l):
    """utilities.ss: nth"""
    return l[n]


def snoc(x, l):
    """utilities.ss: snoc"""
    return list(l) + [x]


def last(l):
    """utilities.ss: last"""
    return l[-1]


def all_but_last(n, l):
    """utilities.ss: all-but-last"""
    return list(l[:len(l) - n])


def x_coord(z):
    """utilities.ss: x-coord (real-part)"""
    return chez.real_part(z)


def y_coord(z):
    """utilities.ss: y-coord (imag-part)"""
    return chez.imag_part(z)


def pause(ms):
    """utilities.ss: pause (SWL's thread-sleep, in milliseconds)"""
    time.sleep(ms / 1000)


# sugar.py's macros call back into this module; importing it here makes either
# import order work.
from metacat import sugar  # noqa: E402,F401
