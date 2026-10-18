"""The Slipnet's definition as data: the 63 nodes with their levels and screen labels (the
48 of the reconstruction and the 15 role nodes, D-080), and the link table with a length
and a source for every row (D-007 to D-010, D-080).

"The Tabletop Slipnet contains forty-six nodes representing the following concepts" [F95
p. 62], and the list that follows has forty-seven entries, the same forty-seven FCCA lists
[FCCA p. 381]; the reconstruction keeps the list, under French's names, and adds "isolated",
the node the static evaluation of a correspondence uses [F95 p. 68] and no list names
(D-007). Each node has one of "three levels of abstractness" [F95 pp. 61-62] (D-008): the
object types at level 1, their categories and the relations between objects at level 2, and
the relations between relations and the superordinates of level-2 concepts at level 3 [F95
p. 62]. The screen draws 41 of the nodes as a grid of boxes [F95 p. 98 (img)]; each node
carries the label the screen prints, and the six nodes the chapter 4 grid lacks (the six
object types absent from that configuration) are marked as not on the screen, as are the
15 role nodes of the re-interpretation (D-080): "Many others could easily be added, but such
additions would require no changes to the architecture of the program." [FCCA p. 381]; the
roles add `role` with its eleven kinds, and `row`, `column` and `corner` under `position`,
none on French's screen.

"Links between nodes in the Slipnet are of only two types: ISA links ... and labeled links."
[F95 p. 56] "Every link in the Slipnet also has a length." [F95 p. 56] No page gives the link
table or a length; the rows below keep every link the sources name, with its page, and mark
the rest `domain judgement` (D-009); the lengths are D-010's, round numbers in a narrow band.
Rows that follow the chapter 4 screen dumps say so: the dumps are the one direct evidence of
which concepts are joined (activation reaches "liquid holder" and "crockery" from "cup"
before any codelet runs [F95 p. 96 (img); F95 p. 97]; "closed obj" rises with "open obj"
while "opposite" is near the ceiling; "food holder" never leaves the floor and "vert" stays
under 8 while "fork", "spoon" and "horiz" are active [F95 pp. 98-107 (img)]).

This module holds data only; `slipnet.py` gives it behaviour.
"""

from dataclasses import dataclass

LEVELS = (1, 2, 3)  # "three levels of abstractness" [F95 pp. 61-62]


@dataclass(frozen=True)
class NodeDef:
    """One concept of the Slipnet (D-007, D-008): its name as French's list gives it [F95
    p. 62], the label the screen prints [F95 p. 98 (img)], its group in the list, its level
    [F95 p. 62], FCCA's name where it differs [FCCA p. 381], and whether the chapter 4 grid
    shows it."""

    name: str
    label: str
    group: str
    level: int
    fcca_name: str = ""
    on_screen: bool = True


def _n(name, label, group, level, fcca_name="", on_screen=True):
    return NodeDef(name, label, group, level, fcca_name, on_screen)


# The 47 of French's list [F95 p. 62], in its order, grouped as SPEC.md 3.1 tabulates it, with
# the screen's labels [F95 p. 98 (img)] and FCCA's names [FCCA p. 381]; then "isolated" (D-007).
# Levels follow p. 62's three heuristics (D-008).
NODES = (
    # Objects (11): "the categories of the actual objects on the table ... are the least
    # abstract" [F95 p. 62].
    _n("cup", "cup", "object", 1),
    _n("saucer", "saucer", "object", 1, on_screen=False),
    _n("plate", "plate", "object", 1, on_screen=False),
    _n("knife", "knife", "object", 1),
    _n("fork", "fork", "object", 1),
    _n("spoon", "spoon", "object", 1),
    _n("saltshaker", "saltshaker", "object", 1, "salt shaker", on_screen=False),
    _n("pepper shaker", "pepper shkr", "object", 1, on_screen=False),
    _n("small glass", "small glass", "object", 1),
    _n("big glass", "big glass", "object", 1, on_screen=False),
    _n("soup bowl", "soup bowl", "object", 1, on_screen=False),
    # Categories (10): "those that contain the first-level categories" [F95 p. 62].
    _n("sharp object", "sharp obj", "category", 2),
    _n("silverware", "silverware", "category", 2),
    _n("crockery", "crockery", "category", 2),
    _n("liquid holder", "liq holder", "category", 2),
    _n("food holder", "food holder", "category", 2),
    _n("eat-from object", "eat-from-ob", "category", 2, "object-that-is-eaten-from"),
    _n("receptacle", "receptacle", "category", 2),
    _n("buccal object", "buccal obj", "category", 2, "object-that-goes-in-mouth"),
    _n("open object", "open obj", "category", 2),
    _n("closed object", "closed obj", "category", 2),
    # Relations (9): relations between objects are level 2 ("used with", "bigger than" [F95
    # p. 62]); relations between relations are level 3 ("opposite" [F95 p. 62], the
    # symmetries, sameness as identity between descriptors).
    _n("sameness", "same", "relation", 3, "same"),
    _n("opposite", "opposite", "relation", 3),
    _n("bigger than", "big. than", "relation", 2),
    _n("smaller than", "sm. than", "relation", 2),
    _n("similar shape to", "sim shape", "relation", 2, "similar-shape"),
    _n("used with", "used with", "relation", 2, "used-together"),
    _n("symmetry", "symmetry", "relation", 3),
    _n("mirror symmetry", "mirror", "relation", 3),
    _n("diagonal symmetry", "diagonal", "relation", 3),
    # Structure and position (12): "neighbor" and "left" are level 2 [F95 p. 62]; "direction"
    # is level 3 ("'left' is a direction" [F95 p. 62]), and "position" over end and middle
    # follows it.
    _n("group", "group", "structure", 2),
    _n("neighbor", "neighbor", "structure", 2),
    _n("direction", "direction", "structure", 3),
    _n("left", "left", "structure", 2),
    _n("right", "right", "structure", 2),
    _n("above", "above", "structure", 2),
    _n("below", "below", "structure", 2),
    _n("horizontal", "horiz", "structure", 2),
    _n("vertical", "vert", "structure", 2),
    _n("position", "position", "structure", 3),
    _n("end", "end", "structure", 2),
    _n("middle", "middle", "structure", 2),
    # Number (5): "number" is the superordinate of the counts, level 3.
    _n("number", "number", "number", 3),
    _n("one", "one", "number", 2),
    _n("two", "two", "number", 2),
    _n("three", "three", "number", 2),
    _n("many", "many", "number", 2),
    # The 48th node (D-007): "if the 'isolated' node is active" [F95 p. 68].
    _n("isolated", "isolated", "structure", 2, on_screen=False),
    # The 15 role nodes (D-080; the roles of D-079), none on French's screen: `role`, the
    # superordinate of the eleven role kinds, at level 3 like the other superordinates
    # (D-008); `counterpart` at level 3 (sameness means counterpart [F95 p. xv]); the other
    # kinds at level 2, relations between objects; `row`, `column` and `corner` are
    # positions in the table frame [F95 p. 85], under `position`.
    _n("role", "role", "role", 3, on_screen=False),
    _n("counterpart", "counterpart", "role", 3, on_screen=False),
    _n("sidekick", "sidekick", "role", 2, on_screen=False),
    _n("anchor", "anchor", "role", 2, on_screen=False),
    _n("figure", "figure", "role", 2, on_screen=False),
    _n("ground", "ground", "role", 2, on_screen=False),
    _n("echo", "echo", "role", 2, on_screen=False),
    _n("container", "container", "role", 2, on_screen=False),
    _n("connector", "connector", "role", 2, on_screen=False),
    _n("barrier", "barrier", "role", 2, on_screen=False),
    _n("separator", "separator", "role", 2, on_screen=False),
    _n("key", "key", "role", 2, on_screen=False),
    _n("row", "row", "structure", 2, on_screen=False),
    _n("column", "column", "structure", 2, on_screen=False),
    _n("corner", "corner", "structure", 2, on_screen=False),
)
ROLE_NODES = tuple(node.name for node in NODES[48:])
"""The 15 nodes the roles add (D-080), in definition order."""

NODE_NAMES = tuple(node.name for node in NODES)
NODE_INDEX = {node.name: i for i, node in enumerate(NODES)}

# The screen's grid of 41 boxes, seven columns by six rows, the last row six wide, read from
# the chapter 4 dumps [F95 p. 98 (img)] (SPEC.md 3.1); the labels are the nodes' `label`s.
SCREEN_GRID = (
    ("spoon", "silverware", "buccal obj", "sim shape", "neighbor", "horiz", "one"),
    ("small glass", "crockery", "open obj", "used with", "direction", "vert", "two"),
    ("fork", "receptacle", "closed obj", "symmetry", "right", "position", "three"),
    ("knife", "eat-from-ob", "big. than", "mirror", "left", "end", "many"),
    ("cup", "liq holder", "sm. than", "diagonal", "above", "middle", "same"),
    ("sharp obj", "food holder", "opposite", "group", "below", "number"),
)

# The object types of `objects.TYPES` whose node is spelt differently [F95 p. 62].
_TYPE_TO_NODE = {"salt shaker": "saltshaker"}


def node_for_type(type_: str) -> str:
    """The node of an object type: the type's name, but "saltshaker" for the palette's "salt
    shaker" (French's list spells it as one word [F95 p. 62]) (D-007)."""
    return _TYPE_TO_NODE.get(type_, type_)


def node_named(name: str) -> NodeDef:
    """The definition of a node by its name (D-007)."""
    return NODES[NODE_INDEX[name]]


def node_labelled(label: str) -> NodeDef:
    """The node whose screen label this is, for reading the chapter 4 grid [F95 p. 98 (img)]
    (D-007)."""
    for node in NODES:
        if node.label == label:
            return node
    raise KeyError(label)


ISA, LABELED, IDENTITY = "isa", "labeled", "identity"  # the kinds of link [F95 pp. 56-63]


@dataclass(frozen=True)
class LinkDef:
    """One link of the table (D-009, D-010): `a` and `b` are nodes; for an ISA link, `a` ISA
    `b` (and `b` HAS-MEMBER `a`, the inverse, "the opposite of ISA links" [F95 p. 63]); a
    labeled link is undirected and carries one or two labels, "controlled by label nodes"
    [F95 p. 62]; an identity link joins a node to itself and names the label of a
    concept-to-itself slippage [F95 p. 68]. `length` is the intrinsic length [F95 p. 56];
    `source` is the page, or `domain judgement`, or the dump that supports the row."""

    a: str
    b: str
    kind: str
    length: int
    labels: tuple = ()
    source: str = "domain judgement"

    @property
    def name(self) -> str:
        """The link's name in traces and events: `cup-liquid holder` (D-009)."""
        return f"{self.a}-{self.b}"


def _isa(a, b, length, source="domain judgement"):
    return LinkDef(a, b, ISA, length, (), source)


def _lab(a, b, length, labels, source="domain judgement"):
    return LinkDef(a, b, LABELED, length, tuple(labels), source)


def _ident(a, length, labels, source="domain judgement"):
    return LinkDef(a, a, IDENTITY, length, tuple(labels), source)


# D-010's lengths: a type's nearest category 30, its other categories 35; the cup's defining
# category, "liquid holder", 15 (the first dump shows it receiving most of what spreads from
# "cup" before any codelet runs [F95 p. 96 (img)]); "receptacle" 60 for every member (the
# vaguest category: the dumps keep it near the floor while the cup and the small glass are
# active [F95 pp. 98-107 (img)]); used-with and similar-shape links 25; smaller-than links 35
# (the cup to the small glass 25); the opposite links between position descriptors 40;
# identity links 10; the number links 30; a descriptor to its superordinate 30.
DEFINING, NEAREST, OTHER, VAGUE = 15, 30, 35, 60
SIZE_CLOSE, SIZE, DESCRIPTOR, IDENTITY_LENGTH = 25, 35, 40, 10
DUMPS = "F95 pp. 96-107 (img)"
REINTERPRETATION = "re-interpretation"   # the roles' rows (D-080), beyond French's program

LINKS = (
    # --- ISA links, a type to its categories (D-009) -----------------------------------
    _isa("cup", "liquid holder", DEFINING, "F95 p. 63"),
    _isa("cup", "crockery", 30, "F95 p. 119"),
    _isa("cup", "open object", OTHER, DUMPS),
    _isa("cup", "eat-from object", OTHER, DUMPS),
    _isa("saucer", "crockery", NEAREST, "F95 p. 119"),
    _isa("saucer", "open object", OTHER),
    _isa("saucer", "receptacle", VAGUE),
    _isa("plate", "crockery", NEAREST),
    _isa("plate", "food holder", OTHER),
    _isa("plate", "eat-from object", OTHER),
    _isa("plate", "open object", OTHER),
    _isa("plate", "receptacle", VAGUE),
    _isa("soup bowl", "liquid holder", NEAREST, "F95 p. 42"),
    _isa("soup bowl", "food holder", OTHER, "F95 p. 42"),
    _isa("soup bowl", "crockery", OTHER, "F95 p. 42"),
    _isa("soup bowl", "eat-from object", OTHER),
    _isa("soup bowl", "open object", OTHER),
    _isa("soup bowl", "receptacle", VAGUE),
    _isa("small glass", "liquid holder", NEAREST, "F95 p. 60 (img)"),
    _isa("small glass", "open object", OTHER),
    _isa("big glass", "liquid holder", NEAREST, "F95 p. 60 (img)"),
    _isa("big glass", "open object", OTHER),
    _isa("knife", "silverware", NEAREST, "F95 p. 42"),
    _isa("knife", "sharp object", OTHER, "F95 p. 42"),
    _isa("knife", "buccal object", OTHER),
    _isa("fork", "silverware", NEAREST, "F95 p. 117"),
    _isa("fork", "sharp object", OTHER),
    _isa("fork", "buccal object", OTHER),
    _isa("spoon", "silverware", NEAREST, "FCCA p. 389"),
    _isa("spoon", "buccal object", OTHER),
    _isa("spoon", "liquid holder", OTHER),
    _isa("saltshaker", "closed object", NEAREST),
    _isa("saltshaker", "receptacle", VAGUE),
    _isa("pepper shaker", "closed object", NEAREST),
    _isa("pepper shaker", "receptacle", VAGUE),
    # --- labeled links between object types (D-009) ------------------------------------
    _lab("knife", "fork", SIZE_CLOSE, ("used with",), "F95 p. 59"),
    _lab("cup", "saucer", SIZE_CLOSE, ("used with",), "F95 p. 59"),
    _lab("saucer", "plate", SIZE, ("smaller than", "bigger than"), "F95 p. 59"),
    _lab("small glass", "big glass", SIZE, ("smaller than", "bigger than"), "F95 p. 60 (img)"),
    _lab("cup", "big glass", SIZE, ("smaller than", "bigger than")),
    _lab("small glass", "cup", SIZE_CLOSE, ("smaller than", "bigger than"), DUMPS),
    _lab("saltshaker", "pepper shaker", SIZE_CLOSE, ("similar shape to",)),
    _lab("fork", "spoon", SIZE_CLOSE, ("similar shape to",)),
    # --- labeled links between categories and between descriptors (D-009) --------------
    _lab("open object", "closed object", DESCRIPTOR, ("opposite",), DUMPS),
    _lab("left", "right", DESCRIPTOR, ("opposite", "diagonal symmetry"), "F95 p. 62; F95 p. 68"),
    _lab("above", "below", DESCRIPTOR, ("opposite", "diagonal symmetry")),
    _lab("end", "middle", DESCRIPTOR, ("position",)),
    _lab("one", "two", NEAREST, ("number",), "F95 p. 68"),
    _lab("two", "three", NEAREST, ("number",)),
    _lab("three", "many", NEAREST, ("number",)),
    # --- identity links: a concept slipping to itself (D-009, D-030) --------------------
    _ident("left", IDENTITY_LENGTH, ("mirror symmetry",), "F95 p. 85"),
    _ident("right", IDENTITY_LENGTH, ("mirror symmetry",), "F95 p. 85"),
    _ident("above", IDENTITY_LENGTH, ("mirror symmetry",)),
    _ident("below", IDENTITY_LENGTH, ("mirror symmetry",)),
    # --- ISA links from descriptors to their superordinates (D-009) ---------------------
    _isa("left", "direction", NEAREST, "F95 p. 62"),
    _isa("right", "direction", NEAREST),
    _isa("above", "direction", NEAREST),
    _isa("below", "direction", NEAREST),
    _isa("end", "position", NEAREST),
    _isa("middle", "position", NEAREST),
    _isa("one", "number", NEAREST),
    _isa("two", "number", NEAREST),
    _isa("three", "number", NEAREST),
    _isa("many", "number", NEAREST),
    _isa("mirror symmetry", "symmetry", NEAREST),
    _isa("diagonal symmetry", "symmetry", NEAREST),
    # --- the roles' links (D-080; source `re-interpretation`) -----------------------------
    # ISA: the eleven role kinds to `role`, and the three table-frame positions to
    # `position`, at NEAREST (D-010).
    _isa("counterpart", "role", NEAREST, REINTERPRETATION),
    _isa("sidekick", "role", NEAREST, REINTERPRETATION),
    _isa("anchor", "role", NEAREST, REINTERPRETATION),
    _isa("figure", "role", NEAREST, REINTERPRETATION),
    _isa("ground", "role", NEAREST, REINTERPRETATION),
    _isa("echo", "role", NEAREST, REINTERPRETATION),
    _isa("container", "role", NEAREST, REINTERPRETATION),
    _isa("connector", "role", NEAREST, REINTERPRETATION),
    _isa("barrier", "role", NEAREST, REINTERPRETATION),
    _isa("separator", "role", NEAREST, REINTERPRETATION),
    _isa("key", "role", NEAREST, REINTERPRETATION),
    _isa("row", "position", NEAREST, REINTERPRETATION),
    _isa("column", "position", NEAREST, REINTERPRETATION),
    _isa("corner", "position", NEAREST, REINTERPRETATION),
    # Labelled: the central slip between the two symmetries, one giving way to the other
    # under pressure [F95 p. 86; F95 p. 115]; figure against ground [F95 p. 140]; the
    # midline as the mirror's axis; corners pairing through the centre [F95 p. 43]; "in
    # front of" as mirror placement [F95 p. 6]; the role kinds to the concepts they refine.
    _lab("mirror symmetry", "diagonal symmetry", DESCRIPTOR, ("opposite",), "F95 p. 86; F95 p. 115"),
    _lab("figure", "ground", DESCRIPTOR, ("opposite",), "F95 p. 140"),
    _lab("separator", "mirror symmetry", DESCRIPTOR, ("symmetry",), REINTERPRETATION),
    _lab("corner", "diagonal symmetry", DESCRIPTOR, ("symmetry",), "F95 p. 43"),
    _lab("column", "mirror symmetry", NEAREST, ("symmetry",), "F95 p. 6"),
    _lab("sidekick", "neighbor", NEAREST, ("role",), REINTERPRETATION),
    _lab("anchor", "middle", NEAREST, ("role",), REINTERPRETATION),
    _lab("connector", "middle", OTHER, ("role",), REINTERPRETATION),
    _lab("connector", "anchor", OTHER, ("role",), REINTERPRETATION),
    _lab("container", "group", OTHER, ("role",), REINTERPRETATION),
    _lab("echo", "sameness", NEAREST, ("role",), REINTERPRETATION),
    _lab("counterpart", "sameness", NEAREST, ("role",), "F95 p. xv"),
    _lab("barrier", "echo", OTHER, ("role",), REINTERPRETATION),
    _lab("isolated", "figure", OTHER, ("role",), REINTERPRETATION),
    _lab("key", "opposite", DESCRIPTOR, ("role",), "F95 p. 140"),
)
ROLE_LINKS = LINKS[66:]
"""The 29 links the roles add (D-080): 14 ISA and 15 labelled."""

# Links the sources name but the table leaves out, each with the reason (D-009): the dumps
# contradict them.
LINKS_LEFT_OUT = (
    ("fork", "food holder", "FCCA p. 389 reads forks and spoons as sharing that they hold "
     "food; 'food holder' never leaves the floor in the dumps while 'fork' and 'spoon' are "
     f"active [{DUMPS}]"),
    ("spoon", "food holder", "the same"),
    ("horizontal", "vertical", "an opposite pair by analogy with left-right; 'vert' stays at "
     f"2-7 while 'horiz' is 132 and 'opposite' 150 [{DUMPS}]"),
    ("cup", "receptacle", "a cup holds things, but 'receptacle' stays at 1-6 through a run "
     f"in which 'cup' is at 51-130 [{DUMPS}]"),
    ("small glass", "receptacle", "the same: 'small glass' reaches 53 and 'receptacle' 6 "
     f"[{DUMPS}]"),
    ("big glass", "receptacle", "as the small glass"),
)


def links_of(name: str, links=LINKS) -> list[LinkDef]:
    """Every link that touches a node, in table order (D-009). The queries take the link
    table as an argument so that a `Slipnet` built on another table answers from its own
    (`Slipnet.links_of`, `categories_of`, `labels_between`)."""
    return [link for link in links if name in (link.a, link.b)]


def categories_of(type_node: str, links=LINKS) -> list[str]:
    """The categories a type ISA, in table order, the nearest first (D-009)."""
    return [link.b for link in links if link.kind == ISA and link.a == type_node]


def labels_between(a: str, b: str, links=LINKS) -> tuple:
    """The labels of the labeled or identity link joining two nodes, `()` when there is none
    or the link is an ISA link; the label a slippage from `a` to `b` activates [F95 p. 62]
    (D-009)."""
    for link in links:
        if link.kind == ISA:
            continue
        if {link.a, link.b} == {a, b}:
            return link.labels
    return ()
