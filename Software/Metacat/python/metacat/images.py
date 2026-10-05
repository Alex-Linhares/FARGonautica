"""images.ss: images of letters, groups and strings, for rule application.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from images.ss, with
racket/engine/images.rktl as a worked translation.

Images are part of the machinery of rule application.  Each letter or group
in a string, as well as the string itself, has an "image" that represents
what the object currently looks like.  Normally, an image just looks like
the object itself.  However, when a rule is applied to the string, the
appearance of these images may change.  For example, if the rule "Increase
length of all objects in string" is applied to the string abc, the
resulting appearance of the string (i.e., its image) is aabbcc.  The string
still contains only three letters, but the images of these letters are now
aa, bb, and cc, respectively.  In contrast, the rule "Swap positions of
leftmost and rightmost letter" would transform the image of the string into
cba.  Images can be reset to their original (unchanged) appearance in order
to try out a different rule.  (Marshall's comment, from images.ss.)

Closures are SchemeObject classes: make-image's is Image, make-string-image's
StringImage.  `fail` is the caller's escape: a procedure that never returns
(rules.ss passes a continuation; here it raises).  An operation can escape
midway through a `map` over sub-images, so `replace-all` and `tell-all` keep
Chez's order of application (porting-notes.md, item 05, "Order in images").
make-letter (workspace-objects.ss), make-group (groups.ss) and make-group-pexp
(group-graphics.ss) are read through the package at call time.  The engine never
imports tkinter.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, setup, slipnet
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (all_but_last, base_object, exists_p, first, last, tell_all)


def make_letter_image(letter_category):
    """images.ss: make-letter-image"""
    return make_image(letter_category, False, False, False, False, [])


class StringImage(SchemeObject):
    """images.ss: make-string-image (the closure)"""
    __slots__ = ("string", "direction", "verbatim_images", "verbatim_p")

    def __init__(this, string, direction):
        this.string = string
        this.direction = direction
        this.verbatim_images = False
        this.verbatim_p = False

    @message("object-type")
    def object_type(this, self):
        return "string-image"

    @message("get-sub-images")
    def get_sub_images(this, self):
        if this.verbatim_p is not False:
            return this.verbatim_images
        return tell_all(tell(this.string, "get-constituent-objects"), "get-image")

    @message("get-ordered-sub-images")
    def get_ordered_sub_images(this, self):
        if this.direction is slipnet.plato_right:
            return tell(self, "get-sub-images")
        return list(reversed(tell(self, "get-sub-images")))

    @message("generate")
    def generate(this, self):
        return tell_all(tell(self, "get-ordered-sub-images"), "generate")

    @message("reset")
    def reset(this, self):
        this.verbatim_p = False
        # 1.2: forgets the direction it was made with (anomalies: "A string image's reset forgets its original direction")
        this.direction = slipnet.plato_right
        for image in tell(self, "get-sub-images"):
            tell(image, "reset")
        return "done"

    @message("do-walk")
    def do_walk(this, self, walk_method, action):
        for image in tell(self, "get-ordered-sub-images"):
            tell(image, walk_method, action)
        return "done"

    @message("get-length")
    def get_length(this, self):
        return slipnet.number_to_platonic_number(len(tell(self, "get-sub-images")))

    @message("new-start-letter")
    def new_start_letter(this, self, arg, fail):
        for i in tell(self, "get-sub-images"):
            tell(i, "new-start-letter", arg, fail)
        return "done"

    @message("new-alpha-position-category")
    def new_alpha_position_category(this, self, arg, fail):
        # 1.2: sends new-start-letter (anomalies: "A string image's new-alpha-position-category sends new-start-letter")
        for i in tell(self, "get-sub-images"):
            tell(i, "new-start-letter", arg, fail)
        return "done"

    @message("new-length")
    def new_length(this, self, arg, fail):
        return fail()

    @message("new-appearance")
    def new_appearance(this, self, letter_categories):
        this.verbatim_images = chez.map_(make_letter_image, letter_categories)
        this.direction = slipnet.plato_right
        this.verbatim_p = True
        return "done"

    @message("reverse-direction")
    def reverse_direction(this, self, fail):
        this.direction = slipnet.inverse(this.direction)
        return "done"

    @message("reverse-medium")
    def reverse_medium(this, self, medium, fail):
        if medium is slipnet.plato_letter_category:
            letters = tell_all(tell(self, "get-sub-images"), "get-letter")
            return tell(self, "replace-all", "new-start-letter", list(reversed(letters)), fail)
        if medium is slipnet.plato_length:
            lengths = tell_all(tell(self, "get-sub-images"), "get-length")
            return tell(self, "replace-all", "new-length", list(reversed(lengths)), fail)
        return None

    @message("replace-all")
    def replace_all(this, self, method_name, new_args, fail):
        # chez: map's order of application; fail can escape midway (porting-notes.md, item 05)
        chez.map_(lambda image, arg: tell(image, method_name, arg, fail),
                  tell(self, "get-sub-images"), new_args)
        return "done"

    @message("letter")
    def letter(this, self, fail):
        return fail()

    @message("group")
    def group(this, self, fail):
        return fail()

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_string_image(string, direction):
    """images.ss: make-string-image"""
    return StringImage(string, direction)


class Image(SchemeObject):
    """images.ss: make-image (the closure)"""
    __slots__ = ("start_letter", "bond_facet", "letter_relation", "length_relation",
                 "direction", "sub_images", "original_state", "swapped_image",
                 "instantiated_object")

    def __init__(this, start_letter, bond_facet, letter_relation, length_relation, direction,
                 sub_images):
        this.start_letter = start_letter
        this.bond_facet = bond_facet
        this.letter_relation = letter_relation
        this.length_relation = length_relation
        this.direction = direction
        this.sub_images = sub_images
        this.original_state = ["*state*", start_letter, bond_facet, letter_relation,
                               length_relation, direction, sub_images]
        this.swapped_image = False
        this.instantiated_object = False

    @message("object-type")
    def object_type(this, self):
        return "image"

    @message("print")
    def print_(this, self):
        chez.printf("swapped-image? = ~a~%", exists_p(this.swapped_image))
        if tell(self, "letter-image?"):
            return chez.printf("start-letter: ~a~%", tell(this.start_letter, "get-lowercase-name"))
        chez.printf("start-letter: ~a~%direction: ~a~%~aletters: ~a ~a~%",
                    tell(this.start_letter, "get-lowercase-name"),
                    tell(this.direction, "get-lowercase-name"),
                    ">" if this.bond_facet is slipnet.plato_letter_category else " ",
                    (tell(this.letter_relation, "get-lowercase-name")
                     if exists_p(this.letter_relation) else "none"),
                    chez.map_(str, tell_all(tell_all(this.sub_images, "get-letter"),
                                            "get-lowercase-name")))
        return chez.printf("~alengths: ~a ~a~%",
                           ">" if this.bond_facet is slipnet.plato_length else " ",
                           (tell(this.length_relation, "get-lowercase-name")
                            if exists_p(this.length_relation) else "none"),
                           chez.map_(lambda length: (slipnet.platonic_number_to_number(length)
                                                     if exists_p(length) else "?"),
                                     tell_all(this.sub_images, "get-length")))

    @message("get-swapped-image")
    def get_swapped_image(this, self):
        return this.swapped_image

    @message("update-swapped-image")
    def update_swapped_image(this, self, image):
        this.swapped_image = image
        return "done"

    @message("get-instantiated-object")
    def get_instantiated_object(this, self):
        return this.instantiated_object

    @message("get-letter")
    def get_letter(this, self):
        return this.start_letter

    @message("get-bond-facet")
    def get_bond_facet(this, self):
        return this.bond_facet

    @message("get-letter-relation")
    def get_letter_relation(this, self):
        return this.letter_relation

    @message("get-length-relation")
    def get_length_relation(this, self):
        return this.length_relation

    @message("get-direction")
    def get_direction(this, self):
        return this.direction

    @message("get-sub-images")
    def get_sub_images(this, self):
        return this.sub_images

    @message("get-length")
    def get_length(this, self):
        if tell(self, "letter-image?"):
            return slipnet.plato_one
        return slipnet.number_to_platonic_number(len(this.sub_images))

    @message("letter-image?")
    def letter_image_p(this, self):
        return len(this.sub_images) == 0

    @message("get-state")
    def get_state(this, self):
        return ["*state*", this.start_letter, this.bond_facet, this.letter_relation,
                this.length_relation, this.direction, this.sub_images]

    @message("new-state")
    def new_state(this, self, state):
        if isinstance(state, list) and len(state) > 0 and state[0] == "*state*":
            (_, this.start_letter, this.bond_facet, this.letter_relation,
             this.length_relation, this.direction, this.sub_images) = state
        return "done"

    @message("reset")
    def reset(this, self):
        tell(self, "new-state", this.original_state)
        this.swapped_image = False
        this.instantiated_object = False
        tell_all(this.sub_images, "reset")
        return "done"

    @message("instantiate-as-letter")
    def instantiate_as_letter(this, self, string, position):
        this.instantiated_object = _metacat.workspace_objects.make_letter(
            string, this.start_letter, position)
        return tell(string, "add-letter", this.instantiated_object, position)

    @message("instantiate-as-group")
    def instantiate_as_group(this, self, string):
        s = slipnet
        if this.direction is s.plato_left:
            ordered_objects = tell_all(list(reversed(this.sub_images)), "get-instantiated-object")
        else:
            ordered_objects = tell_all(this.sub_images, "get-instantiated-object")
        left_object = first(ordered_objects)
        right_object = last(ordered_objects)
        if this.bond_facet is s.plato_letter_category:
            bond_category = (s.plato_sameness if this.letter_relation is s.plato_identity
                             else this.letter_relation)
        else:
            bond_category = (s.plato_sameness if this.length_relation is s.plato_identity
                             else this.length_relation)
        group_category = tell(bond_category, "get-related-node", s.plato_group_category)
        group_direction = False if group_category is s.plato_samegrp else this.direction
        this.instantiated_object = _metacat.groups.make_group(
            string, group_category, this.bond_facet, group_direction,
            left_object, right_object, ordered_objects, [])
        tell(string, "add-group", this.instantiated_object)
        for object_ in ordered_objects:
            tell(object_, "update-enclosing-group", this.instantiated_object)
        if setup.p_workspace_graphics is not False:
            tell(this.instantiated_object, "update-proposal-level", _metacat.workspace.p_built)
            return tell(this.instantiated_object, "set-graphics-pexp",
                        _metacat.group_graphics.make_group_pexp(this.instantiated_object,
                                                                _metacat.workspace.p_built))
        return None

    @message("leaf-walk")
    def leaf_walk(this, self, action):
        if tell(self, "letter-image?"):
            return action(self)
        if this.direction is slipnet.plato_left:
            return chez.for_each(lambda i: tell(i, "leaf-walk", action),
                                 list(reversed(this.sub_images)))
        return chez.for_each(lambda i: tell(i, "leaf-walk", action), this.sub_images)

    @message("postorder-interior-walk")
    def postorder_interior_walk(this, self, action):
        if tell(self, "letter-image?"):
            return "done"
        if this.direction is slipnet.plato_left:
            for i in reversed(this.sub_images):
                tell(i, "postorder-interior-walk", action)
            return action(self)
        for i in this.sub_images:
            tell(i, "postorder-interior-walk", action)
        return action(self)

    @message("generate")
    def generate(this, self):
        if tell(self, "letter-image?"):
            return this.start_letter
        if this.direction is slipnet.plato_left:
            return tell_all(list(reversed(this.sub_images)), "generate")
        return tell_all(this.sub_images, "generate")

    @message("copy")
    def copy(this, self):
        return make_image(this.start_letter, this.bond_facet, this.letter_relation,
                          this.length_relation, this.direction, tell_all(this.sub_images, "copy"))

    @message("reverse-direction")
    def reverse_direction(this, self, fail):
        this.direction = slipnet.inverse(this.direction)
        return "done"

    # medium in {plato-letter-category plato-length}:
    @message("reverse-medium")
    def reverse_medium(this, self, medium, fail):
        if tell(self, "letter-image?"):
            return "done"
        if medium is slipnet.plato_letter_category:
            letters = tell_all(this.sub_images, "get-letter")
            tell(self, "replace-all", "new-start-letter", list(reversed(letters)), fail)
            this.letter_relation = slipnet.inverse(this.letter_relation)
            this.start_letter = last(letters)
            return "done"
        if medium is slipnet.plato_length:
            lengths = tell_all(this.sub_images, "get-length")
            tell(self, "replace-all", "new-length", list(reversed(lengths)), fail)
            this.length_relation = slipnet.inverse(this.length_relation)
            return "done"
        return None

    @message("replace-all")
    def replace_all(this, self, method_name, new_args, fail):
        # chez: map's order of application; fail can escape midway (porting-notes.md, item 05)
        chez.map_(lambda image, arg: tell(image, method_name, arg, fail),
                  this.sub_images, new_args)
        return "done"

    # arg in {opp alphabetic-first alphabetic-last}:
    @message("new-alpha-position-category")
    def new_alpha_position_category(this, self, arg, fail):
        s = slipnet
        if (arg is s.plato_alphabetic_first
                or (arg is s.plato_opposite and this.start_letter is s.plato_z)):
            return tell(self, "new-start-letter", s.plato_a, fail)
        if (arg is s.plato_alphabetic_last
                or (arg is s.plato_opposite and this.start_letter is s.plato_a)):
            return tell(self, "new-start-letter", s.plato_z, fail)
        return fail()

    # arg in {pred succ iden} U {a ... z}:
    @message("new-start-letter")
    def new_start_letter(this, self, arg, fail):
        if not exists_p(arg):
            fail()
        elif tell(self, "letter-image?"):
            if slipnet.platonic_relation_p(arg):
                new_letter = tell(this.start_letter, "get-related-node", arg)
                if not exists_p(new_letter):
                    fail()
                else:
                    this.start_letter = new_letter
            else:
                this.start_letter = arg
        elif slipnet.platonic_relation_p(arg):
            tell_all(this.sub_images, "new-start-letter", arg, fail)
            this.start_letter = tell(this.start_letter, "get-related-node", arg)
        elif slipnet.platonic_letter_p(arg):
            new_letters = enumerate_letter(arg, this.letter_relation, len(this.sub_images), fail)
            tell(self, "replace-all", "new-start-letter", new_letters, fail)
            this.start_letter = arg
        return "done"

    # arg in {pred succ iden} U {one ... five}:
    @message("new-length")
    def new_length(this, self, arg, fail):
        s = slipnet
        if not exists_p(arg):
            return fail()
        if arg is s.plato_identity or (s.platonic_number_p(arg)
                                       and tell(self, "get-length") is arg):
            return "done"
        if tell(self, "letter-image?"):
            tell(self, "letter->singleton-group", fail)
            return tell(self, "new-length", arg, fail)
        if arg is s.plato_predecessor:
            return tell(self, "shorten", fail)
        if arg is s.plato_successor:
            return tell(self, "extend", this.letter_relation, this.length_relation, fail)
        if s.platonic_number_p(arg):
            n = s.platonic_number_to_number(arg)
            length = len(this.sub_images)
            if n <= length:
                for _ in range(length - n):
                    tell(self, "shorten", fail)
            elif n > length:
                for _ in range(n - length):
                    tell(self, "extend", this.letter_relation, this.length_relation, fail)
            return "done"
        return fail()

    @message("shorten")
    def shorten(this, self, fail):
        if len(this.sub_images) < 2:
            fail()
        this.sub_images = all_but_last(1, this.sub_images)
        if len(this.sub_images) > 1:
            if not exists_p(this.letter_relation):
                this.letter_relation = slipnet.relationship_between(
                    tell_all(this.sub_images, "get-letter"))
            if not exists_p(this.length_relation):
                this.length_relation = slipnet.relationship_between(
                    tell_all(this.sub_images, "get-length"))
        return "done"

    # letter-arg in {pred succ iden} U {a ... z}
    # length-arg in {pred succ iden} U {one ... five}:
    @message("extend")
    def extend(this, self, letter_arg, length_arg, fail):
        if tell(self, "letter-image?"):
            tell(self, "letter->singleton-group", fail)
            return tell(self, "extend", letter_arg, length_arg, fail)
        new_image = tell(last(this.sub_images), "copy")
        if change_length_first_p(length_arg, tell(new_image, "get-length")):
            tell(new_image, "new-length", length_arg, fail)
            tell(new_image, "new-start-letter", letter_arg, fail)
        else:
            tell(new_image, "new-start-letter", letter_arg, fail)
            tell(new_image, "new-length", length_arg, fail)
        this.sub_images = this.sub_images + [new_image]
        this.letter_relation = slipnet.relationship_between(tell_all(this.sub_images, "get-letter"))
        this.length_relation = slipnet.relationship_between(tell_all(this.sub_images, "get-length"))
        return "done"

    @message("letter")
    def letter(this, self, fail):
        if tell(self, "letter-image?"):
            return "done"
        if this.bond_facet is slipnet.plato_length:
            return fail()
        this.bond_facet = False
        this.letter_relation = False
        this.length_relation = False
        this.direction = False
        this.sub_images = []
        return "done"

    @message("group")
    def group(this, self, fail):
        if tell(self, "letter-image?"):
            return tell(self, "letter->singleton-group", fail)
        return "done"

    @message("letter->singleton-group")
    def letter_to_singleton_group(this, self, fail):
        sub_image = tell(self, "copy")
        this.bond_facet = slipnet.plato_letter_category
        this.letter_relation = slipnet.plato_identity
        this.length_relation = slipnet.plato_identity
        this.direction = slipnet.plato_right
        this.sub_images = [sub_image]
        return tell(self, "new-length", slipnet.plato_one, fail)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_image(start_letter, bond_facet, letter_relation, length_relation, direction, sub_images):
    """images.ss: make-image"""
    return Image(start_letter, bond_facet, letter_relation, length_relation, direction,
                 list(sub_images))


def change_length_first_p(length_arg, current_length):
    """images.ss: change-length-first?"""
    s = slipnet
    return (length_arg is s.plato_predecessor
            or (s.platonic_number_p(length_arg)
                and (not exists_p(current_length)
                     or (s.platonic_number_to_number(length_arg)
                         < s.platonic_number_to_number(current_length)))))


def enumerate_letter(start, relation, n, fail):
    """images.ss: enumerate-letter"""
    if n > 1 and not exists_p(relation):
        return fail()

    def enum(n, start):
        if n == 1:
            return [start]
        next_ = tell(start, "get-related-node", relation)
        if not exists_p(next_):
            return fail()
        return [start] + enum(chez.sub1(n), next_)
    return enum(n, start)
