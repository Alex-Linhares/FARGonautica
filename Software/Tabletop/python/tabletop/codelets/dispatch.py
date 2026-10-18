"""From a codelet type to its function (D-051, D-052, D-081): the 23 types of the legend
[F95 p. 96 (img)] and the role codelets, each to the function that does its job, and
`run_codelet`, which the run loop calls for the codelet it has chosen.
"""

from tabletop.codelets import builders, finish, judges, scouts, searches
from tabletop.codelets.common import Outcome

# Each type to (module, function name): the function is looked up at run time, so that a
# test-only probe patching one module-level function lesions that codelet (D-104).
FUNCTIONS = {
    "find-group": (scouts, "find_group"),
    "simple-sameness-detect": (scouts, "simple_sameness_detect"),
    "find-end-objs": (scouts, "find_end_objs"),
    "find-middle-objs": (scouts, "find_middle_objs"),
    "find-sim-nhbr": (scouts, "find_sim_nhbr"),
    "find-neighbor": (scouts, "find_neighbor"),
    "build-obj-0": (builders, "build_obj_stage"),
    "build-obj-1": (builders, "build_obj_stage"),
    "build-obj-2": (builders, "build_obj_stage"),
    "build-obj-desc": (builders, "build_obj_desc"),
    "search-for-sam-gp": (searches, "search_for_sam_gp"),
    "search-for-sim-gp": (searches, "search_for_sim_gp"),
    "pick-salient-obj": (scouts, "pick_salient_obj"),
    "pick-salient-subgp": (scouts, "pick_salient_subgp"),
    "pick-2nd-salient-obj": (scouts, "pick_2nd_salient_obj"),
    "create-cp": (builders, "create_cp"),
    "pick-strong-cp": (judges, "pick_strong_cp"),
    "examine-cp": (judges, "examine_cp"),
    "build-cp-descrip": (builders, "build_cp_descrip"),
    "cp-compete": (judges, "cp_compete"),
    "check-worldview": (judges, "check_worldview"),
    "clean-up-corresp": (judges, "clean_up_corresp"),
    "find-answer": (finish, "find_answer"),
    "propose-role-cp": (scouts, "propose_role_cp"),
    "check-roles": (judges, "check_roles"),
    "find-roles": (scouts, "find_roles"),
    "compare-roles": (scouts, "compare_roles"),
    "read-worldview-roles": (judges, "read_worldview_roles"),
    "recast-role": (searches, "recast_role"),
}


def run_codelet(world, codelet, exponent: float) -> Outcome:
    """Run one codelet: its function with the world and the exponent a(T) of the choice
    that picked it (D-052, D-060). Returns its `Outcome`."""
    module, name = FUNCTIONS[codelet.type]
    return getattr(module, name)(world, codelet, exponent)
