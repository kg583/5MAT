import matplotlib.pyplot as plt
import networkx as nx

from lib.laundromat.cfg import *


def simplify(cfg: nx.DiGraph) -> nx.DiGraph:
    def step(node: Node, child: Node):
        marks[child] -= 1

        match node.pointer.on_tape, child.pointer.on_tape:
            case (True, True) | (False, False):
                try:
                    child.pointer |= node.pointer.step(child)

                    if child.pointer.from_end[0] < 0:
                        raise InvalidDirective(node.directive)

                except InvalidDirective:
                    prune.add(child)
                    cfg.add_edge(node, CRASH, condition=Condition())

            case False, True:
                child.pointer = node.pointer.enter_tape()

            case True, False:
                child.pointer = node.pointer.leave_tape()


    def walk(child: Node):
        while len(children := cfg[node := child]) == 1:
            child, = children

            if cfg.in_degree(child) == len(children := [*cfg[child]]) == 1:
                redundant = child.is_conditional
                redundant |= child.writes == child.consumes == 0

                if redundant:
                    cfg.remove_edge(node, child)
                    cfg.add_edge(node, child := children[0])

            step(node, child)
            print(f"Visited {child}\t{child.pointer}")

            if marks[child] and not child.is_loop:
                return

        if len(children) > 1:
            for child in children:
                step(node, child)
                walk(child)

            if "!" in [str(child.directive) for child in children]:
                cond, opp = children
                t, f = cond.directive.check(node.pointer)

                if not t:
                    prune.add(cond)

                if not f:
                    prune.add(opp)

            else:
                for child in children:
                    if not child.directive.check(node.pointer)[0]:
                        prune.add(child)


    while True:
        print()
        cfg = cfg.copy()

        marks = {node: cfg.in_degree(node) for node in cfg}
        prune = set()

        walk(START)

        old = len(cfg)
        cfg.remove_nodes_from(prune)
        cfg = cfg.subgraph(nx.descendants(cfg, START) | {START}).to_directed()

        removed = old - len(cfg)
        print(f"Removed {removed} node{'s' if removed != 1 else ''}!")

        if not removed:
            print("All done!")
            return cfg


CFG = program_to_cfg("~{~a~^~?~}~a")
draw_cfg(CFG)

simplified = simplify(CFG)
draw_cfg(simplified)
