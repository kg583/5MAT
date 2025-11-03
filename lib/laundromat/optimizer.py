import matplotlib.pyplot as plt
import networkx as nx

from lib.laundromat.cfg import *


def prune(cfg: nx.DiGraph) -> nx.DiGraph:
    for node in cfg:
        node.marks = cfg.in_degree(node)
        node.prune = False

    def step(node: Node, child: Node):
        child.marks -= 1
        match node.pointer.on_tape, child.pointer.on_tape:
            case (True, True) | (False, False):
                child.pointer |= node.pointer.step(child.directive)

            case False, True:
                child.pointer = node.pointer.enter_tape()

            case True, False:
                child.pointer = node.pointer.leave_tape()


    def walk(child: Node):
        while len(children := cfg[node := child]) == 1:
            print(f"Visiting {node} ...")

            child, = children
            step(node, child)

            if isinstance(child.directive, Directive) and child.directive.kind == "{":
                continue

            if child.marks:
                return

        if len(children) > 1:
            for child in children:
                step(node, child)
                walk(child)

            if "!" in [child.directive.a for child in children]:
                cond, opp = children if [*children][0].directive.a != "!" else children[::-1]
                t, f = cond.directive.check(node.pointer.from_end, tape)

                if not t:
                    cond.prune = True

                if not f:
                    opp.prune = True

            else:
                for child in children:
                    if not child.directive.check(node.pointer.from_end, tape)[0]:
                        child.prune = True


    START.pointer = Pointer().start()
    tape = [[chr(c) for c in range(128)] for _ in [0, 1, 2]]

    walk(START)

    cfg.remove_nodes_from([node for node in cfg if node.prune])
    return cfg.subgraph(nx.descendants(cfg, START) | {START}).to_directed()
