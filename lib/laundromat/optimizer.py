import matplotlib.pyplot as plt
import networkx as nx

from lib.laundromat.cfg import *


def simplify(cfg: nx.DiGraph) -> nx.DiGraph:
    while True:
        old_nodes, old_edges = len(cfg), len(cfg.edges())
        cfg = cfg.copy()

        # Update pointers
        for path in nx.all_simple_edge_paths(cfg, source=START, target=[CRASH, END]):
            for s, t in path:
                match s.pointer.on_tape, t.pointer.on_tape:
                    case (True, True) | (False, False):
                        try:
                            t.pointer |= cfg[s][t]["condition"].enforce(s.pointer.step(t))

                        except InvalidDirective:
                            cfg.add_edge(s, CRASH, condition=Condition())

                        t.pointer = +t.pointer

                    case False, True:
                        t.pointer = s.pointer.enter_tape()

                    case True, False:
                        t.pointer = s.pointer.leave_tape()

                print(f"Updated {str(t):10}{t.pointer}")

            print()

        for node in [*cfg.nodes()]:
            in_edges = cfg.in_edges(node)
            children = {**cfg[node]}

            # Contract redundant nodes
            if len(children) == len(in_edges) == 1 and node.writes == node.consumes == Range.only(0):
                # TODO: Handle this case
                if condition := cfg[ancestor := [*in_edges][0][0]][node]["condition"]:
                    continue

                cfg.add_edge(ancestor, child := [*children][0], condition=condition)
                cfg.remove_edge(ancestor, node)
                print(f"Contracted    {node} <- {child}")

            else:
                for child, attrs in children.items():
                    # Skip
                    if not attrs["condition"]:
                        continue

                    # Check reachability
                    if not attrs["condition"].check(node.pointer):
                        cfg.remove_edge(node, child)
                        print(f"Removed       {node} -> {child}")

                    # Simplify condition
                    elif attrs["condition"].enforce(node.pointer) == node.pointer:
                        cfg[node][child]["condition"] = Condition()
                        print(f"Simplified    {node} -> {child}")

        # Prune unreachable nodes
        cfg = cfg.subgraph(nx.descendants(cfg, START) | {START}).to_directed()
        print("\nPruning from START node...")

        removed_nodes, removed_edges = old_nodes - len(cfg), old_edges - len(cfg.edges())
        print(f"Removed {removed_nodes} node{'s' if removed_nodes != 1 else ''}"
              f" and {removed_edges} edge{'s' if removed_edges != 1 else ''}!\n")

        if removed_nodes == removed_edges == 0:
            print("All done!")
            return cfg


CFG = program_to_cfg("~{~a~#[~#,1^~]7~}")
draw_cfg(CFG)

simplified = simplify(CFG)
draw_cfg(simplified)
