import matplotlib.pyplot as plt
import networkx as nx

from lib.laundromat.cfg import *


def simplify(cfg: nx.DiGraph) -> nx.DiGraph:
    while True:
        old_nodes, old_edges = len(cfg), len(cfg.edges())
        cfg = cfg.copy()

        # Reset pointers
        for node in cfg:
            node.pointer = node.pointer.clear()

        START.pointer = Pointer().start()

        # Calculate new pointers
        for node in visit_order(cfg):
            for child in cfg[node]:
                child.pointer |= cfg[node][child]["condition"].enforce(node.pointer).step(child)
                print(f"Updated {str(child):10}{child.pointer}")

        for node in [*cfg.nodes()]:
            for child, attrs in {**cfg[node]}.items():
                condition = attrs["condition"]

                # Crash past the end of the tape
                if child.pointer.from_end < Range(0, inf):
                    cfg.remove_edge(node, child)
                    cfg.add_edge(node, CRASH, condition=condition)
                    print(f"Crashed at    {child} <- {node}")


                # Crash infinite loops
                if node.kind == "{" and not {END, CRASH} & nx.descendants(cfg, child):
                    print("Found an infinite loop!")

                    cfg.remove_edges_from([*cfg.edges(child)])
                    cfg.add_edge(child, CRASH, condition=Condition())

                # Skip
                if not condition:
                    continue

                # Check reachability
                if not condition.check(node.pointer) or child.directive == Control.UB:
                    cfg.remove_edge(node, child)
                    print(f"Removed       {node} -> {child}")

                # Simplify condition
                elif condition.enforce(node.pointer) == node.pointer:
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


CFG = program_to_cfg("~@{~#[~a~]~}")
draw_cfg(CFG)

simplified = simplify(CFG)
draw_cfg(simplified)
