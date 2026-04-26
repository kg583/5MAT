from lib.laundromat.cfg import *


def simplify(cfg: CFG) -> CFG:
    while True:
        old_nodes, old_edges = len(cfg), len(cfg.edges())
        cfg = cfg.copy()

        cfg.update_pointers()
        for node in cfg:
            for child, attrs in {**cfg[node]}.items():
                condition = attrs["condition"]

                # Crash past the end of the tape
                if node.pointer.from_end < Range(0, inf):
                    cfg.crash_on(node)
                    print(f"Crashed at    {node}")

                # Crash infinite loops
                if node.kind == "{" and not cfg.terminates_from(child):
                    cfg.crash_on(child)
                    print("Found an infinite loop!")

                # Check reachability
                if not condition.check(node.pointer) or child.directive == Control.UB:
                    cfg.remove_edge(node, child)
                    print(f"Removed       {node} -> {child}")

                # Simplify condition
                elif condition.enforce(node.pointer) == node.pointer and not condition.queries_tape:
                    cfg[node][child]["condition"] = Condition()
                    print(f"Simplified    {node} -> {child}")

        # Prune unreachable nodes
        cfg = cfg.reachable()
        print("\nPruning from START node...")

        removed_nodes, removed_edges = old_nodes - len(cfg), old_edges - len(cfg.edges())
        print(f"Removed {removed_nodes} node{'s' if removed_nodes != 1 else ''}"
              f" and {removed_edges} edge{'s' if removed_edges != 1 else ''}!\n")

        if removed_nodes == removed_edges == 0:
            print("All done!")
            return cfg


graph = CFG("""~1[comment~]""")
graph.draw(size=12)

simplified = simplify(graph)
simplified.draw(size=12)

print(str(graph))
print(str(simplified))
