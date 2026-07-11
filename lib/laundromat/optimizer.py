import logging
import re

from dataclasses import dataclass

from lib.fivemat.util import *
from lib.laundromat.cfg import *
from lib.laundromat.node import *

logger = logging.getLogger(__name__)


def plural(word: str, count: int) -> str:
    return f"{count} {word}{'s' * (count != 1)}"


def cfg_optimize(cfg: CFG) -> CFG:
    while True:
        old_nodes, old_edges = len(cfg), len(cfg.edges())

        # Remove empty blocks
        cfg = CFG(cfg.tree())
        logger.debug("\nRemoved empty blocks")

        cfg.update_pointers()
        for node in cfg:
            # Some children got pruned; best to just restart
            if node not in cfg:
                break

            # Remove no-ops
            if node.kind != "ctrl" and node.consumes == node.writes == Range.only(0):
                cfg.remove_node(node)
                continue

            for child, attrs in {**cfg[node]}.items():
                condition = attrs["condition"]

                # Crash past the end of the tape
                if node.pointer.from_end < Range(0, inf):
                    cfg.crash_on(node)
                    logger.debug(f"Crashed at    {node}")

                # Crash infinite loops
                if node.kind == "{" and not cfg.terminates_from(child):
                    cfg.crash_on(child)
                    logger.debug("Found an infinite loop!")

                # Skip known trivial edges
                if not condition:
                    continue

                # Check reachability
                if not condition.check(node.pointer) or child.directive == Control.UB:
                    cfg.remove_edge(node, child)
                    logger.debug(f"Removed       {node} -> {child}")

                # Simplify condition
                elif condition.enforce(node.pointer) == node.pointer and not condition.queries_tape:
                    cfg[node][child]["condition"] = Always()
                    logger.debug(f"Simplified    {node} -> {child}")

                    # Remove conditionals
                    if node.kind == "[":
                        logger.debug(f"Deleted       {node} .. {node.closing}")

                        node.directive = Control.Empty
                        node.closing.directive = Control.Empty
                        node.closing = None

        # Prune unreachable nodes
        cfg = cfg.reachable()
        logger.debug("\nPruned from START node")

        removed_nodes, removed_edges = old_nodes - len(cfg), old_edges - len(cfg.edges())
        logger.debug(f"Removed {plural('node', removed_nodes)} and {plural('edge', removed_edges)}!\n")

        if removed_nodes == removed_edges == 0:
            logger.debug("All done!")
            return cfg


if __name__ == "__main__":
    program = parse("~1{~#[~:;~#*~^test~]~a~}")
    loop = CFG.extract_loop(program)
    cfg = CFG(loop)

    cfg.draw()
    optimized = cfg_optimize(cfg)
    optimized.draw()
