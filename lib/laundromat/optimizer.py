import logging
import re

from dataclasses import dataclass

from lib.laundromat.cfg import *
from lib.laundromat.node import *

logger = logging.getLogger(__name__)


def plural(word: str, count: int) -> str:
    return f"{count} {word}{'s' * (count != 1)}"


def extract_loop(program: BlockDirective) -> BlockDirective:
    def extractor(current: BlockDirective):
        for clause in current.clauses:
            for directive in clause:
                if isinstance(directive, BlockDirective):
                    if directive.type == "{":
                        yield directive

                    yield from extractor(directive)

    return next(extractor(program))


def cfg_optimize(cfg: CFG) -> CFG:
    cfg = cfg.copy()

    while True:
        old_nodes, old_edges = len(cfg), len(cfg.edges())
        cfg.update_pointers()

        for node in cfg:
            if node not in cfg:
                break

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

        # Prune unreachable nodes
        cfg = cfg.reachable()
        logger.debug("\nPruning from START node...")

        removed_nodes, removed_edges = old_nodes - len(cfg), old_edges - len(cfg.edges())
        logger.debug(f"Removed {plural('node', removed_nodes)} and {plural('edge', removed_edges)}!\n")

        if removed_nodes == removed_edges == 0:
            logger.debug("All done!")
            return cfg

        cfg = cfg.copy()
