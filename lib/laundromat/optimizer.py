import matplotlib.pyplot as plt
import networkx as nx

import sys

from dataclasses import dataclass, replace

from lib.laundromat.cfg import *


INF = sys.maxsize


def clamp(value: int) -> int:
    return max(value, 0)

def expand(a: tuple[int, int], b: tuple[int, int]):
    if a is None:
        return b

    if b is None:
        return a

    return min(a[0], b[0]), max(a[1], b[1])


@dataclass(frozen=True)
class Pointer:
    from_start: tuple[int, int] = None
    from_end: tuple[int, int] = None

    on_tape: bool = False
    tape_length: int = None

    def __str__(self) -> str:
        return f"{'v' if self.on_tape else '^'}[{self.from_start} : {self.from_end}] <= {self.tape_length or '?'}"

    def __contains__(self, item: int) -> bool:
        return self.from_end[0] <= item <= self.from_end[1]

    def __invert__(self) -> 'Pointer':
        return self.copy(from_start=self.from_end, from_end=self.from_start)

    def __add__(self, other: int) -> 'Pointer':
        return self.copy(from_start=(clamp(self.from_start[0] + other), clamp(self.from_start[1] + other)),
                         from_end=(clamp(self.from_end[0] - other), clamp(self.from_end[1] - other)))

    def __sub__(self, other: int) -> 'Pointer':
        return self + -other

    def __or__(self, other: 'Pointer') -> 'Pointer':
        return self.copy(from_start=expand(self.from_start, other.from_start),
                         from_end=expand(self.from_end, other.from_end),
                         tape_length=other.tape_length)

    def clear(self) -> 'Pointer':
        return self.copy(from_start=None, from_end=None)

    def start(self) -> 'Pointer':
        if self.on_tape:
            return self.copy(from_start=(0, 0), from_end=(self.tape_length or 0, self.tape_length or INF))

        else:
            return self.copy(from_start=(0, 0), from_end=(1, 1))

    def end(self) -> 'Pointer':
        return ~self.start()

    def enter_tape(self) -> 'Pointer':
        return self.copy(on_tape=True).start()

    def leave_tape(self) -> 'Pointer':
        return self.copy(on_tape=False).end()

    def copy(self, **changes) -> 'Pointer':
        return replace(self, **changes)

    def step(self, directive) -> 'Pointer':
        if isinstance(directive, (Condition, str)):
            return self

        match directive.kind:
            case 'a' | 'd' | 'e' | 'f' | 'g' | '$' | 'r':
                return self + 1 + directive.params.count(Special.V)

            case 'c':
                return self + 1

            case 't' | '%' | '&' | '|' | '~':
                return self

            case '*' if directive.at_sign:
                match directive.get_param(0, 0):
                    case Special.V:
                        raise InvalidDirective(directive)

                    case Special.Hash:
                        return ~self

                    case n:
                        return self.start() + n

            case '*' if directive.colon:
                match directive.get_param(0, 1):
                    case Special.V:
                        raise InvalidDirective(directive)

                    case Special.Hash:
                        return self - self.from_end[0] | self - self.from_end[1]

                    case n:
                        return self - n

            case '*':
                match directive.get_param(0, 1):
                    case Special.V:
                        raise InvalidDirective(directive)

                    case Special.Hash:
                        return self.end()

                    case n:
                        return self + n

            case '^':
                return self + directive.params.count(Special.V)

            case '?':
                return self + 1

            case '/':
                return self + 1

            # TODO: Correctly handle null tape
            case '[' if directive.colon or directive.at_sign:
                return self + 1

            # TODO: Spell out more cases
            case _:
                return self


def simplify(cfg: nx.DiGraph) -> nx.DiGraph:
    def step(node: Node, child: Node):
        marks[child] -= 1

        match node.on_tape, child.on_tape:
            case (True, True) | (False, False):
                try:
                    pointers[child] |= pointers[node].step(child.directive)

                except InvalidDirective:
                    prune.add(child)
                    cfg.add_edge(node, CRASH)

            case False, True:
                pointers[child] = pointers[node].enter_tape()

            case True, False:
                pointers[child] = pointers[node].leave_tape()


    def walk(child: Node):
        while len(children := cfg[node := child]) == 1:
            child, = children

            if (child.is_condition or child.is_conditional) and cfg.in_degree(child) == 1:
                cfg.remove_edge(node, child)
                cfg.add_edge(node, child := [*cfg[child]][0])

            step(node, child)
            print(f"Visited {child}\t{pointers[child]}")

            if marks[child] and not child.is_loop:
                return

        if len(children) > 1:
            for child in children:
                step(node, child)
                walk(child)

            if "!" in [child.directive.a for child in children]:
                cond, opp = children if [*children][0].directive.a != "!" else children[::-1]
                t, f = cond.directive.check(pointers[node].from_end, tape)

                if not t:
                    prune.add(cond)

                if not f:
                    prune.add(opp)

            else:
                for child in children:
                    if not child.directive.check(pointers[node].from_end, tape)[0]:
                        prune.add(child)


    while True:
        print()
        cfg = cfg.copy()

        marks = {node: cfg.in_degree(node) for node in cfg}
        pointers = {node: Pointer(on_tape=node.on_tape) for node in cfg}

        prune = set()

        pointers[START] = Pointer().start()
        tape = [[chr(c) for c in range(128)] for _ in [0, 1, 2]]

        walk(START)

        old = len(cfg)
        cfg.remove_nodes_from(prune)
        cfg = cfg.subgraph(nx.descendants(cfg, START) | {START}).to_directed()

        removed = old - len(cfg)
        print(f"Removed {removed} node{'s' if removed != 1 else ''}!")

        if not removed:
            print("All done!")
            return cfg


CFG = build_cfg("~{~#*~#[abc~:;def~:*~]~}")
draw_cfg(CFG)

simplified = simplify(CFG)
draw_cfg(simplified)
