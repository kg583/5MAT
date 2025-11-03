import matplotlib.pyplot as plt
import networkx as nx

import operator
import re
import sys

from dataclasses import dataclass, field, replace

from lib.fourmat.parse import *
from lib.sixmat.util import *


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
        return f"[{self.from_start} : {self.from_end}]"

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
            case 'a' | 'e' | 'f' | 'g' | '$':
                return self + 1 + directive.params.count(Special.V)

            case 'c':
                return self + 1

            case 't' | '%' | '&' | '|' | '~':
                return self

            case '*' if directive.at_sign:
                match directive.get_param(0, 0):
                    case Special.Hash:
                        return ~self

                    case n:
                        return self.start() + n

            case '*' if directive.colon:
                match directive.get_param(0, 1):
                    case Special.Hash:
                        return self - self.from_end[0] | self - self.from_end[1]

                    case n:
                        return self - n

            case '*':
                match directive.get_param(0, 1):
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

            case '[' if directive.colon or directive.at_sign:
                return self + 1

            case _:
                return self


Operand = Special | int | str

@dataclass(frozen=True)
class Condition:
    a: Operand
    b: Operand = None
    c: Operand = None

    def __str__(self) -> str:
        match self.params:
            case [a]:
                return a

            case [a, b]:
                return f"{a} = {b}"

            case [a, b, c]:
                return f"{a} <= {b} <= {c}"

    @property
    def op(self):
        match len(self.params):
            case 1:
                return operator.not_

            case 2:
                return operator.eq

            case 3:
                return operator.le

    @property
    def params(self) -> list[Operand]:
        return [x for x in (self.a, self.b, self.c) if x is not None]

    def check(self, pointer: tuple[int, int], tape: list[list[str]]) -> tuple[bool, bool]:
        lower, upper = pointer

        match self.params:
            case ["T"]:
                return True, True

            case [Special.Hash, Special.Hash] | [Special.Hash, Special.Hash, Special.Hash]:
                return True, False

            case [Special.Hash, b] | [b, Special.Hash] | [Special.Hash, b, Special.Hash]:
                return lower <= int(b) <= upper, not lower == int(b) == upper

            case [a, b]:
                t = f = False
                for x in (tape[0] if a == Special.V else [a]):
                    for y in (tape[1] if b == Special.V else [b]):
                        t |= x == y
                        f |= x != y

                        if t and f:
                            return True, True

                return t, f

            case [Special.Hash, Special.Hash, c]:
                return lower <= int(c), int(c) <= upper

            case [a, Special.Hash, Special.Hash]:
                return int(a) <= upper, lower <= int(a)

            case [Special.Hash, b, c]:
                return lower <= int(b) <= int(c), int(b) <= upper or int(b) > int(c)

            case [a, Special.Hash, c]:
                return (int(a) <= lower <= int(c) or int(a) <= upper <= int(c),
                        upper < int(a) or int(c) < lower or int(a) > int(c))

            case [a, b, Special.Hash]:
                return int(a) <= int(b) <= upper, lower <= int(b) or int(a) > int(b)

            case [a, b, c]:
                t = f = False
                for x in (tape[0] if a == Special.V else [a]):
                    for y in (tape[1] if b == Special.V else [b]):
                        for z in (tape[2] if c == Special.V else [c]):
                                t |= x <= y <= z
                                f |= not x <= y <= z

                                if t and f:
                                    return True, True

                return t, f


@dataclass(eq=False)
class Node:
    directive: Directive | Condition | str
    pointer: Pointer = Pointer()

    def __hash__(self) -> int:
        return id(self)

    def __str__(self) -> str:
        return f"{self.directive}"

    def copy(self, **changes) -> 'Node':
        return replace(self, **changes)

    def condition(self, a: Operand, b: Operand = None, c: Operand = None) -> 'Node':
        return self.copy(directive=Condition(a, b, c))


START = Node("S")
END   = Node("E")
CRASH = Node("C")


def build_cfg(program: str) -> nx.DiGraph:
    cfg = nx.DiGraph()

    def build_clause(clause: list[Directive | str], current: Node, end: Node, outer):
        for directive in clause:
            if isinstance(directive, BlockDirective):
                cfg.add_edge(current, current := current.copy(directive=directive))
                current = build_block(directive, current, current.copy(directive=directive.closing_token), outer)

            else:
                print(f"Stepping {directive} ...")
                cfg.add_edge(current, current := current.copy(directive=directive))

                if isinstance(directive, Directive):
                    match directive.kind:
                        case '^':
                            match directive.params:
                                case []:
                                    terminate = current.condition(Special.Hash, 0)

                                case [a]:
                                    terminate = current.condition(a, 0)

                                case params:
                                    try:
                                        terminate = current.condition(*params)

                                    except TypeError:
                                        raise ValueError("invalid break")

                            cfg.add_edge(current, terminate)
                            cfg.add_edge(terminate, outer)

                            cfg.add_edge(current, current := current.condition("!"))

                        case '?':
                            cfg.add_edge(current, CRASH)
                            break

        else:
            cfg.add_edge(current, end)


    def build_block(block: BlockDirective, current: Node, end: Node, outer) -> Node:
        print(f"Walking {block} ...")

        match block.kind:
            case '[' if block.colon:
                for clause, condition in zip(block.clauses, ["T", "!"]):
                    start = current.condition(condition)
                    cfg.add_edge(current, start)
                    build_clause(clause, start, end, outer)

            case '[' if block.at_sign:
                clause = block.clauses[0]

                cfg.add_edge(current, true := current.condition("T"))
                build_clause(clause, true, end, outer)

                cfg.add_edge(current, false := current.condition("!"))
                cfg.add_edge(false, end)

            case '[':
                match block.get_param(0):
                    case Special.V | None:
                        cfg.add_edge(current, CRASH)

                    case Special.Hash:
                        for index, clause in enumerate(block.clauses[:-1] if block.default_token else block.clauses):
                            cfg.add_edge(current, start := current.condition(Special.Hash, index))
                            build_clause(clause, start, end, outer)

                        cfg.add_edge(current, start := current.condition(index + 1, index + 1, Special.Hash))
                        if block.default_token:
                            build_clause(block.clauses[-1], start, end, outer)

                        else:
                            cfg.add_edge(start, end)

                    case n if 0 <= n < len(block.clauses):
                        build_clause(block.clauses[n], current, end, outer)

            case '{':
                if block.colon:
                    raise ValueError("invalid loop")

                clause = block.clauses[0]
                iterations = 0
                loops = block.get_param(0)

                terminate = current.condition("!")
                if not block.at_sign:
                    if current.pointer.on_tape:
                        raise ValueError("invalid loop")

                    current.pointer = Pointer(on_tape=True)
                    end.pointer = end.pointer.copy(on_tape=True)

                if block.closing_token.colon and loops != 0:
                    build_clause(clause, current, end := end.copy(), end)
                    cfg.add_edge(end, current := current.copy())
                    iterations += 1

                match loops:
                    case Special.Hash | None:
                        cfg.add_edge(current, start := current.condition(1, 1, Special.Hash))
                        build_clause(clause, start, end, end)

                        cfg.add_edge(current, terminate)

                        cfg.add_edge(end, current)

                    case n:
                        while True:
                            cfg.add_edge(current, start := current.condition(1, 1, Special.Hash))
                            build_clause(clause, start, end := end.copy(), end)

                            cfg.add_edge(current, terminate)
                            iterations += 1

                            if iterations >= n:
                                break

                            cfg.add_edge(end, current := current.copy())

                        cfg.add_edge(end, terminate)

                return terminate

            case '<':
                build_clause(sum(block.clauses, []), current, end, end)

        return end

    build_clause(parse(tokenize(program)).clauses[0], START, END, END)
    return cfg


def draw_cfg(cfg: nx.DiGraph):
    pos = nx.kamada_kawai_layout(cfg)
    nx.draw_networkx_nodes(cfg, pos=pos, node_shape='none')
    nx.draw_networkx_edges(cfg, pos=pos)

    for node in cfg:
        if str(node).startswith("~"):
            color = "skyblue"

        elif "=" in str(node) or str(node) == "TAPE":
            color = "springgreen"

        elif str(node) == "!":
            color = "lightcoral"

        elif str(node) in "SCE":
            color = "violet"

        else:
            color = "silver"

        nx.draw_networkx_labels(cfg.subgraph([node]), pos=pos, bbox=dict(facecolor=color, edgecolor="black"))

    plt.show()
