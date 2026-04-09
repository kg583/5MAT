import operator

from dataclasses import dataclass, replace

from lib.fourmat.parse import *


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
    on_tape: bool = False

    def __hash__(self) -> int:
        return id(self)

    def __str__(self) -> str:
        return f"{self.directive}"

    @property
    def is_condition(self) -> bool:
        return isinstance(self.directive, Condition)

    @property
    def is_conditional(self) -> bool:
        return isinstance(self.directive, Directive) and self.directive.kind in "[]"

    @property
    def is_loop(self) -> bool:
        return isinstance(self.directive, Directive) and self.directive.kind in "{}"

    @property
    def consumes(self) -> int | None:
        if isinstance(self.directive, (Condition, str)):
            return 0

        match self.directive.kind:
            case 'a' | 'd' | 'e' | 'f' | 'g' | 'o' | 'r' | 's' | 'w' | 'x' | '$':
                return 1 + self.directive.params.count(Special.V) if self.on_tape else 1

            case 'c':
                return 1

            case 't' | '%' | '&' | '|' | '~' | '^':
                return self.directive.params.count(Special.V)

            case '?':
                return 1

            case '/':
                return 1

            case '*' if (n := self.directive.get_param(0, 1)) not in Special:
                if self.directive.at_sign:
                    return None

                elif self.directive.colon:
                    return -n

                else:
                    return n

            case _:
                return None

    @property
    def writes(self) -> int | None:
        if isinstance(self.directive, Condition):
            return 0

        if isinstance(self.directive, str):
            return len(self.directive)

        match self.directive.kind:
            case 'a' | 'd' | 'e' | 'f' | 'g' | 'o' | 'r' | 's' | 'w' | 'x':
                return None

            case 'c':
                return 1

            case 't':
                return None

            case '%' | '&' | '|' | '~':
                return self.directive.get_param(0, 1)

            case '/':
                return None

            case _:
                return 0

    def copy(self, **changes) -> 'Node':
        return replace(self, **changes)

    def condition(self, a: Operand, b: Operand = None, c: Operand = None) -> 'Node':
        return self.copy(directive=Condition(a, b, c))
