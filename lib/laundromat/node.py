import sys

from dataclasses import dataclass, replace
from enum import StrEnum

from lib.fourmat.parse import *


INF = sys.maxsize


class InvalidDirective(TypeError):
    def __init__(self, directive):
        super().__init__(f"invalid directive: {directive}")


@dataclass(frozen=True)
class Range:
    left: int = INF
    right: int = 0

    def __and__(self, other: 'Range') -> 'Range':
        return Range(max(self.left, other.left), min(self.right, other.right)) or Range()

    def __bool__(self) -> bool:
        return self.left <= self.right

    def __contains__(self, item) -> bool:
        return self.left <= item <= self.right

    def __iter__(self):
        yield self.left
        yield self.right

    def __lshift__(self, other: int) -> 'Range':
        return Range(self.left - other, self.right - other)

    def __or__(self, other: 'Range') -> 'Range':
        return Range(min(self.left, other.left), max(self.right, other.right))

    def __pos__(self) -> 'Range':
        return Range(max(self.left, 0), max(self.right, 0))

    def __rshift__(self, other: int) -> 'Range':
        return Range(self.left + other, self.right + other)

    def __str__(self) -> str:
        return str(tuple(self)) if self else "()"

    def __xor__(self, other: 'Range') -> 'Range':
        match self.left < other.left, self.right > other.right:
            case True, True:
                return self

            case True, False:
                return Range(self.left, other.left - 1)

            case False, True:
                return Range(other.right + 1, self.right)

            case _:
                return Range()

    @classmethod
    def only(cls, value: int) -> 'Range':
        return Range(value, value)


@dataclass(frozen=True)
class Pointer:
    from_start: Range = Range()
    from_end: Range = Range()

    on_tape: bool = False

    def __add__(self, other: Range | int) -> 'Pointer':
        if isinstance(other, Range):
            return self + other.left | self + other.right

        else:
            return self.copy(from_start=self.from_start >> other, from_end=self.from_end << other)

    def __and__(self, other: Range) -> 'Pointer':
        return self.copy(from_end=self.from_end & other)

    def __contains__(self, item: int) -> bool:
        return item in self.from_end

    def __invert__(self) -> 'Pointer':
        return self.copy(from_start=self.from_end, from_end=self.from_start)

    def __or__(self, other: 'Pointer') -> 'Pointer':
        return self.copy(from_start=self.from_start | other.from_start, from_end=self.from_end | other.from_end)

    def __pos__(self) -> 'Pointer':
        return self.copy(from_start=+self.from_start, from_end=+self.from_end)

    def __str__(self) -> str:
        return f"{'v' if self.on_tape else '^'}[{self.from_start} : {self.from_end}]"

    def __sub__(self, other: Range | int) -> 'Pointer':
        if isinstance(other, Range):
            return self - other.left | self - other.right

        else:
            return self.copy(from_start=self.from_start << other, from_end=self.from_end >> other)

    def __xor__(self, other: Range) -> 'Pointer':
        return self.copy(from_end=self.from_end ^ other)

    def clear(self) -> 'Pointer':
        return self.copy(from_start=None, from_end=None)

    def start(self) -> 'Pointer':
        if self.on_tape:
            return self.copy(from_start=Range.only(0), from_end=Range(0, INF))

        else:
            return self.copy(from_start=Range.only(0), from_end=Range.only(1))

    def end(self) -> 'Pointer':
        return ~self.start()

    def enter_tape(self) -> 'Pointer':
        return self.copy(on_tape=True).start()

    def leave_tape(self) -> 'Pointer':
        return self.copy(on_tape=False).end()

    def copy(self, **changes) -> 'Pointer':
        return replace(self, **changes)

    def step(self, node: 'Node') -> 'Pointer':
        match node.kind:
            case 'str':
                return self

            case '*' if node.directive.at_sign:
                match node.directive.get_param(0, 0):
                    case Special.V:
                        raise InvalidDirective(node.directive)

                    case Special.Hash:
                        return ~self

                    case n:
                        return +(self.start() + n)

            case '*' if node.directive.colon:
                match node.directive.get_param(0, 1):
                    case Special.V:
                        raise InvalidDirective(node.directive)

                    case Special.Hash:
                        return +(self - self.from_end)

                    case n:
                        return +(self - n)

            case '*':
                match node.directive.get_param(0, 1):
                    case Special.V:
                        raise InvalidDirective(node.directive)

                    case Special.Hash:
                        return self.end()

                    case n:
                        return +(self + n)

            # TODO: Correctly handle null tape
            case '[' if node.directive.at_sign:
                return self | self + 1

            case '[' if node.directive.colon:
                return self + 1

            case '[':
                return self

            case '{':
                return self

            case ']' | '}':
                return self

            # TODO: Spell out more cases
            case _:
                return self + node.consumes


Operand = Special | int | str | None

@dataclass(frozen=True)
class Condition:
    def __bool__(self) -> bool:
        return False

    def __invert__(self) -> 'Condition':
        return self

    def __str__(self) -> str:
        return ""

    def check(self, pointer: Pointer) -> bool:
        return True

    def enforce(self, pointer: Pointer) -> Pointer:
        return pointer


@dataclass(frozen=True)
class Nil(Condition):
    negated: bool = False

    def __bool__(self) -> bool:
        return True

    def __invert__(self) -> 'Condition':
        return replace(self, negated=not self.negated)

    def __str__(self) -> str:
        return "T" if self.negated else "NIL"

    def check(self, pointer: Pointer) -> bool:
        if self.negated:
            return True

        else:
            return not pointer.on_tape


@dataclass(frozen=True)
class Equal(Condition):
    a: Operand
    b: Operand
    negated: bool = False

    def __bool__(self) -> bool:
        return True

    def __invert__(self) -> 'Condition':
        return replace(self, negated=not self.negated)

    def __str__(self) -> str:
        return f"{self.a} {'!=' if self.negated else '='} {self.b}"

    def check(self, pointer: Pointer) -> bool:
        lower, upper = pointer.from_end

        match [self.a, self.b]:
            case [None, b] | [b, None]:
                return b is not None if self.negated else b is None

            case params if Special.V in params:
                raise NotImplementedError

            case [Special.Hash, Special.Hash]:
                return not self.negated

            case [Special.Hash, b] | [b, Special.Hash]:
                return not lower == b == upper if self.negated else lower <= b <= upper

            case [a, b]:
                return a != b if self.negated else a == b

    def enforce(self, pointer: Pointer) -> Pointer:
        match [self.a, self.b]:
            case [Special.Hash, b] | [b, Special.Hash]:
                return pointer ^ Range.only(b) if self.negated else pointer & Range.only(b)

            case _:
                return pointer


@dataclass(frozen=True)
class Less(Condition):
    a: Operand
    b: Operand
    c: Operand
    negated: bool = False

    def __bool__(self) -> bool:
        return True

    def __invert__(self) -> 'Condition':
        return replace(self, negated=not self.negated)

    def __str__(self) -> str:
        string = f"{self.a} <= {self.b} <= {self.c}"
        return f"!({string})" if self.negated else string

    def check(self, pointer: Pointer) -> bool:
        lower, upper = pointer.from_end

        match [self.a, self.b, self.c]:
            case params if None in params:
                return all(x is None for x in params) != self.negated

            case params if Special.V in params:
                raise NotImplementedError

            case [Special.Hash, Special.Hash, Special.Hash]:
                return not self.negated

            case [Special.Hash, b, Special.Hash]:
                return Equal(b, Special.Hash, negated=self.negated).check(pointer)

            case [Special.Hash, Special.Hash, c]:
                return c <= upper if self.negated else lower <= c

            case [a, Special.Hash, Special.Hash]:
                return lower <= a if self.negated else a <= upper

            case [Special.Hash, b, c]:
                return b <= upper or b > c if self.negated else lower <= b <= c

            case [a, Special.Hash, c]:
                return upper < a or c < lower or a > c if self.negated else a <= lower <= c or a <= upper <= c

            case [a, b, Special.Hash]:
                return lower <= b or a > b if self.negated else a <= b <= upper

            case [a, b, c]:
                try:
                    return (a <= b <= c) != self.negated

                except TypeError:
                    return self.negated

    def enforce(self, pointer: Pointer) -> Pointer:
        match [self.a, self.b, self.c]:
            case [Special.Hash, x, Special.Hash]:
                return Equal(x, Special.Hash, negated=self.negated).enforce(pointer)

            case [Special.Hash, Special.Hash, x] | [Special.Hash, x, _]:
                return pointer ^ Range(0, x) if self.negated else pointer & Range(0, x)

            case [x, Special.Hash, Special.Hash] | [_, x, Special.Hash]:
                return pointer ^ Range(x, INF) if self.negated else pointer & Range(x, INF)

            case [a, Special.Hash, c]:
                return pointer ^ Range(a, c) if self.negated else pointer & Range(a, c)

            case _:
                return pointer


class Control(StrEnum):
    Start = "S"
    End = "E"
    Crash = "C"
    Empty = "_"
    UB = "U"


@dataclass(eq=False)
class Node:
    directive: Directive | Control | str

    pointer: Pointer = Pointer()

    def __hash__(self) -> int:
        return id(self)

    def __str__(self) -> str:
        if self.kind == "str" and len(self.directive) > 3:
            return "..."

        else:
            return f"{self.directive}"

    @property
    def kind(self) -> str:
        if isinstance(self.directive, Control):
            return 'ctrl'

        elif isinstance(self.directive, str):
            return 'str'

        else:
            return self.directive.kind

    @property
    def consumes(self) -> Range:
        match self.kind:
            case 'str' | 'ctrl':
                return Range.only(0)

            case 'a' | 'd' | 'e' | 'f' | 'g' | 'o' | 'r' | 's' | 'w' | 'x' | '$':
                return Range.only(1 + self.directive.params.count(Special.V))

            case 'c':
                return Range.only(1)

            case 't' | '%' | '&' | '|' | '~' | '^':
                return Range.only(self.directive.params.count(Special.V))

            case '?':
                return Range.only(1)

            case '/':
                return Range.only(1)

            case '[' if self.directive.at_sign:
                return Range(0, 1)

            case '[' if self.directive.colon:
                return Range.only(1)

            case '[' | ']' | '<' | '>':
                return Range.only(0)

            case _:
                return Range(0, INF)

    @property
    def writes(self) -> Range:
        match self.kind:
            case 'str':
                return Range.only(len(str(self)))

            case 'a' | 'd' | 'e' | 'f' | 'g' | 'o' | 'r' | 's' | 'w' | 'x':
                return Range(0, INF)

            case 'c':
                return Range(0, INF) if self.directive.at_sign or self.directive.colon else Range.only(1)

            case '%' | '|' | '~':
                return Range.only(self.directive.get_param(0, 1))

            case 't' | '&':
                return Range(0, INF)

            case '/':
                return Range(0, INF)

            case _:
                return Range.only(0)

    def copy(self, **changes) -> 'Node':
        return replace(self, **changes)
