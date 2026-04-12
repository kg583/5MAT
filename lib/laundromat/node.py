import sys

from dataclasses import dataclass, replace

from lib.fourmat.parse import *


INF = sys.maxsize

Window = tuple[int, int]


class InvalidDirective(TypeError):
    def __init__(self, directive):
        super().__init__(f"invalid directive: {directive}")


def clamp(value: int) -> int:
    return max(value, 0)

def expand(a: Window, b: Window):
    if a is None:
        return b

    if b is None:
        return a

    return min(a[0], b[0]), max(a[1], b[1])


@dataclass(frozen=True)
class Pointer:
    from_start: Window = None
    from_end: Window = None

    on_tape: bool = False
    tape_length: Window = None

    def __str__(self) -> str:
        return f"{'v' if self.on_tape else '^'}[{self.from_start} : {self.from_end}] <= {self.tape_length or '?'}"

    def __contains__(self, item: int) -> bool:
        return self.from_end[0] <= item <= self.from_end[1]

    def __invert__(self) -> 'Pointer':
        return self.copy(from_start=self.from_end, from_end=self.from_start)

    def __add__(self, other: Window | int) -> 'Pointer':
        if isinstance(other, tuple):
            return self + other[0] | self + other[1]

        else:
            return self.copy(from_start=(self.from_start[0] + other, self.from_start[1] + other),
                             from_end=(self.from_end[0] - other, self.from_end[1] - other))

    def __sub__(self, other: Window | int) -> 'Pointer':
        if isinstance(other, tuple):
            return self - other[0] | self - other[1]

        else:
            return self.copy(from_start=(self.from_start[0] - other, self.from_start[1] - other),
                             from_end=(self.from_end[0] + other, self.from_end[1] + other))

    def __or__(self, other: 'Pointer') -> 'Pointer':
        return self.copy(from_start=expand(self.from_start, other.from_start),
                         from_end=expand(self.from_end, other.from_end),
                         tape_length=other.tape_length)

    def clamp(self) -> 'Pointer':
        return self.copy(from_start=tuple(map(clamp, self.from_start)), from_end=tuple(map(clamp, self.from_end)))

    def clear(self) -> 'Pointer':
        return self.copy(from_start=None, from_end=None)

    def start(self) -> 'Pointer':
        if self.on_tape:
            if self.tape_length:
                return self.copy(from_start=(0, 0), from_end=self.tape_length)

            else:
                return self.copy(from_start=(0, 0), from_end=(1, INF))

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
                        return (self.start() + n).clamp()

            case '*' if node.directive.colon:
                match node.directive.get_param(0, 1):
                    case Special.V:
                        raise InvalidDirective(node.directive)

                    case Special.Hash:
                        return (self - self.from_end).clamp()

                    case n:
                        return (self - n).clamp()

            case '*':
                match node.directive.get_param(0, 1):
                    case Special.V:
                        raise InvalidDirective(node.directive)

                    case Special.Hash:
                        return self.end()

                    case n:
                        return (self + n).clamp()

            # TODO: Correctly handle null tape
            case '[':
                if node.directive.colon or node.directive.at_sign:
                    return self + 1

                else:
                    raise InvalidDirective(node.directive)

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

    def __str__(self) -> str:
        return ""

    def check(self, pointer: Pointer) -> tuple[bool, bool]:
        return True, False


@dataclass(frozen=True)
class Negation(Condition):
    def __bool__(self) -> bool:
        return True

    def check(self, pointer: Pointer) -> tuple[bool, bool]:
        return False, True


@dataclass(frozen=True)
class Unary(Condition):
    a: Operand

    def __str__(self) -> str:
        return f"{self.a}"

    def check(self, pointer: Pointer) -> tuple[bool, bool]:
        return True, not pointer.on_tape


@dataclass(frozen=True)
class Binary(Condition):
    a: Operand
    b: Operand

    def __bool__(self) -> bool:
        return True

    def __str__(self) -> str:
        return f"{self.a} = {self.b}"

    def check(self, pointer: Pointer) -> tuple[bool, bool]:
        lower, upper = pointer.from_end

        match [self.a, self.b]:
            case [None, b] | [b, None]:
                return b is None, b is not None

            case params if Special.V in params:
                raise NotImplementedError

            case [Special.Hash, Special.Hash]:
                return True, False

            case [Special.Hash, b] | [b, Special.Hash]:
                return lower <= b <= upper, not lower == b == upper

            case [a, b]:
                return a == b, a != b

@dataclass(frozen=True)
class Ternary(Condition):
    a: Operand
    b: Operand
    c: Operand

    def __bool__(self) -> bool:
        return True

    def __str__(self) -> str:
        return f"{self.a} <= {self.b} <= {self.c}"

    def check(self, pointer: Pointer) -> tuple[bool, bool]:
        lower, upper = pointer.from_end

        match [self.a, self.b, self.c]:
            case params if None in params:
                return all(x is None for x in params), any(x is not None for x in params)

            case params if Special.V in params:
                raise NotImplementedError

            case [Special.Hash, Special.Hash, Special.Hash]:
                return True, False

            case [Special.Hash, b, Special.Hash]:
                return lower <= b <= upper, not lower == b == upper

            case [Special.Hash, Special.Hash, c]:
                return lower <= c, c <= upper

            case [a, Special.Hash, Special.Hash]:
                return a <= upper, lower <= a

            case [Special.Hash, b, c]:
                return lower <= b <= c, b <= upper or b > c

            case [a, Special.Hash, c]:
                return a <= lower <= c or a <= upper <= c, upper < a or c < lower or a > c

            case [a, b, Special.Hash]:
                return a <= b <= upper, lower <= b or a > b

            case [a, b, c]:
                try:
                    return a <= b <= c, not (a <= b <= c)

                except TypeError:
                    return False, True


@dataclass(eq=False)
class Node:
    directive: Directive | str

    pointer: Pointer = Pointer()
    may_be_nil: bool = True

    def __hash__(self) -> int:
        return id(self)

    def __str__(self) -> str:
        if self.kind == "str" and len(self.directive) > 3:
            return "..."

        else:
            return f"{self.directive}"

    @property
    def kind(self) -> str:
        if isinstance(self.directive, str):
            if self.directive in ["!S", "!E", "!C"]:
                return self.directive

            else:
                return 'str'

        else:
            return self.directive.kind

    @property
    def is_buffer(self) -> bool:
        return self.kind in "<>"

    @property
    def is_conditional(self) -> bool:
        return self.kind in "[]"

    @property
    def is_loop(self) -> bool:
        return self.kind in "{}"

    @property
    def consumes(self) -> Window:
        match self.kind:
            case 'str':
                return 0, 0

            case 'a' | 'd' | 'e' | 'f' | 'g' | 'o' | 'r' | 's' | 'w' | 'x' | '$':
                return (1 + self.directive.params.count(Special.V),) * 2

            case 'c':
                return 1, 1

            case 't' | '%' | '&' | '|' | '~' | '^':
                return (self.directive.params.count(Special.V),) * 2

            case '?':
                return 1, 1

            case '/':
                return 1, 1

            case _:
                return 0, 7

    @property
    def writes(self) -> Window:
        match self.kind:
            case 'str':
                return (len(str(self)),) * 2

            case 'a' | 'd' | 'e' | 'f' | 'g' | 'o' | 'r' | 's' | 'w' | 'x':
                return 0, INF

            case 'c':
                return (0, INF) if self.directive.at_sign or self.directive.colon else (1, 1)

            case '%' | '|' | '~':
                return (self.directive.get_param(0, 1),) * 2

            case 't' | '&':
                return 0, INF

            case '/':
                return 0, INF

            case _:
                return 0, 0

    def copy(self, **changes) -> 'Node':
        return replace(self, **changes)
