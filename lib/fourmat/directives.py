from dataclasses import dataclass, field, replace
from enum import Enum


class Special(Enum):
    V = 'v'
    Hash = '#'

    def __str__(self):
        return self.value


DEFAULTS = {
    "c": [], ":c": [], "@c": [], ":@c": [],
    "%": [1], "&": [1], "|": [1], "~": [1],

    "r": [None, 0, " ", ",", 3],
    ":r": [None, 0, " ", ",", 3],
    "@r": [None, 0, " ", ",", 3],
    ":@r": [None, 0, " ", ",", 3],

    "d": [0, " ", ",", 3], ":d": [0, " ", ",", 3], "@d": [0, " ", ",", 3], ":@d": [0, " ", ",", 3],
    "b": [0, " ", ",", 3], ":b": [0, " ", ",", 3], "@b": [0, " ", ",", 3], ":@b": [0, " ", ",", 3],
    "o": [0, " ", ",", 3], ":o": [0, " ", ",", 3], "@o": [0, " ", ",", 3], ":@o": [0, " ", ",", 3],
    "x": [0, " ", ",", 3], ":x": [0, " ", ",", 3], "@x": [0, " ", ",", 3], ":@x": [0, " ", ",", 3],

    "f": [None, None, 0, None, " "], "@f": [None, None, 0, None, " "],
    "e": [None, None, None, 1, None, " ", "E"], "@e": [None, None, None, 1, None, " ", "E"],
    "g": [None, None, None, 1, None, " ", "E"], "@g": [None, None, None, 1, None, " ", "E"],
    "$": [2, 1, 0, " "], ":$": [2, 1, 0, " "], "@$": [2, 1, 0, " "], ":@$": [2, 1, 0, " "],

    "a": [0, 1, 0, " "], ":a": [0, 1, 0, " "], "@a": [0, 1, 0, " "], ":@a": [0, 1, 0, " "],
    "s": [0, 1, 0, " "], ":s": [0, 1, 0, " "], "@s": [0, 1, 0, " "], ":@s": [0, 1, 0, " "],
    "w": [], ":w": [], "@w": [], ":@w": [],

    "t": [1, 1], ":t": [1, 1], "@t": [1, 1], ":@t": [1, 1],
    "<": [0, 1, 0, " "], ":<": [0, 1, 0, " "], "@<": [0, 1, 0, " "], ":@<": [0, 1, 0, " "], ">": [],
    ";": [], ":;": [0, 72],

    "*": [1], ":*": [1], "@*": [0],
    "[": [Special.V], ":[": [], "@[": [], "]": [],
    "{": [None], ":{": [None], "@{": [None], ":@{": [None], "}": [], ":}": [],
    "?": [], "@?": [],

    "(": [], ":(": [], "@(": [], ":@(": [], ")": [],
    "p": [], ":p": [], "@p": [], ":@p": [],

    "^": [None, None, None], ":^": [None, None, None],

    "": []
}


@dataclass(eq=True)
class Directive:
    kind: str
    params: list[int | str | None | Special]
    defaults: list[int | str | None | Special] = None
    at_sign: bool = False
    colon: bool = False

    def __post_init__(self):
        self.kind = self.kind.lower()
        self.defaults = DEFAULTS.get(self.type)

        # Function calls
        if "/" in self.kind or "!" in self.kind:
            return

        if self.defaults is None:
            raise TypeError(f"unrecognized directive ~{self.type}")

        if len(self.params) > len(self.defaults):
            raise TypeError(f"too many parameters ({len(self.params)} > {len(self.defaults)}) passed to ~{self.type}")

    def get_default(self, index: int):
        return None if self.defaults is None else self.defaults[index]

    def get_param(self, index: int):
        default = self.get_default(index)
        if index < len(self.params):
            val = self.params[index]
            return default if val is None else val

        return default

    def copy(self, **changes):
        return replace(self, **changes)

    @property
    def arity(self) -> int | None:
        return None if self.defaults is None else len(self.defaults)

    @property
    def modifiers(self) -> str:
        return ":" * self.colon + "@" * self.at_sign

    @property
    def type(self) -> str:
        return self.modifiers + self.kind

    def __str__(self) -> str:
        prefix_params = ",".join(map(lambda x: f"'{x}" if isinstance(x, str) else str(x), self.params))
        return f"~{prefix_params}{self.type}"


@dataclass(eq=True)
class BlockDirective(Directive):
    closing_token: Directive = None
    default_token: Directive = None
    clauses: list = field(default_factory=lambda: [[]])

    @classmethod
    def from_embedded(cls, embedded: Directive):
        return BlockDirective(**vars(embedded))

    class Walker:
        def walk(self, directive: str | Directive):
            if isinstance(directive, str):
                self.string(directive)

            elif isinstance(directive, BlockDirective):
                self.block(directive)

            else:
                self.directive(directive)

        def clause(self, clause: list[str | Directive]):
            for directive in clause:
                self.walk(directive)

        def block(self, block: 'BlockDirective'):
            pass

        def directive(self, directive: Directive):
            pass

        def string(self, string: str):
            pass


@dataclass(eq=True)
class FunctionCallDirective(Directive):
    function_name: str = None

    @classmethod
    def from_embedded(cls, embedded: Directive):
        function_name = embedded.kind.split('/')[1].split(':')[-1].upper()
        embedded.kind = '/'
        return FunctionCallDirective(function_name=function_name, **vars(embedded))


__all__ = ["Special", "Directive", "BlockDirective", "FunctionCallDirective"]
