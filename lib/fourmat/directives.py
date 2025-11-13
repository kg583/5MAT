from dataclasses import dataclass, field, replace
from enum import Enum


class Special(Enum):
    V = 'v'
    Hash = '#'

    def __str__(self):
        return self.value


ARITY = {
    "c": 1, "%": 1, "&": 1, "|": 1, "~": 1,
    "r": 5, "d": 4, "b": 4, "o": 4, "x": 4,
    "f": 5, "e": 7, "g": 7, "$": 4,
    "a": 4, "s": 4, "w": 4,
    "t": 2, "<": 4, ";": 2,
    "*": 1, "[": 1, "{": 1, "?": 0,
    "(": 0, "p": 0,
    "^": 3
}


@dataclass(eq=True)
class Directive:
    kind: str
    params: list[int | str | None | Special]
    at_sign: bool = False
    colon: bool = False

    def __post_init__(self):
        self.kind = self.kind.lower()

        # TODO: ~; is nullary unless it's ~:; inside a justification block
        arity = ARITY.get(self.kind)
        if self.kind == "[" and (self.colon or self.at_sign):
            arity = 0

        if arity is not None and len(self.params) > arity:
            raise ValueError(f"too many parameters ({len(self.params)} > {arity}) passed to ~{self.kind}")

    def get_param(self, index: int, default=None):
        if index < len(self.params):
            val = self.params[index]
            return default if val is None else val

        return default

    def copy(self, **changes):
        return replace(self, **changes)

    def __str__(self) -> str:
        at_sign = "@" if self.at_sign else ""
        colon = ":" if self.colon else ""
        prefix_params = ",".join(map(lambda x: f"'{x}" if isinstance(x, str) else str(x), self.params))
        return f"~{prefix_params}{at_sign}{colon}{self.kind}"


@dataclass(eq=True)
class BlockDirective(Directive):
    closing_token: Directive = None
    default_token: Directive = None
    clauses: list = field(default_factory=lambda: [[]])

    @classmethod
    def from_embedded(cls, embedded: Directive):
        return BlockDirective(**vars(embedded))


@dataclass(eq=True)
class FunctionCallDirective(Directive):
    function_name: str = None

    @classmethod
    def from_embedded(cls, embedded: Directive):
        function_name = embedded.kind.split('/')[1].split(':')[-1].upper()
        embedded.kind = '/'
        return FunctionCallDirective(function_name=function_name, **vars(embedded))
