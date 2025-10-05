from dataclasses import dataclass, field, replace
from enum import Enum


class Special(Enum):
    V = 'v'
    Hash = '#'

    def __repr__(self):
        return self.value


@dataclass(eq=True)
class Directive:
    kind: str
    params: list[int | str | None | Special]
    at_sign: bool = False
    colon: bool = False

    def __post_init__(self):
        self.kind = self.kind.lower()

    def get_param(self, index: int, default=None):
        if index < len(self.params):
            val = self.params[index]
            return default if val is None else val

        return default

    def copy(self, **changes):
        return replace(self, **changes)

    def __repr__(self) -> str:
        at_sign = "@" if self.at_sign else ""
        colon = ":" if self.colon else ""
        prefix_params = ",".join(map(repr, self.params))
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
