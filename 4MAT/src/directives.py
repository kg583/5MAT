from dataclasses import dataclass, field, replace
from enum import Enum


class Special(Enum):
    V = 'v'
    Hash = '#'


@dataclass
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
        return f"({self.kind}{at_sign}{colon}{' ' + prefix_params if len(self.params) >= 1 else ''})"


@dataclass
class BlockDirective(Directive):
    closing_token: Directive = None
    default_token: Directive = None
    clauses: list = field(default_factory=lambda: [[]])

    @classmethod
    def from_embedded(cls, embedded: Directive):
        return BlockDirective(**vars(embedded))

    def __repr__(self) -> str:
        at_sign = "@" if self.at_sign else ""
        colon = ":" if self.colon else ""
        prefix_params = ",".join(map(repr, self.params))
        return (f"({self.kind}{at_sign}{colon}{' ' + prefix_params if len(self.params) >= 1 else ''}|"
                f"{' ' + repr(self.clauses) if len(self.clauses) >= 1 else ''})")
