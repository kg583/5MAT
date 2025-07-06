from enum import Enum


class Special(Enum):
    V = 'v'
    Hash = '#'


class Directive:
    def __init__(
            self,
            kind: str,
            prefix_params: list[int | str | None | Special],
            at_sign=False,
            colon=False,
    ):
        self.kind = kind.lower()
        self.prefix_params = prefix_params
        self.at_sign = at_sign
        self.colon = colon

    def param(self, index, default):
        if 0 <= index < len(self.prefix_params):
            val = self.prefix_params[index]
            return default if val is None else val
        return default

    def __repr__(self) -> str:
        at_sign = "@" if self.at_sign else ""
        colon = ":" if self.colon else ""
        prefix_params = ",".join(map(repr, self.prefix_params))
        return f"({self.kind}{at_sign}{colon}{' ' + prefix_params if len(self.prefix_params) >= 1 else ''})"


class BlockDirective(Directive):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.clauses = [[]]
        self.default = False

    @classmethod
    def from_embedded(cls, embedded):
        embedded.__class__ = cls

        embedded.clauses = [[]]
        embedded.default = False

        return embedded

    def __repr__(self) -> str:
        at_sign = "@" if self.at_sign else ""
        colon = ":" if self.colon else ""
        prefix_params = ",".join(map(repr, self.prefix_params))
        return f"({self.kind}{at_sign}{colon}{' ' + prefix_params if len(self.prefix_params) >= 1 else ''}|{' ' + repr(self.clauses) if len(self.clauses) >= 1 else ''})"
