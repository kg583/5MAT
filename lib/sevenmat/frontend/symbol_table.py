from typing import Any

from .typing import VarType

class SymbolTable:
    def __init__(self, parent: "SymbolTable" = None):
        self.parent: SymbolTable | None = parent
        self.entries: dict[str, Any] = {}

    def query(self, identifier: str):
        return self.entries[identifier] if identifier in self.entries else \
            self.parent.query(identifier) if self.parent else \
                None

    def put(self, identifier: str, value: Any = True):
        self.entries[identifier] = value

    def __str__(self) -> str:
        return str(self.entries)


class GlobalSymbolTable(SymbolTable):
    def __init__(self):
        self.types: dict[str, VarType] = {}

        super().__init__(None)

    def define_type(self, name: str, var_type: VarType):
        if name in self.types:
            raise ValueError(f"Duplicate type name '{name}'")

        self.types[name] = var_type

    def get_type(self, name: str) -> VarType:
        if name in self.types:
            return self.types[name]
        else:
            raise ValueError(f"Unknown type name '{name}'")


__all__ = ["GlobalSymbolTable", "SymbolTable"]