from abc import ABC, abstractmethod
from dataclasses import dataclass

from lib.sevenmat.constants import DIGITS


class VarType(ABC):
    @abstractmethod
    def __len__(self) -> int:
        """
        :return: `0` if unbounded, or the length if bounded.
        """
        raise NotImplementedError()


@dataclass
class StringType(VarType):
    max_length: int

    def __len__(self) -> int:
        return self.max_length


@dataclass
class NumericType(VarType):
    radix: int
    max_length: int

    def digits(self):
        return DIGITS[:self.radix]

    def __len__(self) -> int:
        return self.max_length


# Aggregate Types
@dataclass
class StructType(VarType):
    """
    Structs are a set of local variables that are guaranteed to be in a certain order.

    Layout: `field1|field2|field3`
    where `|` is the local variable separator (ControlChar.LOCAL_VAR_SEP)
    """
    fields: list[VarType]
    field_index: dict[str, int]
    """
    Map from field name -> order 
    """

    def __init__(self, fields: list[tuple[str, VarType]]) -> None:
        self.fields = []
        self.field_index = {}

        for idx, (field_name, field_type) in enumerate(fields):
            self.fields.append(field_type)
            self.field_index[field_name] = idx

    def __len__(self) -> int:
        lengths = map(len, self.fields)
        if 0 in lengths:
            return 0
        else:
            separator_count = len(self.fields) - 1
            return sum(lengths) + separator_count

    def field_type(self, field_name: str) -> VarType | None:
        if field_name not in self.field_index:
            return None

        return self.fields[self.field_index[field_name]]

    @classmethod
    def parse(cls, s: str) -> "StructType":
        raise NotImplementedError()


@dataclass
class StructRef(VarType):
    name: str

    def __len__(self) -> int:
        raise NotImplementedError()


@dataclass
class ArrayType(VarType):
    """
    Layout (still TBD)
    ```
    , field1, field2, field3
    ```
    where `,` is the array element separator (`ControlChar.ARRAY_SEP`) and ` ` is a spot for the array element indicator
    (`ControlChar.ARRAY_ELEM_IND`) used as output for the element selection routine.
    """
    element: VarType
    length: int
    """
    The length if this is an immutable array (fixed size), or 0 if this array is mutable (dynamic size).
    """

    def __len__(self) -> int:
        if len(self.element) == 0 or self.length == 0:
            return 0
        else:
            return (2 + len(self.element)) * self.length


__all__ = ["VarType", "StringType", "NumericType", "StructType", "StructRef", "ArrayType"]
