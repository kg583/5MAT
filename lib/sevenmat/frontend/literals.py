from abc import ABC, abstractmethod
from typing import Any

from .typing import VarType, NumericType
from ..backend.sixmat import SixMat, A


class Literal(ABC):
    def __init__(self, var_type: VarType, value: Any):
        self.var_type = var_type
        self.value = value

    @abstractmethod
    def write(self, s: SixMat):
        raise NotImplementedError()


class NumericLiteral(Literal):
    def __init__(self, var_type: NumericType, value: int | str):
        super().__init__(var_type, value)

    def __str__(self):
        max_length = len(self.var_type)
        if isinstance(self.value, int):
            digits, base, n = self.var_type.digits(), len(self.var_type.digits()), self.value
            res = []
            while n and (max_length == 0 or len(res) < max_length):
                n, r = divmod(n, base)
                res.append(digits[r])
            res = ''.join(reversed(res or [digits[0]]))
            return res.rjust(max_length, digits[0])
        elif isinstance(self.value, str):
            return self.value[-max_length:] if max_length else self.value
        else:
            raise TypeError()

    def write(self, s: SixMat):
        s.instn("PRINA", A.str(str(self)))

