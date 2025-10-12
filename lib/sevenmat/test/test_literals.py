import unittest

from ..constants import DIGITS
from ..frontend.literals import NumericLiteral
from ..frontend.typing import *

def expected(*digits: int):
    return "".join(map(lambda x: DIGITS[x], digits))


class NumericLiteralTests(unittest.TestCase):
    def test_unbounded(self):
        binary = NumericType(radix=2, max_length=0)
        hex = NumericType(radix=16, max_length=0)

        test_cases = [
            (NumericLiteral(binary, 0b0), (0,)),
            (NumericLiteral(binary, 0b1), (1,)),
            (NumericLiteral(binary, 0b1111_1111_1111_1111), (1,) * 16),

            (NumericLiteral(hex, 0), (0,)),
            (NumericLiteral(hex, 0xF), (15,)),
            (NumericLiteral(hex, 0x1234ABCDE), (1, 2, 3, 4, 10, 11, 12, 13, 14)),
        ]

        for subject, predicate in test_cases:
            self.assertEqual(str(subject), expected(*predicate))

    def test_bounded(self):
        binary = NumericType(radix=2, max_length=8)
        hex = NumericType(radix=16, max_length=8)

        test_cases = [
            (NumericLiteral(binary, 0b0), (0,) * 8),
            (NumericLiteral(binary, 0b1), (0, 0, 0, 0, 0, 0, 0, 1,)),
            (NumericLiteral(binary, 0b1111_1111_1111_0110), (1, 1, 1, 1, 0, 1, 1, 0)),

            (NumericLiteral(hex, 0), (0,) * 8),
            (NumericLiteral(hex, 0xF), (0, 0, 0, 0, 0, 0, 0, 15,)),

            (NumericLiteral(hex, 0x1234ABCDE), (2, 3, 4, 10, 11, 12, 13, 14)),

        ]

        for subject, predicate in test_cases:
            self.assertEqual(str(subject), expected(*predicate))
