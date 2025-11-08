import unittest

from lib.fourmat.evaluate import *


class ControlFlow(unittest.TestCase):
    def test_goto(self):
        self.assertEqual(fourmat("~3*~c", ['a', 'b', 'c', 'd']), "d")

    def test_conditional(self):
        self.assertEqual(fourmat("~#[abc~;def~]~:[ghi~;jkl~]", [None]), "defghi")

    def test_iteration(self):
        self.assertEqual(fourmat("~{~c~}", [['a', 'b', 'c', 'd']]), "abcd")
        self.assertEqual(fourmat("~@{~c~}", ['a', 'b', 'c', 'd']), "abcd")


if __name__ == '__main__':
    unittest.main()
