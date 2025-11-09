import unittest

from lib.fourmat.evaluate import *


class BasicOutputSpecTests(unittest.TestCase):
    def test_character(self):
        self.assertEqual(fourmat("~C", ['A']), "A")
        self.assertEqual(fourmat("~C", [' ']), " ")

        self.assertEqual(fourmat("~:C", ['A']), "A")
        self.assertEqual(fourmat("~:C", [' ']), "Space")

        # ~:@C not implemented


class BasicOutput(unittest.TestCase):
    def test_literal(self):
        self.assertEqual(fourmat("abc123", []), "abc123")

    def test_repeated(self):
        self.assertEqual(fourmat("~10~~~", []), "~" * 11)

    def test_fresh_line(self):
        self.assertEqual(fourmat("~%~5&", []), "\n" * 5)
        self.assertEqual(fourmat("~&X", []), "X")
        self.assertEqual(fourmat("X~1000@{~&~}X", [0]), "X\nX")


if __name__ == '__main__':
    unittest.main()
