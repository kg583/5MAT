import unittest

from lib.fourmat.evaluate import *


class FloatingPointPrintersSpecTests(unittest.TestCase):
    def test_rounding(self):
        self.assertIn(fourmat("~4,2F", [6.375]), ["6.37", "6.38"])
        self.assertIn(fourmat("~8,2E", [637.5]), [" 6.37E+2", " 6.38E+2"])


class FloatingPointPrinters(unittest.TestCase):
    def test_small_width(self):
        self.assertEqual(fourmat("~1f", [0]), ".")
        self.assertEqual(fourmat("~1f", [1]), "1.")

        self.assertEqual(fourmat("~1,,,'Xf", [0]), ".")
        self.assertEqual(fourmat("~1,,,'Xf", [1]), "X")


if __name__ == '__main__':
    unittest.main()
