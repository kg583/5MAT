import unittest

from lib.fourmat.evaluate import *


class MiscOpsSpecTests(unittest.TestCase):
    def test_case_conversion(self):
        f = lambda n: fourmat("~@(~R~) error~:P detected.", [n])
        self.assertEqual(f(0), "Zero errors detected.")
        self.assertEqual(f(1), "One error detected."),
        self.assertEqual(f(23), "Twenty-three errors detected.")

        self.assertEqual(fourmat("~@(how is ~:(BOB SMITH~)?~)", []), "How is bob smith?")

    def test_plural(self):
        self.assertEqual(fourmat("~D tr~:@P/~D win~:P", [7, 1]), "7 tries/1 win")
        self.assertEqual(fourmat("~D tr~:@P/~D win~:P", [1, 0]), "1 try/0 wins")
        self.assertEqual(fourmat("~D tr~:@P/~D win~:P", [1, 3]), "1 try/3 wins")


if __name__ == '__main__':
    unittest.main()
