import unittest

from lib.fourmat.evaluate import *


class ExampleSpecTests(unittest.TestCase):
    def test_basics(self):
        self.assertEqual(fourmat("foo", []), "foo")
        self.assertEqual(fourmat("The answer is ~D.", [5]), "The answer is 5.")
        self.assertEqual(fourmat("The answer is ~3D.", [5]), "The answer is   5.")
        self.assertEqual(fourmat("The answer is ~:D.", [47 ** 5]), "The answer is 229,345,007.")

        self.assertEqual(fourmat("Look at the ~A!", ["elephant"]), "Look at the elephant!")  # 🐘❤️
        self.assertEqual(fourmat("~D item~:P found.", [3]), "3 items found.")

        self.assertEqual(fourmat("~R dog~:[s are~; is~] here.", [3, False]), "three dogs are here.")
        self.assertEqual(fourmat("~R dog~:*~[s are~; is~:;s are~] here.", [3]), "three dogs are here.")
        self.assertEqual(fourmat("Here ~[are~;is~:;are~] ~:*~R pupp~:@P.", [3]), "Here are three puppies.")

    def test_foo(self):
        foo = lambda x: fourmat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", [x] * 6)
        self.assertEqual(foo(3.14159), "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")
        self.assertEqual(foo(-3.14159), " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")
        self.assertEqual(foo(100.0), "100.00|******|100.00| 100.0|100.00|100.0")
        self.assertEqual(foo(1234.0), "1234.00|******|??????|1234.0|1234.00|1234.0")

    def test_justify(self):
        self.assertEqual(fourmat("~10<foo~;bar~>", []), "foo    bar")
        self.assertEqual(fourmat("~10:<foo~;bar~>", []), "  foo  bar")
        self.assertEqual(fourmat("~10<foobar~>", []), "    foobar")
        self.assertEqual(fourmat("~10:<foobar~>", []), "    foobar")
        self.assertEqual(fourmat("~10:@<foo~;bar~>", []), "  foo bar ")
        self.assertEqual(fourmat("~10@<foobar~>", []), "foobar    ")
        self.assertEqual(fourmat("~10:@<foobar~>", []), "  foobar  ")


if __name__ == '__main__':
    unittest.main()
