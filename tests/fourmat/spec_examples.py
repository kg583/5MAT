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

    def test_fixed_format(self):
        foo = lambda x: fourmat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", [x] * 6)
        self.assertEqual(foo(3.14159), "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")
        self.assertEqual(foo(-3.14159), " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")
        self.assertEqual(foo(100.0), "100.00|******|100.00| 100.0|100.00|100.0")
        self.assertEqual(foo(1234.0), "1234.00|******|??????|1234.0|1234.00|1234.0")
        self.assertEqual(foo(0.006), "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

    def test_exponential_format(self):
        foo = lambda x: fourmat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@E|~9,2E", [x] * 4)
        self.assertEqual(foo(3.14159), "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0")
        self.assertEqual(foo(-3.14159), " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0")
        self.assertEqual(foo(1100.0), "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3")
        self.assertEqual(foo(1.1e13), "*********| 11.00$+12|+.001E+16| 1.10E+13")
        self.assertEqual(foo(1.1e120), "*********|??????????|%%%%%%%%%|1.10E+120")

        self.assertEqual(fourmat("|~13,6,2,VE|", [-5, 3.14159]), "| 0.000003E+06|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [-4, 3.14159]), "| 0.000031E+05|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [-3, 3.14159]), "| 0.000314E+04|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [-2, 3.14159]), "| 0.003142E+03|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [-1, 3.14159]), "| 0.031416E+02|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [0, 3.14159]), "| 0.314159E+01|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [1, 3.14159]), "| 3.141590E+00|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [2, 3.14159]), "| 31.41590E-01|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [3, 3.14159]), "| 314.1590E-02|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [4, 3.14159]), "| 3141.590E-03|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [5, 3.14159]), "| 31415.90E-04|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [6, 3.14159]), "| 314159.0E-05|")
        self.assertEqual(fourmat("|~13,6,2,VE|", [7, 3.14159]), "| 3141590.E-06|")

    def test_general_format(self):
        foo = lambda x: fourmat("~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G", [x] * 4)
        self.assertEqual(foo(0.0314159), "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")
        self.assertEqual(foo(0.314159), "  0.31   |0.314    |0.314    | 0.31    ")
        self.assertEqual(foo(3.14159), "   3.1   | 3.14    | 3.14    |  3.1    ")
        self.assertEqual(foo(31.4159), "   31.   | 31.4    | 31.4    |  31.    ")
        self.assertEqual(foo(314.159), "  3.14E+2| 314.    | 314.    |  3.14E+2")
        self.assertEqual(foo(3141.59), "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")
        self.assertEqual(foo(3.14E12), "*********|314.0$+10|0.314E+13| 3.14E+12")
        self.assertEqual(foo(3.14E120), "*********|?????????|%%%%%%%%%|3.14E+120")

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
