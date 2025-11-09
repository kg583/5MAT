import unittest

from lib.fourmat.evaluate import *


class ControlFlowSpecExamples(unittest.TestCase):
    def test_conditional(self):
        self.assertEqual(fourmat("~@[ print level = ~D~]~@[ print length = ~D~]", [None, 5]), " print length = 5")

        # The HyperSpec erroneously includes a space before the comma
        foo = "Items:~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]."
        self.assertEqual(fourmat(foo, []), "Items: none.")
        self.assertEqual(fourmat(foo, ["FOO"]), "Items: FOO.")
        self.assertEqual(fourmat(foo, ["FOO", "BAR"]), "Items: FOO and BAR.")
        self.assertEqual(fourmat(foo, ["FOO", "BAR", "BAZ"]), "Items: FOO, BAR, and BAZ.")
        self.assertEqual(fourmat(foo, ["FOO", "BAR", "BAZ", "QUUX"]), "Items: FOO, BAR, BAZ, and QUUX.")

    def test_iteration(self):
        self.assertEqual(fourmat("The winners are:~{ ~S~}.", [["FRED", "HARRY", "JILL"]]), "The winners are: FRED HARRY JILL.")

        # ~S has been replaced with ~A since symbols are not a supported type
        pairs = "Pairs: <A,1> <B,2> <C,3>."
        self.assertEqual(fourmat("Pairs:~{ <~A,~A>~}.", [["A", 1, "B", 2, "C", 3]]), pairs)
        self.assertEqual(fourmat("Pairs:~:{ <~A,~A>~}.", [[["A", 1], ["B", 2], ["C", 3]]]), pairs)
        self.assertEqual(fourmat("Pairs:~@{ <~A,~A>~}.", ["A", 1, "B", 2, "C", 3]), pairs)
        self.assertEqual(fourmat("Pairs:~:@{ <~A,~A>~}.", [["A", 1], ["B", 2], ["C", 3]]), pairs)

    def test_recursive(self):
        self.assertEqual(fourmat("~? ~D", ["<~A ~D>", ["Foo", 5], 7]), "<Foo 5> 7")
        self.assertEqual(fourmat("~? ~D", ["<~A ~D>", ["Foo", 5, 14], 7]), "<Foo 5> 7")

        self.assertEqual(fourmat("~@? ~D", ["<~A ~D>", "Foo", 5, 7]), "<Foo 5> 7")
        self.assertEqual(fourmat("~@? ~D", ["<~A ~D>", "Foo", 5, 14, 7]), "<Foo 5> 14")


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
