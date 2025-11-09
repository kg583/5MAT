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
