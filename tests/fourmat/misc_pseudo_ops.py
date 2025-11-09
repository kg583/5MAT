import unittest

from lib.fourmat.evaluate import *


class MiscPseudoOpsSpecTests(unittest.TestCase):
    def test_escape_upward(self):
        donestr = "Done.~^ ~D warning~:P.~^ ~D error~:P."
        self.assertEqual(fourmat(donestr, []), "Done.")
        self.assertEqual(fourmat(donestr, [3]), "Done. 3 warnings."),
        self.assertEqual(fourmat(donestr, [1, 5]), "Done. 1 warning. 5 errors.")

        # We believe that the HyperSpec is wrong on this next case.
        self.assertEqual(fourmat("~:{ ~@?~:^ ...~} ", [[["a"], ["b"]]]), " a ... b ")

        tellstr = "~@(~@[~R~]~^ ~A!~)"
        # This next one too.
        self.assertEqual(fourmat(tellstr, [23]), "Twenty-three")
        self.assertEqual(fourmat(tellstr, [None, "losers"]), " Losers!")
        self.assertEqual(fourmat(tellstr, [23, "losers"]), "Twenty-three losers!")

        self.assertEqual(fourmat("~15<~S~;~^~S~;~^~S~>", ["FOO"]), "            FOO")
        self.assertEqual(fourmat("~15<~S~;~^~S~;~^~S~>", ["FOO", "BAR"]), "FOO         BAR")
        self.assertEqual(fourmat("~15<~S~;~^~S~;~^~S~>", ["FOO", "BAR", "BAZ"]), "FOO   BAR   BAZ")

    def test_ignored_newline(self):
        # And both of these.
        def type_clash_error(fn, nargs, argnum, right_type, wrong_type):
            return fourmat(("~&~S requires its ~:[~:R~;~*~]~\n"
                            "argument to be of type ~S,~%but it was called ~\n"
                            "with an argument of type ~S.~%"),
                           [fn, nargs==1, argnum, right_type, wrong_type])

        self.assertEqual(type_clash_error("AREF", None, 2, "INTEGER", "VECTOR"),
                         "AREF requires its second argument to be of type INTEGER,\nbut it was called with an argument of type VECTOR.\n")
        self.assertEqual(type_clash_error("CAR", 1, 1, "LIST", "SHORT-FLOAT"),
                         "CAR requires its argument to be of type LIST,\nbut it was called with an argument of type SHORT-FLOAT.\n")


if __name__ == '__main__':
    unittest.main()
