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


if __name__ == '__main__':
    unittest.main()
