import unittest

from lib.fourmat.evaluate import *


class RadixControlSpecTests(unittest.TestCase):
    def test_radix(self):
        self.assertEqual(fourmat("~R", [4]), "four")
        self.assertEqual(fourmat("~:R", [4]), "fourth")
        self.assertEqual(fourmat("~@R", [4]), "IV")
        self.assertEqual(fourmat("~:@R", [4]), "IIII")

        self.assertEqual(fourmat("~,,' ,4:B", [13]), "1101")
        self.assertEqual(fourmat("~,,' ,4:B", [17]), "1 0001")
        # We believe the HyperSpec is wrong on this next case
        self.assertEqual(fourmat("~19,'0,' ,4:B", [3333]), "000001101 0000 0101")
        self.assertEqual(fourmat("~3,,,' ,2:R", [17]), "1 22")
        self.assertEqual(fourmat("~,,'|,2:D", [0xFFFF]), "6|55|35")


if __name__ == '__main__':
    unittest.main()
