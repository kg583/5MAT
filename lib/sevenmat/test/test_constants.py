import unittest

from ..constants import *


class TestControlChars(unittest.TestCase):
    def test_uniqueness(self):
        uniques = [
            ControlChar.HELL_FILL_CHAR,
            ControlChar.STACK_FRAME_SEP,
            ControlChar.LOCAL_VAR_SEP,
            ControlChar.ARRAY_SEP,
            ControlChar.ARRAY_ELEM_IND,
            ControlChar.COLLATION_CHAR,
            ControlChar.UTILITY_CHAR,
        ]

        self.assertCountEqual(set(uniques), uniques, msg="Control characters must be unique.")

    def test_hell_constraints(self):
        # these control chars appear in heaven
        heaven_control_chars = [
            ControlChar.STACK_FRAME_SEP,
            ControlChar.LOCAL_VAR_SEP,
            ControlChar.ARRAY_SEP,
            ControlChar.ARRAY_ELEM_IND,
            ControlChar.COLLATION_CHAR,
            ControlChar.UTILITY_CHAR,
        ]

        for char in heaven_control_chars:
            self.assertGreater(char, ControlChar.HELL_FILL_CHAR)

        hell_control_chars = [
            ControlChar.HELL_FILL_CHAR,
        ]

        for char in hell_control_chars:
            self.assertLessEqual(char, ControlChar.HELL_FILL_CHAR)

    def test_digit_uniqueness(self):
        self.assertCountEqual(set(DIGITS), DIGITS, msg="Duplicate digits.")
