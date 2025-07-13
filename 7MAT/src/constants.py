import string


class ControlChar:
    # reminder: hell is chars 0x08 and lower. don't use those here unless the control is happening in hell
    STACK_FRAME_SEP = '\f'
    """
    Separates stack frames.
    """

    LOCAL_VAR_SEP = '\x1d'
    """
    Separates local variables.
    """

    ARRAY_SEP = '\x1e'
    """
    Separates elements of an array.
    """

    ARRAY_ELEM_IND = '\x1f'
    """
    Used by the array access routine to indicate which element has been selected.
    """

    COLLATION_CHAR = '\x17'  # ETB returns!
    """
    Separates printing and non-printing sections of the tape. Recognized by the print/collation routine.
    """

    @staticmethod
    def path(index: int, branch_taken: bool):
        charset = string.ascii_uppercase if branch_taken else string.ascii_lowercase
        # *please* email us if you find yourself using more than 2**26 blocks
        return charset[index % len(charset)]


__all__ = ["ControlChar"]
