class A:
    """
    Arguments
    """

    def __init__(self, value):
        self.repr = value

    @classmethod
    def chr(cls, char: str):
        assert len(char) == 1

        return cls(f"'{repr(char)[1:-1]}'")

    @classmethod
    def str(cls, string: str):
        return cls(f'"{repr(string)[1:-1]}"')

    @classmethod
    def remaining(cls):
        return cls("$R")

    @classmethod
    def read(cls, count: int | None = None):
        if count is None:
            string = "$V"
        else:
            string = f"${count}"

        return cls(string)

    @classmethod
    def peek(cls, count: int | None = None):
        if count is None:
            string = "?V"
        else:
            string = f"?{count}"

        return cls(string)

    def __str__(self):
        return self.repr

    @classmethod
    def int(cls, num: int):
        return cls(repr(num))


class SixMat:
    INDENTATION = "    "
    USER_COMMENTS = True
    DEBUG_COMMENTS = True

    def __init__(self):
        self.blocks = []

        self.result = ""

    def indent(self):
        self.result += SixMat.INDENTATION * len(self.blocks)

    def block_instn(self, opcode: str = "", *args: A):
        return self.group_instn(opcode, *args, _block=True)

    def group_instn(self, opcode: str = "", *args: A, _block=False):
        self.instn(opcode, *args)
        self.push_block(block=_block)

        return self

    def case(self, option: A, _block=True):
        # TODO: check that we're actually in a case
        return self.group_instn(str(option), _block=_block)

    def push_block(self, block=False):
        self.indent()

        if block:
            self.result += "{\n"
            self.blocks.append("}")
        else:
            self.result += "[\n"
            self.blocks.append("]")

    def __enter__(self):
        pass

    def __exit__(self, *args):
        closing_delimiter = self.blocks.pop()
        self.indent()
        self.result += closing_delimiter + "\n"

    def instn(self, opcode: str, *args: A):
        self.indent()
        self.result += f"{opcode}\t{', '.join(map(str, args))}\n".rstrip("\t")

    def comment(self, comment, debug=False):
        if SixMat.DEBUG_COMMENTS and debug or SixMat.USER_COMMENTS and not debug:
            for line in comment.splitlines():
                self.indent()
                self.result += f"; {line}\n"


__all__ = ["A", "SixMat"]
