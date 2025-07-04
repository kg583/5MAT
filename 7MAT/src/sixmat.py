class A:
    """
    Arguments
    """

    def __init__(self, value):
        self.repr = value

    @classmethod
    def chr(cls, char):
        assert len(char) == 1

        return cls(repr(char))

    @classmethod
    def str(cls, string):
        # TODO: Fix. Sometimes uses single quotes.
        return cls(repr(string))

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
    def int(cls, num):
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

    def scope_instn(self, opcode: str, *args: A):
        return self.block_instn(opcode, *args, _scope=True)

    def block_instn(self, opcode: str, *args: A, _scope=False):
        self.instn(opcode, *args)
        self.push_block(scope=_scope)

        return self

    def push_block(self, scope=False):
        self.indent()

        if scope:
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
        self.result += f"{opcode}\t{', '.join(map(str, args))}\n"

    def comment(self, comment, debug=False):
        if SixMat.DEBUG_COMMENTS and debug or SixMat.USER_COMMENTS and not debug:
            for line in comment.splitlines():
                self.indent()
                self.result += f"; {line}\n"


__all__ = ["A", "SixMat"]
