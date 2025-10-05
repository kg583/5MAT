def cstr(s: bytes | str) -> str:
    if isinstance(s, str):
        s = s.encode("utf-8")

    escapes = {
        0: r'\0',
        9: r'\t',
        10: r'\n',
        12: r'\f',
        13: r'\r',
        34: r'\"',
        92: r'\\',
    }

    escaped = []
    for ch in s:
        if ch in escapes:
            escaped.append(escapes[ch])
        elif 32 <= ch < 127:
            escaped.append(chr(ch))
        else:
            escaped.append(f'\\x{ch:02x}')

    return '"' + ''.join(escaped) + '"'


class Writer:
    def __init__(self):
        self.result = ""

        self.stack = []

    def write_raw(self, code: str):
        self.result += code

    def newline(self):
        self.result += "\n" + "\t" * len(self.stack)

    def write_line(self, line: str):
        self.newline()
        self.write_raw(line)

    def comment(self, text: str):
        for line in text.splitlines():
            self.write_line(f"// {line}")

    def function_call(self, name: str, *args):
        self.write_line(f"{name}({', '.join(map(str, args))});")

    def function_decl(self, return_type: str, name: str, *args: tuple[str, str]) -> "Writer":
        self.write_line(f"{return_type} {name}({', '.join(map(lambda x: " ".join(x), args))})")
        return self.scope()

    def function_return(self, value: str):
        self.write_line(f"return {value};")

    def for_loop(self, init: str, condition: str, update: str) -> "Writer":
        self.write_line(f"for ({init}; {condition}; {update})")
        return self.scope()

    def while_loop(self, condition: str) -> "Writer":
        self.write_line(f"while ({condition})")
        return self.scope()

    def do_while_loop(self, condition: str) -> "Writer":
        self.write_line("do {")
        self.stack.append(f"}} while ({condition});")

        return self

    def if_stmt(self, condition: str, body: str = ""):
        if not body:
            self.write_line(f"if ({condition})")
            return self.scope()
        else:
            self.write_line(f"if ({condition}) {body}")
            return None

    def else_stmt(self) -> "Writer":
        self.write_line("else")
        return self.scope()

    def switch_stmt(self, condition: str) -> "Writer":
        self.write_line(f"switch ({condition})")
        return self.scope()

    def case_stmt(self, value=None, allow_fallthrough=False) -> "Writer":
        """
        :param value: None for default.
        """

        if value is None:
            self.write_line("default:")
        else:
            self.write_line(f"case {value}:")

        if not allow_fallthrough:
            self.stack.append("\tbreak;")
        return self

    def scope(self):
        self.write_line("{")
        self.stack.append("}")

        return self

    def __enter__(self):
        pass

    def __exit__(self, *args):
        self.write_line(self.stack.pop())


__all__ = ["Writer", "cstr"]
