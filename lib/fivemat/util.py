import codecs
import re

import lib.fourmat


def decode_escapes(string: str) -> str:
    def decode_match(match):
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return re.sub(r"\\[abfnrtv]|\\x..", decode_match, string).replace("↡", "\f")


def encode_escapes(string: str) -> str:
    for char in range(32):
        match char:
            case 7:
                repl = "\\a"

            case 8:
                repl = "\\b"

            case 9:
                repl = "\\t"

            case 10:
                repl = "\\n"

            case 11:
                repl = "\\v"

            case 12:
                repl = "\\f"

            case 13:
                repl = "\\r"

            case _:
                repl = f"\\x{char:02x}"

        string = string.replace(chr(char), repl)

    return string


def minify(program: str) -> str:
    program = decode_escapes(program)
    program = re.sub(r"~(-?\d+)\[.*?~]", lambda match: match[0] if int(match[1]) == 0 else "", program)
    program = re.sub(r"~\n\s*|~:\n|~@(\n)\s*", r"\1", program)

    return encode_escapes(program)


def parse(program: str) -> lib.fourmat.BlockDirective:
    return lib.fourmat.parse(decode_escapes(program))


__all__ = ["decode_escapes", "encode_escapes", "minify", "parse"]
