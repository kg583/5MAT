import codecs
import re


def decode_escapes(string: str) -> str:
    def decode_match(match):
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return re.sub(r"\\[fnrt]|\\x..", decode_match, string)


def encode_escapes(string: str) -> str:
    for char in range(32):
        match char:
            case 9:
                repl = "\\t"

            case 10:
                repl = "\\n"

            case 12:
                repl = "\\f"

            case 13:
                repl = "\\r"

            case _:
                repl = f"\\x{char:02x}"

        string = string.replace(chr(char), repl)

    return string


__all__ = ["decode_escapes", "encode_escapes"]
