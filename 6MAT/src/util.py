import codecs
import re


def decode_escapes(string: str) -> str:
    def decode_match(match):
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return re.sub(r"\\[abfnrtv]|\\x..", decode_match, string)


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
