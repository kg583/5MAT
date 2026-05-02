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


def parse(program: str) -> lib.fourmat.BlockDirective:
    return lib.fourmat.parse(lib.fourmat.tokenize(decode_escapes(program)))


def unparse(block: lib.fourmat.BlockDirective) -> str:
    return encode_escapes(lib.fourmat.detokenize(lib.fourmat.unparse(block)))


def minify(program: str) -> str:
    return re.sub(r"~([+-]?\d+)\[.*?~]", lambda match: match[0] if int(match[1]) == 0 else "",
                  unparse(parse(program)), flags=re.DOTALL)


__all__ = ["decode_escapes", "encode_escapes", "minify", "parse"]
