import codecs
import re

import lib.fourmat
from lib.fourmat import BlockDirective, Directive


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


class Minifier(lib.fourmat.Unparser):
    def block(self, block: BlockDirective):
        if block.type == "[" and isinstance(index := block.get_param(0), int):
            if index < len(block.clauses) and (clause := block.clauses[index]):
                self.walk(clause)

        else:
            super().block(block)

    def directive(self, directive: Directive):
        super().directive(directive)

        for index, param in enumerate(directive.params):
            if param == directive.get_default(index):
                self[-1].params[index] = None


def minify(program: str) -> str:
    minifier = Minifier()
    minifier.walk(parse(program))

    return encode_escapes(lib.fourmat.detokenize(minifier))


__all__ = ["decode_escapes", "encode_escapes", "minify", "parse", "unparse", "Minifier"]
