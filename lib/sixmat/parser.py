import ast
import re

from lark import Lark
from lark.visitors import Transformer_InPlace
from pathlib import Path

from lib.fivemat.util import *


class Transformer(Transformer_InPlace):
    def __init__(self, params: dict):
        self.params = params

    @staticmethod
    def POS(tok):
        return tok.update(value=int(tok.value))

    @staticmethod
    def NEG(tok):
        return tok.update(value=int(tok.value))

    @staticmethod
    def NIL(tok):
        return tok.update(value=None)

    @staticmethod
    def EXPR(tok):
        print(ast.dump(ast.parse(tok.value[2:-2])))


PARSER = Lark.open(Path(__file__).parent / "6mat.lark",
                   g_regex_flags=re.IGNORECASE, parser="lalr", lexer="contextual", propagate_positions=True,
                   transformer=Transformer({}))
