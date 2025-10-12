import re
from pathlib import Path
from itertools import batched

from lark import Lark
from lark.visitors import Transformer_InPlace, Visitor

from .symbol_table import GlobalSymbolTable, SymbolTable
from .typing import *


class GlobalVisitor(Visitor):
    def __init__(self):
        self.symbol_table = GlobalSymbolTable()
        self.functions = {}
        super().__init__()

    def struct_def(self, tree):
        sym_name, members_list = tree.children

        fields = []
        for var_decl in members_list.children:
            field_type = var_decl.children[0].datatype
            field_name = var_decl.children[1].value
            fields.append((field_name, field_type))

        struct = StructType(fields)
        self.symbol_table.define_type(sym_name.value, struct)


NUMERIC_TYPE_REGEX = re.compile(r"Int(?P<radix>3[0-6]|[12]\d|[2-9])(?:_(?P<max_length>\d{1,3}))?")
STRING_TYPE_REGEX = re.compile(r"Str(?:_(?P<max_length>\d{1,3}))?")


class T(Transformer_InPlace):
    def __init__(self):
        super().__init__(True)

    @staticmethod
    def POSITIVE_INT(tok):
        return tok.update(value=int(tok.value))

    @staticmethod
    def CONSTANT(tok):
        return tok.update(value=int(tok.value))

    @staticmethod
    def type_str(tree):
        matches = STRING_TYPE_REGEX.match(tree.children[0])
        max_length = int(matches.group("max_length") or 0)

        tree.datatype = StringType(max_length)
        return tree

    @staticmethod
    def type_int(tree):
        matches = NUMERIC_TYPE_REGEX.match(tree.children[0])
        radix = int(matches.group("radix"))
        max_length = int(matches.group("max_length") or 0)

        tree.datatype = NumericType(radix, max_length)
        return tree

    @staticmethod
    def type_arr(tree):
        element = tree.children[0].datatype
        length = tree.children[1].value if len(tree.children) > 1 else 0

        tree.datatype = ArrayType(element, length)
        return tree

    @staticmethod
    def type_struct(tree):
        tree.datatype = StructRef(str(tree.children[0]))
        return tree


class Program:
    PARSER = Lark.open(Path(__file__).parent / "7mat.lark", propagate_positions=True, parser="lalr", lexer="contextual",
                       transformer=T())

    def __init__(self, source: str):
        self.source = source
        self.global_symbols = None
        self.functions = {}

        self.tree = None

    def parse(self):
        self.tree = self.PARSER.parse(self.source)

        gv = GlobalVisitor()
        gv.visit_topdown(self.tree)
        self.global_symbols = gv.symbol_table
        self.functions = gv.functions

