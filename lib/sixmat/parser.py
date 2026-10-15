import ast
import re

from collections import defaultdict
from lark import Lark
from lark.visitors import Discard, Transformer_InPlace, Visitor
from pathlib import Path
from warnings import warn

from lib.fivemat.util import *


Instructions = dict[str, dict[tuple[str, ...], str]]


class Preprocessor(Transformer_InPlace):
    def __init__(self, preserve_comments: bool = True, preserve_indents: bool = True):
        self.preserve_comments = preserve_comments
        self.preserve_indents = preserve_indents

        super().__init__(visit_tokens=True)

    @staticmethod
    def PARAM(tok):
        return tok.update(value=tok.value.removeprefix("@"))

    @staticmethod
    def POS(tok):
        return tok.update(value=int(tok.value), pattern=re.compile(r"[%+][A-Z]"))

    @staticmethod
    def NEG(tok):
        return tok.update(value=int(tok.value), pattern=re.compile(r"[%-][A-Z]"))

    @staticmethod
    def REMAIN(tok):
        return tok.update(pattern=re.compile(r"[%+][A-Z]|\$R"))

    @staticmethod
    def READ(tok):
        return tok.update(pattern=re.compile(r"\$V"))

    @staticmethod
    def READS(tok):
        return tok.update(pattern=re.compile(r"\$[a-z]"))

    @staticmethod
    def PEEK(tok):
        return tok.update(pattern=re.compile(r"\?V"))

    @staticmethod
    def PEEKS(tok):
        return tok.update(pattern=re.compile(r"\?[a-z]"))

    @staticmethod
    def NIL(tok):
        return tok.update(value=None, pattern=re.compile(r"[%+-][A-Z]|'[A-Z]"))

    @staticmethod
    def EXPR(tok):
        return tok.update(value=ast.parse(tok.value.removeprefix("@|").removesuffix("|@")))

    @staticmethod
    def RAW(tok):
        return tok.update(value=tok.removeprefix("`").removesuffix("`"))

    @staticmethod
    def string(tree):
        return "".join(tree.children)

    def COMMENT(self, tok):
        if self.preserve_comments:
            return tok.update(value=tok.value.removeprefix(";"))

        else:
            return Discard

    def INDENT(self, tok):
        if self.preserve_indents:
            return tok

        else:
            return Discard


class Assembler(Visitor):
    def __init__(self, instructions: Instructions = None, flags: dict[str, bool] = None):
        self.instructions = instructions
        self.flags = flags

    def instruction(self, tree):
        instr, *args = tree.children
        instr = instr.upper()

        specs = self.instructions.get(instr)
        if not specs:
            specs = self.instructions.get(instr + "!")

            warn(f"instruction '{instr}' at line {tree.line}, col {tree.column} is a macro, "
                 f"and should be written as '{instr}!'",
                 UserWarning)

        for spec, code in specs.items():
            matched = [*zip(spec, args)]
            if len(matched) < len(spec):
                continue

            for index, (name, arg) in enumerate(matched):
                if name.startswith("_"):
                    # Generic placeholders only allowed with templates
                    code = re.sub(r"`(.*?)`", lambda match: match[0].replace(name, str(arg.value)), code)
                    continue

                if name.isnumeric():
                    if name == repr(arg.value):
                        continue

                    else:
                        break

                elif not arg.pattern.fullmatch(name):
                    break

                repls = {}
                match arg.type:
                    case "NIL":
                        repls = {name: ""}

                    case "POS" | "NEG":
                        num = int(arg.value)

                        repls = {
                            f"%{name[1]}-1": num - 1,
                            f"%{name[1]}+1": num + 1,
                            f"%{name[1]}": num,
                            f"+{name[1]}-1": abs(num) - 1,
                            f"+{name[1]}+1": abs(num) + 1,
                            f"+{name[1]}": abs(num),
                            f"-{name[1]}-1": -abs(num) - 1,
                            f"-{name[1]}+1": -abs(num) + 1,
                            f"-{name[1]}": -abs(num),
                        }

                    case "REMAIN":
                        repls = {name: "#"}

                    case "CHAR":
                        character = arg.value[1]

                        repls = {
                            f"'{chr(ord(name[1]) - 1)}": f"'{chr(ord(character) - 1)}",
                            f"'{chr(ord(name[1]))}": f"'{chr(ord(character))}",
                            f"'{chr(ord(name[1]) + 1)}": f"'{chr(ord(character) + 1)}",
                            f'"{name[1]}"': character.replace("~", "~~")
                        }

                    case "READS" | "PEEKS":
                        count = int(arg.value[1:])

                        repls = {
                            name[1] * 2: count,
                            f"${name[1]}-1": f"${count - 1}",
                            f"?{name[1]}-1": f"?{count - 1}",
                            name: arg.value
                        }

                    case "STRING":
                        repls = {
                            f"'{name[1]}'": arg.value[0],
                            f".{name[1]}": arg.value[1:],
                            "$_": f"${len(arg.value)}",
                            "?_": f"?{len(arg.value)}",
                            name: arg.value
                        }

                    case "RAW":
                        repls = {"...": arg.value[1:-1]}

                for sub, value in repls.items():
                    repl = str(value)
                    if sub != "...":
                        repl = encode_escapes(repl)

                    code = code.replace(sub, repl)


def load_grammar(filename: str | Path) -> Instructions:
    instructions = defaultdict(dict[tuple[str, ...], str])

    with open(filename) as file:
        for line in file:
            if line.isspace():
                continue

            if line.startswith(";"):
                continue

            instr, *arg_spec, template = re.split(r" {2,}", line.strip())

            for print_type in "ACLNR":
                instructions[instr.replace("z", print_type)] |= {tuple(arg_spec): template.replace("z", print_type)}

    return instructions


def assemble(program: str, instructions: Instructions = None, flags: dict[str, bool] = None, params: dict = None) -> str:
    program = decode_escapes(program)

    parser = Lark.open(Path(__file__).parent / "6mat.lark",
                       g_regex_flags=re.IGNORECASE, parser="lalr", lexer="contextual", propagate_positions=True)
    instructions = load_grammar(Path(__file__).parent / "instructions.g")
    instructions |= instructions or {}
