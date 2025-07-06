import re

from collections import defaultdict
from dataclasses import asdict, dataclass, field
from pathlib import Path
from warnings import warn

from .util import *


TOKENS = re.compile(
    r"(?P<newline>[\r\n]\s*)|"
    r"(?P<comment>;[^\r\n]*)|"
    r"(?P<lbrace>\{)|"
    r"(?P<rbrace>})|"
    r"(?P<lbracket>\[)|"
    r"(?P<rbracket>])|"
    r"(?P<sep>,)|"
    r"(?P<nil>NIL)|"
    r"(?P<char>'.')|"
    r"(?P<read>\$V)|"
    r"(?P<reads>\$\d+)|"
    r"(?P<peek>\?V)|"
    r"(?P<peeks>\?\d+)|"
    r"(?P<pos>\+?[0-9]+)|"
    r"(?P<neg>-[0-9]+)|"
    r"(?P<remain>\$R)|"
    r'(?P<string>"(?:\\.|[^"])*")|'
    r"(?P<raw>\|(?:\\.|[^`])*\|)|"
    r"(?P<instr>[-#!?A-Za-z]+)|"
    r"(?P<error>\S+)")

ARG_TYPES = {"nil", "char", "read", "reads", "peek", "peeks", "pos", "neg", "remain", "string"}
CASE_SENSITIVE = {"comment", "char", "string", "raw"}

Instructions = dict[str, dict[tuple[str, ...], str]]


@dataclass
class Token:
    type: str = ""
    value: str = ""
    pos: int = 0

    def __str__(self) -> str:
        return self.value.upper()

    @property
    def pattern(self) -> re.Pattern:
        match self.type:
            case "lbrace":
                return re.compile(r"\{\.\.\.}")

            case "lbracket":
                return re.compile(r"\[\.\.\.]")

            case "nil":
                return re.compile(r"[%+-][A-Z]|'[A-Z]")

            case "char":
                return re.compile(r"'[A-Z]")

            case "read":
                return re.compile(r"\$V")

            case "reads":
                return re.compile(r"\$[a-z]")

            case "peek":
                return re.compile(r"\?V")

            case "peeks":
                return re.compile(r"\?[a-z]")

            case "pos":
                return re.compile(r"[%+][A-Z]")

            case "neg":
                return re.compile(r"[%-][A-Z]")

            case "remain":
                return re.compile(r"[%+][A-Z]|\$R")

            case "string":
                return re.compile(r'"["A-Z]')

            case "instr":
                return re.compile(self.value)

            case "raw":
                return re.compile(r"\|\.\.\.\|")

            case _:
                return re.compile(r"$.")


@dataclass
class Context:
    block_name: str = ""
    block_open: str = ""

    instructions: dict = field(default_factory=lambda: {})
    strings: dict = field(default_factory=lambda: {})
    flags: dict = field(default_factory=lambda: {})

    @property
    def block(self) -> bool:
        return bool(self.block_open)

    @property
    def block_close(self) -> str:
        return f"r{self.block_open[1:]}"

    def in_block(self, block_name: str, block_open: str) -> 'Context':
        return Context(block_name, block_open, self.instructions, self.strings, self.flags)

    def add_macro(self, name: str, macro: str):
        self.instructions[name] = {(): macro}

    def get_instruction(self, name: str) -> dict[tuple, str]:
        return self.instructions[name]

    def add_string(self, string: str):
        self.strings[self.next_string()] = string

    def get_escaped_string(self, key: str) -> str:
        return encode_escapes(self.get_string(key).replace("~", "~~"))

    def get_string(self, key: str) -> str:
        key, index = key.strip('"').split(":")
        return self.strings[f'"{key}:0"'][int(index):]

    def last_string(self) -> str:
        return f'"{len(self.strings) - 1}:0"'

    def next_string(self) -> str:
        return f'"{len(self.strings)}:0"'

    def string_first(self, key: str) -> str:
        return f"'{self.get_string(key)[0]}'" if self.get_string(key) else None

    @staticmethod
    def string_rest(key: str) -> str:
        key, index = key.strip('"').split(":")
        return f'"{key}:{int(index) + 1}"'

    def string_length(self, key: str) -> int:
        return len(self.get_string(key))


class AssemblerError(Exception):
    def __init__(self, token: Token, message: str):
        super().__init__(f"{message} at position {{pos}}".format(**asdict(token)))


def load_grammar(filename: str | Path) -> Instructions:
    instructions = defaultdict(dict[tuple[str, ...], str])

    with open(filename) as file:
        for line in file:
            if line.isspace():
                continue

            if line.startswith("# "):
                continue

            instr, *arg_spec, template = re.split(r" {2,}", line.strip())

            for print_type in "ACLNR":
                instructions[instr.replace("z", print_type)] |= {tuple(arg_spec): template.replace("z", print_type)}

    return instructions


def parse(string: str, *, offset: int = 0) -> list[Token]:
    all_tokens = [next(Token(k, v, token.start() + offset) for k, v in token.groupdict().items() if v)
                  for token in TOKENS.finditer(decode_escapes(string))]

    tokens = []
    prev = Token()

    for token in all_tokens:
        # Preprocess separators
        if token.type == "sep":
            if prev.type not in ARG_TYPES:
                raise AssemblerError(prev, "invalid separator following '{value}'")

            continue

        elif token.type not in ARG_TYPES and prev.type == "sep":
            raise AssemblerError(prev, "invalid separator")

        elif token.type not in CASE_SENSITIVE:
            # Capitalize everything else
            token.value = token.value.upper()

        tokens.append(token)
        prev = token

    return tokens


def match_args(tokens: list[Token], context: Context) -> tuple[str, list[Token]]:
    instruction, *tokens = tokens

    specs = context.get_instruction(str(instruction))
    if not specs:
        specs = context.get_instruction(str(instruction) + "!")
        if not specs:
            raise AssemblerError(instruction, "unknown instruction '{value}'")

        warn(f"instruction '{instruction}' at position {instruction.pos} is a macro, "
             f"and should be written as '{instruction}!'",
             UserWarning)

    for spec, code in specs.items():
        matched_args = [*zip(tokens, spec)]

        if len(matched_args) < len(spec):
            continue

        remaining = tokens[len(spec):]
        for index, (arg, name) in enumerate(matched_args):
            if re.fullmatch(r"_\w", name):
                # Generic placeholders only allowed with templates
                code = re.sub(r"`(.*?)`", lambda match: match[0].replace(name, arg.value), code)
                continue

            if re.fullmatch(r"[?$0-9]+", name):
                if name.isnumeric() and (not arg.value.isnumeric() or int(arg.value) != int(name)):
                    break

                elif arg.value != name:
                    break

                continue

            else:
                if not arg.pattern.fullmatch(name):
                    break

            repls = {}
            match arg.type:
                case "nil":
                    repls = {name: ""}

                case "pos" | "neg":
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

                case "remain":
                    repls = {name: "#"}

                case "char":
                    character = arg.value[1]

                    repls = {
                        f"'{chr(ord(name[1]) - 1)}": f"'{chr(ord(character) - 1)}",
                        f"'{chr(ord(name[1]))}": f"'{chr(ord(character))}",
                        f"'{chr(ord(name[1]) + 1)}": f"'{chr(ord(character) + 1)}",
                        f'"{name[1]}"': character
                    }

                case "reads" | "peeks":
                    count = int(arg.value[1:])

                    repls = {
                        "nn": count,
                        "$n-1": f"${count - 1}",
                        "?n-1": f"?{count - 1}",
                        name: arg.value
                    }

                case "string":
                    repls = {
                        f"'{name[1]}'": context.string_first(arg.value),
                        f".{name[1]}": context.string_rest(arg.value),
                        "$n": f"${context.string_length(arg.value)}",
                        "?n": f"?{context.string_length(arg.value)}",
                        name: arg.value
                    }

                case "lbrace" | "lbracket":
                    inner, remaining = match_tokens(remaining, context.in_block(str(instruction), arg.type))
                    repls = {"...": inner}

                case "raw":
                    repls = {"...": arg.value[1:-1]}

                case "error":
                    raise AssemblerError(arg, "unrecognized input '{value}'")

            for sub, value in repls.items():
                repl = str(value)
                if sub != "...":
                    repl = encode_escapes(repl)

                code = code.replace(sub, repl)

        else:
            def match_template(match: re.Match) -> str:
                if not match[1]:
                    # No-op
                    return ""

                elif match[1] == "ERR":
                    # Illegal arguments
                    raise AssemblerError(instruction, "illegal signature for instruction '{value}'")

                return match_args(parse(match[1], offset=instruction.pos), context)[0]

            # Resolve templates
            code = re.sub(r"`(.*?)`", match_template, code)

            return code, remaining

    raise AssemblerError(instruction, "could not match signature of instruction '{value}'")


def match_tokens(tokens: list[Token], context: Context) -> tuple[str, list[Token]]:
    assembled = ""
    clauses = 0

    start = tokens[0]
    while tokens:
        token = tokens.pop(0)

        try:
            match token.type:
                case "newline":
                    if context.flags.get("preserve_indents"):
                        assembled += f"~{token.value}"

                case "comment":
                    if context.flags.get("preserve_comments"):
                        assembled += f"~1[{token.value.lstrip(';').replace('~', '~~')} ~]"

                case "raw":
                    assembled += token.value[1:-1]

                case context.block_close:
                    return assembled, tokens

                case _ if context.block_name.startswith(("CASE", "JUST")) and "#" not in str(token):
                    tokens = [Token("instr", f"#{context.block_name[:4]}", token.pos),
                              Token("pos", str(clauses), token.pos),
                              token,
                              *tokens]

                    clauses += 1

                case "instr" if str(token) == "DEFINE":
                    name = tokens.pop(0)
                    if name.type != "instr" or not str(name).endswith("!"):
                        raise AssemblerError(token, f"invalid macro name '{name}'")

                    if str(name) in context.instructions:
                        raise AssemblerError(token, f"clashing macro name '{name}'")

                    if tokens.pop(0).type != "lbrace":
                        raise AssemblerError(token, f"expected open brace in '{name}' macro definition")

                    code, tokens = match_tokens(tokens, context.in_block(str(name), "lbrace"))
                    context.add_macro(str(name), code)

                case "instr":
                    code, tokens = match_args([token, *tokens], context)
                    assembled += code

                case "lbrace":
                    code, tokens = match_tokens(tokens, context.in_block("", "lbrace"))
                    assembled += f"~1@{{{code}~:}}"

                case "lbracket":
                    code, tokens = match_tokens(tokens, context.in_block("", "lbracket"))

                    if context.flags.get("preserve_groups"):
                        assembled += f"~0[{code}~]"

                    else:
                        assembled += code

                case "error":
                    raise AssemblerError(token, "unrecognized input '{value}'")

                case _:
                    raise AssemblerError(token, "unexpected token '{value}'")

        except ValueError:
            raise AssemblerError(token, "incomplete input following '{value}'")

    if context.block_open:
        raise AssemblerError(start, "unclosed block")

    return assembled, []


def assemble(program: str, instructions: Instructions = None, **flags) -> str:
    # Initial parse
    all_tokens = parse(program)

    context = Context()
    context.instructions = load_grammar(Path(__file__).parent / "instructions.g")
    context.instructions |= instructions or {}

    context.flags = flags

    # Preprocessing
    tokens = []

    for token in all_tokens:
        if token.type == "newline" and not flags.get("preserve_indents"):
            # Excise newlines now
            continue

        elif token.type == "comment" and not flags.get("preserve_comments"):
            # Excise comments now
            continue

        elif token.type == "string":
            # Sequester strings
            context.add_string(token.value[1:-1])
            token.value = context.last_string()

        elif token.type == "instr" and "#" in token.value:
            # No internal instructions from users
            raise AssemblerError(token, "illegal instruction '{value}'")

        tokens.append(token)

    assembled, _ = match_tokens(tokens, context)

    # Insert strings
    assembled = re.sub(r'"\d+:\d+"', lambda match: context.get_escaped_string(match[0]), assembled)

    return assembled
