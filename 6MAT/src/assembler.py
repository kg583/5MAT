import codecs
import os
import re

from collections import defaultdict
from dataclasses import asdict, dataclass


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
    r"(?P<instr>[!A-Z]+)|"
    r"(?P<error>\S+)")

ARG_TYPES = {"nil", "char", "read", "reads", "peek", "peeks", "pos", "neg", "remain", "string"}
CASE_SENSITIVE = {"comment", "char", "string", "raw"}


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
class Block:
    name: str = ""
    args: tuple[Token, ...] = ()
    open: str = ""

    @property
    def close(self) -> str:
        return f"r{self.symbol}"

    @property
    def symbol(self) -> str:
        return self.open[1:]


class Strings(dict):
    def __getitem__(self, key: str):
        key, index = key.strip('"').split(":")
        return super().__getitem__(f'"{key}:0"')[int(index):]

    def add(self, string: str):
        self[self.next_key()] = string

    def first(self, key: str) -> str:
        return f"'{self[key][0]}'" if self[key] else None

    def last_key(self) -> str:
        return f'"{len(self) - 1}:0"'

    def length(self, key: str) -> int:
        return len(self[key])

    def next_key(self) -> str:
        return f'"{len(self)}:0"'

    @staticmethod
    def rest(key: str) -> str:
        key, index = key.strip('"').split(":")
        return f'"{key}:{int(index) + 1}"'


class AssemblerError(Exception):
    def __init__(self, token: Token, message: str):
        super().__init__(f"{message} at position {{pos}}".format(**asdict(token)))


with open(os.path.join(os.path.dirname(__file__), "instructions.g")) as instructions:
    INSTRUCTIONS = defaultdict(dict[tuple[str, ...], str])

    for line in instructions:
        if line.isspace():
            continue

        if line.startswith("#"):
            continue

        instr, *arg_spec, template = re.split(r" {2,}", line.strip())

        for print_type in "ACLNR":
            INSTRUCTIONS[instr.replace("x", print_type)] |= {tuple(arg_spec): template.replace("x", print_type)}


def decode_escapes(string: str) -> str:
    def decode_match(match):
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return re.sub(r"\\[fnrt]|\\x..", decode_match, string)


def encode_escapes(string: str) -> str:
    for char in range(1, 32):
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


def match_args(tokens: list[Token], strings: Strings, **flags) -> tuple[str, list[Token]]:
    instruction, *tokens = tokens

    for spec, code in INSTRUCTIONS[str(instruction)].items():
        matched_args = [*zip(tokens, spec)]
        args = []

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

            context = {}
            match arg.type:
                case "nil":
                    context = {name: ""}

                case "pos" | "neg":
                    num = int(arg.value)

                    context = {
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
                    context = {name: "#"}

                case "char":
                    character = arg.value[1]

                    context = {
                        f"'{chr(ord(name[1]) - 1)}": f"'{chr(ord(character) - 1)}",
                        f"'{chr(ord(name[1]))}": f"'{chr(ord(character))}",
                        f"'{chr(ord(name[1]) + 1)}": f"'{chr(ord(character) + 1)}",
                        f'"{name[1]}"': character
                    }

                case "reads" | "peeks":
                    count = int(arg.value[1:])

                    context = {
                        "nn": count,
                        "$n-1": f"${count - 1}",
                        "?n-1": f"?{count - 1}",
                        name: arg.value
                    }

                case "string":
                    context = {
                        f"'{name[1]}'": strings.first(arg.value),
                        f".{name[1]}": strings.rest(arg.value),
                        "$n": f"${strings.length(arg.value)}",
                        "?n": f"?{strings.length(arg.value)}",
                        name: arg.value
                    }

                case "lbrace" | "lbracket":
                    block = Block(str(instruction), tuple(args), arg.type)

                    inner, remaining = match_tokens(remaining, strings, block, **flags)
                    context = {"...": inner}

                case "raw":
                    context = {"...": arg.value[1:-1]}

                case "error":
                    raise AssemblerError(arg, "unrecognized input '{value}'")

            for sub, value in context.items():
                repl = str(value)
                if sub != "...":
                    repl = encode_escapes(repl)

                code = code.replace(sub, repl)

            args.append(arg)

        else:
            def match_template(match: re.Match) -> str:
                if not match[1]:
                    # No-op
                    return ""

                elif match[1] == "ERR":
                    # Illegal arguments
                    raise AssemblerError(instruction, "illegal signature for instruction '{value}'")

                return match_args(parse(match[1], offset=instruction.pos), strings, **flags)[0]

            # Resolve templates
            code = re.sub(r"`(.*?)`", match_template, code)

            return code, remaining

    raise AssemblerError(instruction, "could not match signature of instruction '{value}'")


def match_tokens(tokens: list[Token], strings: Strings, block: Block = Block(), **flags) -> tuple[str, list[Token]]:
    assembled = ""
    clauses = 0

    start = tokens[0]
    while tokens:
        token = tokens.pop(0)

        try:
            match token.type:
                case "newline":
                    if flags.get("preserve_indents"):
                        assembled += f"~{token.value}"

                case "comment":
                    if flags.get("preserve_comments"):
                        assembled += f"~1[{token.value.lstrip(';').replace('~', '~~')} ~]"

                case "raw":
                    assembled += token.value[1:-1]

                case block.close:
                    return assembled, tokens

                case _ if block.name.startswith(("CASE", "JUST")) and "!" not in block.name and "!" not in str(token):
                    tokens = [Token("instr", f"{block.name[:4]}!", token.pos),
                              Token("pos", str(clauses), token.pos),
                              token,
                              *tokens]

                    clauses += 1

                case "instr":
                    try:
                        code, tokens = match_args([token, *tokens], strings, **flags)
                        assembled += code

                    except KeyError:
                        raise AssemblerError(token, "unknown instruction '{value}'")

                case "lbrace":
                    code, tokens = match_tokens(tokens, strings, Block("", (), "lbrace"), **flags)
                    assembled += f"~1@{{{code}~:}}"

                case "lbracket":
                    code, tokens = match_tokens(tokens, strings, Block("", (), "lbracket"), **flags)

                    if flags.get("preserve_groups"):
                        assembled += f"~0[{code}~]"

                    else:
                        assembled += code

                case "error":
                    raise AssemblerError(token, "unrecognized input '{value}'")

                case _:
                    raise AssemblerError(token, "unexpected token '{value}'")

        except ValueError:
            raise AssemblerError(token, "incomplete input following '{value}'")

    if block.open:
        raise AssemblerError(start, "unclosed block")

    return assembled, []


def assemble(program: str, **flags) -> str:
    # Initial parse
    all_tokens = parse(program)

    # Preprocessing
    tokens = []
    strings = Strings()

    for token in all_tokens:
        if token.type == "newline" and not flags.get("preserve_indents"):
            # Excise newlines now
            continue

        elif token.type == "comment" and not flags.get("preserve_comments"):
            # Excise comments now
            continue

        elif token.type == "string":
            # Sequester strings
            strings.add(token.value[1:-1])
            token.value = strings.last_key()

        elif token.type == "instr" and "!" in token.value:
            # No ! instructions from users
            raise AssemblerError(token, "illegal instruction '{value}'")

        tokens.append(token)

    assembled, _ = match_tokens(tokens, strings, **flags)

    # Insert strings
    assembled = re.sub(r'"\d+:\d+"', lambda match: strings[match[0]].replace("~", "~~"), assembled)

    return assembled


if __name__ == "__main__":
    with open("../samples/tests.6mat", "r") as infile:
        print(assemble(infile.read()))


__all__ = ["Token", "AssemblerError", "assemble"]