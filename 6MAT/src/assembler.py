import codecs
import os
import re

from collections import defaultdict, namedtuple


class Token(namedtuple("Token", ["type", "value", "pos"])):
    def __str__(self) -> str:
        return self.value.upper()

    @property
    def out(self):
        match self.type:
            case "char":
                return self.value[1]

            case "read":
                return "v"

            case "pos" | "neg":
                return int(self.value)

            case "remain":
                return "#"

            case "string":
                return self.value[1:-1]

        return self.value

    @property
    def pattern(self) -> re.Pattern:
        match self.type:
            case "lbrace":
                return re.compile(r"\{\.\.\.}")

            case "lbracket":
                return re.compile(r"\[\.\.\.]")

            case "nil":
                return re.compile(r"[%+-]\w|'C")

            case "char":
                return re.compile(r"'C")

            case "read":
                return re.compile(r"\$V")

            case "peek":
                return re.compile(r"\?V")

            case "pos":
                return re.compile(r"[%+]\w")

            case "neg":
                return re.compile(r"[%-]\w")

            case "remain":
                return re.compile(r"[%+]\w|\$R")

            case "string":
                return re.compile(r'""')

            case "instr":
                return re.compile(self.value, re.IGNORECASE)

            case _:
                return re.compile(r"$.")


TOKENS = re.compile(
    r"(?P<newline>[\r\n]\s*)|"
    r"(?P<comment>;[^\r\n]*)|"
    r"(?P<lbrace>\{)|"
    r"(?P<rbrace>})|"
    r"(?P<lbracket>\[)|"
    r"(?P<rbracket>])|"
    r"(?P<sep>,)|"
    r"(?P<nil>NIL)|"
    r"(?P<char>'.|\\f|\\n)|"
    r"(?P<read>\$V)|"
    r"(?P<peek>\?V)|"
    r"(?P<pos>\+?[0-9]+)|"
    r"(?P<neg>-[0-9]+)|"
    r"(?P<remain>\$R)|"
    r'(?P<string>"(?:[^"]|\\.)*")|'
    r"(?P<instr>\w+)|"
    r"(?P<error>\S+)", flags=re.IGNORECASE)

ARG_TYPES = {"nil", "char", "read", "peek", "pos", "neg", "remain", "string"}

ESCAPES = re.compile(
    r"\\U........|"
    r"\\u....|"
    r"\\x..|"
    r"""\\['"abfnrtv]"""
)


class AssemblerError(Exception):
    def __init__(self, token: Token, message: str):
        super().__init__(f"{message} at position {{pos}}".format(**token._asdict()))


with open(os.path.join(os.path.dirname(__file__), "instructions.g")) as instructions:
    INSTRUCTIONS = defaultdict(dict[tuple[str, ...], str])

    for line in instructions:
        if line.isspace():
            continue

        instr, *arg_spec, template = re.split(r"\t+| {2,}", line.strip())

        for print_type in "ACLNR":
            INSTRUCTIONS[instr.replace("x", print_type)] |= {tuple(arg_spec): template.replace("x", print_type)}


def decode_escapes(string: str) -> str:
    def decode_match(match: re.Match) -> str:
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return ESCAPES.sub(decode_match, string)


def parse(string: str, *, offset: int = 0, **flags) -> list[Token]:
    # Escape sequences
    string = decode_escapes(string)
    string = string.replace("↡", "\f")

    all_tokens = [next(Token(k, v, token.start() + offset) for k, v in token.groupdict().items() if v)
                  for token in TOKENS.finditer(string)]

    # Preprocess separators
    tokens = []

    prev = Token("", "", 0)
    for token in all_tokens:
        if token.type == "sep":
            if prev.type not in ARG_TYPES:
                raise AssemblerError(token, "invalid separator")

        elif token.type not in ARG_TYPES and prev.type == "sep":
            raise AssemblerError(prev, "invalid separator")

        elif token.type == "newline" and not flags.get("preserve_indents"):
            pass

        elif token.type == "comment" and not flags.get("preserve_comments"):
            pass

        else:
            tokens.append(token)

        prev = token

    return tokens


def match_args(instruction: Token, *tokens: Token, **flags) -> tuple[str, list[Token]]:
    tokens = list(tokens)

    for spec, code in INSTRUCTIONS[str(instruction)].items():
        typed_args = [*zip(tokens, spec)]

        if len(typed_args) < len(spec):
            continue

        remaining = tokens[len(spec):]
        for index, (arg, name) in enumerate(typed_args):
            if arg.type == "peek":
                if str(instruction).startswith("BR"):
                    raise AssemblerError(arg, "peek used in breaking instruction")

            if re.fullmatch(r"_\w", name):
                code = code.replace(name, str(arg.out))
                continue

            if name.isnumeric():
                if not arg.value.isnumeric() or int(arg.value) != int(name):
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
                    context = {
                        f"%{name[1]}-1": str(arg.out - 1),
                        f"%{name[1]}+1": str(arg.out + 1),
                        f"%{name[1]}": str(arg.out),
                        f"+{name[1]}-1": str(abs(arg.out) - 1),
                        f"+{name[1]}+1": str(abs(arg.out) + 1),
                        f"+{name[1]}": str(abs(arg.out)),
                        f"-{name[1]}-1": str(-abs(arg.out) - 1),
                        f"-{name[1]}+1": str(-abs(arg.out) + 1),
                        f"-{name[1]}": str(-abs(arg.out)),
                    }

                case "remain":
                    context = {name: arg.out}

                case "char":
                    context = {
                        f"'{chr(ord(name[1]) - 1)}": f"'{chr(ord(arg.out) - 1)}",
                        f"'{chr(ord(name[1]) + 1)}": f"'{chr(ord(arg.out) + 1)}",
                        name: arg.value,
                        f'"{name[1]}"': arg.out
                    }

                case "string":
                    context = {'""': arg.out.replace("~~", "~") if str(instruction) != "FORMAT" else arg.out}

                case "lbrace":
                    inner, remaining = assemble(remaining, block=f"{instruction} {{", **flags)
                    context = {"...": inner}

                case "lbracket":
                    inner, remaining = assemble(remaining, block=f"{instruction} [", **flags)
                    context = {"...": inner}

            for sub, value in context.items():
                code = code.replace(sub, value)

        else:
            def match_template(match: re.Match) -> str:
                if not match[1]:
                    return ""

                elif match[1] == "ERR":
                    raise AssemblerError(instruction, "invalid signature for instruction '{value}'")

                # Kinda yucky and only need once for IFNR
                elif repeat := re.fullmatch(r"(?P<snippet>.*)\*(?P<count>\d+)", match[1]):
                    return repeat["snippet"] * int(repeat["count"])

                else:
                    return match_args(*parse(match[1], offset=instruction.pos), **flags)[0]

            code = re.sub(r"`(.*?)`", match_template, code)
            return code, remaining

    raise AssemblerError(instruction, "could not match signature of instruction '{value}'")


def assemble(tokens: list[Token], *, block: str = "", **flags) -> tuple[str, list[Token]]:
    assembled = ""
    clauses = 0

    if not tokens:
        return "", []

    start = tokens[0]
    try:
        while tokens:
            token = tokens.pop(0)

            match token.type:
                case "newline":
                    if flags.get("preserve_indents"):
                        assembled += f"~{token.value}"

                    continue

                case "comment":
                    if flags.get("preserve_comments"):
                        assembled += f"~1[{token.value.lstrip(';').replace('~', '~~')} ~]"

                    continue

                case "rbrace" if not block.endswith("{"):
                    raise AssemblerError(token, "mismatched closing brace")

                case "rbracket" if not block.endswith("["):
                    raise AssemblerError(token, "mismatched closing bracket")

                case "rbrace" if not block:
                    raise AssemblerError(token, "unmatched closing brace")

                case "rbracket" if not block:
                    raise AssemblerError(token, "unmatched closing bracket")

                case "rbrace" | "rbracket":
                    return assembled, tokens

                case _ if "!" not in block and f"{block[:4]}!" in INSTRUCTIONS and not str(token).endswith("!"):
                    tokens = [Token("instr", f"{block[:4]}!", token.pos),
                              Token("pos", str(clauses), token.pos),
                              token,
                              *tokens]

                    clauses += 1

                case "instr":
                    try:
                        code, tokens = match_args(token, *tokens, **flags)
                        assembled += code

                    except KeyError:
                        raise AssemblerError(token, "unknown instruction '{value}'")

                case "lbrace":
                    code, tokens = assemble(tokens, block="{", **flags)
                    assembled += f"~1@{{{code}~:}}"

                case "lbracket":
                    code, tokens = assemble(tokens, block="[", **flags)

                    if flags.get("preserve_groups"):
                        assembled += f"~0[{code}~]"

                    else:
                        assembled += code

                case "error":
                    raise AssemblerError(token, "unrecognized input '{value}'")

                case _:
                    raise AssemblerError(token, "unexpected token '{value}'")

    except IndexError:
        raise AssemblerError(token, "incomplete input following '{value}'")

    if block:
        raise AssemblerError(start, "unclosed block")

    return assembled, []


if __name__ == "__main__":
    with open("../samples/tests.6mat", "r") as infile:
        print(assemble(parse(infile.read()))[0])


__all__ = ["Token", "AssemblerError", "parse", "assemble"]