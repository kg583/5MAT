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

            case "string":
                return self.value[1:-1]

        return self.value

    @property
    def pattern(self) -> str:
        return {
            "open": r"\{\.\.\.\}",
            "sep": r",",
            "char": r"'C",
            "read": r"\$V",
            "peek": r"\?V",
            "nil": r"[%+-]\w|'C",
            "pos": r"[%+]\w",
            "neg": r"[%-]\w",
            "string": r'""'
        }.get(self.type, "!?")


TOKENS = re.compile(
    r"(?P<newline>[\r\n]\s*)|"
    r"(?P<comment>;[^\r\n]*)|"
    r"(?P<open>\{)|"
    r"(?P<close>})|"
    r"(?P<sep>,)|"
    r"(?P<char>'.|\\f|\\n)|"
    r"(?P<read>\$V)|"
    r"(?P<peek>\?V)|"
    r"(?P<default>DEFAULT)|"
    r"(?P<nil>NIL)|"
    r"(?P<pos>\+?[0-9]+|\$R)|"
    r"(?P<neg>-[0-9]+)|"
    r'(?P<string>"(?:[^"]|\\.)*")|'
    r"(?P<instr>\w+)|"
    r"(?P<error>\S+)", flags=re.IGNORECASE)

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

        arg_spec = [value for pair in zip(arg_spec, "," * len(arg_spec)) for value in pair][:-1]
        if len(arg_spec) > 2 and "{...}" in arg_spec:
            arg_spec.pop(-2)

        for print_type in "ACLNR":
            INSTRUCTIONS[instr.replace("x", print_type)] |= {tuple(arg_spec): template.replace("x", print_type)}


def decode_escapes(string: str) -> str:
    def decode_match(match: re.Match) -> str:
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return ESCAPES.sub(decode_match, string)


def parse(string: str, *, offset: int = 0) -> list[Token]:
    # Escape sequences
    string = decode_escapes(string)
    string = string.replace("↡", "\f")

    return [next(Token(k, v, token.start() + offset) for k, v in token.groupdict().items() if v)
            for token in TOKENS.finditer(string)]


def match_args(instruction: Token, *tokens: Token, **flags) -> tuple[str, list[Token]]:
    tokens = list(tokens)

    for spec, code in INSTRUCTIONS[str(instruction)].items():
        typed_args = [*zip(tokens, spec)]

        if len(typed_args) < len(spec):
            continue

        remaining = tokens[len(spec):]
        for index, (arg, name) in enumerate(typed_args):
            if re.fullmatch(r"_\w", name):
                if arg.type == "peek":
                    if str(instruction).startswith("BR"):
                        raise AssemblerError(arg, "peek used in breaking instruction")

                    if index != len(typed_args) - 1:
                        raise AssemblerError(arg, "non-terminal peek")

                code = code.replace(name, str(arg.out))
                continue

            if name.isnumeric():
                if not arg.value.isnumeric() or int(arg.value) != int(name):
                    break

                continue

            else:
                if not re.fullmatch(arg.pattern, name):
                    break

            context = {}
            match arg.type:
                case "nil":
                    context = {name[1]: ""}

                case "pos" if str(arg) != "$R":
                    context = {
                        f"{name[1]}-1": str(arg.out - 1),
                        f"{name[1]}+1": str(arg.out + 1),
                        f"{name[1]}": str(arg.out)
                    }

                case "pos" if str(arg) == "$R":
                    context = {name[1]: "#"}

                case "neg":
                    context = {
                        f"+{name[1]}": str(abs(arg.out)),
                        f"{name[1]}": str(arg.out)
                    }

                case "char":
                    context = {
                        f"'{chr(ord(name[1]) - 1)}": f"'{chr(ord(arg.out) - 1)}",
                        f"'{chr(ord(name[1]) + 1)}": f"'{chr(ord(arg.out) + 1)}",
                        name: arg.value,
                        f"`{name[1]}`": arg.out
                    }

                case "string":
                    context = {'""': arg.out.replace("~~", "~") if str(instruction) != "FORMAT" else arg.out}

                case "open":
                    inner, remaining = assemble(remaining, block=str(instruction), **flags)
                    context = {"...": inner}

            for sub, value in context.items():
                code = code.replace(sub, value)

        else:
            def match_template(match: re.Match) -> str:
                if not match[1]:
                    return ""

                elif match[1] == "ERR":
                    raise AssemblerError(instruction, "invalid signature for instruction '{value}'")

                else:
                    return match_args(*parse(match[1], offset=instruction.pos), **flags)[0]

            code = re.sub(r"`(.*?)`", match_template, code)
            return code, remaining

    raise AssemblerError(instruction, "could not match signature of instruction '{value}'")


def assemble(tokens: list[Token], *, block: str = None, **flags) -> tuple[str, list[Token]]:
    assembled = ""
    count = 0

    try:
        while tokens:
            token = tokens.pop(0)

            match token.type:
                case "newline":
                    if flags.get("preserve_indents") and block != "INIT":
                        assembled += f"~{token.value}"

                    continue

                case "comment":
                    if flags.get("preserve_comments"):
                        assembled += f"~1[{token.value.lstrip(';').replace('~', '~~')} ~]"

                    continue

                case "open" if block == "JUST":
                    code, tokens = assemble(tokens, block="JUST!", **flags)
                    assembled += code + "~;"

                case "open" if block not in ("CASV!", "CASR!"):
                    code, tokens = assemble(tokens, block="BLOCK", **flags)
                    assembled += f"~<{code}~>"

                case "close" if block:
                    return assembled, tokens

                case "close":
                    raise AssemblerError(token, "unmatched closing brace")

                case "instr" if block not in ("CASV!", "CASR!"):
                    if str(token) == "CASE":
                        # CASE specialization
                        match arg := tokens[0].type:
                            case "read":
                                token = token._replace(value="CASV!")

                            case "pos":
                                token = token._replace(value="CASR!")

                            case _:
                                raise AssemblerError(arg, "illegal CASE switch '{value}'")

                    elif str(token) == "OVER":
                        if count != 0 or block != "JUST":
                            raise AssemblerError(token, "non-initial OVER clause in JUSTIFY")

                    try:
                        code, tokens = match_args(token, *tokens, **flags)
                        assembled += code

                    except KeyError:
                        raise AssemblerError(token, "unknown instruction '{value}'")

                case "char" if block == "CASV!":
                    if tokens[0].type != "open":
                        raise AssemblerError(token, "missing CASE clause for key '{value}'")

                    code, tokens = match_args(Token("instr", "IFEQ", token.pos),
                                              Token("read", "$V", token.pos),
                                              Token("sep", ",", token.pos),
                                              token, *tokens, **flags)

                    assembled += code + "~:*"

                case "pos" | "default" if block == "CASR!":
                    if str(token) == "$R":
                        raise AssemblerError(token, "illegal numeric CASE key '{value}'")

                    if int(token.value) != count:
                        raise AssemblerError(token, "non-sequential numeric CASE key '{value}'")

                    if count < 0:
                        raise AssemblerError(token, "numeric CASE key '{value}' after DEFAULT")

                    match str(token), count > 0:
                        case "DEFAULT", True:
                            assembled += "~:;"
                            count = -99

                        case "DEFAULT", False:
                            raise AssemblerError(token, "DEFAULT clause is sole clause in CASE")

                        case _, True:
                            assembled += "~;"

                    if tokens.pop(0).type != "open":
                        raise AssemblerError(token, "missing CASE clause for key '{value}'")

                    code, tokens = assemble(tokens, block="CASE!", **flags)
                    assembled += code

                case "error":
                    raise AssemblerError(token, "unrecognized input '{value}'")

                case _:
                    raise AssemblerError(token, "unexpected token '{value}'")

            count += 1

    except IndexError:
        raise AssemblerError(token, "incomplete input following '{value}'")

    return assembled, []


if __name__ == "__main__":
    with open("../samples/tests.6mat", "r") as infile:
        print(assemble(parse(infile.read()))[0])


__all__ = ["Token", "AssemblerError", "parse", "assemble"]