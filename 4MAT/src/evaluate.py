from .directives import *
from .parse import parse, tokenize


class Interpreter:
    def __init__(self, program: str | BlockDirective, args: list, args_idx=0):
        if isinstance(program, str):
            self.ast = parse(tokenize(program))
        else:
            self.ast = program

        self.args = args
        self.arg_idx = args_idx

        self.buffer = ""

    def output(self, data):
        self.buffer += data

    def skip_args(self, distance: int):
        self.arg_idx += distance
        self.clamp_arg_idx()

    def clamp_arg_idx(self):
        self.arg_idx = min(max(self.arg_idx, 0), len(self.args) - 1)

    def get_arg(self, index):
        if 0 <= index < len(self.args):
            return self.args[self.arg_idx]
        else:
            return None

    def consume_arg(self):
        arg = self.get_arg(self.arg_idx)
        self.arg_idx += 1
        return arg

    def params(self, directive, count) -> list[int | str | None]:
        result = []
        # for i in range(min(count, len(directive.prefix_params))):
        # match directive.prefix_params[i]:

        if len(result) < count:
            result.extend([None] * (count - len(result)))

        return result

    def get_param(self, directive, index, default: int | str | None = 0):
        if 0 <= index < len(directive.prefix_params):
            match directive[index]:
                case Special.V:
                    arg = self.consume_arg()
                    if isinstance(arg, int) or isinstance(arg, str) and len(arg) == 1:
                        return arg
                    if arg is None:
                        return default
                case Special.Hash:
                    return len(self.args) - self.arg_idx
                case None:
                    return default
        else:
            return default

    def eval(self, token: str | Directive):
        if isinstance(token, str):
            self.output(token)
            return

        directive = token
        match directive.kind:
            # FORMAT Basic Output
            case 'c':
                self.eval_character(directive)
            case '%':
                self.print_repeated(directive, "\n")
            case '&':
                self.eval_fresh_line(directive)
            case '|':
                self.print_repeated(directive, "\f")
            case '~':
                self.print_repeated(directive, "~")

            # FORMAT Radix Control
            # FORMAT Floating-Point Printers
            # FORMAT Layout Control
            # FORMAT Control-Flow Operations
            case '*':
                self.eval_goto(directive)

            # FORMAT Miscellaneous Operations
            case 'p':
                self.eval_plural(directive)

            case _:
                print(f"Unrecognized directive ~{directive.kind}")

    def eval_ast_root(self):
        assert len(self.ast.clauses) == 1

        for token in self.ast.clauses[0]:
            self.eval(token)

    # FORMAT Basic Output
    def eval_character(self, directive: Directive):
        return  # unimplemented

    def print_repeated(self, directive: Directive, char: str):
        self.output(char * self.get_param(directive, 0, default=1))

    def eval_fresh_line(self, directive: Directive):
        count = self.get_param(directive, 0, default=1)

        if count == 0:
            return

        if self.buffer[-1] != '\n':
            self.output("\n")

        self.output("\n" * (count - 1))

    # FORMAT Control-Flow Operations
    def eval_goto(self, directive: Directive):
        if directive.at_sign:
            self.arg_idx = self.get_param(directive, 0, default=0)
            if self.arg_idx < 0:
                print("Error: negative goto arg")
                exit(1)
            self.clamp_arg_idx()
            return

        param = self.get_param(directive, 0, default=1)
        if param < 0:
            print("Error: negative goto arg")
            exit(1)

        if directive.colon:
            self.skip_args(-param)
            return
        self.skip_args(param)

    # FORMAT Miscellaneous Operations
    def eval_plural(self, directive: Directive):
        if directive.colon:
            self.skip_args(-1)

        if directive.at_sign:
            self.output(["ies", "y"][self.args[self.arg_idx] == 1])
        elif self.args[self.arg_idx] == 1:
            self.output("s")


def fourmat(program: str | BlockDirective, args: list):
    interp = Interpreter(program, args)
    interp.eval_ast_root()
    return interp.buffer


def fivemat(program: str):
    parsed = parse(tokenize(program))

    tape = []
    while True:
        tape = list(fourmat(parsed, [tape]))
        for char in tape:
            if char == "\f":
                break

            print(end=char)
