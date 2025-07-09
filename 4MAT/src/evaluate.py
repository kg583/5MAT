import unicodedata

from .directives import *
from .parse import parse, tokenize


DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"


def char_name(char: str) -> str:
    match char:
        # Standard characters
        case "\n":
            return "Newline"

        case " ":
            return "Space"

        # Semi-standard characters
        case "\t":
            return "Tab"

        case "\f":
            return "Page"

        case "\x7f":
            return "Rubout"

        case "\a":
            return "Linefeed"

        case "\b":
            return "Backspace"

        case _:
            return unicodedata.name(char).title()


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

    def get_arg(self, index: int):
        try:
            return self.args[index]
        except IndexError:
            raise IndexError("not enough args")

    def consume_arg(self):
        arg = self.get_arg(self.arg_idx)
        self.arg_idx += 1
        return arg

    def get_param(self, directive, index, default: int | str | None = 0):
        if 0 <= index < len(directive.prefix_params):
            match directive.prefix_params[index]:
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
                case _:
                    return directive.prefix_params[index]
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
            case 'r':
                self.eval_radix(directive)
            case 'd':
                directive.prefix_params.insert(0, 10)
                self.eval_radix(directive)
            case 'b':
                directive.prefix_params.insert(0, 2)
                self.eval_radix(directive)
            case 'o':
                directive.prefix_params.insert(0, 8)
                self.eval_radix(directive)
            case 'x':
                directive.prefix_params.insert(0, 16)
                self.eval_radix(directive)

            # FORMAT Floating-Point Printers
            # FORMAT Printer Operations
            case 'a':
                self.eval_aesthetic(directive)
            case 's':
                # TODO: Add correct escapes to output
                self.eval_aesthetic(directive)

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
        char = self.consume_arg()

        if not isinstance(char, str) or len(char) != 1:
            raise TypeError("~c arg is not a character")

        if directive.colon:
            self.output(char_name(char))

        else:
            self.output(("#\\" if directive.at_sign else "") + char)

    def print_repeated(self, directive: Directive, char: str):
        self.output(char * self.get_param(directive, 0, default=1))

    def eval_fresh_line(self, directive: Directive):
        count = self.get_param(directive, 0, default=1)

        if count == 0:
            return

        if self.buffer[-1] != '\n':
            self.output("\n")

        self.output("\n" * (count - 1))

    # FORMAT Radix Control
    def eval_radix(self, directive: Directive):
        base = self.get_param(directive, 0, default=None)
        arg = self.consume_arg()

        if not isinstance(arg, int):
            self.eval_aesthetic(directive)
            return

        if base is None:
            # TODO: English names and such
            return

        elif not (isinstance(base, int) and 2 <= base <= 36):
            raise ValueError("~r base must be an integer between 2 and 36")

        # Actual numbers
        min_col = self.get_param(directive, 1, default=0)
        pad_char = self.get_param(directive, 2, default=" ")
        comma_char = self.get_param(directive, 3, default=",")
        comma_interval = self.get_param(directive, 4, default=3)

        number = abs(arg)
        output = []
        length = 0

        while number:
            number, remainder = divmod(number, base)

            if directive.colon and output and length % comma_interval == 0:
                output = [DIGITS[remainder], comma_char, *output]

            else:
                output = [DIGITS[remainder], *output]

            length += 1

        if directive.at_sign and arg >= 0:
            output = ["+", *output]

        elif arg < 0:
            output = ["-", *output]

        self.output(f"{''.join(output):{pad_char}>{min_col}}")

    # FORMAT Printer Operations
    def eval_aesthetic(self, directive: Directive):
        arg = self.consume_arg()
        
        if arg is None:
            output = "()" if directive.colon else "NIL"

        else:
            output = str(arg)

        min_col = self.get_param(directive, 0, default=0)
        col_inc = self.get_param(directive, 1, default=1)
        min_pad = self.get_param(directive, 2, default=0)
        pad_char = self.get_param(directive, 3, default=" ")

        padding = min_pad * pad_char
        while len(padding) + len(output) < min_col:
            padding += col_inc * pad_char

        self.output(padding + output if directive.at_sign else output + padding)

    # FORMAT Control-Flow Operations
    def eval_goto(self, directive: Directive):
        if directive.at_sign:
            self.arg_idx = self.get_param(directive, 0, default=0)
            if self.arg_idx < 0:
                raise ValueError("negative ~* arg")

            self.clamp_arg_idx()
            return

        param = self.get_param(directive, 0, default=1)
        if param < 0:
            raise ValueError("negative ~* arg")

        if directive.colon:
            self.skip_args(-param)

        else:
            self.skip_args(param)

    # FORMAT Miscellaneous Operations
    def eval_plural(self, directive: Directive):
        if directive.colon:
            self.skip_args(-1)

        if directive.at_sign:
            options = ["ies", "y"]

        else:
            options = ["s", ""]

        self.output(options[self.consume_arg() == 1])


def fourmat(program: str | BlockDirective, args: list):
    interp = Interpreter(program, args)
    interp.eval_ast_root()
    return interp.buffer


def fivemat(program: str):
    parsed = parse(tokenize(program))

    tape = []
    while True:
        tape = fourmat(parsed, [list(tape)])
        print(end=tape[tape.rfind("\f") + 1:])
