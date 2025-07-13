import codecs
import math
import re
import unicodedata

from dataclasses import dataclass
from importlib import import_module

from .directives import *
from .parse import parse, tokenize


DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"


def decode_escapes(string: str) -> str:
    def decode_match(match):
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return re.sub(r"\\[abfnrtv]|\\x..", decode_match, string).replace("↡", "\f")


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


@dataclass
class Args:
    args: list
    index: int = 0

    def __iter__(self):
        return self.args.__iter__()

    def __post_init__(self):
        if self.args is None:
            self.args = []

        if not isinstance(self.args, list):
            raise TypeError(f"invalid arguments: {self.args}")

    def clamp(self):
        self.index = min(max(self.index, 0), len(self.args))

    def consume(self):
        arg = self.peek()
        self.index += 1
        return arg

    def hash(self):
        return len(self.args) - self.index

    def goto(self, index: int):
        self.index = index
        self.clamp()

    def peek(self):
        arg = self.args[self.index]

        # NILs
        if arg in (None, [], ()):
            return None

        return arg

    def remaining(self) -> list:
        return self.args[self.index:]

    def skip(self, dist: int):
        self.index += dist
        self.clamp()


class Interpreter:
    def __init__(self, program: str | BlockDirective, *, args: list | Args, position: int = 0, outer: int = None):
        if isinstance(program, str):
            self.ast = parse(tokenize(program))
        else:
            self.ast = program

        if isinstance(args, Args):
            self.args = args
        else:
            self.args = Args(args)

        self.buffer = ""
        self.position = position
        self.outer = outer

    def child(self, program: str | BlockDirective = BlockDirective("", []), *, args: list | Args) -> 'Interpreter':
        return Interpreter(program, args=args, position=self.get_position(), outer=self.outer)

    def output(self, data: str):
        self.buffer += data

    def get_param(self, directive: Directive, index: int, default=None):
        param = directive.get_param(index, default)

        match param:
            case Special.V:
                arg = self.args.consume()
                param = default if arg is None else arg

            case Special.Hash:
                param = self.args.hash()

        if default is not None and not isinstance(default, Special) and not isinstance(param, type(default)):
            raise ValueError(f"invalid type for ~{directive.kind} parameter {index}")

        return param

    def get_position(self) -> int:
        if "\n" in self.buffer:
            return len(self.buffer) - 1 - self.buffer.rfind("\n")

        else:
            return len(self.buffer) + self.position

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
                self.eval_radix(directive.copy(kind='r', params=[10, *directive.params]))
            case 'b':
                self.eval_radix(directive.copy(kind='r', params=[2, *directive.params]))
            case 'o':
                self.eval_radix(directive.copy(kind='r', params=[8, *directive.params]))
            case 'x':
                self.eval_radix(directive.copy(kind='r', params=[16, *directive.params]))

            # FORMAT Floating-Point Printers
            # FORMAT Printer Operations
            case 'a':
                self.eval_aesthetic(directive)
            case 's':
                # TODO: Add correct escapes to output
                self.eval_aesthetic(directive)

            # FORMAT Layout Control
            case 't':
                self.eval_tabulate(directive)
            case '<':
                self.eval_justification(directive)

            # FORMAT Control-Flow Operations
            case '*':
                self.eval_goto(directive)
            case '[':
                self.eval_conditional(directive)
            case '{':
                self.eval_iteration(directive)
            case '?':
                self.eval_recursive(directive)

            # FORMAT Miscellaneous Operations
            case '(':
                self.eval_case(directive)
            case 'p':
                self.eval_plural(directive)

            # FORMAT Miscellaneous Pseudo-Operations
            case '^':
                self.eval_escape_upward(directive)

            case _:
                print(f"Unrecognized directive ~{directive.kind}")

    def eval_ast_root(self):
        assert len(self.ast.clauses) == 1
        self.eval_clause(self.ast.clauses[0])

    def eval_clause(self, tokens: list):
        for token in tokens:
            self.eval(token)

    # FORMAT Basic Output
    def eval_character(self, directive: Directive):
        char = self.args.consume()

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
        arg = self.args.consume()

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
        arg = self.args.consume()
        
        if arg is None:
            output = "()" if directive.colon else "NIL"

        else:
            output = str(arg)

        min_col = self.get_param(directive, 0, default=0)
        col_inc = self.get_param(directive, 1, default=1)
        min_pad = self.get_param(directive, 2, default=0)
        pad_char = self.get_param(directive, 3, default=" ")

        min_padding = min_pad * pad_char

        if directive.at_sign:
            segments = ["", min_padding, output]

        else:
            segments = [output, min_padding, ""]

        self.output(self.justify(segments, min_col, col_inc, pad_char))

    @staticmethod
    def justify(segments: list, min_col: int, col_inc: int, pad_char: str):
        if col_inc > 0:
            while min_col < len("".join(segments)):
                min_col += col_inc

        index = 1
        while len("".join(segments)) < min_col:
            segments[index] += pad_char
            index += 2

            if index >= len(segments):
                index = 1

        return "".join(segments)

    # FORMAT Layout Control
    def eval_tabulate(self, directive: Directive):
        position = self.get_position()

        if directive.at_sign:
            col_rel = self.get_param(directive, 0, default=1)
            col_inc = self.get_param(directive, 1, default=1)

            k = math.ceil((position + col_rel) / (col_inc or 1))
            inc = k * col_inc - (position + col_rel)
            inc = max(inc, 1)

        else:
            col_num = self.get_param(directive, 0, default=1)
            col_inc = self.get_param(directive, 1, default=1)

            k = max(math.ceil((position - col_num) / (col_inc or 1)), 1)
            inc = k * col_inc - (position - col_num)
            inc = max(inc, 1 if col_inc else 0)

        self.output(" " * inc)

    def eval_justification(self, directive: BlockDirective):
        # Buffer optimization
        if not directive.params and not directive.default_token:
            interp = self.child(args=self.args)

            try:
                interp.eval_clause(directive.clauses[0])

            except StopIteration:
                return

            self.output(interp.buffer)
            return

        min_col = self.get_param(directive, 0, default=0)
        col_inc = self.get_param(directive, 1, default=1)
        min_pad = self.get_param(directive, 2, default=0)
        pad_char = self.get_param(directive, 3, default=" ")

        # Handle the overflow clause
        if directive.default_token:
            interp = self.child(args=self.args)

            try:
                interp.eval_clause(directive.clauses[0])

            except StopIteration:
                return

            index = 1
            overflow = interp.buffer
            line_pad = self.get_param(directive.default_token, 0, default=0)
            line_width = self.get_param(directive.default_token, 1, default=72)

        else:
            index = 0
            overflow = ""
            line_pad = 0
            line_width = 0

        segments = []
        min_padding = min_pad * pad_char

        if directive.colon:
            segments.extend(["", min_padding])

        for clause in directive.clauses[index:]:
            interp = self.child(args=self.args)

            try:
                interp.eval_clause(clause)

            except StopIteration:
                break

            segments.extend([interp.buffer, min_padding])

        if directive.at_sign:
            segments.extend(["", min_padding])

        output = self.justify(segments[:-1], min_col, col_inc, pad_char)

        if overflow and self.get_position() + len(output) + line_pad > line_width:
            output = overflow + output

        self.output(output)

    # FORMAT Control-Flow Operations
    def eval_goto(self, directive: Directive):
        if directive.at_sign:
            param = self.get_param(directive, 0, default=0)
            if param < 0:
                raise ValueError("negative ~@* arg")

            self.args.goto(param)

        else:
            param = self.get_param(directive, 0, default=1)
            if param < 0:
                raise ValueError("negative ~* arg")

            if directive.colon:
                self.args.skip(-param)

            else:
                self.args.skip(param)

    def eval_conditional(self, directive: BlockDirective):
        if directive.colon:
            if directive.at_sign:
                raise TypeError("~:@[ is invalid")

            if len(directive.clauses) != 2:
                raise ValueError("~:[ must contain exactly two clauses")

            if directive.default_token:
                raise TypeError("default clause in ~:[")

            no, yes = directive.clauses

            self.eval_clause(no if self.args.consume() is None else yes)

        elif directive.at_sign:
            if self.args.peek() is None:
                self.args.consume()
                return

            if len(directive.clauses) > 1:
                raise ValueError("multiple clauses in ~@[")

            self.eval_clause(directive.clauses[0])

        else:
            index = self.get_param(directive, 0, default=Special.V)

            if not isinstance(index, int):
                raise TypeError("invalid index for ~[")

            if index < 0:
                raise ValueError("negative ~[ arg")

            try:
                self.eval_clause(directive.clauses[index])

            except IndexError:
                if directive.default_token:
                    self.eval_clause(directive.clauses[-1])

    def eval_iteration(self, directive: BlockDirective):
        limit = self.get_param(directive, 0)

        if not isinstance(limit, int | None):
            raise TypeError("invalid iteration limit")

        if limit is not None and limit < 0:
            raise ValueError("negative iteration limit")

        # Empty body
        if not directive.clauses[0]:
            directive.clauses = parse(tokenize(self.args.consume())).clauses

        args = self.args if directive.at_sign else self.args.consume()
        if directive.colon:
            # ~:}
            if not args and directive.closing_token.colon:
                args = [[]]

            try:
                lst_hash = len(args)

            except TypeError:
                raise TypeError("~{ argument is not a list")

            for sublist in args[:limit]:
                interp = self.child(directive, args=sublist)

                lst_hash -= 1
                interp.outer = lst_hash

                try:
                    interp.eval_ast_root()

                except StopIteration as stop:
                    if stop.value:
                        break

                finally:
                    self.output(interp.buffer)

        else:
            interp = self.child(directive, args=args)
            iterations = 0

            try:
                # ~:}
                if directive.closing_token.colon and limit != 0:
                    interp.eval_ast_root()
                    iterations += 1

                while interp.args.hash() and (limit is None or iterations < limit):
                    interp.eval_ast_root()
                    iterations += 1

            except StopIteration:
                pass

            self.output(interp.buffer)

    def eval_recursive(self, directive: Directive):
        if directive.at_sign:
            interp = self.child(self.args.consume(), args=self.args)

        else:
            interp = self.child(self.args.consume(), args=self.args.consume())

        interp.eval_ast_root()
        self.output(interp.buffer)

    # FORMAT Miscellaneous Operations
    def eval_case(self, directive: BlockDirective):
        # TODO: Implement without recursion?

        interp = self.child(directive, args=self.args)
        interp.eval_clause(directive.clauses[0])
        output = interp.buffer

        if directive.colon:
            if directive.at_sign:
                self.output(output.upper())

            else:
                self.output(output.title())

        else:
            if directive.at_sign:
                output = list(output)
                while output:
                    char = output.pop(0)
                    if char.isalpha():
                        self.output(char.upper())
                        break

                    else:
                        self.output(char.lower())

                self.output("".join(output).lower())

            else:
                self.output(output.lower())

    def eval_plural(self, directive: Directive):
        if directive.colon:
            self.args.skip(-1)

        if directive.at_sign:
            options = ["ies", "y"]

        else:
            options = ["s", ""]

        self.output(options[self.args.consume() == 1])

    # FORMAT Miscellaneous Pseudo-Operations
    def eval_escape_upward(self, directive: Directive):
        if directive.at_sign:
            raise ValueError("~@^ is invalid")

        def escape():
            raise StopIteration(directive.colon)

        params = [self.get_param(directive, index) for index in range(len(directive.params))]
        if len(params) == 3 and not type(params[0]) is type(params[1]) is type(params[2]):
            return

        match params:
            case [] if directive.colon and self.outer == 0: escape()

            case [] if not directive.colon and self.args.hash() == 0: escape()

            case [a] if a == 0: escape()

            case [a, b] if a == b: escape()

            case [a, b, c] if a <= b <= c: escape()


def fourmat(program: str | BlockDirective, args: list | Args):
    interp = Interpreter(program, args=args)
    try:
        interp.eval_ast_root()

    except StopIteration:
        pass

    return interp.buffer


def fivemat(program: str, *, max_loops: int = None):
    parsed = parse(tokenize(decode_escapes(program)))
    tape = []

    try:
        iterations = 0
        while max_loops is None or iterations < max_loops:
            tape = fourmat(parsed, [list(tape)])
            print(end=tape[tape.rfind("\f") + 1:])
            iterations += 1

    except Exception:
        pass
