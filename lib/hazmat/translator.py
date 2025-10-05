from pathlib import Path

from lib.fourmat.directives import BlockDirective, Directive, Special
from lib.fourmat.parse import parse

from lib.hazmat.c_writer import Writer, cstr

def numeric_param(directive, index, default=0):
    param = directive.get_param(index, default)

    match param:
        case Special.V:
            param = "V"

        case Special.Hash:
            param = "HASH"

        case _:
            if isinstance(param, str):
                param = ord(param)

    return param


class Translator:
    def __init__(self, program: str | BlockDirective):
        if isinstance(program, str):
            self.ast = parse(program)
        else:
            self.ast = program

        self.writer = Writer()
        self.writer.write_raw((Path(__file__).parent / "boilerplate.c").read_text("utf-8"))

        self.stack = []
        self.in_outer_scope = True
        self.internal_counters = 0

    def next_internal_counter(self) -> str:
        name = f"_i{self.internal_counters}"
        self.internal_counters += 1
        return name

    def emit_str(self, data: str, prealloc=False):
        encoded = data.encode('utf-8')
        if len(encoded) == 1:
            self.writer.function_call("buffer_append_char" if not prealloc else "buffer_append_char_raw", "w",
                                      encoded[0])
        else:
            self.writer.function_call("buffer_append", "w", cstr(encoded), len(encoded))

    def emit_crash(self):
        self.writer.function_return("1")

    def translate(self, token: str | Directive):
        self.writer.comment(str(token))
        if isinstance(token, str):
            self.emit_str(token)
            return

        directive = token
        match directive.kind:
            # FORMAT Basic Output
            case 'c':
                self.emit_character(directive)
            case '%':
                self.emit_repeated(directive, "\n")
            case '&':
                self.eval_fresh_line(directive)
            case '|':
                self.emit_repeated(directive, "\f")
            case '~':
                self.emit_repeated(directive, "~")

            # FORMAT Printer Operations
            case 'a':
                self.eval_aesthetic(directive)

            # FORMAT Pretty Printer Operations
            case '/':
                self.eval_call_function(directive)

            # FORMAT Layout Control
            case 't':
                self.eval_tabulate(directive)
            case '<':
                self.emit_justification(directive)

            # FORMAT Control-Flow Operations
            case '*':
                self.emit_goto(directive)
            case '[':
                self.emit_conditional(directive)
            case '{':
                self.emit_iteration(directive)
            case '?':
                self.emit_recursive(directive)

            # FORMAT Miscellaneous Operations
            case '(':
                self.eval_case(directive)

            # FORMAT Miscellaneous Pseudo-Operations
            case '^':
                self.emit_escape_upward(directive)

            case _:
                print(f"Unrecognized directive ~{directive.kind}")

    def translate_ast_root(self):
        assert len(self.ast.clauses) == 1

        with self.writer.function_decl("int", "fourmat", ("OutputBuffer", "*w"), ("char", "*r")):
            self.writer.write_line("size_t a, b, c;")  # used for ~^
            self.writer.write_line("int outer_idx = 0;")
            self.translate_clause(self.ast.clauses[0])
            self.writer.function_return("0")

    def translate_clause(self, tokens: list):
        for token in tokens:
            self.translate(token)

    # FORMAT Basic Output
    def emit_character(self, directive: Directive):
        if self.in_outer_scope:
            self.emit_crash()
            return

        if directive.colon or directive.at_sign:
            if directive.at_sign and not directive.colon:
               self.emit_str("#\\")

            with self.writer.scope():
                self.writer.write_line("char ch = *r++;")
                self.writer.write_line("const char *chname = charname(ch);")
                with self.writer.if_stmt("chname"):
                    self.writer.function_call("buffer_append", "w", "chname+1", "(size_t) *chname")
                with self.writer.else_stmt():
                    self.writer.function_call("buffer_append_char", "w", "ch")
        else:
            self.writer.function_call("buffer_append_char", "w", "*r++")

    def emit_repeated(self, directive: Directive, char: str):
        match directive.get_param(0, default=1):
            case Special.V:
                self.emit_crash()
            case Special.Hash:
                # this can be optimized but who tf writes ~#~
                self.writer.function_call("buffer_ensure_capacity", "w", "HASH")
                with self.writer.for_loop("int i = HASH", "i > 0", "--i"):
                    self.emit_str(char, prealloc=True)
            case n:
                if isinstance(n, str):
                    self.emit_crash()
                else:
                    self.emit_str(char * n)

    # FORMAT Layout Control
    def emit_justification(self, directive: BlockDirective):
        # Buffer optimization
        if not directive.params and not directive.default_token:
            with self.writer.scope():
                varname = self.next_internal_counter()
                self.writer.write_line(f"size_t {varname} = w->size;")
                with self.writer.while_loop("1"):
                    self.translate_clause(directive.clauses[0])
                    self.writer.write_line(f"{varname} = w->size;")
                    self.writer.write_line("break;")
                self.writer.write_line(f"w->size = {varname};")
            return

    # FORMAT Control-Flow Operations
    def emit_goto(self, directive: Directive):
        if self.in_outer_scope:
            self.writer.write_line("outer_idx = 0;")
            return

        if directive.at_sign:
            param = numeric_param(directive, 0, default=0)
            if param == "V":
                self.emit_crash()
                return

            self.writer.write_line(f"r = start + {param};")
            self.writer.if_stmt("r > end", body="r = end;")
        else:
            param = numeric_param(directive, 0, default=1)
            if param == "V":
                self.emit_crash()
                return

            if directive.colon:
                self.writer.write_line(f"r -= {param};")
                self.writer.if_stmt("r < start", body="r = start;")
            elif param == "HASH":
                # ~#* is incredibly common in 5MAT code... might as well simplify.
                self.writer.write_line("r = end;")
            else:
                self.writer.write_line(f"r += {param};")
                self.writer.if_stmt("r > end", body="r = end;")

    def emit_conditional(self, directive: BlockDirective):
        if not directive.at_sign and not directive.colon:
            if directive.get_param(0, default=None) in (None, Special.V):
                self.emit_crash()
                return
            chosen_path = numeric_param(directive, 0, default=0)

            if chosen_path == "HASH":
                # emit a switch block... it'd be nice to help clang out & collapse identical branches but it's fine
                with self.writer.switch_stmt("HASH"):
                    for i, clause in enumerate(directive.clauses):
                        is_last = i == len(directive.clauses) - 1
                        case_id = None if (is_last and directive.default_token) else i
                        with self.writer.case_stmt(case_id):
                            self.translate_clause(clause)
                return

            if chosen_path > len(directive.clauses) - 1:
                if directive.default_token:
                    self.translate_clause(directive.clauses[-1])
                else:
                    self.writer.comment("\n".join(map(lambda x: x[0], directive.clauses)))
                return

        if self.in_outer_scope and directive.colon:
            self.writer.write_line("++outer_idx;")
            with self.writer.if_stmt("start == end"):
                self.translate_clause(directive.clauses[0])
            if directive.clauses[1]:
                with self.writer.else_stmt():
                    self.translate_clause(directive.clauses[1])
            return
        raise NotImplementedError()

    def emit_iteration(self, directive: BlockDirective):
        if (len(directive.clauses[0]) == 0 # stupid behavior - recursion
                or directive.colon # argument is never a list of sublists
                or (not directive.at_sign and not self.in_outer_scope)): # once we're on the tape, ~{ won't work
            self.emit_crash()
            return

        use_do_while = directive.closing_token.colon and not directive.get_param(0, default=0) == Special.Hash
        loop_generator = self.writer.do_while_loop if use_do_while else self.writer.while_loop

        if directive.colon or (not directive.at_sign and not self.in_outer_scope):
            self.emit_crash()

        if directive.at_sign:
            if self.in_outer_scope:
                with self.writer.while_loop("outer_idx == 0"):
                    self.translate_clause(directive.clauses[0])
                    self.writer.write_line("outer_idx += 1;")
                    return

            if directive.params:
                if isinstance(directive.params[0], int):
                    if directive.params[0] != 0:
                        temp = self.next_internal_counter()
                        self.writer.write_line(f"int {temp} = {directive.params[0]};")
                        with loop_generator(condition=f"{temp} && r != end"):
                            self.translate_clause(directive.clauses[0])
                            self.writer.write_line(f"--{temp};")
            else:
                with loop_generator(condition="r != end"):
                    self.translate_clause(directive.clauses[0])
        else:
            self.in_outer_scope = False
            with self.writer.if_stmt("outer_idx == 0"):
                with loop_generator(condition="r != end"):
                    self.translate_clause(directive.clauses[0])
            self.in_outer_scope = True

    def emit_recursive(self, _directive: Directive):
        self.emit_crash()

    # FORMAT Miscellaneous Pseudo-Operations
    def emit_escape_upward(self, directive: Directive):
        escape_operation = "return 0;" if self.in_outer_scope else "break;"

        params = [numeric_param(directive, index, default=None) for index in range(len(directive.params))]

        match params:
            case [] if directive.colon:
                raise NotImplementedError()

            case [] if not directive.colon:
                self.writer.if_stmt("r == end", body=escape_operation)

            case [a]:
                self.writer.if_stmt(f"{a} == 0", body=escape_operation)

            case [a, b]:
                if a == "V" and not isinstance(b, int):
                    # avoid undefined behavior...
                    self.writer.write_line("a = V;")
                    self.writer.if_stmt(f"a == {b}", body=escape_operation)
                else:
                    self.writer.if_stmt(f"{a} == {b}", body=escape_operation)

            case [a, b, c]:
                self.writer.write_line(f"a = {a}; b = {b}; c = {c};")
                self.writer.if_stmt("a <= b && b <= c", body=escape_operation)
