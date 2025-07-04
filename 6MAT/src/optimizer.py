import re

from warnings import warn


try:
    from util import *

except ImportError:
    from .util import *


class Opt:
    def __init__(self, pattern: str, name: str, flags: int | re.RegexFlag = 0):
        self.pattern = re.compile(pattern, flags=flags)
        self.name = name

    def __getattr__(self, item: str):
        try:
            return getattr(self.pattern, item)
        
        except AttributeError:
            return self.__getattribute__(item)


def arg_to_dist(arg: str) -> int:
    return int(arg.strip(":") or "1") * (-1 if ":" in arg else 1)


def dist_to_arg(dist: int) -> str:
    return str(abs(dist)) + (":" if dist < 0 else "")


def cleanup_args(args: str) -> str:
    return re.sub(r"\+?(\d+)", lambda match: str(int(match[1])), args)


def cleanup_directive(modifiers: str, directive: str) -> str:
    if directive in "%&|~":
        return directive.lower()

    else:
        return ''.join(sorted(modifiers)) + directive.lower()


def excise_common_prefix(match: re.Match) -> str:
    body = match[1]
    clauses = re.split("~:?;", body)
    index = min(map(len, clauses))
    prefix = ""

    while index:
        prefix = clauses[0][:index]
        if all(clause[:index] == prefix for clause in clauses):
            break

        index -= 1

    return f"{prefix}~#[{re.sub(r'(^|~:?;)' + prefix, lambda m: m[1], body)}~]"


CHAR = r"'\\.|'[^\\]"
CONST = r"~[%&|.]|~\n\s*|[^~]"

MOVE_OPTS = {
    # Any move followed by an absolute move
    Opt(r"~(?:#|\d+)?:?@?\*~(#|\d*)?:?@\*", "overwritten_move"):
        lambda match: f"~{int(match[1])}@*",

    # Repeated unidirectional moves
    Opt(r"~(?P<arg_1>(\d+)?(?P<back>:)?)\*~(?P<arg_2>(\d+)?(?(back):|))\*", "repeat_unidirectional_moves"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}*",

    # Using loops to move
    Opt(r"~@\{~\*~}", "move_loop"): "~#*",
    Opt(r"~(\d+)@\{~\*~}", "finite_move_loop"):
        lambda match: f"~{int(match[1])}*",

    # Non-reading operations between moves
    Opt(rf"(?P<first>(~(?:#|\d+)?:?@?)\*)(?P<const>({CONST})+)(?P<second>~(?:#|\d+)?:?@?\*)",
        "constants_between_moves"):
        lambda match: f"{match['first']}{match['second']}{match['const']}",

    # Trivial moves
    Opt(r"~0:?\*", "zero_step"): "",
    Opt(r"~0*1\*", "one_skip"): "~*",
    Opt(r"~0*1:\*", "one_back"): "~:*",
    Opt(r"~0@\*", "zero_goto"): "~@*",
    Opt(r"~#\*~\d*\*", "clamped_skip"): "~#*",
    Opt(r"~@\*~\d*:\*", "clamped_back"): "~@*",
    Opt(r"^~\d*,?:\*", "initial_back"): "",
    Opt(r"~\d*,?:?@?\*$", "final_skip"): "",
    Opt(r"~#@\*~#@\*", "double_damn"): ""
}

BREAK_OPTS = {
    # Constant unary numeric breaks
    Opt(r"~(-?\d+)\^", "unary_numeric_break"):
        lambda match: "~0^" if int(match[1]) == 0 else "",

    # Constant unary character breaks
    Opt(rf"~({CHAR})\^", "unary_character_break", flags=re.DOTALL): "",

    # Constant binary numeric breaks
    Opt(r"~(-?\d+)?,([-+]?\d+)?\^", "binary_numeric_break"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) == int(match[2])
                      or not any(match.groups()) else "",

    # Constant binary character breaks
    Opt(rf"~({CHAR})?,({CHAR})?\^", "binary_character_break", flags=re.DOTALL):
        lambda match: "~0^" if decode_escapes(match[1]) == decode_escapes(match[2]) else "",

    # Constant ternary numeric breaks
    Opt(r"~(-?\d+)?,([-+]?\d+)?,([-+]?\d+)?\^", "ternary_numeric_break"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) <= int(match[2]) <= int(match[3])
                      or not any(match.groups()) else "",

    # Constant ternary character breaks
    Opt(rf"~({CHAR})?,({CHAR})?,({CHAR})?\^", "ternary_character_break", flags=re.DOTALL):
        lambda match: "~0^" if decode_escapes(match[1]) <= decode_escapes(match[2]) <= decode_escapes(match[3]) else "",

    # Unreachable code
    Opt(r"(?P<exit>~0\^|~\?)[^<{\[]*?(?P<close>~>|~:?}|~])", "unreachable_code", flags=re.DOTALL):
        lambda match: f"{match['exit']}{match['close']}",

    Opt(r"~0\^(~:?[^:>}])*?$", "unreachable_end"): "",

    # '#' optimizations
    Opt(r"~#\^", "unary_remainder"): "~^",
    Opt(r"~(-\d+?|#),#(,#)?\^", "n-ary_remainder"): "~0^",
    Opt(r"~\^~}", "redundant_remainder_break"): "~}",
    Opt(r"~#,-\d+\^|~-\d+,#\^", "negative_binary_remainder_break"): "",
    Opt(r"~#,-\d+,(\d+|#)\^|~(\d+|#),#,-\d+\^", "negative_ternary_remainder_break"): "",
}

BLOCK_OPTS = {
    # Blocks that are never broken out of
    Opt(r"(~<|~1@\{)(?P<body>[^^{<]*?)(~>|~:})", "redundant_block"):
        lambda match: match["body"],

    # Constant blocks
    Opt(rf"~(?P<count>[1-6])@?\{{(?P<body>({CONST})*?)~}}", "unrolled_constant_block"):
        lambda match: min(match['body'] * int(match['count']), match[0], key=len),

    # Empty blocks
    Opt(r"(~<|~\d*@\{)(~0\^)*(~>|~:?})|~#?\[(~;)*(~:;)?~]", "empty_block"): "",

    # INIT - DO rearranging
    Opt(r"^~:\[(?P<init>.*?)~;~](?P<do>.*)$", "init_do", flags=re.DOTALL):
        lambda match: f"~:[{match['init']}~;{match['do']}~]",

    # Empty INIT
    Opt(r"^~:\[~;~:\*(?P<do>~\d*\{.*~})~]$", "empty_init"):
        lambda match: match['do'],

    # Adjacent case conversion blocks
    Opt(r"~(:@?)?\(([^(]*?)~\)~\1\(([^(]*?)~\)", "adjacent_case_blocks"):
        lambda match: f"~{match[1]}({match[2]}{match[3]}~)",

    # Common clause prefixes
    Opt(r"~#\[((~\S+|[^\[])*?~:;[^\[]*?)~]", "common_case_prefix"): excise_common_prefix,

    # Expandable default clause
    Opt(r"~;([^\[]*?)~:;\1~]", "merged_default_clause"):
        lambda match: f"~:;{match[1]}~]"
}

CRASH_OPTS = {
    # Loops which do not move the tape pointer
    Opt(rf"~@?\{{(?P<body>({CONST})*?)~:?}}", "infinite_loop", flags=re.DOTALL): "~?",

    # Reading past the end of the tape
    Opt(rf"~#\[({CONST})*?~\w[^\[]*?~]", "invalid_read"): "~#[~?~]"
}

BOUNDEDNESS_OPTS = {
    # Relative move followed by a relative move
    Opt(r"~(?P<arg_1>(\d+)?:?)(?P<mod>@?)\*~(?P<arg_2>(\d+)?:?)\*", "repeat_moves"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}{match['mod']}*",

    # Short loops
    Opt(rf"~(?P<count>[1-3])@?\{{(?P<body>(~[ac]|{CONST})*?)~:?}}", "unrolled_loop"):
        lambda match: min(match['body'] * int(match['count']), match[0], key=len),
}

SPECIAL_DIRECTIVES = {
    # ~p
    Opt(r"ies~\*", "plural_y"): "~@p",
    Opt(r"s~\*", "plural"): "~p",

    # ~$
    Opt(r"~(?P<width>-?\d+|#),*@a", "dollar"):
        lambda match: f"~{match['width']}$",

    Opt(rf"~(?P<width>-?\d+|#),,,(?P<char>{CHAR})@a", "dollar_pad"):
        lambda match: f"~{match['width']},,,{match['char']}$",

    Opt(rf"~\*~(?P<width>-?\d+),,,(?P<char>{CHAR})@a", "dollar_pad_skip"):
        lambda match: f"~{match['width']},v,,{match['char']}$",

    Opt(rf"~2\*~(?P<width>-?\d+),,,(?P<char>{CHAR})@a", "dollar_pad_double_skip"):
        lambda match: f"~{match['width']},v,v,{match['char']}$",

    Opt(r"~(?P<width>-?\d+|#),,,v@a", "dollar_read"):
        lambda match: f"~{match['width']},,,v$",

    Opt(r"~\*~(?P<width>-?\d+),,,v@a", "dollar_read_skip"):
        lambda match: f"~{match['width']},v,,v$",

    Opt(r"~2\*~(?P<width>-?\d+),,,v@a", "dollar_read_double_skip"):
        lambda match: f"~{match['width']},v,v,v$",

    # ~@c
    Opt(r"#\\~:c", "character_reader_name"): "~@c"
}

DEFAULT_PARAMETERS = {
    # Prints
    Opt(r"~0?(,1?(,0?(,' )?)?)?:?(?P<mod>@?)a", "default_print"):
        lambda match: f"~{match['mod']}a",

    Opt(r"~@a", "unpadded_print"): "~a",

    # Repeated characters
    Opt(r"~1?([%&|.])", "default_basic_character"):
        lambda match: f"~{match[1]}",

    # Justification
    Opt(r"~0?(,1?(,0?(,' )?)?)?(?P<mod>:?@?)<", "default_justification"):
        lambda match: f"~{match['mod']}<",

    Opt(r"~1?(,(72)?)?:;", "default_overflow"): "~:;",

    # Tabulation
    Opt(r"~1?(,1?)?:?(?P<mod>@?)t", "default_tabulation"):
        lambda match: f"~{match['mod']}t"
}

FORMATTING = {
    # Comments
    Opt(r"~(-?\d+)\[.*?~]", "comment", flags=re.DOTALL):
        lambda match: match[0] if int(match[1]) == 0 else "",

    # Newlines/indentation
    Opt(r"~\n\s*|~:\n|~@(\n)\s*", "indentation"):
        lambda match: match[1]
}

BASIC_OPTS = {
    **MOVE_OPTS,
    **BREAK_OPTS,
    **BLOCK_OPTS,
    **CRASH_OPTS
}

UNSAFE_OPTS = {
    **BASIC_OPTS,
    **BOUNDEDNESS_OPTS
}

GOLF_OPTS = {
    **BASIC_OPTS,
    **SPECIAL_DIRECTIVES,
    **DEFAULT_PARAMETERS,
    **FORMATTING
}


def optimize(program: str, optimizations: dict[Opt, ...], **flags) -> tuple[str, int]:
    # Standardize arguments and modifiers
    program = re.sub(r"~(?P<args>([+-]?\d+|'.|[v#])?(,([+-]?\d+|'.|[v#])?)*),?(?P<mod>(:?@?|@?:?))(?P<dir>[^:@])",
                     lambda match: f"~{cleanup_args(match['args'])}{cleanup_directive(match['mod'], match['dir'])}",
                     program, flags=re.DOTALL | re.IGNORECASE)

    # Sequester escaped tildes
    program = re.sub(r"(~(#|\d*)~)+", lambda match: f"~TILDE<{match[0]}~>", program)

    done = False
    saved = 0
    while not done:
        try:
            done = True
            for regex, repl in optimizations.items():
                new = regex.sub(repl, program)

                if new != program:
                    done = False
                    saved += len(program) - len(new)
                    program = new

                    if flags.get("verbose"):
                        print(f"Applied {regex.name}")

        except Exception:
            warn("optimizer ran into an error during execution; partial optimization was returned", UserWarning)
            break

    # Put them back
    program = re.sub(r"~TILDE<(.*?)~>", lambda match: match[1], program)

    return program, saved


if __name__ == "__main__":
    print(optimize("~:[~;~:*~{~:(~00,+1@:a~)~:(~<~c~>~)~#[a~;ab~:;ab~]~-1,#,#^~a7~}~]",
                   UNSAFE_OPTS | GOLF_OPTS, verbose=True))


__all__ = ["FORMATTING", "BASIC_OPTS", "UNSAFE_OPTS", "GOLF_OPTS", "optimize"]
