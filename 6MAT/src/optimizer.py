import re

from warnings import warn


try:
    from util import *

except ImportError:
    from .util import *


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


CHAR = r"'\\.|'[^\\]"
CONST = r"~[%&|.]|~\n\s*|[^~]"

MOVE_OPTIMIZATIONS = {
    # Any move followed by an absolute move
    re.compile(r"~(?:#|\d+)?:?@?\*~(#|\d*)?:?@\*"):
        lambda match: f"~{int(match[1])}@*",

    # Repeated unidirectional moves
    re.compile(r"~(?P<arg_1>(\d+)?(?P<back>:)?)\*~(?P<arg_2>(\d+)?(?(back):|))\*"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}*",

    # Using loops to move
    re.compile(r"~@\{~\*~}"): "~#*",
    re.compile(r"~(\d+)@\{~\*~}"):
        lambda match: f"~{int(match[1])}*",

    # Non-reading operations between moves
    re.compile(rf"(?P<first>(~(?:#|\d+)?:?@?)\*)(?P<const>({CONST})+)(?P<second>~(?:#|\d+)?:?@?\*)"):
        lambda match: f"{match['first']}{match['second']}{match['const']}",

    # Trivial moves
    re.compile(r"~0:?\*"): "",
    re.compile(r"~0*1\*"): "~:*",
    re.compile(r"~0*1:\*"): "~:*",
    re.compile(r"~#:\*|~0@\*"): "~@*",
    re.compile(r"~@\*~\d*:\*"): "~@*",
    re.compile(r"~#\*~\d*\*"): "~#*",
    re.compile(r"^~\d*,?:\*$"): "",
    re.compile(r"~\d*,?:?@?\*$"): "",
    re.compile(r"~#@\*~#@\*"): ""
}

BREAK_OPTIMIZATIONS = {
    # Constant unary number breaks
    re.compile(r"~(-?\d+)\^"):
        lambda match: "~0^" if int(match[1]) == 0 else "",

    # Constant unary character breaks
    re.compile(rf"~({CHAR})\^", flags=re.DOTALL): "",

    # Constant binary number breaks
    re.compile(r"~(-?\d+)?,([-+]?\d+)?\^"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) == int(match[2])
                      or not any(match.groups()) else "",

    # Constant binary character breaks
    re.compile(rf"~({CHAR})?,({CHAR})?\^", flags=re.DOTALL):
        lambda match: "~0^" if decode_escapes(match[1]) == decode_escapes(match[2]) else "",

    # Constant ternary number breaks
    re.compile(r"~(-?\d+)?,([-+]?\d+)?,([-+]?\d+)?\^"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) <= int(match[2]) <= int(match[3])
                      or not any(match.groups()) else "",

    # Constant ternary character breaks
    re.compile(rf"~({CHAR})?,({CHAR})?,({CHAR})?\^", flags=re.DOTALL):
        lambda match: "~0^" if decode_escapes(match[1]) <= decode_escapes(match[2]) <= decode_escapes(match[3]) else "",

    # Unreachable code
    re.compile(r"(~0\^|~\?).*?(~>|~:?}|~])", flags=re.DOTALL):
        lambda match: f"{match[1]}{match[2]}",

    re.compile(r"~0\^(~:?[^:>}])*?$"): "",

    # '#' optimizations
    re.compile(r"~#\^"): "~^",
    re.compile(r"~#,#(,#)?\^"): "~0^",
    re.compile(r"~\^~}"): "~}",
    re.compile(r"~#,(-.*?)\^|~(-.*?),#\^"): "",
    re.compile(r"~#,(-.*?),.*?\^|~.*?,#,(-.*?)\^"): "",
}

BLOCK_OPTIMIZATIONS = {
    # Blocks that are never broken out of
    re.compile(r"(~<|~1@\{)(?P<body>[^^{<]*?)(~>|~:})"):
        lambda match: match["body"],

    # Constant blocks
    re.compile(rf"~(?P<count>[1-6])@?\{{(?P<body>({CONST})*?)~}}"):
        lambda match: min(match['body'] * int(match['count']), match[0], key=len),

    # Empty blocks
    re.compile(r"(~<|~\d*@\{)(~0\^)*(~>|~:?})"): "",

    # INIT - DO rearranging
    re.compile(r"^~:\[(?P<init>.*?)~;~](?P<do>.*)$", flags=re.DOTALL):
        lambda match: f"~:[{match['init']}~;{match['do']}~]",

    # Empty INIT
    re.compile(r"^~:\[~;~:\*(?P<do>~\d*\{.*~})~]$"):
        lambda match: match['do'],

    # Adjacent case conversion blocks
    re.compile(r"~(:@?)?\(([^(]*?)~\)~\1\(([^(]*?)~\)"):
        lambda match: f"~{match[1]}({match[2]}{match[3]}~)"
}

DETECTABLE_CRASHES = {
    # Loops which do not move the tape pointer
    re.compile(rf"~@?\{{(?P<body>({CONST})*?)~:?}}", flags=re.DOTALL): "~?",

    # Reading past the end of the tape
    re.compile(rf"~#\[({CONST})*?~\w(~[^\[]|[^~])*~]"): "~#[~?~]"
}

BOUNDEDNESS_OPTIMIZATIONS = {
    # Relative move followed by a relative move
    re.compile(r"~(?P<arg_1>(\d+)?:?)(?P<mod>@?)\*~(?P<arg_2>(\d+)?:?)\*"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}{match['mod']}*",

    # Short loops
    re.compile(rf"~(?P<count>[1-3])@?\{{(?P<body>(~[ac]|{CONST})*?)~:?}}"):
        lambda match: min(match['body'] * int(match['count']), match[0], key=len),
}

SPECIAL_DIRECTIVES = {
    # ~p
    re.compile(r"ies~\*"): "~@p",
    re.compile(r"s~\*"): "~p",

    # ~@[~]
    re.compile(r"^~:\[~;(?P<body>.*)~]$", flags=re.DOTALL):
        lambda match: f"~@[{match['body']}~]",

    # ~$
    re.compile(r"~(?P<width>-?\d+|#),*@a"):
        lambda match: f"~{match['width']}$",

    re.compile(rf"~(?P<width>-?\d+|#),,,(?P<char>{CHAR})@a"):
        lambda match: f"~{match['width']},,,{match['char']}$",

    re.compile(rf"~\*~(?P<width>-?\d+),,,(?P<char>{CHAR})@a"):
        lambda match: f"~{match['width']},v,,{match['char']}$",

    re.compile(rf"~2\*~(?P<width>-?\d+),,,(?P<char>{CHAR})@a"):
        lambda match: f"~{match['width']},v,v,{match['char']}$",

    re.compile(r"~(?P<width>-?\d+|#),,,v@a"):
        lambda match: f"~{match['width']},,,v$",

    re.compile(r"~\*~(?P<width>-?\d+),,,v@a"):
        lambda match: f"~{match['width']},v,,v$",

    re.compile(r"~2\*~(?P<width>-?\d+),,,v@a"):
        lambda match: f"~{match['width']},v,v,v$",

    # ~@c
    re.compile(r"#\\~:c"): "~@c"
}

DEFAULT_PARAMETERS = {
    # Prints
    re.compile(r"~0?(,1?(,0?(,' )?)?)?:?(?P<mod>@?)a"):
        lambda match: f"~{match['mod']}a",

    # Repeated characters
    re.compile(r"~1?([%&|.])"):
        lambda match: f"~{match[1]}",

    # Justification
    re.compile(r"~0?(,1?(,0?(,' )?)?)?(?P<mod>:?@?)<"):
        lambda match: f"~{match['mod']}<",

    re.compile(r"~1?(,(72)?)?:;"): "~:;",

    # Tabulation
    re.compile(r"~1?(,1?)?:?(?P<mod>@?)t"):
        lambda match: f"~{match['mod']}t"
}

FORMATTING = {
    # Comments
    re.compile(r"~(-?\d+)\[.*?~]", flags=re.DOTALL):
        lambda match: match[0] if int(match[1]) == 0 else "",

    # Newlines
    re.compile(r"~\n\s*|~:\n|~@(\n)\s*"):
        lambda match: match[1]
}

O1 = {
    **MOVE_OPTIMIZATIONS,
    **BREAK_OPTIMIZATIONS,
    **BLOCK_OPTIMIZATIONS,
    **DETECTABLE_CRASHES
}

O2 = {
    **O1,
    **BOUNDEDNESS_OPTIMIZATIONS
}

O3 = {
    **O2,
    **SPECIAL_DIRECTIVES,
    **DEFAULT_PARAMETERS,
    **FORMATTING
}


def optimize(program: str, optimizations: dict[re.Pattern, ...]) -> tuple[str, int]:
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

        except Exception:
            warn("optimizer ran into an error during execution; partial optimization was returned", UserWarning)
            break

    # Put them back
    program = re.sub(r"~TILDE<(.*?)~>", lambda match: match[1], program)

    return program, saved


if __name__ == "__main__":
    print(optimize("~:[~;~:*~{~:(~00,+1@:a~)~:(~<~c~>~)~}~]", O3))


__all__ = ["FORMATTING", "O1", "O2", "O3", "optimize"]
