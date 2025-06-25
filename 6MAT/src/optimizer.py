import re

from warnings import warn


def arg_to_dist(arg: str) -> int:
    return int(arg.strip(":") or "1") * (-1 if ":" in arg else 1)


def dist_to_arg(dist: int) -> str:
    return str(abs(dist)) + (":" if dist < 0 else "")


MOVE_OPTIMIZATIONS = {
    # Any move followed by an absolute move
    re.compile(r"~(?:#|(?:\+?\d+)?):?@?\*~(\d*):?@\*"):
        lambda match: f"~{int(match[1])}@*",

    # Repeated unidirectional moves
    re.compile(r"~(?P<arg_1>(\+?\d+)?(?P<back>:)?)\*~(?P<arg_2>(\+?\d+)?(?(back):|))\*"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}*",

    # Using loops to move
    re.compile(r"~#?@\{~\*~}"): "~#*",
    re.compile(r"~(\+?\d+)@\{~\*~}"):
        lambda match: f"~{int(match[1])}*",

    # Trivial moves
    re.compile(r"~0:?\*"): "",
    re.compile(r"~#:\*|~0@\*"): "~@*",
    re.compile(r"~#:?@\*"): "~#*",
}

BREAK_OPTIMIZATIONS = {
    # Constant unary number breaks
    re.compile(r"~([-+]?\d+)\^"):
        lambda match: "~0^" if int(match[1]) == 0 else "",

    # Constant unary character breaks
    re.compile(r"~'.\^", flags=re.DOTALL): "",

    # Constant binary number breaks
    re.compile(r"~([-+]?\d+)?,([-+]?\d+)?\^"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) == int(match[2])
                      or not any(match.groups()) else "",

    # Constant binary character breaks
    re.compile(r"~('.)?,('.)?\^", flags=re.DOTALL):
        lambda match: "~0^" if match[1] == match[2] else "",

    # Constant ternary number breaks
    re.compile(r"~([-+]?\d+)?,([-+]?\d+)?,([-+]?\d+)?\^"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) <= int(match[2]) <= int(match[3])
                      or not any(match.groups()) else "",

    # Constant ternary character breaks
    re.compile(r"~('.)?,('.)?,('.)?\^", flags=re.DOTALL):
        lambda match: "~0^" if match[1] <= match[2] <= match[3] else "",

    # Unreachable code
    re.compile(r"(~0\^|~\?).*?(~>|~:?})", flags=re.DOTALL):
        lambda match: match[1] + match[2],

    re.compile(r"~0(~:?[^:>}])*?\^$"): "",

    # '#' optimizations
    re.compile(r"~#\^"): "~^",
    re.compile(r"~#,(-.*?)\^|~(-.*?),#\^"): "",
    re.compile(r"~#,(-.*?),.*?\^|~.*?,#,(-.*?)\^"): "",
}

BLOCK_OPTIMIZATIONS = {
    # Blocks that are never broken out of
    re.compile(r"(~<|~1@\{)(?P<body>[^^]*?)(~>|~:})"):
        lambda match: match["body"],

    # Constant blocks
    re.compile(r"~(?P<count>[1-6])@?\{(?P<body>(~~|[^~])*?)~}"):
        lambda match: min(match['body'] * int(match['count']), match[0], key=len),

    # Empty blocks
    re.compile(r"(~<|~\d*@\{)(~0\^)*(~>|~:?})"): "",

    # INIT - DO rearranging
    re.compile(r"^~:\[(?P<init>.*?)~;~](?P<body>.*)$", flags=re.DOTALL):
        lambda match: f"~:[{match['init']}~;{match['body']}~]",
}

DETECTABLE_CRASHES = {
    # Loops which do not move the tape pointer
    re.compile(r"~@?\{(?P<body>[^~]*?)~}", flags=re.DOTALL): "~?",
}

BOUNDEDNESS_OPTIMIZATIONS = {
    # Any move followed by a relative move
    re.compile(r"~(?P<arg_1>(\+?\d+)?:?)(?P<absolute>@?)\*~(?P<arg_2>(\+?\d+)?:?)\*"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}"
                      f"{'@' if match['absolute'] else ''}*",

    # Short loops
    re.compile(r"~(?P<count>[1-3])@?\{(?P<body>(~:?@?[^:@~<{]|[^~])*?)~}"):
        lambda match: min(match['body'] * int(match['count']), match[0], key=len),
}

FORMATTING = {
    # Comments
    re.compile(r"~([-+]?\d+)\[.*?~]", flags=re.DOTALL):
        lambda match: match[0] if int(match[1]) == 0 else "",

    # Newlines
    re.compile(r"~\n\s*"): "",
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


def optimize(program: str, optimizations: dict[re.Pattern, ...]):
    # Sequester escaped tildes
    program = re.sub(r"(~(#|\d*)~)+", lambda match: f"~TILDE<{match[0]}~>", program)

    done = False
    while not done:
        try:
            done = True
            for regex, repl in optimizations.items():
                new = regex.sub(repl, program)

                if new != program:
                    done = False
                    program = new

        except Exception:
            warn("optimizer ran into an error during execution; partial optimization was returned", UserWarning)
            break

    # Put them back
    program = re.sub(r"~TILDE<(.*?)~>", lambda match: match[1], program)

    return program


__all__ = ["FORMATTING", "O1", "O2", "optimize"]
