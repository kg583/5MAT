import re

from warnings import warn


def arg_to_dist(arg: str) -> int:
    return int(arg.strip(":") or "1") * (-1 if ":" in arg else 1)


def dist_to_arg(dist: int) -> str:
    return str(abs(dist)) + (":" if dist < 0 else "")


MOVE_OPTIMIZATIONS = {
    # Any forward move followed by a relative move
    re.compile(r"~(?P<arg_1>(\+?\d+)?)(?P<absolute>@?)\*~(?P<arg_2>(\+?\d+)?:?)\*"):
        lambda match: f"~{dist_to_arg(arg_to_dist(match['arg_1']) + arg_to_dist(match['arg_2']))}"
                      f"{'@' if match['absolute'] else ''}*",

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

_break_pattern = re.compile(r"~([-+]?\d+|#|'.|v)?(,([-+]?\d+|#|'.|v))?(,([-+]?\d+|#|'.|v))?\^", flags=re.DOTALL)

BREAK_OPTIMIZATIONS = {
    # Constant unary number breaks
    re.compile(r"~([-+]?\d+)\^"):
        lambda match: "~0^" if int(match[1]) == 0 else "",

    # Constant unary character breaks
    re.compile(r"~'.\^"): "",

    # Constant binary number breaks
    re.compile(r"~([-+]?\d+)?,([-+]?\d+)?\^"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) == int(match[2])
                      or not any(match.groups()) else "",

    # Constant binary character breaks
    re.compile(r"~('.)?,('.)?\^"):
        lambda match: "~0^" if match[1] == match[2] else "",

    # Constant ternary number breaks
    re.compile(r"~([-+]?\d+)?,([-+]?\d+)?,([-+]?\d+)?\^"):
        lambda match: "~0^" if all(match.groups()) and int(match[1]) <= int(match[2]) <= int(match[3])
                      or not any(match.groups()) else "",

    # Constant ternary character breaks
    re.compile(r"~('.)?,('.)?,('.)?\^"):
        lambda match: "~0^" if match[1] <= match[2] <= match[3] else "",

    # Unreachable code
    re.compile(r"(~0\^|~\?).+?(?:~>|~:?})"):
        lambda match: match[1],

    re.compile(r"~0(~:?[^:>}])*?\^$"): "",

    # '#' optimizations
    re.compile(r"~#\^"): "~^",
    re.compile(r"~#,(-.*?)\^|~(-.*?),#\^"): "",
    re.compile(r"~#,(-.*?),.*?\^|~.*?,#,(-.*?)\^"): "",
}

DETECTABLE_CRASHES = {
    # Loops which do not move the tape pointer
    re.compile(r"~@?\{(?P<body>[^~]*?)~}", flags=re.DOTALL): "~?",
}

BLOCK_OPTIMIZATIONS = {
    # Nested blocks
    re.compile(r"(?P<left>(~<|~1@\{))(~<|~1@\{)+(?P<body>(~:?@?[^:@~<{]|[^~])*?)(~>|~:})+(?P<right>(~>|~:}))"):
        lambda match: match['left'] + match['body'] + match['right'],

    # Blocks that are never broken out of
    re.compile(r"(~<|~1@\{)(?P<body>(~:?@?[^:@~<{]|[^~])*?)(~>|~:})"):
        lambda match: match[0] if re.search(_break_pattern, match["body"]) else match["body"]
}

LAYOUT_OPTIMIZATIONS = {
    # INIT - DO compression
    re.compile(r"^~:\[(?P<init>.*?)~;~](?P<body>.*)$"):
        lambda match: f"~:[{match['init']}~;{match['body']}~]",

    # Comments
        re.compile(r"~([-+]?\d+)\[.*?~]", flags=re.DOTALL):
            lambda match: match[0] if int(match[1]) == 0 else "",

    # Newlines
    re.compile(r"~\n\s*"): ""
}

OPTIMIZE = {
    **MOVE_OPTIMIZATIONS,
    **BREAK_OPTIMIZATIONS,
    **DETECTABLE_CRASHES
}


COMPRESS = {
    **OPTIMIZE,
    **BLOCK_OPTIMIZATIONS,
    **LAYOUT_OPTIMIZATIONS
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


__all__ = ["OPTIMIZE", "COMPRESS", "optimize"]
