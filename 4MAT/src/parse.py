import re
from .directives import *


def tokenize(source) -> list[str | Directive]:
    # handle ~newline
    stripped_source = re.sub(r"~\n\s*|~:\n|~@(\n)\s*", r"\1", source)

    # this doesn't /really/ work in the case of malformed embedded directives, I don't think?
    textual_tokens = re.split(
        r"(~(?:[+-]?\d+|'.|[v#])?(?:,(?:[+-]?\d+|'.|[v#])?)*(?::?@?|@?:?).)", stripped_source
    )

    tokens = []
    for token in textual_tokens:
        if len(token) == 0:
            continue

        if token.startswith("~"):
            match = re.fullmatch(
                r"~((?:[+-]?\d+|'.|[v#])?(?:,(?:[+-]?\d+|'.|[v#])?)*)(:?@?|@?:?)(.)",
                token,
                re.DOTALL,
            )
            if match is None:
                print(f"Error on token {token}")
                exit(1)

            params = []
            if len(match[1]) > 0:
                for param in re.split(r"(?<!'),", match[1]):
                    if param.startswith("'"):
                        params.append(param[1])
                    elif param.lower() == "v":
                        params.append(Special.V)
                    elif param == '#':
                        params.append(Special.Hash)
                    elif len(param) == 0:
                        params.append(None)
                    else:
                        params.append(int(param))

            tokens.append(
                Directive(
                    match[3],
                    params=params,
                    at_sign="@" in match[2],
                    colon=":" in match[2],
                )
            )
        else:
            tokens.append(token)

    return tokens


def parse(tokens: list[str | Directive]) -> BlockDirective:
    stack = [BlockDirective("", [])]
    for token in tokens:
        if isinstance(token, Directive):
            if token.kind in "{[<(":  # opening block
                stack.append(BlockDirective.from_embedded(token))
                continue
            if token.kind == ";":
                if stack[-1].kind not in "[<":
                    print(f"~; is only supported in ~[ and ~< blocks")
                    exit(1)

                # TODO: Correctly check for invalid ~:;

                if token.colon:
                    stack[-1].default = True
                stack[-1].clauses.append([])
                continue

            if token.kind in "}]>)":  # closing block
                pairings = {
                    "{": "}",
                    "[": "]",
                    "<": ">",
                    "(": ")"
                }

                closed_block = stack.pop()
                if closed_block.kind == "":
                    print(f"Unmatched ~{token.kind}")
                    exit(1)
                if pairings[closed_block.kind] != token.kind:
                    print(
                        f"Unbalanced ~{closed_block.kind} (was closed with ~{token.kind})"
                    )
                    exit(1)

                stack[-1].clauses[-1].append(closed_block)
                continue
        stack[-1].clauses[-1].append(token)

    if len(stack) > 1:
        print(f"Unclosed ~{stack[1].kind}")
        exit(1)
    return stack[0]
