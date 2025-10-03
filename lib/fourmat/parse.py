import re
from lib.fourmat.directives import *


def tokenize(source) -> list[str | Directive]:
    # handle ~newline
    stripped_source = re.sub(r"~\n\s*|~:\n|~@(\n)\s*", r"\1", source)

    # this doesn't /really/ work in the case of malformed embedded directives, I don't think?
    textual_tokens = re.split(
        r"(~(?:[+-]?\d+|'.|[v#])?(?:,(?:[+-]?\d+|'.|[v#])?)*(?::?@?|@?:?)(?:/[^/\s]+/|[^/]))", stripped_source, flags=re.IGNORECASE
    )

    tokens = []
    for token in textual_tokens:
        if len(token) == 0:
            continue

        if token.startswith("~"):
            match = re.fullmatch(
                r"~((?:[+-]?\d+|'.|[v#])?(?:,(?:[+-]?\d+|'.|[v#])?)*)(:?@?|@?:?)(/[^/\s]+/|[^/])",
                token,
                re.DOTALL | re.IGNORECASE,
            )
            if match is None:
                raise SyntaxError(f"Error on token {token}")

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

            if token.kind.startswith("/"):
                stack[-1].clauses[-1].append(FunctionCallDirective.from_embedded(token))
                continue

            if token.kind == ";":
                if stack[-1].kind not in "[<":
                    raise SyntaxError(f"~; is only supported in ~[ and ~< blocks")

                if token.colon:
                    if stack[-1].default_token:
                        raise SyntaxError(f"multiple ~:; separators provided")

                    if stack[-1].kind == "<" and len(stack[-1].clauses) > 1:
                        raise SyntaxError(f"overflow clause is not first ~< clause")

                    stack[-1].default_token = token

                elif stack[-1].kind == "[" and stack[-1].default_token:
                    raise SyntaxError(f"default clause is not final ~[ clause")

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
                    raise SyntaxError(f"Unmatched ~{token.kind}")

                if pairings[closed_block.kind] != token.kind:
                    raise SyntaxError(f"Unbalanced ~{closed_block.kind} (was closed with ~{token.kind})")

                closed_block.closing_token = token
                stack[-1].clauses[-1].append(closed_block)
                continue

        stack[-1].clauses[-1].append(token)

    if len(stack) > 1:
        raise SyntaxError(f"Unclosed ~{stack[1].kind}")

    return stack[0]
