import argparse
import logging
import sys
from typing import TYPE_CHECKING

from lib.fourmat.evaluate import fourmat

if TYPE_CHECKING:
    from argparse import _SubParsersAction

import ast


def checked_literal_eval(s: str):
    try:
        return ast.literal_eval(s)
    except Exception as e:
        raise argparse.ArgumentTypeError(f"Invalid Python literal: {s}") from e


def checked_list_literal_eval(s: str):
    try:
        result = ast.literal_eval(s)
    except Exception as e:
        raise argparse.ArgumentTypeError(f"Invalid Python list literal: {s}") from e

    if not isinstance(result, list):
        raise argparse.ArgumentTypeError(f"Expected a list, got {type(result).__name__}")

    return result


def register(subparsers: "_SubParsersAction[argparse.ArgumentParser]") -> None:
    parser = subparsers.add_parser(name="4MAT", aliases=["4", "fourmat"],
                                   description="Executes 4MAT programs")
    parser.set_defaults(func=run)

    parser.add_argument("filename",
                        type=argparse.FileType("r", encoding='utf-8'),
                        help="The .4mat file to execute")
    parser.add_argument("--log", default="trace.log",
                        help="Log file (used by debug instructions)")

    args_group = parser.add_mutually_exclusive_group()
    args_group.add_argument("--args", type=checked_literal_eval, nargs="+",
                            help="Arguments for the format string, as Python literals (passed as separate arguments to nmat)")
    args_group.add_argument("--args-list", type=checked_list_literal_eval,
                            help="Arguments for the format string, as a single Python list literal")

    parser.add_argument(
        "--in",
        type=argparse.FileType('r'),
        help="Take input from file (default: stdin)",
        default=sys.stdin
    )

    parser.add_argument("--out", type=argparse.FileType('w'),
                        help="Write output to file (default: stdout)",
                        default=sys.stdout)


def run(args: argparse.Namespace, unknown: list[str]) -> None:
    if unknown:
        raise ValueError("Unknown arguments: %s" % unknown)

    format_args = args.args_list or args.args or []
    input_stream = getattr(args, "in", sys.stdin)

    logging.basicConfig(filename=args.log, level=logging.DEBUG)

    output = fourmat(args.filename.read(), format_args, input_stream=input_stream)

    args.out.write(output)
