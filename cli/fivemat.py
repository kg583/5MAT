import argparse
import logging
import sys
from typing import TYPE_CHECKING

from lib.fivemat.evaluate import fivemat

if TYPE_CHECKING:
    from argparse import _SubParsersAction



def register(subparsers: "_SubParsersAction[argparse.ArgumentParser]") -> None:
    parser = subparsers.add_parser(name="5MATG", aliases=["5", "fivemat"],
                                   description="Executes 5MAT programs")
    parser.set_defaults(func=run)

    parser.add_argument("filename",
                        type=argparse.FileType('r'),
                        help="The .5mat file to execute")
    parser.add_argument("--log", default="trace.log",
                        help="Log file (used by debug instructions)")

    parser.add_argument("--max-lifetimes", type=int, default=None, help="Max lifetimes to execute (default: None)")

    parser.add_argument(
        "--in",
        type=argparse.FileType('r'),
        help="Take input from file (default: stdin)",
        default=sys.stdin
    )


def run(args: argparse.Namespace, unknown: list[str]) -> None:
    if unknown:
        raise ValueError("Unknown arguments: %s" % unknown)

    input_stream = getattr(args, "in", sys.stdin)

    logging.basicConfig(filename=args.log, level=logging.DEBUG)

    fivemat(args.filename.read(), max_lifetimes=args.max_lifetimes, input_stream=input_stream)
