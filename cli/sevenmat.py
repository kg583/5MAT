import argparse
import sys
from typing import TYPE_CHECKING

from lib.sevenmat.frontend.parse import Program

if TYPE_CHECKING:
    from argparse import _SubParsersAction

def register(subparsers: "_SubParsersAction[argparse.ArgumentParser]") -> argparse.ArgumentParser:
    parser = subparsers.add_parser(name="7MAT", aliases=["7"],
                                   description="Compile 7MAT programs.")
    parser.set_defaults(func=run)

    parser.add_argument("filename",
                        type=argparse.FileType("r", encoding='utf-8'),
                        help=".7mat file for compilation")

    parser.add_argument("-o", "--out", type=argparse.FileType('w'),
                        help="write output to file (default: stdout)",
                        default=sys.stdout)

    # bit of an argparse hack. I want a section just to list these output formats.
    # modes not documented here are for debugging.
    _modes = parser.add_argument_group("modes", description="5mat, 6mat, ast")
    parser.add_argument("-m", "--mode",
                        choices=["5mat", "6mat", "ast", "gs"],
                        default="5mat",
                        metavar="MODE",
                        help="output format (default: 5mat)")

    parser.add_argument("--debug-trace", action="store_true",
                        help="insert trace information into the produced code")

    return parser

def run(args: argparse.Namespace, unknown: list[str]) -> None:
    if unknown:
        raise ValueError("Unknown arguments: %s" % unknown)

    source = args.filename.read()

    output = ""
    match args.mode:
        case "5mat":
            raise NotImplementedError()
        case "6mat":
            raise NotImplementedError()
        case "ast" | "gs":
            program = Program(source)
            program.parse()

            output = program.tree.pretty()
            if args.mode == "gs":
                print(program.global_symbols.types)

    args.out.write(output)
