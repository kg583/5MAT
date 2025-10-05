import argparse
import sys
from typing import TYPE_CHECKING

from lib.hazmat import fivemat_to_c

if TYPE_CHECKING:
    from argparse import _SubParsersAction


def register(subparsers: "_SubParsersAction[argparse.ArgumentParser]") -> None:
    parser = subparsers.add_parser(name="hazmat",
                                   description="Translate 5MAT programs to C")
    parser.set_defaults(func=run)

    parser.add_argument("filename",
                        type=argparse.FileType('r', encoding='utf-8'),
                        help="The .5mat file to transpile. (default: stdin)",
                        default=sys.stdin,
                        nargs="?")
    parser.add_argument("-o", "--out",
                        type=argparse.FileType('w', encoding='utf-8'),
                        help="Output file location. (default: stdout)",
                        default=sys.stdout)

def run(args: argparse.Namespace, unknown: list[str]) -> None:
    if unknown:
        raise ValueError("Unknown arguments: %s" % unknown)
    indata = args.filename.read()

    if len(indata) == 0:
        raise ValueError("No input provided")

    output = fivemat_to_c(indata)
    args.out.write(output)