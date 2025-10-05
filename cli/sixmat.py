import argparse
import os
from typing import TYPE_CHECKING

from lib.sixmat import optimizer, assembler

if TYPE_CHECKING:
    from argparse import _SubParsersAction


def register(subparsers: "_SubParsersAction[argparse.ArgumentParser]") -> None:
    parser = subparsers.add_parser(name="6MAT", aliases=["6", "sixmat"],
                                   description="Assembles 6MAT programs into 5MAT with optional formatting and optimizations")
    parser.set_defaults(func=run)

    parser.add_argument("filename",
                        help="the file to assemble or optimize; output will be <filename>.5mat")
    parser.add_argument("-m", "--macros", default=None,
                        help="custom macro file; defaults to None")
    parser.add_argument("-u", "--unsafe", action="store_true",
                        help="apply optimizations that assume the tape pointer is never moved off the tape")
    parser.add_argument("-g", "--golf", action="store_true",
                        help="apply aggressive golf optimizations; discards all formatting")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="show optimization steps")
    parser.add_argument("--no-OPT-NAME", action="store_true",
                        help="disable optimization OPT-NAME")
    parser.add_argument("--preserve-comments", action="store_true",
                        help="preserve comments")
    parser.add_argument("--preserve-groups", action="store_true",
                        help="preserve unscoped grouping blocks")
    parser.add_argument("--preserve-indents", action="store_true",
                        help="preserve newlines and indentation")
    parser.add_argument("--debug-trace", action="store_true",
                        help="insert trace information; massively (2-3x) increases code size, disables optimizations")


def run(args: argparse.Namespace, disables: list[str]) -> None:
    path, ext = os.path.splitext(args.filename)

    instructions = {}
    if args.macros:
        instructions = assembler.load_grammar(args.macros)

    with open(args.filename, "r", encoding="utf8") as infile:
        match ext:
            case ".6mat":
                print(f"Assembling {path}.6mat...")
                code = assembler.assemble(infile.read(), instructions, **vars(args))

            case ".5mat":
                code = infile.read()

            case _:
                raise ValueError(f"unrecognized file extension: '{args.filename}'")

        if not args.debug_trace:
            print(f"Optimizing {path}.5mat...")
            opts = optimizer.BASIC_OPTS

            if args.unsafe:
                opts |= optimizer.UNSAFE_OPTS

            if args.golf:
                opts |= optimizer.GOLF_OPTS

            code, saved = optimizer.optimize(code, opts, [name.removeprefix("--no-") for name in disables],
                                             **vars(args))
            print(f"Saved {saved} characters!")

        else:
            print("Optimizations disabled due to presence of --debug-trace flag.")

        with open(path + ".5mat", "w+", encoding="utf8") as outfile:
            outfile.write(code)

        print(f"Output written to {path}.5mat!")


__all__ = ["register"]