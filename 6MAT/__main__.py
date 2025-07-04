import argparse
import os

from .src import *


parser = argparse.ArgumentParser(
    prog="6MAT Assembler & Optimizer",
    description="Assembles 6MAT programs into 5MAT with optional formatting and optimizations"
)

parser.add_argument("filename",
                    help="the file to assemble or optimize; output will be <filename>.5mat")
parser.add_argument("-m", "--macros", default=None,
                    help="custom macro file; defaults to none")
parser.add_argument("-u", "--unsafe", action="store_true",
                    help="apply optimizations that assume the tape pointer is never moved off the tape")
parser.add_argument("-g", "--golf", action="store_true",
                    help="apply aggressive golf optimizations; discards all formatting")
parser.add_argument("-v", "--verbose", action="store_true",
                    help="show optimization steps")
parser.add_argument("--preserve-comments", action="store_true",
                    help="preserve comments")
parser.add_argument("--preserve-groups", action="store_true",
                    help="preserve non-semantic groups")
parser.add_argument("--preserve-indents", action="store_true",
                    help="preserve newlines and indentation")


if __name__ == "__main__":
    args, disables = parser.parse_known_args()
    path, ext = os.path.splitext(args.filename)

    instructions = {}
    if args.macros:
        instructions = load_grammar(args.macros)

    with open(args.filename, "r", encoding="utf8") as infile:
        match ext:
            case ".6mat":
                print(f"Assembling {path}.6mat...")
                code = assemble(infile.read(), instructions, **vars(args))

            case ".5mat":
                code = infile.read()

            case _:
                raise ValueError(f"unrecognized file extension: '{args.filename}'")

        print(f"Optimizing {path}.5mat...")
        opts = BASIC_OPTS

        if args.unsafe:
            opts |= UNSAFE_OPTS

        if args.golf:
            opts |= GOLF_OPTS

        code, saved = optimize(code, opts, [name.removeprefix("--no-") for name in disables], **vars(args))
        with open(path + ".5mat", "w+", encoding="utf8") as outfile:
            outfile.write(code)

        print(f"Saved {saved} characters!")
