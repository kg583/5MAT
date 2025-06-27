import argparse
import os

from .src.assembler import *
from .src.optimizer import *


parser = argparse.ArgumentParser(
    prog="6MAT Assembler",
    description="Assembles 6MAT programs into 5MAT with optional formatting and optimizations"
)

parser.add_argument("filename",
                    help="the file to assemble; output will be <filename>.5mat")
parser.add_argument("-O1", "-O", action="store_true",
                    help="apply optimizations to output, reducing code size and improving execution speed")
parser.add_argument("-O2", action="store_true",
                    help="O1 + optimizations that assume the tape pointer is never moved off the tape")
parser.add_argument("-O3", action="store_true",
                    help="O2 + aggressive size optimizations; disregards all formatting flags")
parser.add_argument("--preserve-comments", action="store_true",
                    help="preserve comments")
parser.add_argument("--preserve-groups", action="store_true",
                    help="preserve non-semantic groups")
parser.add_argument("--preserve-indents", action="store_true",
                    help="preserve newlines and indentation")


if __name__ == "__main__":
    args = parser.parse_args()
    path, ext = os.path.splitext(args.filename)

    with open(args.filename, "r", encoding="utf8") as infile:
        match ext:
            case ".6mat":
                code = assemble(infile.read(), **vars(args))

            case ".5mat":
                code = infile.read()

            case _:
                raise ValueError(f"unrecognized file extension: '{args.filename}'")

        if args.O1:
            code = optimize(code, O1)

        elif args.O2:
            code = optimize(code, O2)

        elif args.O3:
            code = optimize(code, O3)

        with open(path + ".5mat", "w+", encoding="utf8") as outfile:
            outfile.write(code)
