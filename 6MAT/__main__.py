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
parser.add_argument("--preserve-comments", action="store_true",
                    help="preserve comments")
parser.add_argument("--preserve-groups", action="store_true",
                    help="preserve non-semantic groups")
parser.add_argument("--preserve-indents", action="store_true",
                    help="preserve newlines and indentation")


if __name__ == "__main__":
    args = parser.parse_args()

    with open(args.filename, "r") as infile:
        assembled = assemble(infile.read(), **vars(args))

        if args.O1:
            assembled = optimize(assembled, O1)

        elif args.O2:
            assembled = optimize(assembled, O2)

        with open(os.path.splitext(args.filename)[0] + ".5mat", "w+") as outfile:
            outfile.write(assembled)
