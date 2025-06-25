import argparse
import os

from .src.assembler import *
from .src.optimizer import *


parser = argparse.ArgumentParser(
    prog="6MAT Assembler",
    description="Assembles 6MAT programs into 5MAT with optional formatting and optimizations"
)

parser.add_argument("filename", help="the file to assemble; output will be <filename>.5mat")
parser.add_argument("--optimize", action="store_true", help="apply logic optimizations to output")
parser.add_argument("--compress", action="store_true", help="apply logic and size optimizations to output")
parser.add_argument("--preserve-comments", action="store_true", help="preserve comments")
parser.add_argument("--preserve-groups", action="store_true", help="preserve non-semantic groups")
parser.add_argument("--preserve-indents", action="store_true", help="preserve indentation")


if __name__ == "__main__":
    args = parser.parse_args()

    with open(args.filename, "r") as infile:
        assembled = assemble(infile.read(), **vars(args))

        if args.optimize:
            assembled = optimize(assembled, OPTIMIZE)

        if args.compress:
            assembled = optimize(assembled, COMPRESS)

        with open(os.path.splitext(os.path.basename(args.filename))[0] + ".5mat", "w+") as outfile:
            outfile.write(assembled)
