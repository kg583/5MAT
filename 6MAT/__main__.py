import argparse
import os

from .src.assembler import *
from .src.optimizer import *


parser = argparse.ArgumentParser(
    prog="6MAT Assembler",
    description="Assembles 6MAT programs into 5MAT with optional formatting and optimizations"
)

parser.add_argument("filename", help="the file to assemble; output will be <filename>.5mat")
parser.add_argument("-o", action="store_true", help="apply optimizations to output")
parser.add_argument("--preserve-indents", action="store_true", help="preserve indentation when assembling")
parser.add_argument("--preserve-comments", action="store_true", help="preserve comments when assembling")


if __name__ == "__main__":
    args = parser.parse_args()

    with open(args.filename, "r") as infile:
        tokens = parse(infile.read())
        assembled, _ = assemble(tokens, **vars(args))

        if args.o:
            assembled = optimize(assembled, ALL_OPTIMIZATIONS)

        with open(os.path.splitext(os.path.basename(args.filename))[0] + ".5mat", "w+") as outfile:
            outfile.write(assembled)
