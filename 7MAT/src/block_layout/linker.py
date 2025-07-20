import re
from typing import Callable

from .block_registry import QualifiedKey
from ..sixmat import SixMat, A


class Linker:
    @staticmethod
    def insert_path(s: SixMat, key: str | Callable, *parameters, reverse=False):
        s.raw_text(f"%{'htaP' if reverse else 'Path'} {QualifiedKey(key, *parameters)}\n")

    @staticmethod
    def resolve_path_directives(unlinked: str, paths: dict[QualifiedKey, str]):
        def match_fn(match: re.Match):
            indent = match.group(1)
            directive = match.group(2)
            args = match.group(3)

            match directive:
                case "Path":
                    key = QualifiedKey.from_fragments(args.split(" "))
                    return indent + f"PRINA\t{A.str(paths[key])}"
                case "htaP":
                    key = QualifiedKey.from_fragments(args.split(" "))
                    return indent + f"PRINA\t{A.str(paths[key][::-1])}"
                case _:
                    return ""

        return re.sub(re.compile(r"^(\s*)%(\w+)(?: (.+))?", re.MULTILINE), match_fn, unlinked)
