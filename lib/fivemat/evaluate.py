import logging
import sys

from lib.fourmat.evaluate import fourmat
from lib.fivemat.util import *

logger = logging.getLogger(__name__)


def fivemat(program: str, *, max_lifetimes: int = None, input_stream=sys.stdin, output_stream=sys.stdout):
    parsed = parse(decode_escapes(program))
    tape = []

    try:
        iterations = 0
        while max_lifetimes is None or iterations < max_lifetimes:
            tape = fourmat(parsed, [list(tape)], input_stream=input_stream)
            print(end=tape[tape.rfind("\f") + 1:], file=output_stream)
            iterations += 1

    except Exception as e:
        logger.error(e, exc_info=True)
        pass


__all__ = ["fivemat"]
