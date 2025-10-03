import codecs
import logging
import re
import sys

from lib.fourmat.evaluate import fourmat
from lib.fourmat.parse import parse, tokenize

logger = logging.getLogger(__name__)


def decode_escapes(string: str) -> str:
    def decode_match(match):
        try:
            return codecs.decode(match[0], 'unicode-escape')

        except UnicodeDecodeError:
            return match[0]

    return re.sub(r"\\[abfnrtv]|\\x..", decode_match, string).replace("↡", "\f")


def fivemat(program: str, *, max_lifetimes: int = None, input_stream=sys.stdin):
    parsed = parse(tokenize(decode_escapes(program)))
    tape = []

    try:
        iterations = 0
        while max_lifetimes is None or iterations < max_lifetimes:
            tape = fourmat(parsed, [list(tape)], input_stream=input_stream)
            print(end=tape[tape.rfind("\f") + 1:])
            iterations += 1

    except Exception as e:
        logger.error(e, exc_info=True)
        pass
