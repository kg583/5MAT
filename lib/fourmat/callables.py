import sys
from typing import Callable

lisp_functions = {}

def lisp_function(name: str) -> Callable:
    def decorator(func: Callable) -> Callable:
        lisp_functions[name] = func
        return func
    return decorator


@lisp_function("READ-CHAR")
def read_char(stream=sys.stdin, eof_error_p=True, eof_value=None, _recursive_p=False) -> str:
    ch = stream.read(1)

    if ch == "":  # EOF condition
        if eof_error_p:
            raise EOFError("End of file reached while reading character")
        return eof_value

    return ch


@lisp_function("READ-CHAR-NO-HANG")
def read_char_no_hang(*args, **kwargs) -> str:
    return read_char(*args, **kwargs)


# read-byte doesn't take recursive-p so it doesn't apply here


__all__ = {"lisp_functions"}