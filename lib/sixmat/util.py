from lib.fivemat.evaluate import decode_escapes


def encode_escapes(string: str) -> str:
    for char in range(32):
        match char:
            case 7:
                repl = "\\a"

            case 8:
                repl = "\\b"

            case 9:
                repl = "\\t"

            case 10:
                repl = "\\n"

            case 11:
                repl = "\\v"

            case 12:
                repl = "\\f"

            case 13:
                repl = "\\r"

            case _:
                repl = f"\\x{char:02x}"

        string = string.replace(chr(char), repl)

    return string


__all__ = ["decode_escapes", "encode_escapes"]
