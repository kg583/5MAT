# 4MAT

Common Lisp's `format` function is an odd one, having come into existence following thirteen years of development and specification, and nonetheless being slightly wrong in just about every implementation (even CLISP's, depending on how you read it).

Our implementation adds to the pile, with three specific goals:

1. Fully comply with the Common Lisp HyperSpec (except the pretty printer).
2. Support efficiently running 5MAT programs which are too large or too slow for CLISP.
3. Align as closely as possible with CLISP's interpretation of the specification, as determined by observable behavior.

The first is tedious but straightforward if you know how to read and test; the second is, in part, because CLISP has a limit of ~6 million characters[^1] in a format string, which given the developing exploits in `6MAT/` and `7MAT/` is dangerously small; and the third is to ensure our driver is equivalent to the "canonical" 5MAT driver found in `5MAT/`.

[^1]: https://gitlab.com/gnu-clisp/clisp/-/blob/master/src/TODO#L257

But, if it proves *un*reasonable, we happily abandon CLISP's ways. Below are the known ways in which we do so (while still adhering to the HyperSpec), which should have minimal impact on 5MAT programs.

- Symbols are not supported; Python ain't got 'em, and they display like strings anyway.
- Character names used by `~:c` are derived from official Unicode names, whereas some of CLISP's character names are older than Unicode itself. The only character names guaranteed to match CLISP are those of [standard and semi-standard characters](https://www.lispworks.com/documentation/HyperSpec/Body/f_char_n.htm#char-name).
- Special key sequences are not displayed by `~:@c`, making it equivalent to `~:c`.
