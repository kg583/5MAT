# 4MAT

Common Lisp's `format` function is an odd one, having come into existence following thirteen years of development and specification, and nonetheless being slightly wrong in just about every implementation (even CLISP's, depending on how you read it).

Our implementation adds to the pile, with three specific goals:

1. Fully comply with the Common Lisp HyperSpec (except the pretty printer).
2. Support efficiently running 5MAT programs which are too large or too slow for CLISP.
3. Align as closely as possible with CLISP's interpretation of the specification, as determined by observable behavior.

The first is tedious but straightforward if you know how to read and test; the second is, in part, because CLISP has a limit of ~6 million characters[^1] in a format string, which given the developing exploits in `6MAT/` and `7MAT/` is dangerously small; and the third is to ensure our driver is equivalent to the "canonical" 5MAT driver found in `5MAT/`.

Though 5MAT does not require or support all format directives, we seek to implement the whole thing anyway (except, again, the pretty printer). Below is a rough categorization of the directives by their importance to 5MAT, and thus their importance to us in getting them right.

- Fundamentals: `~a ~c ~% ~& ~| ~~ ~/ ~t ~< ~* ~[ ~{ ~( ~^`
- Canonical (but not strictly necessary): `~? ~!`
- Redundancies: `~r ~d ~b ~o ~x ~s ~w`
- Valid (but *really* niche): `~f ~e ~g ~$ ~p`

[^1]: https://gitlab.com/gnu-clisp/clisp/-/blob/master/src/TODO#L257

If it proves unreasonable, we happily abandon CLISP's ways. Below are the known ways in which we do so (while still adhering to the HyperSpec), which, per the categories above, should have minimal impact on 5MAT programs.

- Symbols are not supported; Python ain't got 'em, and they display like strings anyway.
- Special key sequences are not displayed by `~:@c`, making it equivalent to `~:c`.
- Formatted floats may differ greatly in their insignificant digits compared to CLISP's output.
- "Ordinary free-format" float output, used by `~e` and `~f` when no parameters are specified, may differ from CLISP's output.
