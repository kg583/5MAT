# 5MAT

5MAT is the brainchild of doing too much code golf in Common Lisp, and enables writing arbitrary programs using just FORMAT strings. Let's see how this is possible, and how you can do it too!

## Making 5MAT

Unlike `printf`, `format` is unable to write arbitrary data (and, in particular, modify its own arguments); the only output we can snatch is the final formatted string. Thus, we'll have 5MAT operate by indefinitely passing the output of a `format` call back into itself, starting from `nil` and terminating by crashing. This keeps the premise of 5MAT fairly "pure"; all the logic still lives in the FORMAT string itself.

To make this setup viable, though, we need one other concession. While there *are* FORMAT directives which operate on strings, they can't operate on strings character-by-character. We'll thus allow a slightly more flexible version of `~{` which iterates over strings as if they were lists[^1].

In all, we find ourselves with a *tape* of characters that is repeatedly passed through a FORMAT string, processing each character to make a new copy of the tape. While a Turing machine doesn't care about "output" in the human sense, leaving meaningful answers to computations to live on the tape in some particular place, it'd be nice for 5MAT to actually print things to STDOUT.

We accomplish this by dividing the tape in two: an output section, which is always printed, and a data section, which is not. Since output could presumably be any printable text, we'll use `\f` (`0x0C`), the [form feed](https://en.m.wikipedia.org/w/index.php?title=Page_break&useskin=vector#Form_feed) control character, to separate the sections. Programs can place the `\f` wherever they like, in particular placing it at the front of the tape to output nothing (perhaps during a lengthy computation step). Only the first `\f` is checked by the driver, allowing the program to use future copies to further conveniently divide their data sections.

For convenience, since a vertical spacing character not seen since the days of yore might not play nice with certain text editors, `↡` (`0x21A1`), the Unicode symbol for form feed, may be used in place of `\f`.

[^1]: This is accomplished in the driver via `coerce`.

## Running 5MAT

Pass your program via STDIN to `src/driver.lisp` to run it.

To debug your program, insert it into `src/debug.lisp` and tweak the debugging parameters to your liking.

## Writing 5MAT

<sup><sub>This guide assumes familiarity with Common Lisp FORMAT strings for, ya know, normal use cases</sub></sup>

Writing non-trivial 5MAT programs is tedious but doable, heavily using the `~{`, `~<`, `~[`, and `~^` directives.

### Initialization

Use `~[INIT-OUTPUT↡INIT-DATA~;~]~:*` at the start of your program to initialize the tape.

### Conditionals

To identify a character on the tape, we employ a scantily-documented version of `~^`:
> If two parameters are given, termination occurs if they are equal. If three parameters are given, termination occurs if the first is less than or equal to the second and the second is less than or equal to the third.

The above applies to integers *and* characters, granting us `/=` and `=` for any one character:
```lisp
; Iff the current character is not 'C'
~<~v,'C^STUFF~>

; Iff the current character is 'C'
~<~v,'B,'B^~:*~'D,'D,v^STUFF~>
```

Checking character ranges where possible can greatly shorten a program.

### Looping

Since the tape is passed as a list, programs must use a containing `~{~}` to loop over it, even if all characters are exhausted in a single iteration.

Here are some common idioms:
```lisp
; Go to the start of the tape
~@*

; Go to the end of the tape
~#*

; Go to the start of the data section
~@*~@{~v,'↡^~}

; Print a form feed
~|

; Print characters until <char>
~@{~v,'<char>^~:*~a~}

; Print characters in reverse
~@{~a~2:*~}

; Check how many characters remain
~#[ZERO~;ONE~;...~]

; Break if no characters remain
~^

; Break unconditionally
~0^
```

### Termination

Since 5MAT programs run forever, only a runtime error can end execution. The easiest option is `~?`, which requires a list parameter, but `~v*`, `~[~]`, `~{~}` (inside the main loop), and `~a` (if no characters remain) also suffice.

### Arithmetic

Unary arithmetic is trivial in 5MAT; decimal, not so much. Check out `samples/counter.5mat` for a replete example; its general approach is to store a counter backwards, then increment as follows:

1. Turn all trailing 9's to 0's
2. Increment the first digit that isn't a 9 by checking for each digit individually
3. Pad with a leading zero if there are no digits left
4. Copy the digits in reverse to output

The above generalizes easily to any base and any system of digits.

### Miscellany

- 5MAT programs can be made "readable" using the `~\n` directive.
- Comments may be inserted via unreachable pathways, e.g. `~1[COMMENT HERE~]`.

## Turing Completeness

A proof of 5MAT's Turing completeness exists via `samples/bct.5mat`, which can interpret an arbitrary [bitwise cyclic tag](https://esolangs.org/wiki/Bitwise_Cyclic_Tag). BCTs can emulate any cyclic tag system, which in turn can emulate Turing machines.

```
~:[~%101~|00111~;~]~1[ The data string is stored in the output section of the tape, with a preceding newline. ~]~
~:*~{~
  ~@{~v,'↡^~}~1[                     Navigate to the program section so we can check the first bit. ~]~
  ~<~v,'1^~1[                        Is the first program bit a 0? ~]~
    ~%~2@*~@{~v,'↡^~:*~a~}~|~1[      Delete the first data bit. ~]~
    ~*~@{~a~}0~>~^~1[                Left cyclic shift the program once. ~]~
  ~:*~<~%~1@*~v,'0^~1[               Is the first data bit a 1? ~]~
    ~:*~@{~v,'↡^~:*~a~}~*~a~|~1[     Copy the second program bit to the right end of the data. ~]~
    ~@{~a~}1~1[                      Copy the program with the first program bit moved to the end. ~]~
    ~@*~@{~v,'↡^~}~*~a~@{~*~}~>~^~1[ Tack on the second program bit. ~]~
  ~%~@{~a~}~}~1[                     If the first data bit was a 0, copy everything that's left. ~]
```
