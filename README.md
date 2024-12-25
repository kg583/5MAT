# 5MAT

Like most sensible languages, Common Lisp has a handy-dandy function for producing and combining string representations of various objects, so creatively named `format`. Its facilities are familiar to users of C's `printf` or Python's f-strings: a string containing some magic character sequences has those sequences replaced with representations of the arguments passed in, the rest of the string being the glue connecting said arguments together for our needy human eyes.

```lisp
(format nil "This number has padding: ~3,'0d. This one's in hex: ~x." 9 255)
>>> "This number has padding: 009. This one's in hex: FF."
```

Such formatters are usually quite terse and quite powerful[^1], and `format` is the king of them all (to me, anyway). Its [Wikipedia article](https://en.wikipedia.org/wiki/Format_(Common_Lisp)?useskin=vector) is a fantastic synopsis, but I think the following chapter title from the [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm) should be enough to convince you of the tomfuckery we are about to employ:

> 22.3.7 FORMAT Control-Flow Operations

Indeed, `format` is armed with the power to loop, backtrack, and dance wildly across its arguments. Further empowered with conditionals, break statements, and even external function calls, one might, nay *should*, naturally ask if `format` is Turing-complete. The answer is **yes... with a few modifications**. These modifications grant us 5MAT, a Turing-complete esolang whose source is a single Common Lisp FORMAT string.

[^1]: https://github.com/carlini/printf-tac-toe

## Making 5MAT

Unlike `printf`, `format` is unable to write arbitrary data (and, in particular, modify its own arguments); the only output we can snatch is the final formatted string. Thus, we'll have 5MAT operate by indefinitely passing the output of a `format` call back into itself, starting from `nil` and terminating by crashing. This keeps the premise of 5MAT fairly "pure"; all the logic still lives in the FORMAT string itself.

To make this setup viable, though, we need one other concession. While there *are* FORMAT directives which operate on strings, they can't operate on strings character-by-character. We'll thus allow a slightly more flexible version of `~{` which iterates over strings as if they were lists[^2].

In all, we find ourselves with a *tape* of characters that is repeatedly passed through a FORMAT string, processing each character to make a new copy of the tape. While a Turing machine doesn't care about "output" in the human sense, leaving meaningful answers to computations to live on the tape in some particular place, it'd be nice for 5MAT to actually print things to STDOUT.

We accomplish this by dividing the tape in two: an output section, which is always printed, and a data section, which is not. Since output could presumably be any printable text, we'll use `` (`0x17`), the [End of Transmission Block](https://en.m.wikipedia.org/wiki/End-of-Transmission-Block_character?useskin=vector) (ETB) control character, to separate the sections. Programs can place the ETB wherever they like, in particular placing it at the front of the tape to output nothing (perhaps during a lengthy computation step). Only the first ETB is checked by the driver, allowing the program to use future copies to further conveniently divide their data sections.

[^2]: This is accomplished in the driver via `coerce`.

## Running 5MAT

Pass your program via STDIN to `src/driver.lisp` to run it. The driver currently cannot accept programs containing newlines.

To debug your program, insert it into `src/debug.lisp` and tweak the debugging paramaters to your liking.

## Writing 5MAT

<sup><sub>This guide assumes familiarity with Common Lisp FORMAT strings for, ya know, normal use cases</sub></sup>

Writing non-trivial 5MAT programs is tedious but doable, heavily using the `~{`, `~<`, `~[`, and `~^` directives.

### Initialization

Use `~[INIT-OUTPUTINIT-DATA~;~]~:*` at the start of your program to initialize the tape.

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
~@{~*~}

; Go to the start of the data section
~@*~@{~v,'^~}

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

Since 5MAT programs run forever, only a runtime error can end execution. The easiest option is `~v*`, which requires an integer parameter, but `~[~]`, `~{~}` (inside the main loop), and `~*` (if no characters remain) also suffice.

### Arithmetic

Unary arithmetic is trivial in 5MAT; decimal, not so much. Check out `samples/counter.5mat` for a replete example; its general approach is to store a counter backwards, then increment as follows:

1. Turn all trailing 9's to 0's
2. Increment the first digit that isn't a 9 by checking for each digit individually
3. Pad with a leading zero if there are no digits left
4. Copy the digits in reverse to output

The above generalizes easily to any base and any system of digits.

### Miscellany

- 5MAT programs can be made "readable" using the `~\n` directive, though the driver currently cannot parse these from STDIN.
- Comments may be inserted via unreachable pathways, e.g. `~1[COMMENT HERE~]`.

## Turing Completeness

A proof of 5MAT's Turing completeness exists via `samples/bct.5mat`, which can interpret an arbitrary [bitwise cyclic tag](https://esolangs.org/wiki/Bitwise_Cyclic_Tag). BCTs can emulate any cyclic tag system, which in turn can emulate Turing machines.

```
~:[~%10100111~;~]~1[ The data string is stored in the output section of the tape, with a preceding newline. ~]~
~:*~{~
  ~@{~v,'^~}~1[                     Navigate to the program section so we can check the first bit. ~]~
  ~<~v,'1^~1[                        Is the first program bit a 0? ~]~
    ~%~2@*~@{~v,'^~:*~a~}~1[       Delete the first data bit. ~]~
    ~*~@{~a~}0~>~^~1[                Left cyclic shift the program once. ~]~
  ~:*~<~%~1@*~v,'0^~1[               Is the first data bit a 1? ~]~
    ~:*~@{~v,'^~:*~a~}~*~a~1[      Copy the second program bit to the right end of the data. ~]~
    ~@{~a~}1~1[                      Copy the program with the first program bit moved to the end. ~]~
    ~@*~@{~v,'^~}~*~a~@{~*~}~>~^~1[ Tack on the second program bit. ~]~
  ~%~@{~a~}~}~1[                     If the first data bit was a 0, copy everything that's left. ~]
```
