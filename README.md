# 5MAT

Like most sensible languages, Common Lisp has a handy-dandy function for producing and combining string representations of various objects, so creatively named `format`. Its facilities are familiar to users of C's `printf` or Python's f-strings: a string containing some magic character sequences has those sequences replaced with representations of the arguments passed in, the rest of the string being the glue connecting said arguments together for our needy human eyes.

```lisp
(format nil "This number has padding: ~3,'0d. This one's in hex: ~x." 9 255)
>>> "This number has padding: 009. This one's in hex: FF."
```

Such formatters are usually quite terse and quite powerful[^1], and `format` is the king of them all (to me, anyway). Its [Wikipedia article](https://en.wikipedia.org/wiki/Format_(Common_Lisp)?useskin=vector) is a fantastic synopsis, but I think the following chapter title from the [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm) should be enough to convince you of the tomfuckery we are about to employ:

> 22.3.7 FORMAT Control-Flow Operations

Indeed, `format` is armed with the power to loop, backtrack, and dance wildly across its arguments. Further empowered with conditionals, break statements, and even external function calls, one might, nay *should*, naturally ask if `format` is Turing-complete. The answer is **yes... with a few modifications**. These modifications grant us 5MAT, a Turing-complete esolang whose source is a single Common Lisp FORMAT string.

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

The above applies to integers *and* characters, granting us `=` and `/=` for any one character:
```lisp
; Skip iff the current character is 'C'
~v,'C^

; Continue iff the current character is 'C'
~v,'B,'B^~:*~'D,'D,v^
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
~@{~v,'^~}

; Print characters until <char>
~@{~v,'<char>^~:*~a~}

; Print characters in reverse
~@{~a~2:*~}

; Check how many characters remain
~#[ZERO~;ONE~;...~]

; Exit if no characters remain
~^

; Exit uncondtionally
~0^
```

### Termination

Since 5MAT programs run forever, only a runtime error can end execution. Here are a few easy options:
```lisp
; Next argument is not a list
~{~}

; Next argument is not an integer
~[~]
~v*

; There are fewer than N args
~N@*

; There are fewer than N args remaining
~N*

; Fuck preconditions
~@{~*~}~*
```

### Arithmetic

Unary arithmetic is trivial in 5MAT; decimal, not so much. Check out `samples/counter.5mat` for a replete example; its general approach is to store its value backwards, then increment as follows:

1. Turn all trailing 9's to 0's
2. Increment the first digit that isn't a 9 by checking for each digit individually
3. Pad with a leading zero if there are no digits left
4. Copy the digits in reverse to output

[^1]: https://github.com/carlini/printf-tac-toe
