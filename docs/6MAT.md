# 6MAT

6MAT is an esoteric assembly-like programming language which compiles to 5MAT, itself an esolang built off of Lisp FORMAT strings.

6MAT programs are composed of **instructions** which consume zero or more **arguments** (duh). Instructions operate on the **tape**, a sequence of **characters** which serves as the program's memory. The **tape pointer** indicates where on the tape the next character will be read.

On a single **lifetime** of execution, the tape is processed by the program, **printing** characters that become the new contents of the tape on the next cycle. Those characters on the tape which appear after the last **form feed** character (`\f`) are **output** to STDOUT once the current lifetime is completed; if none exist, the entire tape is output

This loop continues indefinitely, terminating only by a **crash**. This execution model, along with FORMAT's surprising control flow capabilities, enable 5MAT and 6MAT to be Turing-complete (see `bct.5mat` and `bct.6mat`).

### Token Types

- `-456`: An integer literal (in decimal)
- `'C'`: A character literal
- `"XYZ"`: A string literal
- `NIL`: The empty list object
- `FOO`: An instruction
- `; Blah`: A comment

Escape sequences `\a`, `\b`, `\t`, `\n`, `\v`, `\f`, `\r`, `\"` (inside strings), and `\xXX`. are recognized. `↡`, the Unicode symbol for a form feed, is also recognized as a replacement for `\f`.

### Instruction Types

Some instructions compile (nearly) directly to only a few FORMAT directives, representing the primitives of 5MAT; others, however, assemble into long sequences or nestings of directives. These are called **macros**, and are denoted with an ending `!`. While convenient, macros present greater abstraction and reduced efficiency over other instructions. If optimizing for 5MAT size, more primitive instructions may yield better results.

All instructions are expanded into FORMAT directives by matching signatures defined in a grammar file (see `instructions.g`), which serves as a viable at-a-glance reference of all available instructions. To enable certain recursive expansions and other macros, additional internal instructions beginning with a `#` are defined; these cannot be used in user code.

### Argument Types

The following variables are recognized.
- `$R`: Read the number of characters remaining on the tape
- `$V`: Read the next tape character
- `$n`: Read the next `n` tape characters as a string
- `?V`: Peek the next tape character
- `?n`: Peek the next `n` tape characters as a string

A `?V` or `?n` argument can only be passed at the end of an argument list. Other contexts where peeking is illegal are noted in their respective sections; do note, though, that peeking is equivalent to reading then rewinding the tape pointer by `n` characters, so the same effect may be achievable directly.

Instruction arguments are denoted in this document and the instruction grammar using the following placeholders. Separate arguments using `,`.
- `%N`: A signed decimal integer literal, or `$R`
- `+N`: A positive decimal integer literal, or `$R`
- `-N`: A negative decimal integer literal
- `!V`: A character literal, `$V`, or `?V`
- `$V`: A character literal, or `$V`
- `'C`: A character literal
- `!n`: A string literal, or `$n`, or `?n`
- `$n`: A string literal, or `$n`
- `"X`: A string literal
- `_I`: Any previously listed type (with potential restrictions)

Default values for arguments are denoted like `_I = default`. Passing `NIL` as an argument is equivalent to passing the default value. If an instruction has adjacent defaulted arguments of the same type, they are bound from left to right; e.g. if `FOO %N = 1, %M = 2` is called with `FOO 3`, then `3` is bound to `%N`.

Multiple `!n` arguments are never permitted in a single instruction call. If an instruction accepts `!n` and `"X` in a call, `n` must be the length of the string literal.

## Control Flow

### Groups and Blocks

**Groups** are collections of instructions, usually indented for clarity. Some groups create **blocks**, whose execution can be terminated early using **break** instructions.

#### `{ ... }`
Create a block and execute it. Breaking instructions terminate execution in the *current* block. Block argument placeholders are denoted by `{ ... }`.

#### `[ ... ]`
Create an instruction group and execute it. Breaking instructions terminate execution in the *containing* block. Group argument placeholders are denoted by `[ ... ]`, with `[{ ... }]` meaning either a group or a block is permitted.

#### `BUFFER { ... }`
Create a buffer block and execute it. Prints which occur inside buffer blocks do *not* succeed if the block is broken out of at any point.

### Breaking

Breaking is the central tool for almost all control flow in 5MAT. Though 6MAT provides additional abstractions, it's mostly breaks all the way down, which dictates the legality of certain argument combinations. In particular, peeking is *not* permitted in any break instruction.

#### `BREAK`
Break out of the current scope. The tape pointer is unaltered.

#### `BRFF`
Read a character from the tape and break if it is `\f`.

#### `BRNR +N`
Break out of the current block if the tape pointer is `N` characters from the end of the tape (i.e. `$R = N`).

#### `BRNZ`
Break out of the current scope if the tape pointer is not at the end of the tape (i.e. `$R /= 0`).

#### `BRZR`
Break out of the current scope if the tape pointer is at the end of the tape (i.e. `$R = 0`). Equivalent to (but more efficient than) [`BRNR 0`](#brnr-n).

#### `BRxx? _I, _J`
Break out of the current scope if condition `xx` holds for `_I` and `_J`. The following binary comparisons are supported.

| Condition | Comparison | Supported Types |
|-----------|------------|-----------------|
| `EQ`      | `=`        | `%N`, `$V`      |
| `GE`      | `>=`       | `%N`, `$V`      |
| `GT`      | `>`        | `%N`, `$V`      |
| `LE`      | `<=`       | `%N`, `$V`      |
| `LT`      | `<`        | `%N`, `$V`      |
| `NE!`     | `/=`       | `%N`, `$n`      |

Mismatched argument types are not permitted. Furthermore, the following calls are disallowed.
- `BRGE $V, $V`
- `BRGT $V, $V`
- `BRLT $V, $V`
- `BRNE! $V, $V`

#### `BRINC _I, _J, _K`
Break out of the current scope if `_I <= _J <= _K` holds for numbers or characters. Mismatched argument types are not permitted.

### Outer Blocks

The outermost scope may consist only of the following blocks.

#### `DO { ... }`
If the tape is non-empty, bind `$V` and `$R` and execute the block.

#### `INIT { ... }`
If the tape is empty, set its contents by executing the block.

#### `DEFINE name! { ... }`
Define a user macro. The contents of `{ ... }` are copied exactly in place of every use of `name!`, which should, for readability, contain lowercase letters and end in an `!`. Do note, though, that macro (and instruction) names are case-insensitive.

To define a macro which accepts arguments, you may add it for personal use to a custom macro file. The format is detailed in the base instruction grammar, `instructions.g`.

### Inner Blocks

All outer blocks may include the following blocks, with arbitrary nesting.

#### `IFFF! !V { ... }`
Read or peek a character from the tape and execute the block if it is `\f`.

#### `IFNR! +N [{ ... }]`
Execute the group or block if the tape pointer is `N` characters from the end of the tape (i.e. `$R = N`).

#### `IFNZ [{ ... }]`
Execute the group or block if the tape pointer is not at the end of the tape (i.e. `$R /= 0`).

#### `IFZR [{ ... }]`
Execute the group or block if the tape pointer is at the end of the tape (i.e. `$R = 0`). Equivalent to (but more efficient than) [`IFNR! 0 [{ ... }]`](#ifnr-n---).

#### `IFyy! _I, _J { ... }`
Execute the block if condition `yy` holds for `_I` and `_J`; that is, invoke [`BRxx _I, _J`](#brxx-_i-_j) at the top of the block, where `xx` is the negation of `yy`.

| Condition | Break   | Supported Types  |
|-----------|---------|------------------|
| `EQ!`     | `BRNE!` | `%N`, `!V`, `$n` |
| `GE!`     | `BRLT`  | `%N`, `!V`       |
| `GT!`     | `BRLE`  | `%N`, `!V`       |
| `LE!`     | `BRGT`  | `%N`, `!V`       |
| `LT!`     | `BRGE`  | `%N`, `!V`       |
| `NE!`     | `BREQ`  | `%N`, `!V`       |

As such, the following calls are disallowed.
- `IFEQ! $V, !V { ... }`
- `IFGE! $V, !V { ... }`
- `IFLE! $V, !V { ... }`
- `IFLT! $V, !V { ... }`

#### `LOOP { ... }`
Repeatedly execute the block, terminating if the tape is exhausted at the top of the block. Note that the tape pointer does *not* move by default at any point, and thus failing to cause net movement of the tape pointer during execution will result in a crash.

#### `LOOP +N { ... }`
Execute the block at most `N` times, terminating if the tape is exhausted at the top of the block.

### Case Blocks

#### `CASER! [{ ... }]`
Conditionally execute groups based on the value of `$R`, which may be tested against any contiguous span of integers starting from `0`. Additionally, a terminal `DEFAULT` clause may be provided, which executes if `$R` is not equal to any listed value.
```
CASER! [{
    0 [{ ... }]
    1 [{ ... }]
    2 [{ ... }]
    ...
    n [{ ... }]
    DEFAULT [{ ... }]
}]
```

#### `CASES! { ... }`
Conditionally execute blocks based on the value of `?n`, where `n` characters are read to test against a string of length `n`. If a test would read past the end of the tape, crash.
```
CASES! {
    "FOO" { ... }
    "BAR" { ... }
    "BAZ" { ... }
    ...
    "QUX" { ... }
}
```
All clauses which match `?n` are executed. For example, if `?3 = "FOO"`, the following prints `ABC`.
```
CASES! {
    "F"   { PRINC 'A' }
    "FO"  { PRINC 'B' }
    "FOO" { PRINC 'C' }
}
```

Each clause must advance the tape pointer by the same amount in all circumstances to prevent unexpected additional fallthrough. 

#### `CASEV! { ... }`
Conditionally execute blocks based on the value of `$V`, which may be tested against any set of characters.
```
CASEV! {
    'A' { ... }
    'B' { ... }
    'D' { ... }
    ...
    'Z' { ... }
}
```
Each clause must advance the tape pointer by the same amount in all circumstances to prevent unexpected fallthrough.


## Tape Movement

### Relative Tape Movement

Whenever a character is read from the tape, the tape pointer automatically advances by one. The tape pointer cannot advance past the end of the tape, nor rewind past the front. Care should be taken with repeated movement which may "get stuck" at the start or end of the tape.

#### `BACK +N = 1`
Move the tape pointer backward by `N` characters.

#### `BACKC! 'C, +N = 1`
Move the tape pointer backward past the `N`th preceding appearance of `C`. Crashes if there are less than `N` preceding appearances of `C`.

#### `BACKF! +N = 1`
Move the tape pointer backward past the `N`th preceding appearance of `\f`. Equivalent to [`BACKC! '\f', +N`](#backc-c-n--1).

#### `SKIP +N = 1`
Move the tape pointer forward by `N` characters.

#### `SKIPC! 'C, +N = 1`
Move the tape pointer forward past the `N`th succeeding appearance of `C`.

#### `SKIPF! +N = 1`
Move the tape pointer forward past the `N`th succeeding appearance of `\f`. Equivalent to [`SKIPC! '\f', +N`](#skipc-c-n--1).

### Absolute Tape Movement

Absolute position of the tape pointer is counted from the front of the tape, position `0`. Negative positions count from the end, so that `-1` is the last character on the tape.

#### `GOTO %N = 1`
Move the tape pointer to position `N`. Negative values count from the end of the tape.

#### `GOTOC! 'C, %N = 1`
Move the tape pointer past the `N`th appearance of `C`. Negative values count from the end of the tape.

#### `GOTOF! %N = 1`
Move the tape pointer past the `N`th appearance of `\f`. Negative values count from the end of the tape.


## Crashing

All 5MAT (and thus 6MAT) programs cycle indefinitely, using the previous tape contents to produce the next. Thus, ending a program can only be accomplished via an illegal formatting operation which crashes the entire Common Lisp REPL.

**Crash** instructions insert a format operation guaranteed to error given the structure of a 5MAT program. Crashing will also occur if a character is read past the end of the tape, so the lack of a crash instruction does not guarantee the program will continue indefinitely.

#### `CRASH`
Irrecoverably terminate execution immediately. All output produced during the current lifetime is discarded.

#### `CRFF! !V`
Read or peek a character from the tape and crash if it is `\f`.

#### `CRNR! +N`
Crash if the tape pointer is `N` characters from the end of the tape (i.e. `$R = N`).

#### `CRNZ`
Crash if the tape pointer is not at the end of the tape (i.e. `$R /= 0`).

#### `CRZR`
Crash if the tape pointer is at the end of the tape (i.e. `$R = 0`). Equivalent to [`CRNR! 0`](#crnr-n).

#### `CRyy! _I, _J`
Crash if condition `yy` holds for `_I` and `_J`. Equivalent to [`{ BRxx _I, _J | CRASH }`](#brxx-_i-_j), where `xx` is the negation of `yy`.

| Condition | Break   | Supported Types |
|-----------|---------|-----------------|
| `EQ!`     | `BRNE!` | `%N`, `!n`      |
| `GE!`     | `BRLT`  | `%N`, `!V`      |
| `GT!`     | `BRLE`  | `%N`, `!V`      |
| `LE!`     | `BRGT`  | `%N`, `!V`      |
| `LT!`     | `BRGE`  | `%N`, `!V`      |
| `NE!`     | `BREQ`  | `%N`, `!V`      |

As such, the following calls are disallowed.
- `CREQ! $V, !V`
- `CRGE! $V, !V`
- `CRLE! $V, !V`
- `CRLT! $V, !V`


## Printing

**Printing** a character results in its appearance in the next contents of the tape; it is only **output** to STDOUT at the end of the current lifetime if it appears after the last `\f` character. Printed characters cannot be overwritten or undone within a lifetime.

#### `PRINA _I`
Print `_I` as it appears, which may be a character or string variable.

#### `PRINC !V`
Print `V` as it appears.

#### `PRINN !V`
Print the name of `V`. This may be identical to `V` (`A`) or it may not (`Space`).

#### `PRFF +N = 1`
Print `N` form feeds (`\f`).

### Formatted Printing

#### `PRINL %N = 0, %M = 1, %L = 0, $W = Space, !V`
Read or peek a character from the tape and print it with `k*M + L` copies of `W` padding on the left to reach a total width of at least `N`. Character literals are not permitted in place of `!V`.

#### `PRINR %N = 0, %M = 1, %L = 0, $W = Space, !V`
Read or peek a character from the tape and print it with `k*M + L` copies of `W` padding on the right to reach a total width of at least `N`. Character literals are not permitted in place of `!V`.

### Conditional Printing

#### `PRNRz! _K`
Print `_K` using `PRINz _K` if the tape pointer is `N` characters from the end of the tape (i.e. `$R = N`).

#### `PRNZz _K`
Print `_K` using `PRINz _K` if the tape pointer is not at the end of the tape (i.e. `$R /= 0`).

#### `PRZRz _K`
Print `_K` using `PRINz _K` if the tape pointer is at the end of the tape (i.e. `$R = 0`). Equivalent to (but more efficient than) [`PRNRz 0 _K`](#prnrz-_k).

### Copying

**Copying** instructions print some span of the tape without modification.

#### `COPY +N = 1`
Copy exactly `N` characters. Equivalent to (but more readable than) [`PRINA $N`](#prina-_i).

#### `COPYC! 'C, +N = 1`
Copy characters up to but not including the `N`th succeeding appearance of `C`.

#### `COPYF! +N = 1`
Copy characters up to but not including the `N`th succeeding appearance of `\f`. Equivalent to [`COPYC! '\f', +N`](#copyc-c-n--1).

#### `COPYR! 'C, +N = 1`
Copy characters in reverse up to but not including the `N`th previous appearance of `C`. `$R` is not permitted.

### Special Output

Certain characters may be printed with multiplicity in a compact way.

#### `FRESH +N = 1`
Print a newline (`\n`) if the previous printed character is not a newline, then print `N-1` newlines.

#### `TERP +N = 1`
Print `N` newlines (`\n`).

#### `TILDE +N = 1`
Print `N` tildes (`~`).

## Reading

**Reading** contents from STDIN results in those characters being printed. A single read operation always consumes exactly one character from the tape, and will crash if no input remains unless noted otherwise.

#### `READ`
Read an entire Lisp object from STDIN and print its string representation, crashing if the input is incomplete. This instruction calls the Lisp function [`read`](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm#read).

#### `READC`
Read and print a single character from STDIN, crashing if no input remains. This instruction calls the Lisp function [`read-char`](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_cha.htm#read-char).

#### `READH`
Read and print a single character from STDIN, printing nothing if no input remains. This instruction calls the Lisp function [`read-char-no-hang`](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_c_1.htm#read-char-no-hang).

#### `READL`
Read and print a single line from STDIN, including its line separator, crashing if no input remains. This instruction calls the Lisp function [`read-line`](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_lin.htm#read-line).

#### `READW`
Read an entire Lisp object from STDIN and print its string representation with whitespace preserved, crashing if the input is incomplete. This instruction calls the Lisp function [`read-preserving-whitespace`](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm#read-preserving-whitespace).


## Special Forms

FORMAT directives support additional miscellaneous operations not succinctly abstracted by previous instructions. While potentially complex in description, it should be noted that none of the following instructions are macros.

#### `JUSTz %N = 0, %M = 1, %L = 0, !V = Space { ... }`
Justify the subsequent blocks by padding each block's content with at least `L` copies of `V` (with left-to-right precedence if padding must be unevenly allotted), arranged within a field of width `N+k*M` for the smallest possible choice of `k`.

The value of `z` dictates padding options at the edges of the field.

| Instruction | Effect                                           |
|-------------|--------------------------------------------------|
| `JUST`      | No padding may be added at the edges             |
| `JUSTL`     | Padding may be added after the last clause       |
| `JUSTR`     | Padding may be added before the first clause     |
| `JUSTC`     | Padding may be added at either edge of the field |

```
JUST %N, %M, %L, !V {
    [{ ... }]
    [{ ... }]
    ...
}
```

An `OVER` instruction may be included as the first clause (see below). Each clause acts a buffer block (see [`BUFFER`](#buffer---)).

#### `OVER %P = 0, %O = 72 { ... }`
Create a buffer by executing the block. If the output of the containing [`JUST`](#justz-n--0-m--1-l--0-v--space---) instruction exceeds `O-P` characters, this buffer is printed *before* the justified content. Otherwise, it is discarded.

Since this clause is always executed, breaking out of it cancels the entirety of the containing `JUST` instruction. Tape pointer movement is not undone.

```
JUST %N, %M, %L, !V {
    OVER %P, %O [{ ... }]
    [{ ... }]
    [{ ... }]
    ...
}
```

See the [HyperSpec Section on Justification](https://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm) for a full specification of these instructions' target directives' behavior.

#### `TABA +N = 1, +M = 1`
Print spaces (` `) until at least `N+k*M` characters have been printed since the last newline was printed this lifetime for the smallest possible choice of `k`.

#### `TABR +N = 1, +M = 1`
Print `N` spaces (` `), then print spaces until at least `k*M` characters have been printed since the last newline was printed this lifetime for the smallest choice of `k`.

#### `LOWER [ ... ]`
Fold all characters printed within the group to lowercase.

#### `TITLE [ ... ]`
Capitalize all words separated by spaces printed within the group; that is, make the first character of each word uppercase (if possible), and all other characters lowercase.

#### `TITLE 1 [ ... ]`
Capitalize the first word beginning with an alphabetical character printed within the group. Make all other characters lowercase.

#### `UPPER [ ... ]`
Fold all characters printed within the group to uppercase.

#### `|...|`
Insert the contents of `|...|`, which should be some obtuse FORMAT directives, directly into the assembled 5MAT.

*This syntax should be used sparingly, if at all. No checks are performed on the syntactic or semantic validity of injected code.*

## Appendix
In the spirit of assembly, have an opcode table for 6MAT:

| __    | 0 | 1       | 2 | 3      | 4      | 5      | 6       | 7 | 8       | 9       | A      | B      | C        | D      | E        | F       |
|-------|---|---------|---|--------|--------|--------|---------|---|---------|---------|--------|--------|----------|--------|----------|---------|
| **2** |   |         |   | `$R`   |        | `TERP` | `FRESH` |   | `LOWER` | `LOWER` | `SKIP` |        |          |        |          |         |
| **3** |   |         |   |        |        |        |         |   |         |         |        |        | `BUFFER` |        | `BUFFER` | `CRASH` |
| **4** |   | `PRINR` |   | `COPY` |        |        |         |   |         |         |        |        |          |        |          |         |
| **5** |   |         |   |        | `TABA` |        | `$V`    |   |         |         |        | `IFZR` |          | `IFZR` | `BRZR`   |         |
| **6** |   | `PRINR` |   | `COPY` |        |        |         |   |         |         |        |        |          |        |          |         |
| **7** |   |         |   |        | `TABA` |        | `$V`    |   |         |         |        | `DO`   | `PRFF`   | `DO`   | `TILDE`  |         |

| :_    | 0 | 1 | 2 | 3       | 4 | 5 | 6    | 7 | 8       | 9       | A      | B         | C       | D      | E       | F |
|-------|---|---|---|---------|---|---|------|---|---------|---------|--------|-----------|---------|--------|---------|---|
| **2** |   |   |   | `$R`    |   |   |      |   | `TITLE` | `TITLE` | `BACK` |           |         |        |         |   |
| **3** |   |   |   |         |   |   |      |   |         |         |        | `DEFAULT` | `JUSTR` |        | `JUSTR` |   |
| **4** |   |   |   | `PRINN` |   |   |      |   |         |         |        |           |         |        |         |   |
| **5** |   |   |   |         |   |   | `$V` |   |         |         |        | `INIT`    |         | `INIT` |         |   |
| **6** |   |   |   | `PRINN` |   |   |      |   |         |         |        |           |         |        |         |   |
| **7** |   |   |   |         |   |   | `$V` |   |         |         |        |           |         |        |         |   |

| _@    | 0 | 1       | 2 | 3    | 4      | 5 | 6    | 7 | 8         | 9         | A      | B      | C       | D      | E       | F |
|-------|---|---------|---|------|--------|---|------|---|-----------|-----------|--------|--------|---------|--------|---------|---|
| **2** |   |         |   | `$R` |        |   |      |   | `TITLE 1` | `TITLE 1` | `GOTO` |        |         |        |         |   |
| **3** |   |         |   |      |        |   |      |   |           |           |        |        | `JUSTL` |        | `JUSTL` |   |
| **4** |   | `PRINL` |   |      |        |   |      |   |           |           |        |        |         |        |         |   |
| **5** |   |         |   |      | `TABR` |   | `$V` |   |           |           |        |        |         |        |         |   |
| **6** |   | `PRINL` |   |      |        |   |      |   |           |           |        |        |         |        |         |   |
| **7** |   |         |   |      | `TABR` |   | `$V` |   |           |           |        | `LOOP` |         | `LOOP` |         |   |

| :@    | 0 | 1 | 2 | 3    | 4 | 5 | 6    | 7 | 8       | 9       | A | B | C       | D | E       | F |
|-------|---|---|---|------|---|---|------|---|---------|---------|---|---|---------|---|---------|---|
| **2** |   |   |   | `$R` |   |   |      |   | `UPPER` | `UPPER` |   |   |         |   |         |   |
| **3** |   |   |   |      |   |   |      |   |         |         |   |   | `JUSTC` |   | `JUSTC` |   |
| **4** |   |   |   |      |   |   |      |   |         |         |   |   |         |   |         |   |
| **5** |   |   |   |      |   |   | `$V` |   |         |         |   |   |         |   |         |   |
| **6** |   |   |   |      |   |   |      |   |         |         |   |   |         |   |         |   |
| **7** |   |   |   |      |   |   | `$V` |   |         |         |   |   |         |   |         |   |
