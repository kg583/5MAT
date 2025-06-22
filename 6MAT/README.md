# 6MAT

6MAT is an esoteric assembly-like programming language which compiles to 5MAT, itself an esolang built off of Lisp FORMAT strings.

6MAT programs are composed of **instructions** which consume zero or more **arguments** (duh). Instructions operate on the **tape**, a sequence of **characters** which serves as the program's memory. The **tape pointer** indicates where on the tape the next character will be read.

On a single **cycle** of execution, the tape is processed by the program, **printing** characters that become the new contents of the tape on the next cycle. Those characters on the tape which appear before the first **form feed** character (`0x0C`) are **output** to STDOUT once the current cycle is completed.

This loop continues indefinitely, terminating only by a **crash**. This execution model, along with FORMAT's surprising control flow capabilities, enable 5MAT and 6MAT to be Turing-complete.

### Argument Types

The following variables and constants are recognized.
- `$R`: Read the number of characters remaining on the tape
- `$V`: Read the next tape character
- `?V`: Peek the next tape character
- `NIL`: The empty list object
- `\f`: The form feed character
- `\n`: The newline character

A `?V` argument can only be passed at the end of an argument list. Other contexts where peeking is illegal are noted in their respective sections; do note, though, that peeking is equivalent to reading then rewinding the tape pointer by one character, so the same effect may be achievable directly.

Instruction arguments are denoted using the following placeholders.
- `%N`: A signed decimal integer literal, or `$R`
- `+N`: A positive decimal integer literal, or `$R`
- `-N`: A negative decimal integer literal
- `!V`: A character literal, `$V`, or `?V`
- `$V`: A character literal, or `$V`
- `'C`: A character literal
- `""`: A string literal
- `_I`: Any previously listed type

Default values for arguments are denoted like `_I = default`. Passing `NIL` as an argument is equivalent to passing the default value. If an instruction has adjacent defaulted arguments of the same type, they are bound from left to right; e.g. if `FOO %N = 1, %M = 2` is called with `FOO 3`, then `3` is bound to `%N`.


## Control Flow

### Breaking

**Blocks** are groups of instructions whose execution can be terminated early using **break** instructions. Breaking is the central tool for almost all control flow in 5MAT. Though 6MAT provides additional abstractions, it's mostly breaks all the way down, which dictates the legality of certain argument combinations (see below). In particular, peeking is not permitted in any break instruction.

#### `{ ... }`
Create a bare block and execute it. Execution can be terminated early using break instructions.

#### `BREAK`
Break out of the current block. The tape pointer is unaltered.

#### `BRFF`
Read a character from the tape and break if it is `↡`.

#### `BRZR`
Break out of the current block if the tape pointer is at the end of the tape (i.e. `$R = 0`).

#### `BRxx _I, _J`
Break out of the current block if comparison `xx` holds for `_I` and `_J`. The following binary comparisons are supported.

- `EQ`: `=`
- `GE`: `>=`
- `GT`: `>`
- `LE`: `<=`
- `LT`: `<`
- `NE`: `/=`

Mismatched argument types are not permitted. Furthermore, the following calls are disallowed.
- `BRGE $V, $V`
- `BRGT $V, $V`
- `BRLT $V, $V`
- `BRNE $V, $V`

#### `BRINC _I, _J, _K`
Break out of the current block if `_I <= _J <= _K`. Mismatched argument types are not permitted.

### Crashing

All 5MAT (and thus 6MAT) programs cycle indefinitely, using the previous tape contents to produce the next. Thus, ending a program can only be accomplished via an illegal formatting operation which crashes the entire Common Lisp REPL.

**Crash** instructions insert a format operation guaranteed to error given the structure of a 5MAT program. Crashing will also occur if a character is read past the end of the tape, so the lack of a crash instruction does not guarantee the program will continue indefinitely.

#### `CRASH`
Irrecoverably terminate execution immediately. All output produced during the current cycle is discarded.

#### `CRFF`
Read a character from the tape and crash if it is `↡`.

#### `CRZR`
Crash if the tape pointer is at the end of the tape (i.e. `$R = 0`).

#### `CRyy _I, _J`
Crash if condition `yy` holds for `_I` and `_J`.

- `EQ <-> NE`
- `GE <-> LT`
- `GT <-> LE`

As such, the following calls are disallowed.
- `CREQ $V, !V`
- `CRGE $V, !V`
- `CRLE $V, !V`
- `CRLT $V, !V`

### Outer Blocks

The outermost scope may consist only of the following blocks, each at most once.

#### `DO { ... }`
If the tape is non-empty, bind `$V` and `$R` and execute the block.

#### `INIT { ... }`
If the tape is empty, set its contents by executing the block.

### Inner Blocks

All inner scopes may include the following blocks, with arbitrary nesting.

#### `IFFF { ... }`
Read a character from the tape and execute the block if it is `↡`.

#### `IFZR { ... }`
Execute the block if the tape pointer is at the end of the tape (i.e. `$R = 0`).

#### `IFyy _I, _J { ... }`
Execute the block if condition `yy` holds for `_I` and `_J`; that is, invoke `BRxx _I, _J` at the top of the block, where `xx` is the negation of `yy`.

- `EQ <-> NE`
- `GE <-> LT`
- `GT <-> LE`

As such, the following calls are disallowed.
- `IFEQ $V, $V { ... }`
- `IFGE $V, $V { ... }`
- `IFLE $V, $V { ... }`
- `IFLT $V, $V { ... }`

#### `LOOP { ... }`
Repeatedly execute the block, terminating if the tape is exhausted at the top of the block. Note that the tape pointer does *not* move by default at any stage in a `LOOP`, and thus failing to cause net movement of the tape pointer during execution will result in a crash.

#### `LOOP +N { ... }`
Execute the block at most `N` times, terminating if the tape is exhausted at the top of the block.

### Nested Blocks

#### `CASE +N { ... }`
Conditionally execute blocks based on the value of `N`, which may be tested against any contiguous span of integers starting from `0`. Additionally, a default block may be provided.
```
CASE +N {
    0 { ... }
    1 { ... }
    2 { ... }
    ...
    n { ... }
    DEFAULT { ... }
}
```

#### `CASE $V { ... }`
Conditionally execute blocks based on the value of `V`, which may be tested against any set of characters.
```
CASE $V {
    'A { ... }
    'B { ... }
    'D { ... }
    ...
    'Z { ... }
}
```
Each block must advance the tape pointer by the same amount in all circumstances to prevent unexpected fallthrough.

### Relative Tape Movement

Whenever a character is read from the tape, the tape pointer automatically advances by one. The tape pointer cannot advance past the end of the tape, nor rewind past the front. Care should be taken with repeated movement which may "get stuck" at the start or end of the tape.

#### `BACK +N = 1`
Move the tape pointer backward by `N` characters.

#### `BACKC 'C, +N = 1`
Move the tape pointer backward to after the `N`th preceding appearance of `C`.

#### `BACKS +N = 1`
Move the tape pointer backward to after the `N`th preceding appearance of `↡`. Equivalent to `BACKC '↡, +N`.

#### `SKIP +N = 1`
Move the tape pointer forward by `N` characters.

#### `SKIPC 'C, +N = 1`
Move the tape pointer forward past the `N`th succeeding appearance of `C`.

#### `SKIPS +N = 1`
Move the tape pointer forward past the `N`th succeeding appearance of `↡`. Equivalent to `SKIPC '↡, +N`.

### Absolute Tape Movement

Absolute position of the tape pointer is counted from the front of the tape, position `0`. Negative positions count from the end, so that `-1` is the last character on the tape.

#### `GOTO %N = 1`
Move the tape pointer to position `N`. Negative values count from the end of the tape.

#### `GOTOC 'C, %N = 1`
Move the tape pointer past the `N`th appearance of `C`. Negative values count from the end of the tape.

#### `GOTOS %N = 1`
Move the tape pointer past the `N`th appearance of `↡`. Negative values count from the end of the tape.


## Printing

**Printing** a character results in its appearance in the next contents of the tape; it is only **output** to STDOUT at the end of the current cycle if it appears before the first `↡` character. Printed characters cannot be overwritten or undone within a cycle.

#### `PRINA _I`
Print `_I` as it appears, which may be a character, character variable, or string literal.

#### `PRINC !V`
Print `V` as it appears.

#### `PRINN !V`
Print the name of `V`. This may be identical to `V` (`A`) or it may not (`Space`).

#### `PRFF +N = 1`
Print `N` form feeds (`↡`).

### Formatted Printing

#### `PRINL +N = 0, +M = 1, +L = 0, $W = #Space, !V`
Read or peek a character from the tape and print it with `k*M + L` copies of `W` padding on the left to reach a total width of at least `N`.

#### `PRINR +N = 0, +M = 1, +L = 0, $W = #Space, !V`
Read or peek a character from the tape and print it with `k*M + L` copies of `W` padding on the right to reach a total width of at least `N`.

### Conditional Printing

#### `PRZRz _K`
Print `_K` using `PRINz _K` if the tape pointer is at the end of the tape (i.e. `$R = 0`). Naturally, `_K` may not be equal to `$V`.

#### `PRyyz _I, _J, _K`
Print `_K` using `PRINz _K` if condition `yy` holds for `_I` and `_J`. Formatting arguments for `PRINA` can _not_ be passed.

- `EQ <-> NE`
- `GE <-> LT`
- `GT <-> LE`

As such, the following calls are disallowed.
- `PREQz $V, $V, _K`
- `PRGEz $V, $V, _K`
- `PRLEz $V, $V, _K`
- `PRLTz $V, $V, _K`

### Special Output

Certain characters may be printed with multiplicity in a compact way. These instructions are only nontrivial if passed `$R` as their arguments.

#### `FRESH +N = 1`
Print a newline (`\n`) if the previous printed character is not a newline, then print `N-1` newlines.

#### `TERP +N = 1`
Print `N` newlines (`\n`).

#### `TILDE +N = 1`
Print `N` tildes (`~`).


## Special Forms

#### `FORMAT ""`
Insert the contents of `""` directly into the assembled 5MAT.

#### `JUSTz +N = 0, +M = 1, +L = 0, !V = Space { ... }`
Justify the subsequent blocks by padding each block's content with at least `L` copies of `V` (with left-to-right precedence if padding must be unevenly allotted), arranged within a field of width `N+k*M` for the smallest possible choice of `k`.

The value of `z` dictates padding options at the edges of the field.

| Instruction | Effect                                           |
|-------------|--------------------------------------------------|
| `JUST`      | No padding may be added at the edges             |
| `JUSTL`     | Padding may be added after the last clause       |
| `JUSTR`     | Padding may be added before the first clause     |
| `JUSTC`     | Padding may be added at either edge of the field |

If any block is broken out of, the entire `JUSTz` instruction is canceled. Only previously processed blocks' contents are justified.

An `OVER` instruction may be included as the first clause (see below).

#### `OVER +P = 0, +O = 72 { ... }`
Create a temporary tape "buffer" by executing the block. If the output of the containing `JUST` instruction exceeds `O-P` characters, this buffer is printed *before* the justified content. Otherwise, it is discarded.

Since the block is always executed, breaking out of it cancels the entirety of the containing `JUST` instruction. Tape pointer movement is not undone.

Thus, the most general syntax for `JUST` is given below.
```
JUST +N, +M, +L, !V {
    OVER +P, +O { ... }
    { ... }
    { ... }
    ...
}
```

See the [HyperSpec Section on Justification](https://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm) for a full specification of these instructions' target directives' behavior.

#### `LOWER { ... }`
Fold all characters printed within the block to lowercase.

#### `TITLE { ... }`
Capitalize all words separated by spaces printed within the block; that is, make the first character of each word uppercase (if possible), and all other characters lowercase.

#### `TITLE 1 { ... }`
Capitalize the first word beginning with an alphabetical character printed within the block.

#### `TABA +N = 1, +M = 1`
Print spaces (` `) until at least `N+k*M` characters have been printed this cycle for the smallest possible choice of `k`.

#### `TABR +N = 1, +M = 1`
Print `N` spaces (` `), then print spaces until at least `k*M` characters have been printed this cycle for the smallest choice of `k`.

#### `UPPER { ... }`
Fold all characters printed within the block to uppercase.
