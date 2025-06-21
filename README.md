# 5MAT

Like most sensible languages, Common Lisp has a handy-dandy function for producing and combining string representations of various objects, so creatively named `format`. Its facilities are familiar to users of C's `printf` or Python's f-strings: a string containing some magic character sequences has those sequences replaced with representations of the arguments passed in, the rest of the string being the glue connecting said arguments together for our needy human eyes.

```lisp
(format nil "This number has padding: ~3,'0d. This one's in hex: ~x." 9 254)
>>> "This number has padding: 009. This one's in hex: FE."
```

Such formatters are usually quite terse and quite powerful[^1], and `format` is the king of them all (to me, anyway). Its [Wikipedia article](https://en.wikipedia.org/wiki/Format_(Common_Lisp)?useskin=vector) is a fantastic synopsis, but I think the following chapter title from the [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm) should be enough to convince you of the tomfuckery we are about to employ:

> 22.3.7 FORMAT Control-Flow Operations

Indeed, `format` is armed with the power to loop, backtrack, and dance wildly across its arguments. Further empowered with conditionals, break statements, and even external function calls, one might, nay *should*, naturally ask if `format` is Turing-complete. The answer is **yes... with a few modifications**. These modifications grant us 5MAT, a Turing-complete esolang whose source is a single Common Lisp FORMAT string.

[^1]: https://github.com/carlini/printf-tac-toe

Don't believe me? Head down into the `5MAT/` directory to see how it works for yourself! Only a few realizations (beyond base knowledge of FORMAT) are necessary to make it all come together, and once familiar with its fundamental directives, reading and writing 5MAT *can* become quite natural. A basic driver and slightly less basic debugger are included, as well as many instructive sample programs.


# 6MAT

Anything is possible in 5MAT, and devising basic programs to show a glimpse of how is a delightful excursion... for a time. A nice handwritten FizzBuzz or Fibonacci iterator nears the limit of most 5matters'[^2] acceptable tedium and headache, and thus one would reasonably enjoy some tools that can help them along.

[^2]: At the time of writing, there exist precisely 1½ fluent 5matters.

Enter 6MAT, one abstraction level up from raw 5MAT. Programs are written using an assembly-like syntax, with instructions and control flow blocks corresponding to sequences of format directives. Certain common 5MAT snippets can be realized with just one instruction, and directives useless to 5MAT's computation model need not clutter your mind.

The `6MAT/` directory contains a detailed instruction and paradigm guide, as well as an assembler and optimizer which can be run from the command line.
```
python -m 6MAT <filename>.6mat
```


# 7MAT

But can we go further beyond? 6MAT is more-or-less directly translation into 5MAT, with little by way of *semantic* abstraction to help inspired programmers. Though it does morph an affront to Lisp itself into an assembly language for a weird computer, this is hardly enough mental relief to concoct truly complex programs.

And what does one do with assembly languages besides compile into them. So, coming soon: 7MAT, a low-level declarative language with variables, data types, and a plethora of other features that comprise the bare minimum for actually usable languages.

The process of compiling 7MAT to 6MAT requires far more inventive thinking than anything previous, requiring a sophisticated tape layout and calling convention to permit actions such as adding two numbers together without any mental overhead for the programmer. I'm sure it will make for a great write-up when I figure it all out, so stay tuned.
