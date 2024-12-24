# 5MAT

Like most sensible languages, Common Lisp has a handy-dandy function for producing and combining string representations of various objects, so creatively named `format`. Its facilities are familiar to users of C's `printf` or Python's f-strings: a string containing some magic character sequences has those sequences replaced with representations of the arguments passed in, the rest of the string being the glue connecting said arguments together for our needy human eyes.

```lisp
(format nil "This number has padding: ~3,'0d. This one's in hex: ~x." 9 255)
>>> "This number has padding: 009. This one's in hex: FF."
```

Such formatters are usually quite terse and quite powerful[^1], and `format` is the king of them all (to me, anyway). Its [Wikipedia article](https://en.wikipedia.org/wiki/Format_(Common_Lisp)?useskin=vector) is a fantastic synopsis, but I think the following chapter title from the [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm) should be enough to convince you of the tomfuckery we are about to employ:

> 22.3.7 FORMAT Control-Flow Operations

Indeed, `format` is armed with the power to loop, backtrack, and dance wildly across its arguments. Further empowered with conditionals, break statements, and even external function calls, one might, nay *should*, naturally ask if `format` is Turing-complete. The answer is **yes... with a few modifications**.

[^1]: https://github.com/carlini/printf-tac-toe
