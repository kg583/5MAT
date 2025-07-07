# 6MAT assembler grammar file
# Instruction : arguments : code; each column is separated by at least two spaces
# Instruction signatures are matched top to bottom, so more specific signatures must appear first

# Literals are substituted using the name of the corresponding argument
# Numbers support setting the sign and adding/subtracting one
# Characters support getting either adjacent character in ASCII
# Strings support car/cdr to build code recursively
# Blocks can only substitute their own assembled code

# Templates are wrapped in backticks `` and must contain valid 6MAT after substituting arguments
# Generic types may only substitute into templates
# Templates may be empty, indicating a no-op, or contain 'ERR', indicating an illegal call signature
# Instructions with a '#' cannot appear in user code and are used internally
# A lowercase 'z' is a placeholder for any of the print types (ACLNR)


BACK        0                       ``
BACK        1                       `BACK`
BACK        +N                      ~+N:*
BACK                                ~:*

BACKC!  'C  0                       ``
BACKC!  'C  1                       `BACKC! 'C'`
BACKC!  'C  +N                      ~+N@{`BACKC! 'C'`~:}
BACKC!  'C                          ~@{~:*`BREQ $V, 'C'`~:*~:}~:*

BACKF!      +N                      `BACKC! '\f', +N`
BACKF!                              `BACKC! '\f'`

BREAK                               ~0^

BREQ    $V  $V                      ~v,v^
BREQ    $V  'C                      ~v,'C^
BREQ    $R  $R                      ~0^
BREQ    'C  $V                      ~'C,v^
BREQ    'C  'F                      ~'C,'F^
BREQ    %N  %M                      ~%N,%M^

BRFF                                ~v,'\f^

BRGE    $V  $V                      `ERR`
BRGE    $V  'C                      ~'C,'C,v^
BRGE    $R  $R                      ~0^
BRGE    'C  $V                      ~v,'C,'C^
BRGE    'C  'F                      ~'F,'F,'C^
BRGE    %N  %M                      ~%M,%M,%N^

BRGT    $V  $V                      `ERR`
BRGT    $V  'C                      ~'C,'D,v^
BRGT    $R  $R                      ``
BRGT    $R  %N                      ~%N+1,%N,#^
BRGT    'C  $V                      ~v,'B,'C^
BRGT    'C  'F                      ~'F,'B,'C^
BRGT    %N  $R                      ~#,%N-1,%N^
BRGT    %N  %M                      ~%M,%N-1,%N^

BRINC   $V  $V  $V                  ~v,v,v^
BRINC   $V  $V  'C                  ~v,v,'C^
BRINC   $V  'C  $V                  ~v,'C,v^
BRINC   $V  'C  'F                  ~v,'C,'F^
BRINC   $R  $R  $R                  ~0^
BRINC   $R  $R  %N                  `BRLE $R, %N`
BRINC   $R  %N  $R                  `BREQ $R, %N`
BRINC   $R  %N  %M                  ~#,%N,%M^
BRINC   'C  $V  $V                  ~'C,v,v^
BRINC   'C  $V  'F                  ~'C,v,'F^
BRINC   'C  'F  $V                  ~'C,'F,v^
BRINC   'C  'F  'I                  ~'C,'F,'I^
BRINC   %N  $R  $R                  `BRLE %N, $R`
BRINC   %N  $R  %M                  ~%N,#,%M^
BRINC   %N  %M  $R                  ~%N,%M,#^
BRINC   %N  %M  %L                  ~%N,%M,%L^

BRLE    $V  $V                      ~'\x01,v,v^
BRLE    $V  'C                      ~v,'C,'C^
BRLE    $R  $R                      ~0^
BRLE    'C  $V                      ~'C,'C,v^
BRLE    'C  'F                      ~'C,'C,'F^
BRLE    %N  %M                      ~%N,%M,%M^

BRLT    $V  $V                      `ERR`
BRLT    $V  'C                      ~v,'B,'C^
BRLT    $R  $R                      ``
BRLT    $R  %N                      ~#,%N-1,%N^
BRLT    'C  $V                      ~'C,'D,v^
BRLT    'C  'F                      ~'C,'D,'F^
BRLT    %N  $R                      ~%N,%N+1,#^
BRLT    %N  %M                      ~%N,%N+1,%M^

BRNE!   $V  $V                      `ERR`
BRNE!   $V  'C                      ~v,'B,'C^~:*~'C,'D,v^
BRNE!   $n  $n                      `ERR`
BRNE!   $n  "X                      `#BRNE $n, "X`
BRNE!   $R  $R                      ``
BRNE!   $R  %N                      ~#,%N-1,%N^~%N,%N+1,#^
BRNE!   'C  $V                      ~v,'B,'C^~:*~'C,'D,v^
BRNE!   'C  'F                      ~'C,'D,'F^~:*~'F,'B,'C^~:*
BRNE!   "X  $n                      `#BRNE "X, $n`
BRNE!   %N  $R                      ~#,%N-1,%N^~%N,%N+1,#^

#BRNE   $0  "X                      ``
#BRNE   $n  "X                      `BRNE! $V, 'X'``#BRNE $n-1, .X`
#BRNE   "X  $0                      ``
#BRNE   "X  $n                      `BRNE! 'X', $V``#BRNE .X, $n-1`

BRNR!   +N                          ~#[`#BRNR +N`~]

#BRNR   0                           ~0^
#BRNR   +N                          ~;`#BRNR +N-1`

BRNZ                                ~#[~:;~0^~]

BRZR                                ~^

BUFFER  {...}                       ~<...~>

CASER!  {...}                       ~1@{~#[...~]~:}
CASER!  [...]                       ~#[...~]

CASES!  {...}                       ...

CASEV!  {...}                       ...

#CASE   0   0           {...}       ~1@{...~:}
#CASE   +N  +M          {...}       ~;~1@{...~:}
#CASE   0   DEFAULT     {...}       `ERR`
#CASE   +N  DEFAULT     {...}       ~:;~1@{...~:}
#CASE   0   0           [...]       ...
#CASE   +N  +M          [...]       ~;...
#CASE   0   DEFAULT     [...]       `ERR`
#CASE   +N  DEFAULT     [...]       ~:;...
#CASE   0   'C          {...}       ~1@{`BRNE! $V, 'C'`...~:}
#CASE   +N  'C          {...}       ~:*~1@{`BRNE! $V, 'C'`...~:}
#CASE   +N  "X          {...}       `#CASE +N, ?n, "X |...|`
#CASE   +N  ?0  "X      |...|       ~1@{...~:}
#CASE   +N  ?n  "X      |...|       ~<`BRNE! $V, 'X'``#CASE +N, ?n-1, .X |...|`~>~:*

COPY        1                       `COPY`
COPY        $R                      ~@{~c~}
COPY        +N                      ~+N@{~c~}
COPY                                ~c

COPYC!  'C  1                       `COPYC! 'C'`
COPYC!  'C  $R                      `COPY $R`
COPYC!  'C  +N                      ~+N-1@{`COPYC! 'C'`"C"~}`COPYC! 'C'`
COPYC!  'C                          ~@{`BREQ $V, 'C'`~:*`COPY`~}

COPYF!      $R                      `COPY $R`
COPYF!      +N                      `COPYC! '\f', +N`
COPYF!                              `COPYC! '\f'`

COPYR!  'C  1                       `COPYR! 'C'`
COPYR!  'C  $R                      `ERR`
COPYR!  'C  +N                      ~:*~+N-1@{~@{`BREQ $V, 'C'`~:*`COPY`~2:*~}"C"~2:*~}`COPYR! 'C'`
COPYR!  'C                          ~:*~@{`BREQ $V, 'C'`~:*`COPY`~2:*~}

CRASH                               ~?

CREQ!   _I  ?V                      `CREQ! _I, $V`~:*
CREQ!   _I  _J                      ~<`BRNE! _I, _J``CRASH`~>

CRFF!       ?V                      `CRFF! $V`~:*
CRFF!       $V                      ~<`BREQ $V, '\f'``CRASH`~>

CRGE!   _I  ?V                      `CRGE! _I, $V`~:*
CRGE!   _I  _J                      ~<`BRLT _I, _J``CRASH`~>

CRGT!   _I  ?V                      `CRGT! _I, $V`~:*
CRGT!   _I  _J                      ~<`BRLE _I, _J``CRASH`~>

CRLE!   _I  ?V                      `CRLE! _I, $V`~:*
CRLE!   _I  _J                      ~<`BRGT _I, _J``CRASH`~>

CRLT!   _I  ?V                      `CRLT! _I, $V`~:*
CRLT!   _I  _J                      ~<`BRGE _I, _J``CRASH`~>

CRNE!   _I  ?V                      `CRNE! _I, $V`~:*
CRNE!   _I  _J                      ~<`BREQ _I, _J``CRASH`~>

CRNR!   +N                          ~<`BRNE! $R, +N``CRASH`~>

CRNZ                                ~#[~:;`CRASH`~]

CRZR                                ~#[`CRASH`~]

DO      {...}                       ~:*~1{...~}

FRESH   +N                          ~+N&
FRESH                               ~&

GOTO        0                       `GOTO`
GOTO        +N                      ~+N@*
GOTO        -N                      `SKIP $R``BACK +N`
GOTO                                ~@*

GOTOC!  'C  0                       `GOTO`
GOTOC!  'C  $R                      `ERR`
GOTOC!  'C  +N                      `GOTO``SKIPC! 'C', +N`
GOTOC!  'C  -N                      `SKIP $R``BACKC! 'C', +N`
GOTOC!  'C                          `GOTO``SKIPC! 'C'`

GOTOF!      %N                      `GOTOC! '\f', %N`
GOTOF!                              `GOTOC! '\f'`

IFEQ!   _I  ?V  {...}               ~1@{`BRNE! _I, $V`~:*~1@{...~:}~*~:}~:*
IFEQ!   _I  ?n  {...}               `ERR`
IFEQ!   _I  _J  {...}               ~1@{`BRNE! _I, _J`...~:}

IFFF!       ?V  {...}               ~1@{`BRNE! $V, '\f'`~:*~1@{...~:}~*~:}~:*
IFFF!       $V  {...}               ~1@{`BRNE! $V, '\f'`...~:}

IFGE!   _I  ?V  {...}               ~1@{`BRLT _I, $V`~:*~1@{...~:}~*~:}~:*
IFGE!   _I  _J  {...}               ~1@{`BRLT _I, _J`...~:}

IFGT!   _I  ?V  {...}               ~1@{`BRLE _I, $V`~:*~1@{...~:}~*~:}~:*
IFGT!   _I  _J  {...}               ~1@{`BRLE _I, _J`...~:}

IFLE!   _I  ?V  {...}               ~1@{`BRGT _I, $V`~:*~1@{...~:}~*~:}~:*
IFLE!   _I  _J  {...}               ~1@{`BRGT _I, _J`...~:}

IFLT!   _I  ?V  {...}               ~1@{`BRGE _I, $V`~:*~1@{...~:}~*~:}~:*
IFLT!   _I  _J  {...}               ~1@{`BRGE _I, _J`...~:}

IFNE!   _I  ?V  {...}               ~1@{`BREQ _I, $V`~:*~1@{...~:}~*~:}~:*
IFNE!   _I  _J  {...}               ~1@{`BREQ _I, _J`...~:}

IFNR!   +N  {...}                   ~1@{`BRNE! $R, +N`...~:}
IFNR!   +N  [...]                    ~#[`#IFNR +N`...~]

#IFNR   0                           ``
#IFNR   +N                          ~;`#IFNR +N-1`

IFNZ        {...}                   ~1@{~^...~:}
IFNZ        [...]                   ~#[~:;...~]

IFZR        {...}                   ~1@{~1,1,#^...~:}
IFZR        [...]                   ~#[...~]

INIT    {...}                       ~:[...~;~]

LOOP    0   {...}                   ``
LOOP    +N  {...}                   ~+N@{...~}
LOOP    {...}                       ~@{...~}

JUST    %N  %M  %L  ?V  {...}       ~%N,%M,%L,v<~:*...~>
JUST    %N  %M      ?V  {...}       ~%N,%M,,v<~:*...~>
JUST    %N          ?V  {...}       ~%N,,,v<~:*...~>
JUST    %N  %M  %L  $V  {...}       ~%N,%M,%L,v<...~>
JUST    %N  %M      $V  {...}       ~%N,%M,,v<...~>
JUST    %N          $V  {...}       ~%N,,,v<...~>
JUST    %N  %M  %L  'C  {...}       ~%N,%M,%L,'C<...~>
JUST    %N  %M      'C  {...}       ~%N,%M,,'C<...~>
JUST    %N          'C  {...}       ~%N,,,'C<...~>
JUST    %N  %M  %L      {...}       ~%N,%M,%L<...~>
JUST    %N  %M          {...}       ~%N,%M<...~>
JUST    %N              {...}       ~%N<...~>
JUST                    {...}       ~<...~>

JUSTC   %N  %M  %L  ?V  {...}       ~%N,%M,%L,v:@<~:*...~>
JUSTC   %N  %M      ?V  {...}       ~%N,%M,,v:@<~:*...~>
JUSTC   %N          ?V  {...}       ~%N,,,v:@<~:*...~>
JUSTC   %N  %M  %L  $V  {...}       ~%N,%M,%L,v:@<...~>
JUSTC   %N  %M      $V  {...}       ~%N,%M,,v:@<...~>
JUSTC   %N          $V  {...}       ~%N,,,v:@<...~>
JUSTC   %N  %M  %L  'C  {...}       ~%N,%M,%L,'C:@<...~>
JUSTC   %N  %M      'C  {...}       ~%N,%M,,'C:@<...~>
JUSTC   %N          'C  {...}       ~%N,,,'C:@<...~>
JUSTC   %N  %M  %L      {...}       ~%N,%M,%L:@<...~>
JUSTC   %N  %M          {...}       ~%N,%M:@<...~>
JUSTC   %N              {...}       ~%N:@<...~>
JUSTC                   {...}       ~:@<...~>

JUSTL   %N  %M  %L  ?V  {...}       ~%N,%M,%L,v@<~:*...~>
JUSTL   %N  %M      ?V  {...}       ~%N,%M,,v@<~:*...~>
JUSTL   %N          ?V  {...}       ~%N,,,v@<~:*...~>
JUSTL   %N  %M  %L  $V  {...}       ~%N,%M,%L,v@<...~>
JUSTL   %N  %M      $V  {...}       ~%N,%M,,v@<...~>
JUSTL   %N          $V  {...}       ~%N,,,v@<...~>
JUSTL   %N  %M  %L  'C  {...}       ~%N,%M,%L,'C@<...~>
JUSTL   %N  %M      'C  {...}       ~%N,%M,,'C@<...~>
JUSTL   %N          'C  {...}       ~%N,,,'C@<...~>
JUSTL   %N  %M  %L      {...}       ~%N,%M,%L@<...~>
JUSTL   %N  %M          {...}       ~%N,%M@<...~>
JUSTL   %N              {...}       ~%N@<...~>
JUSTL                   {...}       ~@<...~>

JUSTR   %N  %M  %L  ?V  {...}       ~%N,%M,%L,v:<~:*...~>
JUSTR   %N  %M      ?V  {...}       ~%N,%M,,v:<~:*...~>
JUSTR   %N          ?V  {...}       ~%N,,,v:<~:*...~>
JUSTR   %N  %M  %L  $V  {...}       ~%N,%M,%L,v:<...~>
JUSTR   %N  %M      $V  {...}       ~%N,%M,,v:<...~>
JUSTR   %N          $V  {...}       ~%N,,,v:<...~>
JUSTR   %N  %M  %L  'C  {...}       ~%N,%M,%L,'C:<...~>
JUSTR   %N  %M      'C  {...}       ~%N,%M,,'C:<...~>
JUSTR   %N          'C  {...}       ~%N,,,'C:<...~>
JUSTR   %N  %M  %L      {...}       ~%N,%M,%L:<...~>
JUSTR   %N  %M          {...}       ~%N,%M:<...~>
JUSTR   %N              {...}       ~%N:<...~>
JUSTR                   {...}       ~:<...~>

LOWER   [...]                       ~(...~)

#JUST   0                   {...}   ~1@{...~:}
#JUST   +N                  {...}   ~;~1@{...~:}
#JUST   0                   [...]   ...
#JUST   +N                  [...]   ~;...
#JUST   0   OVER    %P  %O  {...}   ~1@{...~:}~%P,%O:;
#JUST   0   OVER    %P      {...}   ~1@{...~:}~%P:;
#JUST   0   OVER    %P  %O  [...]   ...~%P,%O:;
#JUST   0   OVER    %P      [...]   ...~%P:;
#JUST   +N  OVER    +P  +O  {...}   `ERR`
#JUST   +N  OVER    +P      {...}   `ERR`
#JUST   +N  OVER    +P  +O  [...]   `ERR`
#JUST   +N  OVER    +P      [...]   `ERR`

PRINA   ?V                          ~a~:*
PRINA   $V                          ~a
PRINA   'C                          "C"
PRINA   ?n                          `#PRINA ?n`
PRINA   $n                          `#PRINA $n`
PRINA   "X                          "X
PRINA   %N                          %N

#PRINA  $0                          ``
#PRINA  $n                          ~a`#PRINA $n-1`
#PRINA  ?0                          ``
#PRINA  ?n                          ~a`#PRINA ?n-1`~:*

PRINC   ?V                          ~c~:*
PRINC   $V                          ~c
PRINC   'C                          "C"

PRINL   %N  %M  %L  $V  ?V          ~%N,%M,%L,v@a~:*
PRINL   %N  %M      $V  ?V          ~%N,%M,,v@a~:*
PRINL   %N          $V  ?V          ~%N,,,v@a~:*
PRINL   %N  %M  %L  'C  ?V          ~%N,%M,%L,'C@a~:*
PRINL   %N  %M      'C  ?V          ~%N,%M,,'C@a~:*
PRINL   %N          'C  ?V          ~%N,,,'C@a~:*
PRINL   %N  %M  %L      ?V          ~%N,%M,%L@a~:*
PRINL   %N  %M          ?V          ~%N,%M@a~:*
PRINL   %N              ?V          ~%N@a~:*
PRINL   %N  %M  %L  $V  $V          ~%N,%M,%L,v@a
PRINL   %N  %M      $V  $V          ~%N,%M,,v@a
PRINL   %N          $V  $V          ~%N,,,v@a
PRINL   %N  %M  %L  'C  $V          ~%N,%M,%L,'C@a
PRINL   %N  %M      'C  $V          ~%N,%M,,'C@a
PRINL   %N          'C  $V          ~%N,,,'C@a
PRINL   %N  %M  %L      $V          ~%N,%M,%L@a
PRINL   %N  %M          $V          ~%N,%M@a
PRINL   %N              $V          ~%N@a

PRINN   ?V                          ~:c~:*
PRINN   $V                          ~:c

PRINR   %N  %M  %L  $V  ?V          ~%N,%M,%L,va~:*
PRINR   %N  %M      $V  ?V          ~%N,%M,,va~:*
PRINR   %N          $V  ?V          ~%N,,,va~:*
PRINR   %N  %M  %L  'C  ?V          ~%N,%M,%L,'Ca~:*
PRINR   %N  %M      'C  ?V          ~%N,%M,,'Ca~:*
PRINR   %N          'C  ?V          ~%N,,,'Ca~:*
PRINR   %N  %M  %L      ?V          ~%N,%M,%La~:*
PRINR   %N  %M          ?V          ~%N,%Ma~:*
PRINR   %N              ?V          ~%Na~:*
PRINR   %N  %M  %L  $V  $V          ~%N,%M,%L,va
PRINR   %N  %M      $V  $V          ~%N,%M,,va
PRINR   %N          $V  $V          ~%N,,,va
PRINR   %N  %M  %L  'C  $V          ~%N,%M,%L,'Ca
PRINR   %N  %M      'C  $V          ~%N,%M,,'Ca
PRINR   %N          'C  $V          ~%N,,,'Ca
PRINR   %N  %M  %L      $V          ~%N,%M,%La
PRINR   %N  %M          $V          ~%N,%Ma
PRINR   %N              $V          ~%Na

PRFF    +N                          ~+N|
PRFF                                ~|

PRNRz   +N      _K                  ~<`BREQ $R, +N``PRINz _K`~>

PRNZz           _K                  ~#[~:;`PRINz _K`~]

PRZRz           _K                  ~#[`PRINz _K`~]

SKIP        0                       ``
SKIP        1                       `SKIP`
SKIP        +N                      ~+N*
SKIP                                ~*

SKIPC!  'C  0                       ``
SKIPC!  'C  1                       `SKIPC! 'C'`
SKIPC!  'C  +N                      ~+N@{`SKIPC! 'C'`~}
SKIPC!  'C                          ~@{`BREQ $V, 'C'`~}

SKIPF!      +N                      `SKIPC! '\f', +N`
SKIPF!                              `SKIPC! '\f'`

TABA    +N  +M                      ~+N,+Mt
TABA    +N                          ~+Nt
TABA                                ~t

TABR    +N  +M                      ~+N,+M@t
TABR    +N                          ~+N@t
TABR                                ~@t

TERP    +N                          ~+N%
TERP                                ~%

TILDE   +N                          ~+N~
TILDE                               ~~

TITLE   1   [...]                   ~@(...~)
TITLE   [...]                       ~:(...~)

UPPER   [...]                       ~:@(...~)
