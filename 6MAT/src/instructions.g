# 6MAT assembler grammar file
# Instruction : arguments : code; each column is separated by at least two spaces

# Literals are substituted using the name of the corresponding argument
# Numbers support setting the sign and adding/subtracting one
# Characters support getting either adjacent character in ASCII
# Strings support car/cdr to build code recursively
# Blocks can only substitute their own assembled code

# Templates are wrapped in backticks `` and must contain valid 6MAT after substituting arguments
# Generic types may only substitute into templates
# Templates may be empty, indicating a no-op, or contain 'ERR', indicating an illegal call signature
# Instructions with a '!' cannot appear in user code and are used internally

BACK        +N                      ~+N:*
BACK                                ~:*

BACKF   'C  +N                      ~:*~+N@{~@{`BREQ $V, 'C'`~:2*~}~2:*~}
BACKC   'C                          ~:*~@{`BREQ $V, 'C'`~2:*~}

BACKF       +N                      ~:*~+N@{~@{`BRFF`~2:*~}~2:*~}
BACKF                               ~:*~@{`BRFF`~2:*~}

BREAK                               ~0^

BREQ    $V  $V                      ~v,v^
BREQ    $V  'C                      ~v,'C^
BREQ    $R  $R                      ~0^
BREQ    'C  $V                      ~'C,v^
BREQ    'C  'F                      ~'C,'F^
BREQ    %N  %M                      ~%N,%M^

BRFF                                ~v,'\f'^

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

BRLE    $V  $V                      ~'\1,v,v^
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

BRNE    $V  $V                      `ERR`
BRNE    $V  'C                      ~v,'B,'C^~:*~'C,'D,v^
BRNE    $n  $n                      `ERR`
BRNE    $n  "X                      `BRNE! $n, "X`
BRNE    $R  $R                      ``
BRNE    $R  %N                      ~#,%N-1,%N^~%N,%N+1,#^
BRNE    'C  $V                      ~v,'B,'C^~:*~'C,'D,v^
BRNE    'C  'F                      ~'C,'D,'F^~:*~'F,'B,'C^~:*
BRNE    "X  $n                      `BRNE! "X, $n`
BRNE    "X  "Y                      `BRNE! "X, "Y`
BRNE    %N  $R                      ~#,%N-1,%N^~%N,%N+1,#^

BRNE!   $n  "X                      `BRNE $V, 'X'``BRNE! $n-1, .X`
BRNE!   "X  $n                      `BRNE 'X', $V``BRNE! .X, $n-1`
BRNE!   "X  "Y                      `BRNE 'X', 'Y'``BRNE! .X, .Y`

BRZR                                ~^

BUFFER  {...}                       ~<...~>

CASER   [...]                       ~#[...~]

CASES   {...}                       ...

CASEV   {...}                       ...

CASE!   0   0           {...}       ~1@{...~:}
CASE!   +N  +M          {...}       ~;~1@{...~:}
CASE!   0   DEFAULT     {...}       `ERR`
CASE!   +N  DEFAULT     {...}       ~:;~1@{...~:}
CASE!   0   0           [...]       ...
CASE!   +N  +M          [...]       ~;...
CASE!   0   DEFAULT     [...]       `ERR`
CASE!   +N  DEFAULT     [...]       ~:;...
CASE!   0   'C          {...}       ~1@{`BRNE $V, 'C'`...~:}
CASE!   +N  'C          {...}       ~:*~1@{`BRNE $V, 'C'`...~:}
CASE!   +N  "X          {...}       `CASE! +N, ?n, "X |...|`
CASE!   +N  ?0  "X      |...|       ~1@{...~:}
CASE!   +N  ?n  "X      |...|       ~<`BRNE $V, 'X'``CASE! +N, ?n-1, .X |...|`~:*~>

COPY        +N                      ~+N@{~c~}
COPY                                ~c

COPYC   'C  +N                      ~+N-1@{`COPYC 'C'`C~}`COPYC 'C'`
COPYC   'C                          ~@{`BREQ $V, 'C'`~:*`COPY`~}

COPYF       +N                      `COPYC '\f', +N`
COPYF                               `COPYC '\f'`

COPYR       +N                      ~:*~+N-1@{~@{`BRFF`~:*`COPY`~2:*~}~|~2:*~}~@{`BRFF`~:*`COPY`~2:*~}
COPYR                               ~:*~@{`BRFF`~:*`COPY`~2:*~}

CRASH                               ~?

CREQ    _I  ?V                      `CREQ _I, $V`~:*
CREQ    _I  _J                      ~<`BRNE _I, _J``CRASH`~>

CRFF                                ~<`BRNE $V, '\f'``CRASH`~>

CRGE    _I  ?V                      `CRGE _I, $V`~:*
CRGE    _I  _J                      ~<`BRLT _I, _J``CRASH`~>

CRGT    _I  ?V                      `CRGT _I, $V`~:*
CRGT    _I  _J                      ~<`BRLE _I, _J``CRASH`~>

CRLE    _I  ?V                      `CRLE _I, $V`~:*
CRLE    _I  _J                      ~<`BRGT _I, _J``CRASH`~>

CRLT    _I  ?V                      `CRLT _I, $V`~:*
CRLT    _I  _J                      ~<`BRGE _I, _J``CRASH`~>

CRNE    _I  ?V                      `CRNE _I, $V`~:*
CRNE    _I  _J                      ~<`BREQ _I, _J``CRASH`~>

CRZR                                ~#[`CRASH`~]

DO      {...}                       ~{...~}

FRESH   +N                          ~+N&
FRESH                               ~&

GOTO        +N                      ~+N@*
GOTO        -N                      `GOTO $R``BACK +N`
GOTO                                ~@*

GOTOC   'C  $R                      `GOTO $R`
GOTOC   'C  +N                      `GOTO``SKIP 'C', +N`
GOTOC   'C  -N                      `GOTO $R``BACKC 'C', +N`
GOTOC   'C                          `GOTO``SKIPC 'C'`

GOTOF       %N                      `GOTOC '\f', %N`
GOTOF                               `GOTOC '\f'`

IFEQ    _I  _J  {...}               ~1@{`BRNE _I, _J`...~:}

IFFF            {...}               ~1@{`BRNE $V, '\f'`...~:}

IFGE    _I  _J  {...}               ~1@{`BRLT _I, _J`...~:}

IFGT    _I  _J  {...}               ~1@{`BRLE _I, _J`...~:}

IFLE    _I  _J  {...}               ~1@{`BRGT _I, _J`...~:}

IFLT    _I  _J  {...}               ~1@{`BRGE _I, _J`...~:}

IFNE    _I  _J  {...}               ~1@{`BREQ _I, _J`...~:}

IFNR    +N  {...}                   ~1@{`BRNE $R, +N`...~:}
IFNR    +N  [...]                   ~#[`IFNR! +N`...~]

IFNR!   0                           ``
IFNR!   +N                          ~;`IFNR! +N-1`

IFZR        {...}                   ~1@{~1,1,#^...~:}
IFZR        [...]                   ~#[...~]

INIT    {...}                       ~:[...~;~]~:*

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

LOWER   {...}                       ~(...~)

JUST!   0                   {...}   ~1@{...~:}
JUST!   +N                  {...}   ~;~1@{...~:}
JUST!   0                   [...]   ...
JUST!   +N                  [...]   ~;...
JUST!   0   OVER    %P  %O  {...}   ~1@{...~:}~%P,%O:;
JUST!   0   OVER    %P      {...}   ~1@{...~:}~%P:;
JUST!   0   OVER    %P  %O  [...]   ...~%P,%O:;
JUST!   0   OVER    %P      [...]   ...~%P:;
JUST!   +N  OVER    +P  +O  {...}   `ERR`
JUST!   +N  OVER    +P      {...}   `ERR`
JUST!   +N  OVER    +P  +O  [...]   `ERR`
JUST!   +N  OVER    +P      [...]   `ERR`

PRINA   ?V                          ~a~:*
PRINA   $V                          ~a
PRINA   'C                          "C"
PRINA   ?n                          `PRINA! ?n`
PRINA   $n                          `PRINA! $n`
PRINA   "X                          "X
PRINA   %N                          %N

PRINA!  $0                          ``
PRINA!  $n                          ~a`PRINA! $n-1`
PRINA!  ?0                          ``
PRINA!  ?n                          ~a`PRINA! ?n-1`~:*

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

PREQx   _I  _J  _K                  ~<`BRNE _I, _J``PRINx _K`~>

PRFF    +N                          ~+N|
PRFF                                ~|

PRGEx   _I  _J  _K                  ~<`BRLT _I, _J``PRINx _K`~>

PRGTx   _I  _J  _K                  ~<`BRLE _I, _J``PRINx _K`~>

PRLEx   _I  _J  _K                  ~<`BRGT _I, _J``PRINx _K`~>

PRLTx   _I  _J  _K                  ~<`BRGE _I, _J``PRINx _K`~>

PRNEx   _I  _J  _K                  ~<`BREQ _I, _J``PRINx _K`~>

PRZRx           _K                  ~#[`PRINx _K`~]

SKIP        +N                      ~+N*
SKIP                                ~*

SKIPC   'C  +N                      ~+N@{`SKIPC 'C'`~}
SKIPC   'C                          ~@{`BREQ $V, 'C'`~}

SKIPF       +N                      `SKIPC '\f', +N`
SKIPF                               `SKIPC '\f'`

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

TITLE   1   {...}                   ~@(...~)
TITLE   {...}                       ~:(...~)

UPPER   {...}                       ~:@(...~)
