BACK        +N                  ~N:*
BACK                            ~:*

BACKC   'C  +N                  ~:*~N@{~@{`BREQ $V, 'C`~:2*~}~2:*~}
BACKC   'C                      ~:*~@{`BREQ $V, 'C`~2:*~}

BACKS       +N                  ~:*~N@{~@{`BRFF`~2:*~}~2:*~}
BACKS                           ~:*~@{`BRFF`~2:*~}

BREAK                           ~0^

BREQ    _I  _J                  ~_I,_J^

BRFF                            ~v,'^

BRGE    $V  $V                  `ERR`
BRGE    $V  'C                  ~'C,'C,v^
BRGE    'C  $V                  ~v,'C,'C^
BRGE    'C  'F                  ~'F,'F,'C^
BRGE    %N  %M                  ~M,M,N^

BRGT    $V  $V                  `ERR`
BRGT    $V  'C                  ~'C,'D,v^
BRGT    $R  $R                  ``
BRGT    $R  %N                  ~N+1,N,#^
BRGT    'C  $V                  ~v,'B,'C^
BRGT    'C  'F                  ~'F,'B,'C^
BRGT    %N  $R                  ~#,N-1,N^

BRINC   _I  _J  _K              ~_I,_J,_K^

BRLE    $V  $V                  ~',v,v^
BRLE    $V  'C                  ~v,'C,'C^
BRLE    'C  $V                  ~'C,'C,v^
BRLE    'C  'F                  ~'C,'C,'F^
BRLE    %N  %M                  ~N,M,M^

BRLT    $V  $V                  `ERR`
BRLT    $V  'C                  ~v,'B,'C^
BRLT    $R  $R                  ``
BRLT    $R  %N                  ~#,N-1,N^
BRLT    'C  $V                  ~'C,'D,v^
BRLT    'C  'F                  ~'C,'D,'F^
BRLT    %N  $R                  ~N,N+1,#^

BRNE    $V  $V                  `ERR`
BRNE    $V  'C                  ~v,'B,'B^~:*~'D,'D,v^
BRNE    $R  $R                  ``
BRNE    $R  %N                  ~#,N-1,N^~:*~N,N+1,#^
BRNE    'C  $V                  ~v,'B,'C^~:*~'C,'D,v^
BRNE    'C  'F                  ~'C,'D,'F^~:*~'F,'B,'C^~:*
BRNE    %N  $R                  ~#,N-1,N^~:*~N,N+1,#^

BRZR                            ~^

CASR!   $R  {...}               ~#[...~]

CASV!   $V  {...}               ~<...~*~>

COPY        +N                  ~N@{~c~}
COPY                            ~c

COPYC   'C  +N                  ~N-1@{`COPYC 'C`C~}`COPYC 'C`
COPYC   'C                      ~@{`BREQ $V, 'C`~:*`COPY`~}

COPYR       +N                  ~:*~N-1@{~@{`BRFF`~:*`COPY`~2:*~}~|~2:*~}~@{`BRFF`~:*`COPY`~2:*~}
COPYR                           ~:*~@{`BRFF`~:*`COPY`~2:*~}

COPYS       +N                  `COPYC '\f, N`
COPYS                           `COPYC '\f`

CRASH                           ~?

CREQ    _I  ?V                  `CREQ _I, $V`~:*
CREQ    _I  _J                  ~<`BRNE _I, _J``CRASH`~>

CRFF                            ~<`BRNE $V, '\f``CRASH`~>

CRGE    _I  ?V                  `CRGE _I, $V`~:*
CRGE    _I  _J                  ~<`BRLT _I, _J``CRASH`~>

CRGT    _I  ?V                  `CRGT _I, $V`~:*
CRGT    _I  _J                  ~<`BRLE _I, _J``CRASH`~>

CRLE    _I  ?V                  `CRLE _I, $V`~:*
CRLE    _I  _J                  ~<`BRGT _I, _J``CRASH`~>

CRLT    _I  ?V                  `CRLT _I, $V`~:*
CRLT    _I  _J                  ~<`BRGE _I, _J``CRASH`~>

CRNE    _I  ?V                  `CRNE _I, $V`~:*
CRNE    _I  _J                  ~<`BREQ _I, _J``CRASH`~>

CRZR                            ~#[`CRASH`~]

DO      {...}                   ~{...~}

FORMAT  ""                      ""

FRESH                           ~&
FRESH   +N                      ~N&

GOTO        +N                  ~N@*
GOTO        -N                  `GOTO $R``BACK +N`
GOTO                            ~@*

GOTOC   'C  $R                  `GOTO $R`
GOTOC   'C  +N                  `GOTO``SKIP 'C, N`
GOTOC   'C  -N                  `GOTO $R``BACKC 'C, +N`
GOTOC   'C                      `GOTO``SKIPC 'C`

GOTOS       %N                  `GOTOC '\f, N`
GOTOS                           `GOTOC '\f`

IFEQ    _I  _J  {...}           ~<`BRNE _I, _J`...~>

IFFF    {...}                   ~<`BRNE $V, '\f`...~>

IFGE    _I  _J  {...}           ~<`BRLT _I, _J`...~>

IFGT    _I  _J  {...}           ~<`BRLE _I, _J`...~>

IFLE    _I  _J  {...}           ~<`BRGT _I, _J`...~>

IFLT    _I  _J  {...}           ~<`BRGE _I, _J`...~>

IFNE    _I  _J  {...}           ~<`BREQ _I, _J`...~>

IFZR    {...}                   ~#[...~]

INIT    {...}                   ~:[...~;~]~:*

LOOP    0   {...}               ``
LOOP    1   {...}               ~<...~>
LOOP    +N  {...}               ~N@{...~}
LOOP    {...}                   ~@{...~}

JUST    +N  +M  +L  ?V  {...}   ~N,M,L,v<~:*...~>
JUST    +N  +M      ?V  {...}   ~N,M,,v<~:*...~>
JUST    +N          ?V  {...}   ~N,,,v<~:*...~>
JUST    +N  +M  +L  $V  {...}   ~N,M,L,v<...~>
JUST    +N  +M      $V  {...}   ~N,M,,v<...~>
JUST    +N          $V  {...}   ~N,,,v<...~>
JUST    +N  +M  +L  'C  {...}   ~N,M,L,'C<...~>
JUST    +N  +M      'C  {...}   ~N,M,,'C<...~>
JUST    +N          'C  {...}   ~N,,,'C<...~>
JUST    +N  +M  +L      {...}   ~N,M,L<...~>
JUST    +N  +M          {...}   ~N,M<...~>
JUST    +N              {...}   ~N<...~>
JUST                    {...}   ~<...~>

JUSTC   +N  +M  +L  ?V  {...}   ~N,M,L,v:@<~:*...~>
JUSTC   +N  +M      ?V  {...}   ~N,M,,v:@<~:*...~>
JUSTC   +N          ?V  {...}   ~N,,,v:@<~:*...~>
JUSTC   +N  +M  +L  $V  {...}   ~N,M,L,v:@<...~>
JUSTC   +N  +M      $V  {...}   ~N,M,,v:@<...~>
JUSTC   +N          $V  {...}   ~N,,,v:@<...~>
JUSTC   +N  +M  +L  'C  {...}   ~N,M,L,'C:@<...~>
JUSTC   +N  +M      'C  {...}   ~N,M,,'C:@<...~>
JUSTC   +N          'C  {...}   ~N,,,'C:@<...~>
JUSTC   +N  +M  +L      {...}   ~N,M,L:@<...~>
JUSTC   +N  +M          {...}   ~N,M:@<...~>
JUSTC   +N              {...}   ~N:@<...~>
JUSTC                   {...}   ~:@<...~>

JUSTL   +N  +M  +L  ?V  {...}   ~N,M,L,v@<~:*...~>
JUSTL   +N  +M      ?V  {...}   ~N,M,,v@<~:*...~>
JUSTL   +N          ?V  {...}   ~N,,,v@<~:*...~>
JUSTL   +N  +M  +L  $V  {...}   ~N,M,L,v@<...~>
JUSTL   +N  +M      $V  {...}   ~N,M,,v@<...~>
JUSTL   +N          $V  {...}   ~N,,,v@<...~>
JUSTL   +N  +M  +L  'C  {...}   ~N,M,L,'C@<...~>
JUSTL   +N  +M      'C  {...}   ~N,M,,'C@<...~>
JUSTL   +N          'C  {...}   ~N,,,'C@<...~>
JUSTL   +N  +M  +L      {...}   ~N,M,L@<...~>
JUSTL   +N  +M          {...}   ~N,M@<...~>
JUSTL   +N              {...}   ~N@<...~>
JUSTL                   {...}   ~@<...~>

JUSTR   +N  +M  +L  ?V  {...}   ~N,M,L,v:<~:*...~>
JUSTR   +N  +M      ?V  {...}   ~N,M,,v:<~:*...~>
JUSTR   +N          ?V  {...}   ~N,,,v:<~:*...~>
JUSTR   +N  +M  +L  $V  {...}   ~N,M,L,v:<...~>
JUSTR   +N  +M      $V  {...}   ~N,M,,v:<...~>
JUSTR   +N          $V  {...}   ~N,,,v:<...~>
JUSTR   +N  +M  +L  'C  {...}   ~N,M,L,'C:<...~>
JUSTR   +N  +M      'C  {...}   ~N,M,,'C:<...~>
JUSTR   +N          'C  {...}   ~N,,,'C:<...~>
JUSTR   +N  +M  +L      {...}   ~N,M,L:<...~>
JUSTR   +N  +M          {...}   ~N,M:<...~>
JUSTR   +N              {...}   ~N:<...~>
JUSTR                   {...}   ~:<...~>

LOWER   {...}                   ~(...~)

OVER    +P  +O          {...}   ...~P,O:;
OVER    +P              {...}   ...~P:;

PRINA   ""                      ""
PRINA   ?V                      ~a~:*
PRINA   $V                      ~a
PRINA   'C                      C
PRINA   %N                      N

PRINC   ?V                      ~c~:*
PRINC   $V                      ~c
PRINC   'C                      C

PRINL   +N  +M  +L  $V  $V      ~N,M,L,v@a
PRINL   +N  +M      $V  $V      ~N,M,,v@a
PRINL   +N          $V  $V      ~N,,,v@a
PRINL   +N  +M  +L      $V      ~N,M,L@a
PRINL   +N  +M          $V      ~N,M@a
PRINL   +N              $V      ~N@a
PRINL   +N  +M  +L  'C  $V      ~N,M,L,'C@a
PRINL   +N  +M      'C  $V      ~N,M,,'C@a
PRINL   +N          'C  $V      ~N,,,'C@a
PRINL   +N  +M  +L      $V      ~N,M,L@a
PRINL   +N  +M          $V      ~N,M@a
PRINL   +N              $V      ~N@a

PRINN   ?V                      ~:c~:*
PRINN   $V                      ~:c

PRINR   +N  +M  +L  $V  $V      ~N,M,L,va
PRINR   +N  +M      $V  $V      ~N,M,,va
PRINR   +N          $V  $V      ~N,,,va
PRINR   +N  +M  +L      $V      ~N,M,La
PRINR   +N  +M          $V      ~N,Ma
PRINR   +N              $V      ~Na
PRINR   +N  +M  +L  'C  $V      ~N,M,L,'Ca
PRINR   +N  +M      'C  $V      ~N,M,,'Ca
PRINR   +N          'C  $V      ~N,,,'Ca
PRINR   +N  +M  +L      $V      ~N,M,La
PRINR   +N  +M          $V      ~N,Ma
PRINR   +N              $V      ~Na

PREQx   _I  _J  _K              ~<`BRNE _I, _J``PRINx _K`~>

PRFF    +N                      ~N|
PRFF                            ~|

PRGEx   _I  _J  _K              ~<`BRLT _I, _J``PRINx _K`~>

PRGTx   _I  _J  _K              ~<`BRLE _I, _J``PRINx _K`~>

PRLEx   _I  _J  _K              ~<`BRGT _I, _J``PRINx _K`~>

PRLTx   _I  _J  _K              ~<`BRGE _I, _J``PRINx _K`~>

PRNEx   _I  _J  _K              ~<`BREQ _I, _J``PRINx _K`~>

PRZRx           _K              ~#[`PRINx _K`~]

SKIP        +N                  ~N*
SKIP                            ~*

SKIPC   'C  +N                  ~N@{`SKIPC 'C`~}
SKIPC   'C                      ~@{`BREQ $V, 'C`~}

SKIPS       +N                  `SKIPC '\f, N`
SKIPS                           `SKIPC '\f`

TABA    +N  +M                  ~N,Mt
TABA    +N                      ~Nt
TABA                            ~t

TABR    +N  +M                  ~N,M@t
TABR    +N                      ~N@t
TABR                            ~@t

TERP    +N                      ~N%
TERP                            ~%

TILDE   +N                      ~N~
TILDE                           ~~

TITLE   1   {...}               ~@(...~)
TITLE   {...}                   ~:(...~)

UPPER   {...}                   ~:@(...~)
