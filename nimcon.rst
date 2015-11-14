========================================
    Workshop
========================================

..
  Plan:

  starts at 10:30. Yuriys introduction, my talk from 11:30 to 1:30

  Sunday starts at 10:30,



.. raw:: html
  <br />
  <br />
  <br />
  <br />
  <br />
  <br />
  <br />
  <br />
  <center><big>Slides</big></center>
  <br />
  <center><big><big>git clone https://github.com/Araq/NimCon2015</big></big></center>
  <br />
  <br />
  <br />
  <center><big>Download</big></center>
  <br />
  <center><big><big><a href="http://nim-lang.org/download.html">http://nim-lang.org/download.html</a></big></big></center>


Outline
=======

1. Goals, history, philosophy
2. Basics
3. Meta programming
4. c2nim



Goals
=====

"The reasonable man adapts himself to the world: the unreasonable one persists
in trying to adapt the world to himself. Therefore all progress depends on
the unreasonable man." -- George Bernard Shaw

..
  I wanted a programming language that is

* as fast as C
* as expressive as Python
* as extensible as Lisp



The history of Nim
==================

* Development started in 2006.
* First bootstrapping succeeded in 2008.
  - Compiler written in Pascal.
  - Translated by a tool (pas2nim) to Nim
  - used pas2nim to produce the first wrappers

The history of Nim
==================

* Development started in 2006.
* First bootstrapping succeeded in 2008.
  - Compiler written in Pascal.
  - Translated by a tool (pas2nim) to Nim
  - used pas2nim to produce the first wrappers

* Goals:
  - leverage meta programming to keep the language small
  - compile to C
  - implementation size: 20_000 lines of code
  - learn from C's, C++'s, Ada's mistakes


The history of Nim (2)
======================

  "Good Software Takes Ten Years. Get Used To it." -- Joel Spolsky

- current implementation size: about 90_000 lines of code
- language is actually pretty big: generics, concepts, exceptions, procs,
  templates, macros, methods, inheritance, pointers, effect system, ...


Language influences
===================

The language borrows heavily from:

- Modula 3:
  * traced vs untraced pointers

- Delphi
  * type safe bit sets (``set of char``)
  * the parts of the syntax people don't like

- Ada
  * subrange types
  * distinct type
  * safe variants / case objects

- C++
  * Excessive overloading
  * Generic programming


Language influences
===================

- Python
  * indentation based syntax
  * programming should be fun
  * the parts of the syntax people do like

- Lisp
  * we really want a macro system
  * embrace the AST
  * homoiconicity; everything is a function application
    (well, in Nim's case ... not really)

- Oberon
  * the export marker

- C#
  * async / await
  * lambda macros


Why Nim?
========

* One language to rule them all.


Why Nim?
========

* One language to rule them all.
* "Nim is good at everything".

  - web development
  - games
  - compilers
  - operating system development
  - scientific computing
  - scripting
  - command line applications
  - UI applications
  - And lots more!


Why Nim?
========

* One language to rule them all.
* "Nim is good at everything".

  - web development
  - games
  - compilers
  - operating system development
  - scientific computing
  - scripting
  - command line applications
  - UI applications
  - And lots more!

* Convince programmers with code


Function application
====================

Function application is ``f()``, ``f(a)``, ``f(a, b)``.

And here is the sugar:

===========    ==================   ===============================
Sugar          Meaning              Example
===========    ==================   ===============================
``f a``        ``f(a)``             ``spawn log("some message")``
``a.f()``      ``f(a)``             ``db.fetchRow()``
``a.f``        ``f(a)``             ``mystring.len``
``f a, b``     ``f(a, b)``          ``echo "hello ", "world"``
``a.f(b)``     ``f(a, b)``          ``myarray.map(f)``
``a.f b``      ``f(a, b)``          ``db.fetchRow 1``
===========    ==================   ===============================


**BUT**: ``f`` does not mean ``f()``; ``myarray.map(f)`` passes ``f`` to ``map``


if statement
============

.. code-block:: nim
  # no indentation needed for single assignment statement:
  if x: x = false

  # indentation needed for nested if statement:
  if x:
    if y:
      y = false
  elif someCondition:
    echo "abc"
  else:
    y = true

  # indentation needed, because two statements follow the condition:
  if x:
    x = false
    y = false


indentation based syntax
========================

.. code-block:: nim

  if thisIsaLongCondition() and
      thisIsAnotherLongCondition(1,
         2, 3, 4):
    x = true

- Rule of thumb: optional indentation after operators, ``(`` and ``,``
- ``if``, ``case`` etc also available as expressions


if expression
=============

.. code-block:: nim
  let foo = if x < 0: "less"
            elif x == 0: "equal"
            else: "greater"


If vs when
==========

.. code-block:: nim
   :number-lines:

  when defined(posix):
    proc getCreationTime(file: string): Time =
      var res: Stat
      if stat(file, res) < 0'i32:
        let error = osLastError()
        raiseOSError(error)
      return res.st_ctime


case statement
==============

.. code-block:: nim
   :number-lines:

  type
    Directions = enum left, right, up, down

  case dir
  of left, right:
    echo "horizontal direction"
  of up, down:
    echo "vertical direction"


Var vs let vs const
===================

.. code-block:: nim
   :number-lines:

  var x = 12
  x = 80

  let y = 12
  # reassignments disallowed:
  y = 12

  # evaluated at compile time:
  const v = 12


Var vs let vs const
===================


.. code-block:: nim
   :number-lines:

  proc f(x: var int) =
    x = 22

  var a = 8
  f(a)

  let b = 4
  # disallowed:
  f(b)

  const c = 5
  # disallowed:
  f(c)


loops
=====

.. code-block:: nim
   :number-lines:

  for x in ["some", "words", "here"]:
    echo x

  for x in 0..<4:
    echo x

  var y = 0
  while y < 20:
    echo y
    y += 1



Builtin types: numbers
======================

.. code-block:: nim
  const
    myInt = 90
    myFloat = 30.0
    myInt8 = 90'i8
    myFloat32 = 30.0'f32

    myUnsignedInt = 4'u
    myUnsignedInt16 = 5'u16


Type system
===========

- strict and statically typed
- type system weakened for the meta-programming
- value based datatypes (like in C++)
- subtyping via single inheritance (``object of RootObj``)
- subtyping via ``range``: ``type Natural = range[0..high(int)]``
- generics (``HashSet[string]``)
- "concepts": constraints for generic types
- no interfaces, use (tuple of) closures instead
- no Hindley-Milner type inference, Nim embraces overloading
- limited amount of flow typing


Flow typing
===========

.. code-block:: nim
  proc f(p: ref int not nil)

  var x: ref int
  if x != nil:
    f(x)


Effect system
=============

- model effects as tuples ``(T, E)`` rather than ``E[T]``
- every effect is inferred


Effect system
=============

- tracks side effects
- tracks exceptions
- tracks "tags": ReadIOEffect, WriteIoEffect, TimeEffect,
  ReadDirEffect, **ExecIOEffect**
- tracks locking levels; deadlock prevention at compile-time
- tracks "GC safety"



Effect system
=============

.. code-block:: nim
   :number-lines:

  proc foo() {.noSideEffect.} =
    echo "is IO a side effect?"




Builtin types: arrays
=====================

- ``array[FixedSize, T]``
  * fixed size in Nim
  * value based datatypes
  * layout is compatible to C
  * create via ``[1, 2, 3]`` construction

- ``seq[T]``
  * dynamically resizable at runtime
  * grow with ``add``, resize with ``setLen``
  * create via ``@`` or ``newSeq``: ``@[1, 2, 3]``
  * allocated on the heap and GC'ed

- ``openArray[T]``
  * allows to pass ``seq`` or ``array`` to a routine
  * internally a (pointer, length) pair


Builtin types: strings
======================

.. code-block:: nim
   :number-lines:

  proc separateByComma(x: openarray[string]): string =
    result = ""
    for a in x:
      if result.len > 0: result = result & ", "
      result.add a




Builtin types: pointers
=======================

.. code-block:: nim
   :number-lines:

  proc manual(p: ptr int) =
    p[] = 12

  proc automatic(p: ref int) =
    p[] = 12

  proc mutable(p: var int) =
    p = 12

  var x: int
  mutable(x)

  var r: ref int = new(int)
  automatic(r)

  var p: ptr int = cast[ptr int](alloc(40))
  manual(p)
  dealloc p

  # or:
  manual(addr x)



Builtin types
=============

``tuple``

* value based datatypes
* structural typing
* optional field names
* construct with ``()``

.. code-block:: Nim
   :number-lines:

  proc `+-`(x, y: int): (int, int) = (x - y, x + y)
  # alternatively
  proc `+-`(x, y: int): tuple[lowerBound, upperBound: int] = (x - y, x + y)

  let tup = 100 +- 10
  echo tup[0], " ", tup.upperBound

  # tuple unpacking
  let (lower, _) = 100 +- 10


Builtin types
=============

``object``

* value based datatypes
* ``ref object`` an idiom to get reference semantics out of objects

.. code-block:: nim
   :number-lines:

  type
    Rect = object
      x, y, w, h: int

  # construction:
  let r = Rect(x: 12, y: 22, w: 40, h: 80)

  # field access:
  echo r.x, " ", r.y




Builtin types: functions
========================

- ``type Callback = proc (a, b: string) {.closure.}``
  * functions are first class in Nim
  * "calling convention" affects type compatibility
  * ``closure`` is a special calling convention (closures are GC'ed)


Builtin types: functions
========================


.. code-block:: nim
   :number-lines:

  type
    ITest = tuple[
      setter: proc(v: int) {.closure.},
      getter1: proc(): int {.closure.},
      getter2: proc(): int {.closure.}]

  proc getImpl(): ITest =
    var shared1, shared2: int

    return (setter: proc (x: int) =
              shared1 = x
              shared2 = x + 10,
            getter1: proc (): int = result = shared1,
            getter2: proc (): int = return shared2)

  var i = getImpl()
  i.setter(56)

  echo i.getter1(), " ", i.getter2()







Regular expressions
===================

.. code-block:: nim
   :number-lines:

  # Model a regular expression
  type
    RegexKind = enum          ## the regex AST's kind
      reChar,                 ## character node  "c"
      reCClass,               ## character class node   "[a-z]"
      reStar,                 ## star node   "r*"
      rePlus,                 ## plus node   "r+"
      reOpt,                  ## option node  "r?"
      reCat,                  ## concatenation node "ab"
      reAlt,                  ## alternatives node "a|b"
      reWordBoundary          ## "\b"

    RegExpr = ref object
      case kind: RegexKind
      of reWordBoundary: discard
      of reChar:
        c: char
      of reCClass:
        cc: set[char]
      of reStar, rePlus, reOpt:
        child0: RegExpr
      of reCat, reAlt:
        child1, child2: RegExpr


Equality
========

.. code-block:: nim
   :number-lines:

  proc `==`(a, b: RegExpr): bool =
    if a.kind == b.kind:
      case a.kind
      of reWordBoundary: result = true
      of reChar: result = a.c == b.c
      of reCClass: result = a.cc == b.cc
      of reStar, rePlus, reOpt: result = `==`(a.child0, b.child0)
      of reCat, reAlt: result = `==`(a.child1, b.child1) and
                                `==`(a.child2, b.child2)


Accessors
=========

.. code-block:: nim
   :number-lines:

  type
    HashTable[K, V] = object
      data: seq[(K, V)]

  proc hash[K](k: K): int = 0

  proc `[]`*[K, V](x: HashTable[K, V]; k: K): V =
    result = x.data[hash(k)][1]

  proc `[]=`*[K, V](x: var HashTable[K, V]; k: K, v: V) =
    x.data[hash(k)][1] = v


  proc initHashTable[K, V](): HashTable[K, V] =
    result.data = @[]

  var tab = initHashTable[string, string]()
  tab["key"] = "abc"  # calls '[]=' accessor

  echo tab["key"]     # calls '[]' accessor


Accessors
=========

.. code-block:: nim
   :number-lines:

  type
    HashTable[K, V] = object
      data: seq[(K, V)]

  proc hash[K](k: K): int = 0

  proc `[]`*[K, V](x: HashTable[K, V]; k: K): V =
    result = x.data[hash(k)][1]

  proc `[]=`*[K, V](x: var HashTable[K, V]; k: K, v: V) =
    x.data[hash(k)][1] = v


  proc initHashTable[K, V](): HashTable[K, V] =
    result.data = @[]

  var tab = initHashTable[string, string]()
  tab["key"] = "abc"  # calls '[]=' accessor

  echo tab["key"]     # calls '[]' accessor

  # ouch:
  tab["key"].add "xyz"


Accessors
=========

.. code-block:: nim
   :number-lines:


  proc `[]`*[K, V](x: var HashTable[K, V]; k: K): var V =
    result = x.data[hash(key)]


  var
    tab = initHashTable[string, string]()

  # compiles :-)
  tab["key"].add "xyz"




Distinct
========

.. code-block:: nim
   :number-lines:

  # Taken from system.nim
  const taintMode = compileOption("taintmode")

  when taintMode:
    type TaintedString* = distinct string
    proc len*(s: TaintedString): int {.borrow.}
  else:
    type TaintedString* = string

  proc readLine*(f: File): TaintedString {.tags: [ReadIOEffect], benign.}


Distinct
========

.. code-block:: nim
   :number-lines:
  # does not compile:

  echo readLine(stdin)

::
  nim c -r --taintMode:on taintmode_ex



Distinct
========

.. code-block:: nim
   :number-lines:
  # compiles :-)

  proc validate(input: TaintedString): string =
    result = ""
    for c in items(input.string):
      if c in {'a..'z', 'A'..'Z', '_', '0'..'9'}:
        result.add c

  echo readLine(stdin).validate

::
  nim c -r --taintMode:on taintmode_ex




Module system
=============

.. code-block::nim
   :number-lines:

  # Module A
  var
    global*: string = "A.global"

  proc p*(x: string) = echo "exported ", x


.. code-block::nim
   :number-lines:

  # Module B
  import A

  echo p(global)


Module system
=============

.. code-block::nim
   :number-lines:

  # Module A
  var
    global*: string = "A.global"

  proc p*(x: string) = echo "exported ", x


.. code-block::nim
   :number-lines:

  # Module B
  from A import p

  echo p(A.global)


Module system
=============

.. code-block::nim
   :number-lines:

  # Module A
  var
    global*: string = "A.global"

  proc p*(x: string) = echo "exported ", x


.. code-block::nim
   :number-lines:

  # Module B
  import A except global

  echo p(A.global)



Routines
========

- ``proc``
- ``iterator``
- ``template``
- ``macro``
- ``method``
- ``converter``
- (``func``)


Iterators
=========

.. code-block:: nim
   :number-lines:

  iterator `..<`(a, b: int): int =
    var i = a
    while i < b:
      yield i
      i += 1

  for i in 0..<10:
    echo i+1, "-th iteration"


Iterators
=========

.. code-block:: nim
   :number-lines:

  for x in [1, 2, 3]:
    echo x



Iterators
=========

.. code-block:: nim
   :number-lines:

  for x in [1, 2, 3]:
    echo x


Rewritten to:


.. code-block:: nim
   :number-lines:

  for x in items([1, 2, 3]):
    echo x

..
  for i, x in foobar   is rewritten to use the pairs iterator


Iterators
=========

.. code-block:: nim
   :number-lines:

  iterator items*[IX, T](a: array[IX, T]): T {.inline.} =
    var i = low(IX)
    while i <= high(IX):
      yield a[i]
      i += 1


Iterators
=========

.. code-block:: nim
   :number-lines:

  for x in [1, 2, 3]:
    x = 0      # doesn't compile



Iterators
=========

.. code-block:: nim
   :number-lines:

  var a = [1, 2, 3]
  for x in a:
    x = 0     # doesn't compile


Iterators
=========

.. code-block:: nim
   :number-lines:

  iterator mitems*[IX, T](a: var array[IX, T]): var T {.inline.} =
    var i = low(IX)
    if i <= high(IX):
      while true:
        yield a[i]
        if i >= high(IX): break
        i += 1

  var a = [1, 2, 3]
  for x in mitems(a):
    x = 0     # compiles


Concepts
========

.. code-block:: nim
   :number-lines:

  type
    Container[T] = concept c
      c.len is Ordinal
      items(c) is iterator
      for value in c:
        type(value) is T

  proc takesIntContainer(c: Container[int]) =
    for e in c: echo e


  proc takesContainer(c: Container) =
    for e in c: echo e

  takesIntContainer(@[1, 2, 3])
  takesContainer(@[4, 5, 6])
  takesContainer(@["a", "b"])
  takesContainer "test"


Concepts (2)
============

.. code-block:: nim
   :number-lines:

  type
    Node* = concept n
      `==`(n, n) is bool

    Graph* = concept g
      var x: Node
      distance(g, x, x) is float

    XY* = tuple[x, y: int]

    MyGraph* = object
      points: seq[XY]

  if XY is Node:
    echo "XY is Node"

  proc distance*(g: MyGraph, a, b: XY): float =
    sqrt(pow(float(a.x - b.x), 2) + pow(float(a.y - b.y), 2))

  if MyGraph is Graph:
    echo "MyGraph is Graph"



Templates (1)
=============

.. code-block::nim
   :number-lines:

  template `??`(a, b: untyped): untyped =
    let x = a
    (if x.isNil: b else: x)

  var x: string
  echo x ?? "woohoo"


Templates (2)
=============

.. code-block:: nim
   :number-lines:

  proc signalHandler(sig: cint) =
    template processSignal(s, action: untyped) =
      if s == SIGINT: action("SIGINT: Interrupted by Ctrl-C.\n")
      elif s == SIGSEGV:
        action("SIGSEGV: Illegal storage access. (Attempt to read from nil?)\n")
      elif s == SIGBUS:
        action("SIGBUS: Illegal storage access. (Attempt to read from nil?)\n")
      else:
        action("unknown signal\n")

    when hasSomeStackTrace:
      var buf = newStringOfCap(2000)
      rawWriteStackTrace(buf)
      processSignal(sig, buf.add) # nice hu? currying a la Nim :-)
      showErrorMessage(buf)
    else:
      var msg: cstring
      template asgn(y: expr) = msg = y
      processSignal(sig, asgn)
      showErrorMessage(msg)



Templates (3)
=============

.. code-block:: nim
   :number-lines:

  proc drawLine(context: Context; color: Color; p1, p2: Point) = ...

  proc drawRect(c: Context; col: Color; p1, p2: Point) =
    drawLine c, col, p1, Point(...)
    drawLine c, col, p1, Point(...)
    drawLine c, col, p2, Point(...)
    drawLine c, col, p2, Point(...)


Templates (3)
=============

.. code-block:: nim
   :number-lines:

  var drawColor {.threadvar.}: Color

  proc setDrawColor*(context: Context; col: Color) =
    drawColor = col

  proc drawLine(context: Context; p1, p2: Point) = ...

  proc drawRect(c: Context; p1, p2: Point) =
    drawLine c, p1, Point(...)
    drawLine c, p1, Point(...)
    drawLine c, p2, Point(...)
    drawLine c, p2, Point(...)



Templates (3)
=============

.. code-block:: nim
   :number-lines:

  proc drawLine(context: Context; color: Color; p1, p2: Point) = ...

  template drawLine(p1, p2: Point) =
    drawLine(context, color, p1, p2)

  template drawRect(p1, p2: Point) =
    drawLine p1, Point(...)
    drawLine p1, Point(...)
    drawLine p2, Point(...)
    drawLine p2, Point(...)


Templates (3)
=============

.. code-block:: nim
   :number-lines:

  proc main =
    const color = White
    var context = newContext()
    drawRect Rect(...)
    drawRect Rect(...)


Templates (4)
=============

.. code-block:: nim
   :number-lines:

  proc threadTests(r: var Results, cat: Category, options: string) =
    template test(filename: untyped) =
      testSpec r, makeTest("tests/threads" / filename, options, cat, actionRun)
      testSpec r, makeTest("tests/threads" / filename, options &
        " -d:release", cat, actionRun)
      testSpec r, makeTest("tests/threads" / filename, options &
        " --tlsEmulation:on", cat, actionRun)

    test "tactors"
    test "tactors2"
    test "threadex"


Macros
======

.. code-block:: nim

  test "tactors"
  test "tactors2"
  test "threadex"

-->

.. code-block:: nim

   test "tactors", "tactors2", "threadex"


Macros
======

.. code-block:: nim
   :number-lines:
  import macros

  macro lift(caller: untyped; args: varargs[untyped]): untyped =
    result = newStmtList()
    for a in args:
      result.add(newCall(caller, a))

  lift test, "tactors", "tactors2", "threadex"


Macros
======

.. code-block::nim
   :number-lines:

  proc write(f: File; a: int) =
    echo a

  proc write(f: File; a: bool) =
    echo a

  proc write(f: File; a: float) =
    echo a

  proc writeNewline(f: File) =
    echo "\n"

  macro writeln*(f: File; args: varargs[typed]) =
    result = newStmtList()
    for a in args:
      result.add newCall(bindSym"write", f, a)
    result.add newCall(bindSym"writeNewline", f)


Quoting
=======

.. code-block::nim
   :number-lines:

  import macros

  macro quoteWords(names: varargs[untyped]): untyped =
    result = newNimNode(nnkBracket)
    for i in 0..names.len-1:
      expectKind(names[i], nnkIdent)
      result.add(toStrLit(names[i]))

  const
    myWordList = quoteWords(this, an, example)

  for w in items(myWordList):
    echo w



Currying
========

.. code-block::nim
   :number-lines:

  proc f(a, b, c: int): int = a+b+c

  echo curry(f, 10)(3, 4)


Currying
========

.. code-block::nim
   :number-lines:

  proc f(a, b, c: int): int = a+b+c

  echo((proc (b, c: int): int = f(10, b, c))(3, 4))


Currying
========

.. code-block::nim
   :number-lines:

  macro curry(f: typed; args: varargs[untyped]): untyped =
    let ty = getType(f)
    #echo treerepr ty
    assert($ty[0] == "proc", "first param is not a function")
    let n_remaining = ty.len - 2 - args.len
    assert n_remaining > 0, "cannot curry all the parameters"
    var callExpr = newCall(f)
    args.copyChildrenTo callExpr

    var params: seq[NimNode] = @[]
    # return type
    params.add ty[1]

    for i in 0 .. <n_remaining:
      let param = ident("arg"& $i)
      params.add newIdentDefs(param, ty[i+2+args.len])
      callExpr.add param
    result = newProc(procType = nnkLambda, params = params, body = callExpr)


Slides
======

::
  git clone https://github.com/Araq/NimCon2015
  nim c -r build.nim



CPU emulation
=============

.. code-block::nim
   :number-lines:

  type
    Wdc65C02* = ref object
      ## Emulates the WDC 65C02 microprocessor, which is an enhanced CMOS version
      ## of the popular MOS Technology 6502 microprocessor with an extended
      ## instruction set and some bug fixes.
      bus: Wdc65C02Bus    ## The memory bus that this CPU is attached to
      a: uint8            ## Accumulator
      p: uint8            ## Processor status flags
      pc: uint16          ## Program counter
      s: uint8            ## Stack (pointer) register (8 + 1 bits; high bit is always `1`)
      x: uint8            ## X index register
      y: uint8            ## Y index register
      cycles: int64       ## Total number of cycles executed so far
      goalCycles: int64   ## Total nmber of cycles to execute up to



CPU emulation
=============

.. code-block::nim
   :number-lines:

  template pop(v: uint8) =
    # Pop an 8-bit value from the stack.
    inc x.s
    v = x.bus.read(stackBase + x.s)


  template popWord(v: uint16) =
    # Pop a 16-bit value from the stack.
    inc x.s
    v = x.bus.readWord(stackBase + x.s)
    inc x.s

  template push(v: uint8) =
    # Push an 8-bit value onto the stack.
    x.bus.write(stackBase + x.s, v)
    dec x.s


CPU emulation
=============

.. code-block::nim
   :number-lines:

  const
    addrModes = [
      #  0   |   1   |   2   |   3   |   4   |   5   |   6   |   7
      "imp",  "indx", "imp",  "indx", "zp",   "zp",   "zp",   "zp",  # 0
      "rel",  "indy", "imp",  "indy", "zpx",  "zpx",  "zpx",  "zpx", # 1
      "abso", "indx", "imp",  "indx", "zp",   "zp",   "zp",   "zp",  # 2
      ...
    ]

    ops = [
      #  0   |   1   |   2   |   3   |   4   |   5   |   6   |   7
      "brk",  "ora",  "nop",  "slo",  "nop",  "ora",  "asl",  "slo", # 0
      "bpl",  "ora",  "nop",  "slo",  "nop",  "ora",  "asl",  "slo", # 1
      "jsr",  "and",  "nop",  "rla",  "bit",  "and",  "rol",  "rla", # 2
      ...
    ]

    cycles = [
      #  0   |   1   |   2   |   3   |   4   |   5   |   6   |   7
         7,      6,      2,      8,      3,      3,      5,      5,  # 0
         2,      5,      2,      8,      4,      4,      6,      6,  # 1
         ...
    ]


CPU emulation
=============

.. code-block::nim
   :number-lines:

  macro generateDispatcher(): untyped =
    # Expand opcode tables into switch block for instruction execution.
    #
    # dumpTree:
    #   case opcode
    #   of 0:
    #     impAddr()
    #     brkOp()
    #     inc x.cycles, 7
    #   ...
    # ----------------------------------------------------
    # CaseStmt
    #   Ident !"opcode"
    #   OfBranch
    #     IntLit 0
    #     StmtList
    #       Call
    #         Ident !"impAddr"


CPU emulation
=============

.. code-block::nim
   :number-lines:

  macro generateDispatcher(): untyped =
    let caseStmt = newTree(nnkCaseStmt, newIdentNode("opcode"))
    for i in 0..255:
      caseStmt.add(
        newTree(nnkOfBranch,
          newIntLitNode(i),
          newTree(nnkStmtList,
            newCall(
              newIdentNode(addrModes[i] & "Addr"),
              ...
            )
          )
        )
      )
    result = newStmtList()
    result.add(caseStmt)


CPU emulation
=============

.. code-block::nim
   :number-lines:

  proc execute(x: Wdc65C02) =
    ## Execute a single instruction.
    var ea: uint16 = 0
    var ra: uint16 = 0
    var penaltyOp = false
    var penaltyAddr = false
    let opcode = x.bus.read(x.pc)
    generateDispatcher()
    if penaltyOp and penaltyAddr:
      inc x.cycles



Interfacing with C
==================

2 options

- via ``dynlib``
- via ``header``


Dynlib import
=============

.. code-block:: Nim
   :number-lines:
  type
    GtkWidget = object
      data: cint
      binary: cfloat
      compatible: char

  proc gtk_image_new(): ptr GtkWidget
    {.cdecl, dynlib: "libgtk-x11-2.0.so", importc.}



Header import
=============

.. code-block::
   :number-lines:

  type
    GtkWidget {.importc: "GtkWidget_t", header: "<gtk.h>".} = object
      data {.importc: "Data".}: cint
      binary {.importc: "Binary".}: cfloat
      compatible: char

  proc gtk_image_new(): ptr GtkWidget
    {.cdecl, header: "<gtk.h>", importc.}

  {.passC: staticExec("pkg-config --cflags gtk").}
  {.passL: staticExec("pkg-config --libs gtk").}



Header import
=============

.. code-block::
   :number-lines:

  proc printf(formatstr: cstring)
    {.header: "<stdio.h>", importc: "printf", varargs.}

  printf("%s%s", "Nim strings ", "converted to cstring for you")


Data exchange with C
====================

=================   ==========================================================
C type              Nim type
=================   ==========================================================
``int``             ``cint``
``unsigned long``   ``culong``
``float``           ``cfloat``
``int x[4]``        ``array[4, cint]``
``int*``            ``ptr int``
``char*``           ``cstring``
``char**``          ``cstringArray = ptr array [0..ArrayDummySize, cstring]``
=================   ==========================================================


Data exchange with C
====================

.. code-block:: C
   :number-lines:

  int sum(int* x, size_t len) {
    int result = 0;
    for (size_t i = 0; i < len; i++)
      result += x[i];
    return result;
  }


Data exchange with C
====================

.. code-block:: C
   :number-lines:

  int sum(int* x, size_t len) {
    int result = 0;
    for (size_t i = 0; i < len; i++)
      result += x[i];
    return result;
  }

.. code-block:: Nim
   :number-lines:

  proc sum(x: ptr cint; len: int): cint
    {.importc: "sum", cdecl, header: "foo.h".}

  proc callSum =
    var x = @[1.cint, 2, 3, 4]
    echo sum(addr x[0], x.len)

    var y = [1.cint, 2, 3, 4]
    echo sum(addr y[0], y.len)


Data exchange with C (2)
========================

.. code-block:: Nim
   :number-lines:

  proc cstringArrayToSeq*(a: cstringArray, len: Natural): seq[string] =
    ## converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
    ## of length ``len``.
    newSeq(result, len)
    for i in 0..len-1: result[i] = $a[i]

  proc cstringArrayToSeq*(a: cstringArray): seq[string] =
    ## converts a ``cstringArray`` to a ``seq[string]``. `a` is supposed to be
    ## terminated by ``nil``.
    var L = 0
    while a[L] != nil: inc(L)
    result = cstringArrayToSeq(a, L)


Data exchange with C (2)
========================

.. code-block:: Nim
   :number-lines:

  proc allocCStringArray*(a: openArray[string]): cstringArray =
    ## creates a NULL terminated cstringArray from `a`. The result has to
    ## be freed with `deallocCStringArray` after it's not needed anymore.
    result = cast[cstringArray](alloc0((a.len+1) * sizeof(cstring)))
    let x = cast[ptr array[0..ArrayDummySize, string]](a)
    for i in 0 .. a.high:
      result[i] = cast[cstring](alloc0(x[i].len+1))
      copyMem(result[i], addr(x[i][0]), x[i].len)

  proc deallocCStringArray*(a: cstringArray) =
    ## frees a NULL terminated cstringArray.
    var i = 0
    while a[i] != nil:
      dealloc(a[i])
      inc(i)
    dealloc(a)



CodegenDecl pragma
==================


.. code-block:: nim
   :number-lines:

  var
    a {.codegenDecl: "$# progmem $#".}: int

  proc myinterrupt() {.codegenDecl: "__interrupt $# $#$#".} =
    echo "realistic interrupt handler"





Wrapping C++
============

.. code-block:: C++
   :number-lines:

  class Foo {
  public:
    int value;
    int GetValue() { return value; }
    int& SetValue(int x) { field = x; return &field; }
  };

.. code-block:: Nim
   :number-lines:

  type
    Foo* {.importcpp: "Foo", header: "file.h".} = object
      value*: cint

  proc getValue*(this: var Foo): cint
    {.importcpp: "GetValue", header: "file.h".}
  proc setValue*(this: var Foo; x: cint): var cint
    {.importcpp: "SetValue", header: "file.h".}


Wrapping C++
============

.. code-block:: C++
   :number-lines:

  class Foo {
  public:
    int value;
    int GetValue() { return value; }
    int& SetValue(int x) { field = x; return &field; }
  };

.. code-block:: Nim
   :number-lines:

  type
    Foo* {.importcpp: "Foo", header: "file.h".} = object
      value*: cint

  proc getValue*(this: var Foo): cint
    {.importcpp: "#.GetValue(@)", header: "file.h".}
  proc setValue*(this: var Foo; x: cint): var cint
    {.importcpp: "#.SetValue(@)", header: "file.h".}



Constructors
============

.. code-block:: C++
   :number-lines:

  class Foo {
  public:
    int value;
    int GetValue() { return value; }
    int& SetValue(int x) { field = x; return &field; }

    Foo(int x): field(x) {}
  };

.. code-block:: Nim
   :number-lines:

  type
    Foo* {.importcpp: "Foo", header: "file.h".} = object
      value*: cint

  proc getValue*(this: var Foo): cint
    {.importcpp: "#.GetValue(@)", header: "file.h".}
  proc setValue*(this: var Foo; x: cint): var cint
    {.importcpp: "#.SetValue(@)", header: "file.h".}

  proc constructFoo*(x: cint): Foo
    {.importcpp: "Foo(@)", header: "file.h".}


Constructors
============

.. code-block:: C++
   :number-lines:

  Foo foo = Foo(1, 2, 3);

  auto foo = Foo(1, 2, 3);


Constructors
============

.. code-block:: C++
   :number-lines:

  Foo foo = Foo(1, 2, 3);
  // Calls copy constructor!
  auto foo = Foo(1, 2, 3);


Constructors
============

.. code-block:: C++
   :number-lines:

  Foo foo = Foo(1, 2, 3);
  // Calls copy constructor!
  auto foo = Foo(1, 2, 3);

  Foo foo(1, 2, 3);


Constructors
============

.. code-block:: Nim
   :number-lines:

  proc constructFoo*(x: cint): Foo
    {.importcpp: "Foo(@)", header: "file.h", constructor.}


.. code-block:: nim
   :number-lines:

  proc newFoo(a, b: cint): ptr Foo {.importcpp: "new Foo(@)".}

  let x = newFoo(3, 4)


  proc cnew*[T](x: T): ptr T {.importcpp: "(new '*0#@)", nodecl.}



Generics
========

For example:

.. code-block:: nim
   :number-lines:

  type Input {.importcpp: "System::Input".} = object
  proc getSubsystem*[T](): ptr T
    {.importcpp: "SystemManager::getSubsystem<'*0>()", nodecl.}

  let x: ptr Input = getSubsystem[Input]()

Produces:

.. code-block:: C
   :number-lines:

  x = SystemManager::getSubsystem<System::Input>()



Emit pragma
===========

.. code-block:: Nim
   :number-lines:

  {.emit: """
  static int cvariable = 420;
  """.}

  {.push stackTrace:off.}
  proc embedsC() =
    var nimVar = 89
    # use backticks to access Nim symbols within an emit section:
    {.emit: """fprintf(stdout, "%d\n", cvariable + (int)`nimVar`);""".}
  {.pop.}

  embedsC()


Parallelism
===========

.. code-block::nim
   :number-lines:

  import tables, strutils

  proc countWords(filename: string): CountTableRef[string] =
    ## Counts all the words in the file.
    result = newCountTable[string]()
    for word in readFile(filename).split:
      result.inc word


Parallelism
===========

.. code-block::nim
   :number-lines:

  #
  #
  const
    files = ["data1.txt", "data2.txt", "data3.txt", "data4.txt"]

  proc main() =
    var tab = newCountTable[string]()
    for f in files:
      let tab2 = countWords(f)
      tab.merge(tab2)
    tab.sort()
    echo tab.largest

  main()


Parallelism
===========

.. code-block::nim
   :number-lines:

  import threadpool

  const
    files = ["data1.txt", "data2.txt", "data3.txt", "data4.txt"]

  proc main() =
    var tab = newCountTable[string]()
    var results: array[files.len, ***FlowVar[CountTableRef[string]]***]
    for i, f in files:
      results[i] = ***spawn*** countWords(f)
    for i in 0..high(results):
      tab.merge(*** ^results[i] ***)
    tab.sort()
    echo tab.largest

  main()


Parallelism
===========

.. code-block::nim
   :number-lines:

  import strutils, math, threadpool

  proc term(k: float): float = 4 * math.pow(-1, k) / (2*k + 1)

  proc computePi(n: int): float =
    var ch = newSeq[FlowVar[float]](n+1)
    for k in 0..n:
      ch[k] = spawn term(float(k))
    for k in 0..n:
      result += ^ch[k]


Happy hacking!
==============

============       ================================================
Website            http://nim-lang.org
Mailing list       http://www.freelists.org/list/nim-dev
Forum              http://forum.nim-lang.org
Github             https://github.com/nim-lang/Nim
IRC                irc.freenode.net/nim
============       ================================================

