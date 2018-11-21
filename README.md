# Lyra Lang

A dynamic, mostly functional programming language, featuring:

- immutable values
- lexically scoped closures
- prototype-based polymorphism
- an event loop

It's like somebody combined the worst parts of clojure and javascipt.

## General Syntax

Comments begin with `#` and extend until the next LINEFEED (ASCII 0x20). Whitespace is either SPACE (ASCII 0x20), LINEFEED (ASCII 0x0a) or a comment. All whitespace is completely ignored.

Identifiers begin with an underscore (`_`) or an ASCII alphabetic character, and continue with any number of underscores and/or ASCII alphanumerics,

## Values

All values are immutable.

### Nil

The unit type. Literal: `nil`.

### Bool

True or false. Literals: `true` and `false`.

### Rational

Arbitrary precision raional numbers.

Literals: decimal (any number of decimal digits and underscores, underscores are ignored, must begin with a non-underscore), or hex (`0x` followed by at least one hexadecimal digit (case-insensitive) and underscores, underscores are ignored, first digit must be a non-underscore). No negative literals (unary minus is an operator), no fractional literals (division is an operator).

### Float

A 64 bit IEE 754 floating point number. Literals: all valid JSON numbers without sign, `NaN`, `Infinity`. Literals use rounding mode "Round to nearest, ties to even" if they can not be represented exactly.

### Char

A unicode scalar value.

### String

Ordered sequence of valid utf8 bytes. Has at worst logarithmic time complexity for pretty much everything: Concatenation, insertion, deletion, splitting, indexing the n-th scalar value. Persistent ropes are cool. Also exposes some useful unicode and utf8 functionality I suppose.

### Sequence

Ordered sequence of elements, supporting logarithmic indexing by index, concatenation, insertion, deletion, splitting (persistent ropes again).

### Set

Unordered sequence of elements, supporting logarithmic insertiong, deletion, membership test. TODO: Guarantee an iteration order (based on a total order on values)? How to handle functions?

### Map

Bla, standard immutable maps. All the TODOs from Set apply as well.

### Future
TODO figure this out

Lazy, cancellable, always go through the event loop (no synchronous usage)?

Only interface to the event loop?

Join, select?

Syntax sugar?

### Function

Closures, capturing their environment. Can access their args as a sequence for dynamic shenanigans.

### FFI stuff?
TODO figure this out

- only allow pure functions?

## Stuff

- number coercion?
- if/else (type coercion?)
- exceptions?
- event loop?
  - structural concurrency?
  - frp?
- module loading?
- typeof operator
- guarantee string literal interning?
- mutually recursive functions?
- special members for manipulating operator behavior?
- (conditional) breakpoints
- prototype setting
- prototype access
- self keyword in functions
- standard "type classes" (pattern for builtin prototype entries to follow)
- standard functions (or operators?)
  - typeof
  - prototype stuff
  - non-prototype-based primitive manipulation
- |> operator?
- apply a function to a list

### Syntax Sugar

- async/await sugar? minimal syntax via `<foo> ~> {}` declarations and `bar<foo>` calls?
- sugar for map access? `foo.bar` for `get(foo, "bar")`, `foo.2` for `get(foo, 2)`

### Non-literal expressions

- application
- let
- throw
- try <expression> catch <expression>
- if
- case
- pause
- pause if <expression>
- trace <expression> (?)
- trace <expression> if <expression> (?)
- self
- operators

maybe expressions, maybe functions:

- typeof
- prototype access
- prototype assoc
- self binding

### Built-in Functions
- halt
- operations on primitives
- apply function to a list
- eval
- to_string
- trace

## Advanced Stuff
- coverage and fuzzing
- memory sharing among multiple processes
