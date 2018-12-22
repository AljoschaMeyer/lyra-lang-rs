# The Lyra Programming Language

Lyra is a dynamically typed, imperative programming language. A distinct feature is the use of immutable values, even for compound data structures like arrays. Mutation only happens by assigning different values to variables, and aside from this feature, the language is functional. Values themselves do not change, except for the state of variables in the captured environment of a closure.

The built-in values are a nil value, booleans, arbitrary-size rational numbers (but no floats), characters (unicode scalar values), unicode strings, byte strings, ordered sequences, sets, maps, and first-class functions (or lexically scoped closures to be more precise). Finally there are futures, which provide the interface to the event loop used for nonblocking operations in the otherwise single-threaded execution.

The data types have been chosen to satisfy useful algebraic or algorithmic properties, among them:

- equality is reflexive (the float NaN violates this in other languages)
- if two values compare as equal, pure functions will compute the same result when given either of them as input (the floats 0.0 and -0.0 violate this in other languages)
- equality is based on value equality rather than pointer identity, except for functions and futures (where value identity is undecidable and/or meaningless due to mutable state)
- there's a total order over all values (unspecified for functions and futures, fully deterministic for all other values)
- characters are always valid unicode scalar values, strings are always valid unicode strings
- strings and sequences guarantee logarithmic indexing, concatenation and slicing time complexity
- sets and maps guarantee logarithmic retrieval, insertion and deletion time complexity
- futures are lazy and can be cancelled

The language provides the ability to evaluate source code strings, and it also comes with a module-loading system. The ability to dynamically execute (potentially unknown) code requires an exception handling mechanism. This is implemented through standard throw/try/catch constructs - any value can be thrown.

Even though the language is dynamically typed and provides the ability to evaluate source code strings, it is otherwise fairly non-dynamic. There is no way to programatically inspect or set the environment in which a function executes, and a function containing undeclared variables results in a syntax error rather than a runtime error. Native errors (such as type errors, or division by zero) do not include source code locations. Note that this does not preclude the runtime from presenting error locations to the programmer, but programs can't dynamically inspect them. There's no mechanism for introspection/reflection either.

Beyond these design decisions, lyra is mostly unopinionated. The language itself does not prescribe how I/O happens, all I/O facilities are provided by the runtime and must be loaded just like regular modules. A runtime might specify how native modules can be implemented via an FFI, lyra does not impose this either. The module-loading system expects code to live at specific places in the file system, but otherwise doesn't care how it got there, i.e. there's no mandatory versioning or dependency-resolution scheme, let alone a package manager.

There's one final peculiarity about lyra: It's definition is static. Lyra isn't versioned, there won't be any feature additions, there won't be any changes to the specification. If anyone (including the original author) wants to "fix" something, or add new things, they will have to fork. The new language won't be lyra.

Though many languages influenced the design of lyra, the two most direct influences are clojure and javascript. Some good (but still imprecise) approximations of lyra are:

- clojure, but with an event loop, syntax, and more lenient about mutable state
- javascript, but with immutable values, no object-orientation, and lazy, cancellable futures rather than callbacks

Whether these design decisions are actually improvements over those other languages can't be said objectively (well, chances are you are actually better off with clojure). But hopefully, lyra occupies an interesting place in the language design space and can enable productive and enjoyable programming.

---

Toplevel values

- halt
- is_nil, is_bool, is_foo
- is_truthy
- toplevel (returns the current toplevel as a map from strings (holding the identifiers) to the values)
- eval (string, second arg is a map that becomes the toplevel env (error on non-identifier keys), defaults toplevel())
- eval_pure (? see require_pure ?)
- require
- require_pure (should this alter the behavior of nested require, or should it disallow it?)
- require_async
- require_pure_async
- constants for some errors/error tags?

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
