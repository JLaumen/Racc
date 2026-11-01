# Racc design

Racc is a language for algorithm design: code reads like pseudocode, runs
fast, and never fails at runtime. This file describes what programs mean;
the syntax is in `GRAMMAR.md`.

## Overview

| Area         | Decision                                                      |
|--------------|---------------------------------------------------------------|
| Types        | Hindley-Milner inference; signatures optional; type classes   |
| Safety       | No runtime errors: risky operations are proven safe at compile time |
| Mutation     | Only declared `state` and inferred mutated parameters; no aliasing |
| Effects      | Inferred; optionally stated in an `effects` clause            |
| Memory       | Reference counting with copy-on-write                         |
| Compilation  | To C; the compiler is written in Haskell                      |

## Functions

- A function is defined by equations, tried from top to bottom; local
  functions too. A clause whose guards all fail falls through to the next
  clause.
- A value is a function without parameters.
- A signature line gives the type of the function or value defined directly
  after it, also for state variables.
- Every `match` and every function must cover all values (checked at
  compile time). Patterns in bindings must always match.
- Top-level declarations may appear in any order. A top-level value may
  not depend on itself, directly or through functions; values are evaluated
  in the order of their dependencies.
- A block's value is that of its last statement. An `if` with `else` and a
  `match` have the value of their branches; other statements have the
  value `()`. `return`, `break`, `continue`, and a `while True` without a
  `break` for it never finish, so they fit any type.
- Functions are pure unless they mutate their parameters or have effects.
- Every program has a `main` of type `Void`, where it starts.

## Scope

- Top-level names are visible everywhere. Types, constructors, and values
  have separate namespaces.
- In a body, a name is visible from its definition to the end of its block
  (an `if` branch, a loop body, a `match` arm, ...). A local function can
  call itself; a local value cannot refer to itself.
- Within one function a name has one meaning: defining a name that is in
  scope from the same function is an error, also in an inner block. Parameters and locals
  may shadow top-level names and, in a local function, the names of the
  enclosing functions.
- A local function sees the names of the enclosing functions and can assign
  their state. If it uses their state, it can only be called, not passed as
  a value.
- Built-ins are defined in Racc, in a prelude, except primitives that Racc
  cannot express or that must be fast, such as `Int` and `print`. They
  cannot be redefined at top level.

## Types

- Every equation, also a local one, is generalized: what it defines can be
  used at several types. Pattern bindings, parameters, and state variables
  are not.
- A signature may give a more specific type than the inferred one, not a
  more general one.
- The type variables of a signature are in scope in the function's body,
  so local signatures can refer to them. A state variable's signature can
  only use those.
- `Void` is the type of `()`.
- `Int` is unbounded; small values are stored as machine integers. `/`
  rounds down, and `%` has the sign of the divisor.
- `Bool` is an ordinary type with constructors `False` and `True`.
- Type classes (`Eq`, `Ord`, `Num`, `Show`, ...) provide overloading.

## Bindings and mutation

Inside a body, `x = e` assigns if `x` is a place, is an error if `x` is
another local of the same function, and otherwise binds a new immutable
local. A place is a state variable, a mutated parameter, or a field or
element of a place.

- A parameter cannot be reassigned as a whole; assigning into it
  (`xs[i] = v`) mutates the caller's value.
- A pattern on the left, such as a tuple, assigns only places or binds only
  new names. Assigning to places is simultaneous: `(a, b) = (b, a + b)`.
- A statement whose value is not `()` is discarded with a warning; for an
  `if` or `match`, each branch's value is discarded on its own, so their
  types may differ.
- An unused local (a parameter, pattern variable, binding, state variable,
  or local function) gives a warning, unless its name starts with `_`.

### State

A function whose body starts with a `state` block is an algorithm. The block
lists everything the function may mutate. State is local, so the function is
pure from the outside.

```
dijkstra : Graph -> Int -> List Int
dijkstra g src =
  state
    dist = fill (size g) infinity
    pq = PriorityQueue.from [(0, src)]
  dist[src] = 0
  while not (isEmpty pq)
    (d, u) = pop pq
    if d <= dist[u]
      for (v, w) in g[u]
        if d + w < dist[v]
          dist[v] = d + w
          push pq (dist[v], v)
  dist
```

### Mutated parameters

A parameter is mutated if the body assigns into it or passes it to a
function that mutates that argument; this is inferred. A `modifies` clause
can state it, and is then checked.

```
find parent x =
  if parent[x] != x
    parent[x] = find parent parent[x]
  parent[x]
```

Rules that rule out aliasing:

1. A mutated argument must be a place.
2. One call may not pass the same place to two mutated parameters. Distinct
   fields are allowed; elements of the same list are not.
3. A mutated parameter cannot be returned, stored, or captured.
4. A function with mutated parameters can only be called where a place
   exists, and cannot be passed as a value.

### Structures

A structure is a type with state and operations on it; whether an operation
mutates the structure is inferred. Built-in containers are structures. The
syntax is not designed yet.

## Effects

Effects (`io`, ...) are inferred. An `effects` clause can state them,
including parameters whose effects are passed through (`effects f`). An
empty clause states that a function is pure.

A value is evaluated once. A dedicated effect, stated in the value's
signature, makes it evaluated at each use instead (so `roll = rand 1 10`
can give a new number each time). Its name and details are not designed
yet.

## Safety

Every operation that could fail creates a proof obligation. The compiler
proves it with an SMT solver (Z3) or rejects the program.

| Operation        | Obligation                        |
|------------------|-----------------------------------|
| `xs[i]`          | `0 <= i < len xs`                 |
| `x / y`, `x % y` | `y != 0`                          |
| `m[k]`           | `k` is a key of `m`               |
| call to `f`      | `f`'s `requires` clauses hold     |
| end of `f`       | `f`'s `ensures` clauses hold      |
| loop iteration   | the loop's `invariant`s hold      |

Contracts give the prover facts it cannot find itself; they are not checked
at runtime. Non-termination, stack overflow, and running out of memory are
not covered. Until the verifier exists, obligations are checked at runtime.

## Collections

Each collection only has operations that are cheap for it.

| Type              | Operations                                          |
|-------------------|-----------------------------------------------------|
| `List a`          | fixed size; O(1) index, update, length, slice       |
| `Deque a`         | O(1) index; amortized O(1) push and pop at both ends |
| `PriorityQueue a` | O(log n) push and pop-min; O(1) peek                |
| `Map k v`         | O(log n) lookup, insert, delete                     |
| `Set a`           | O(log n) membership, insert, delete                 |
| `LinkedList a`    | O(1) prepend, head, tail                            |

- `[...]` builds a `List`, `{...}` a `Set` or `Map`, and `(...)` with a
  comprehension a `Seq`.
- A slice `xs[a until b]` is a view, not a copy.
- `get m k` returns an `Option`, without a proof obligation.

## Sequences

- Ranges and `(...)` comprehensions are lazy sequences (`Seq a`); everything
  else is strict.
- `for` iterates over ranges, sequences, and collections.
- A function whose body contains `yield` returns a `Seq`.
- Iterating a `Seq` again restarts it.
- A generator cannot mutate its parameters.

## Memory

Values have value semantics. Heap values are reference counted; writing to a
shared value copies it first. Without references, values cannot form
cycles, so reference counting frees everything. The compiler warns where it
inserts a copy.

## Version 0.1

- `Int`, `Bool`, tuples, and `type` declarations.
- Functions with equations, guards, and `match`; local functions; `main`.
- `state`, assignment, `if`, `while`, `for` over ranges, `break`,
  `continue`, `return`.
- A built-in `print` for `Int` and `Bool`.
- Reference counting.

A range may only appear directly after `for ... in`. Without type
classes, arithmetic, bitwise, and ordering operators and ranges work on
`Int`, `==` and `!=` on `Int` and `Bool`, and `print` takes
`Int` or `Bool`. Where `==`, `!=`, or `print` is used, the type must be
known by the end of the definition that uses it.

Every call passes all arguments, and division by zero is checked at runtime.
