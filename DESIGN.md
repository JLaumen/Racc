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

- A function is defined by equations, tried from top to bottom. A clause
  whose guards all fail falls through to the next clause.
- Every `match` and every function must cover all values (checked at
  compile time). Patterns in bindings must always match.
- Top-level declarations may appear in any order.
- Functions are pure unless they mutate their parameters or have effects.
- The program starts at `main`.

## Types

- `Int` is unbounded; small values are stored as machine integers. `/`
  rounds down, and `%` has the sign of the divisor.
- `Bool` is an ordinary type with constructors `False` and `True`.
- Type classes (`Eq`, `Ord`, `Num`, `Show`, ...) provide overloading.

## Bindings and mutation

Inside a body, `x = e` assigns if `x` is a place, and otherwise binds a new
immutable local. A place is a state variable, a mutated parameter, or a
field or element of a place.

- A local is bound once; assigning it again is an error.
- A parameter cannot be reassigned as a whole; assigning into it
  (`xs[i] = v`) mutates the caller's value.
- A tuple on the left is all places or all new names. Assigning to places is
  simultaneous: `(a, b) = (b, a + b)`.
- A statement whose value is not `()` is discarded with a warning; so is an
  unused local, unless its name starts with `_`.

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

Every call passes all arguments, and division by zero is checked at runtime.
