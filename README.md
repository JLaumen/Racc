# Racc

Racc is a programming language for algorithm design. Code reads like
pseudocode, compiles to fast C, and is proven free of runtime errors at
compile time.

```
-- Steps for n to reach 1 in the Collatz sequence
collatz : Int -> Int
collatz n =
  state
    x = n
    steps = 0
  while x != 1
    if x % 2 == 0
      x = x / 2
    else
      x = 3 * x + 1
    steps = steps + 1
  return steps

main =
  print (collatz 27)
```

## Status

The language is being designed. The compiler checks names in programs but
does not generate code yet. To check a program and see its syntax tree:

```
cabal run racc resolve FILE.racc
```

- `DESIGN.md`: what programs mean.
- `GRAMMAR.md`: the syntax.
