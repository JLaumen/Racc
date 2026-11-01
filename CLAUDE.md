# Racc

A language for algorithm design. The compiler is written in Haskell and
emits C.

- `README.md`: introduction, for users.
- `DESIGN.md`: what programs mean.
- `GRAMMAR.md`: the syntax; alternatives marked `not in v0` are not
  implemented yet.

## Working rules

- Minimalism everywhere:
  - Design: one way to do each thing. Point out redundancy or overlap in the
    language when you see it.
  - Code: as little code as possible, kept clean.
  - Documentation: concise, and only what the reader of that file needs.
    `README.md` is for users, `DESIGN.md` and `GRAMMAR.md` for users and
    agents, this file for agents. Say each thing in one place.
- The user makes every design decision, including all syntax. Present
  options with trade-offs and a recommendation; never decide alone, not
  even small defaults.
- When a decision changes, update `DESIGN.md` and `GRAMMAR.md` in the same
  step, including examples.

## Status

Design phase; the compiler is a stub (`compile` in `src/Racc.hs`).

## Build

- `cabal build`; `cabal run racc -- FILE.racc` prints C to stdout.
- `cabal test`: golden tests. Each `test/golden/X.racc` is compiled with
  `cc`, run, and its output compared with `X.out`; `--test-options=--accept`
  writes the `.out` files.
- Warnings are errors (`racc.cabal`); fix them, don't silence them.
