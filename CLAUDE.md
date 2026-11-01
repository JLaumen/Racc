# Racc

A language for algorithm design. The compiler is written in Haskell and
emits C.

- `README.md`: introduction, for users.
- `DESIGN.md`: what programs mean.
- `GRAMMAR.md`: the syntax; alternatives marked `not in 0.1` are not part
  of version 0.1.

## Working rules

- Minimalism everywhere:
  - Design: one way to do each thing; a second way only where it improves
    the quality of programs. Point out redundancy or overlap in the
    language when you see it.
  - Code: as little code as possible, kept clean.
  - Tests: cover every case and edge case, with as little test code as
    possible; one test per behavior, table-driven where it fits.
  - Documentation: concise, and only what the reader of that file needs.
    `README.md` is for users, `DESIGN.md` and `GRAMMAR.md` for users and
    agents, this file for agents. Say each thing in one place.
- The user makes every design decision, including all syntax. Present
  options with trade-offs and a recommendation; never decide alone, not
  even small defaults.
- When a decision changes, update `DESIGN.md` and `GRAMMAR.md` in the same
  step, including examples.
- Before committing a significant change, do a pass over it for the
  minimalism rules above, then run a code review (`/code-review`) on it
  and address the findings.

## Versioning

- SemVer, staying at 0.x while the language may change. One version covers
  language and compiler; it lives only in `racc.cabal`.
- Releases happen at milestones; the first is 0.1.0, when everything in
  DESIGN.md's "Version 0.1" works. A release commit sets the version in
  `racc.cabal` and is tagged `vX.Y.Z`; release notes go on GitHub only.
- When a milestone is reached, tell the user and draft the release commit
  and notes (summarised from git history). Commit, tag, push, and publish
  with `gh release create` only after the user approves that release.
- Releases include a prebuilt binary, built by CI: Linux x86_64 only, fully
  static (built against musl in an Alpine container). Write that CI job
  when preparing 0.1.0, and test it with a manual run before publishing.

## Status

The lexer and parser are done; later phases are not (`compile` in
`src/Racc.hs`).

## Build

- `cabal build`; `cabal run racc -- FILE.racc` prints C to stdout.
- `cabal test`: unit tests plus golden tests. Each `test/run/X.racc` is
  compiled with `cc`, run, and its output compared with `X.out`; each
  `test/errors/X.racc` must fail with the rendered error in `X.err`. Unit
  tests check every error message; the error goldens only cover rendering.
  `--test-options=--accept` writes missing or changed golden files.
- `scripts/check`: everything CI runs: ormolu, cabal-gild, hlint, build,
  weeder, tests. Run it before committing. Tool versions are pinned in
  `.github/workflows/ci.yml`; install them with `cabal install`.
- Warnings are errors (`racc.cabal`); fix them and hlint and weeder
  findings, don't silence them.

## Planned tests

Tell the user when work reaches the point where one of these should be
added; move it to Build once added.

- Coverage report (`cabal test --enable-coverage`): on demand, to check
  that tests reach all code. Available now, not yet run.
- Property tests: with the parser. "The lexer and parser never crash on any
  input" is cheap; a print-then-parse round trip needs a pretty-printer and
  a generator, so ask first.
- C sanitizers and warnings in the run goldens
  (`cc -fsanitize=address,undefined -Wall -Werror`): with code generation.
- Benchmarks (tasty-bench) for compile speed and generated code: once
  there is something worth comparing over time.
