# Racc grammar

The syntax of Racc. Alternatives marked `-- not in 0.1` are not part of
version 0.1. Meaning and restrictions are in `DESIGN.md`.

## Lexical structure

- Comments start with `--` and run to the end of the line.
- Only spaces are whitespace; a tab is an error. Lines end with LF or CRLF.
- Names and literals:

  ```
  LOWER  ::= [a-z_] { [A-Za-z0-9_'] }     -- a lone `_` is the token '_'
  UPPER  ::= [A-Z] { [A-Za-z0-9_'] }
  INT    ::= [0-9] { [ '_' ] [0-9] }       -- 1_000_000
  ```

  Lowercase names are variables, functions, and type variables; uppercase
  names are types and constructors. A letter, `_`, or `'` directly after
  an `INT` is an error: `12abc`, `1__0`, and `1_` are not numbers.
- Keywords:

  ```
  and  as  break  by  continue  downto  effects  elif  else  ensures  for
  from  if  in  invariant  match  modifies  not  or  otherwise  pass
  requires  result  return  state  then  to  type  until  while  yield
  ```

Two tokens depend on the whitespace around them:

- `[` directly after a name, `)`, or `]` is `INDEX` (indexing); otherwise it
  opens a list. So `xs[i]` indexes and `f [1, 2]` applies `f` to a list.
- `-` is `NEG` (negation) when no operand precedes it, or when whitespace
  precedes it and none follows it; otherwise it is subtraction. So `f -1` is
  `f (-1)`, while `a - 1` and `a-1` subtract.

## Layout

- A block is the lines after a header line that are indented further than
  the header. All lines of a block start at the same column, and a line
  indented less than its block must line up with an enclosing block.
- Blank lines and comment-only lines are ignored.
- Inside `( )`, `[ ]`, and `{ }`, line breaks and indentation are ignored.
  This is the only way to continue a line.
- Every logical line ends with `NEWLINE`; the `INDENT` or `DEDENT` tokens of
  the next line follow it:

  ```
  f x =            -- LOWER LOWER '=' NEWLINE
    if x < 0       -- INDENT 'if' LOWER '<' INT NEWLINE
      0            -- INDENT INT NEWLINE
    else           -- DEDENT 'else' NEWLINE
      x            -- INDENT LOWER NEWLINE
                   -- DEDENT DEDENT
  ```

## Grammar

`{ x }` is zero or more, `[ x ]` is optional, and quoted text is literal.

### Declarations

```
program      ::= { topDecl }
topDecl      ::= signature | function | typeDecl | typeAlias

signature    ::= LOWER ':' [ constraints '=>' ] type NEWLINE
                 [ INDENT clause { clause } DEDENT ]
constraints  ::= constraint | '(' constraint { ',' constraint } ')'  -- not in 0.1
constraint   ::= UPPER atype { atype }
clause       ::= 'modifies' param { ',' param } NEWLINE           -- not in 0.1
               | 'effects' [ param { ',' param } ] NEWLINE        -- not in 0.1
               | 'requires' expr NEWLINE                          -- not in 0.1
               | 'ensures' expr NEWLINE                           -- not in 0.1
param        ::= LOWER | INT

function     ::= equation { equation }        -- same name, consecutive
equation     ::= LOWER { apat } rhs
rhs          ::= '=' body
               | NEWLINE INDENT guardEq { guardEq } DEDENT
guardEq      ::= '|' guard '=' body
guard        ::= 'otherwise' | expr
body         ::= expr NEWLINE | NEWLINE block

typeDecl     ::= 'type' UPPER { LOWER } NEWLINE
                 INDENT constructor NEWLINE { constructor NEWLINE } DEDENT
constructor  ::= UPPER { atype }
typeAlias    ::= 'type' UPPER { LOWER } '=' type NEWLINE
```

### Statements

```
block        ::= INDENT [ stateBlock ] stmt { stmt } DEDENT
stateBlock   ::= 'state' NEWLINE INDENT stateLine { stateLine } DEDENT
stateLine    ::= signature | LOWER '=' body

stmt         ::= signature                          -- local signature
               | equation                           -- local function or assignment
               | pattern '=' body                   -- binding or assignment
               | 'if' expr NEWLINE block
                 { 'elif' expr NEWLINE block }
                 [ 'else' NEWLINE block ]
               | 'while' expr NEWLINE loopBlock
               | 'for' pattern 'in' expr NEWLINE loopBlock
               | 'match' expr NEWLINE INDENT arm { arm } DEDENT
               | 'return' [ expr ] NEWLINE
               | 'break' [ INT ] NEWLINE
               | 'continue' [ INT ] NEWLINE
               | 'pass' NEWLINE
               | 'yield' [ 'from' ] expr NEWLINE    -- not in 0.1
               | expr NEWLINE

loopBlock    ::= INDENT { invariant } stmt { stmt } DEDENT
invariant    ::= 'invariant' expr NEWLINE           -- not in 0.1
arm          ::= pattern ( '->' body
                         | NEWLINE INDENT guardArm { guardArm } DEDENT )
guardArm     ::= '|' guard '->' body
```

A `stateBlock` is only allowed in a function's body, including each guard's
body.

### Expressions

```
expr         ::= lambda | inlineIf | orExpr
lambda       ::= '\' apat { apat } '->' expr                 -- not in 0.1
inlineIf     ::= 'if' expr 'then' expr 'else' expr

orExpr       ::= andExpr { 'or' andExpr }
andExpr      ::= notExpr { 'and' notExpr }
notExpr      ::= 'not' notExpr | cmpExpr
cmpExpr      ::= rangeExpr { cmpOp rangeExpr }
cmpOp        ::= '==' | '!=' | '<' | '<=' | '>' | '>='
rangeExpr    ::= bitOr [ rangeOp bitOr [ 'by' bitOr ] ]
               | 'from' bitOr                                 -- not in 0.1
rangeOp      ::= 'to' | 'until' | 'downto'
bitOr        ::= bitXor { '|' bitXor }
bitXor       ::= bitAnd { '^' bitAnd }
bitAnd       ::= shift { '&' shift }
shift        ::= additive { ( '<<' | '>>' ) additive }
additive     ::= multiplicative { ( '+' | '-' ) multiplicative }
multiplicative ::= unary { ( '*' | '/' | '%' ) unary }
unary        ::= ( NEG | '~' ) unary | power
power        ::= app [ '**' unary ]
app          ::= postfix { arg }
arg          ::= postfix | ( NEG | '~' ) postfix
postfix      ::= atom { INDEX expr ']' }                      -- INDEX: not in 0.1

atom         ::= INT | LOWER | UPPER | 'result'
               | '(' ')'
               | '(' expr ')'
               | '(' expr ',' expr { ',' expr } ')'
               | '(' expr '|' qualifiers ')'                  -- not in 0.1
               | '(' binOp expr ')' | '(' expr binOp ')'      -- not in 0.1
               | '[' [ expr { ',' expr } ] ']'                -- not in 0.1
               | '[' expr '|' qualifiers ']'                  -- not in 0.1
               | '{' [ expr { ',' expr } ] '}'                -- not in 0.1
               | '{' expr ':' expr { ',' expr ':' expr } '}'  -- not in 0.1
               | '{' expr [ ':' expr ] '|' qualifiers '}'     -- not in 0.1
qualifiers   ::= pattern '<-' expr { ',' ( pattern '<-' expr | expr ) }
binOp        ::= any binary operator symbol except '-'
```

### Patterns

```
pattern      ::= orPattern [ 'as' LOWER ]
orPattern    ::= conPattern { 'or' conPattern }
conPattern   ::= UPPER apat { apat } | apat
apat         ::= '_' | LOWER | INT | NEG INT | UPPER
               | '(' ')'
               | '(' pattern ')'
               | '(' pattern ',' pattern { ',' pattern } ')'
               | '[' [ listItem { ',' listItem } ] ']'        -- not in 0.1
listItem     ::= pattern | '..' [ LOWER ]                     -- at most one '..'
```

### Types

```
type         ::= btype [ '->' type ]
btype        ::= UPPER atype { atype } | atype
atype        ::= LOWER | UPPER
               | '(' ')'
               | '(' type ')'
               | '(' type ',' type { ',' type } ')'
```

## Notes

- Comparisons chain: `a < b <= c` means `a < b and b <= c`, with `b`
  evaluated once.
- An attached `-x` or `~x` after a function is a separate argument:
  `f -x ** 2` is `(f (-x)) ** 2`, while `-x ** 2` is `-(x ** 2)`.
- In brackets, `|` starts a comprehension only if a generator `p <- e`
  follows it; otherwise it is bitwise or.
- `x = e` is an `equation`, not a `pattern` binding.
- Consecutive equations with the same name form one function, also in a
  body.
- `result` may only appear in `ensures`.

## Not yet designed

Records, structures, type classes and instances, modules and qualified
names, strings, and floating-point literals.
