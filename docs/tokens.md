# Runic Token Definitions

The lexical tags in `src/frontend/token.zig` model every syntactic feature
described in `features.md`. Each token pairs a `Tag` enum value with the raw
slice and span information so diagnostics and downstream parser stages receive
exact source locations. The sections below group the tags by the Runic concepts
they unlock to show how commands, declarations, and type operators share the
same machinery.

## Commands and Pipelines

Bare words are emitted as the generic `identifier` tag. The parser determines
whether a given identifier should run as a command or evaluate as a pure
expression depending on its position. Command separators come from the same
token stream:

| Tag                    | Lexeme/Example  | Notes                                                   |
| ---------------------- | --------------- | ------------------------------------------------------- | ---------------------- | ---------------------------------------------- |
| `identifier`           | `echo`, `upper` | Every bare command or variable name                     |
| `pipe`                 | `               | `                                                       | Splits pipeline stages |
| `pipe_pipe`            | `               |                                                         | `                      | Logical-or operator between stages/expressions |
| `dollar`               | `$`             | Starts `${expr}` command arguments or `$ident` captures |
| `amp`                  | `&`             | Background processes                                    |
| `amp_amp`              | `&&`            | Logical-and operator                                    |
| `newline`, `semicolon` | `\n`, `;`       | End of statements or command chains                     |

## Declarations

Variable and function declarations rely on dedicated keywords so bindings stay
predictable. Type annotations and assignment reuse punctuation tokens:

| Tag                  | Lexeme   | Purpose                           |
| -------------------- | -------- | --------------------------------- |
| `kw_const`           | `const`  | Immutable bindings                |
| `kw_var`             | `var`    | Mutable bindings                  |
| `kw_fn`              | `fn`     | Function declarations/expressions |
| `assign`             | `=`      | Assignment                        |
| `colon`              | `:`      | Type annotation separator         |
| `l_paren`, `r_paren` | `(`, `)` | Function signatures and grouping  |

## Literal Data Structures

Arrays, maps, and primitive literal forms all have explicit tokens so parsing
preserves structure without juggling whitespace:

| Tag                                              | Lexeme               | Purpose                        |
| ------------------------------------------------ | -------------------- | ------------------------------ |
| `int_literal`, `float_literal`, `string_literal` | `42`, `3.14`, `"hi"` | Primitive literals             |
| `l_bracket`, `r_bracket`                         | `[`, `]`             | Array literals                 |
| `l_brace`, `r_brace`                             | `{`, `}`             | Map literals and scoped blocks |
| `comma`                                          | `,`                  | Item separator                 |
| `colon`                                          | `:`                  | Map key/value separator        |

## Error Declarations and Propagation

Error sets mirror Zig’s syntax, so token coverage includes declaration keywords
and the `!` operator used in signatures plus the `try`/`catch` recovery flow:

| Tag                   | Lexeme          | Purpose                                 |
| --------------------- | --------------- | --------------------------------------- |
| `kw_error`            | `error`         | Introduces named error sets             |
| `kw_enum`, `kw_union` | `enum`, `union` | Shapes error payloads                   |
| `bang`                | `!`             | Error-union marker and logical negation |
| `kw_try`, `kw_catch`  | `try`, `catch`  | Error propagation/handling              |

## Optionals

Optionals take Zig’s `?T` syntax and reserve `null` as the absence of a value:

| Tag        | Lexeme | Purpose                       |
| ---------- | ------ | ----------------------------- |
| `question` | `?`    | Optional prefix               |
| `kw_null`  | `null` | Explicit empty optional value |

## Promises and Asynchronous Blocks

Promises use the dedicated `^T` shorthand and lean on `async`/`await` keywords:

| Tag        | Lexeme  | Purpose                                   |
| ---------- | ------- | ----------------------------------------- |
| `caret`    | `^`     | Promise prefix                            |
| `kw_async` | `async` | Async blocks/functions returning promises |
| `kw_await` | `await` | Await expressions and capture clauses     |

## Modules and Imports

Modules load through the special `import()` call inside `let` bindings, and
member access uses the existing `dot` token:

| Tag         | Lexeme   | Purpose                               |
| ----------- | -------- | ------------------------------------- |
| `kw_import` | `import` | Starts an import binding              |
| `dot`       | `.`      | Member access within imported modules |

## `bash { ... }` Compatibility Blocks

The compatibility mode is initiated by a keyword and delimited by braces. The
tokenizer does not treat Bash contents specially—parsing happens inside a single
Runic block once the lexer hands over `kw_bash` and the brace tokens.

| Tag                  | Lexeme   | Purpose                  |
| -------------------- | -------- | ------------------------ |
| `kw_bash`            | `bash`   | Signals a legacy block   |
| `l_brace`, `r_brace` | `{`, `}` | Wrap the raw Bash script |

---

These categories align directly with the `Tag` enum and give the parser enough
vocabulary to differentiate commands, pipelines, data declarations, and the
type-level operators (`!`, `?`, `^`) that Runic inherits from Zig.
