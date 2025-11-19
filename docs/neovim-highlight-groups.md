# Runic Neovim Highlight Groups

This reference backs Task 3 of the Neovim extension plan. It locks in the
semantic highlight groups, their Tree-sitter capture names, and the fallback
links used by the regex syntax file. Future runtime files (`lua/runic_highlight/`
and `syntax/runic.vim`) must read from this document so regex-based matches and
Tree-sitter captures stay visually consistent.

## Namespace & Linking Rules

- Every custom highlight group is prefixed with `Runic` to avoid clashing with
  user themes. Tree-sitter queries emit the matching `@*.runic` capture and the
  regex syntax file applies the same `Runic*` group via `:syn match`.
- `lua/runic_highlight/highlights.lua` will expose a single table of
  `{ group = "RunicDeclKeyword", capture = "@keyword.declaration.runic",
     link = "Keyword" }` specs. Both the syntax file and Tree-sitter query loader
  iterate over this table to define highlight links.
- Fallback colors rely on built-in groups (`Keyword`, `Type`, `Conditional`,
  `Function`, `Operator`, `Number`, `String`, `Boolean`, `Constant`,
  `SpecialChar`, `Delimiter`, `Comment`, `SpecialComment`). Themes that already
  style these base names inherit Runic-specific colors without extra work.
- Future Tree-sitter captures that do not exist yet should adhere to the same
  naming shape: `@{category}.{role?}.runic`.

## Group Inventory

Each entry below lists the canonical `Runic*` group, the Tree-sitter capture
string, which Runic tokens feed it, and the fallback link that determines the
default colors in Neovim.

### Keywords & Flow

| Runic Group / Capture | Applies To | Fallback Link | Notes |
| --- | --- | --- | --- |
| `RunicDeclKeyword` / `@keyword.declaration.runic` | `kw_let`, `kw_mut`, `kw_fn` | `Keyword` | Covers bindings and function declarations; regex matches `\<\(let\|mut\|fn\)\>`. |
| `RunicTypeKeyword` / `@keyword.type.runic` | `kw_error`, `kw_enum`, `kw_union` | `Type` | Highlights type-level constructs to match Zig-inspired syntax. |
| `RunicAsyncKeyword` / `@keyword.coroutine.runic` | `kw_async`, `kw_await` | `Keyword` | Tree-sitter capture chosen to stay compatible with `@keyword.coroutine`. |
| `RunicControlKeyword` / `@keyword.control.runic` | `kw_if`, `kw_else`, `kw_match`, `kw_return` | `Conditional` | `return` sticks with the control keyword palette; loops live in the next row. |
| `RunicLoopKeyword` / `@keyword.repeat.runic` | `kw_for`, `kw_while` | `Repeat` | Split from the control keywords so themes can color loops distinctly. |
| `RunicImportKeyword` / `@keyword.import.runic` | `kw_import` | `Include` | Maps to built-in import/include colors. |
| `RunicInteropKeyword` / `@keyword.special.runic` | `kw_bash` | `Keyword` | Signals Bash compatibility blocks; kept separate in case we dim it later. |
| `RunicErrorKeyword` / `@keyword.exception.runic` | `kw_try`, `kw_catch` | `Exception` | Aligns with exception-style palettes in Neovim themes. |
| `RunicBooleanLiteral` / `@constant.builtin.boolean.runic` | `kw_true`, `kw_false` | `Boolean` | Emits the built-in boolean colors regardless of context. |
| `RunicNullLiteral` / `@constant.builtin.nil.runic` | `kw_null` | `Constant` | Linked to `Constant` so it stands out from booleans. |

### Literals & Builtins

| Runic Group / Capture | Applies To | Fallback Link | Notes |
| --- | --- | --- | --- |
| `RunicNumber` / `@number.runic` | `int_literal`, `float_literal` | `Number` | Both Tree-sitter and regex capture base-10 plus hex forms. |
| `RunicString` / `@string.runic` | `string_literal` bodies | `String` | Regex uses regions to keep interpolation spans nested. |
| `RunicStringEscape` / `@string.escape.runic` | `\n`, `\t`, `\"`, `\\`, etc. | `SpecialChar` | Linked for better contrast within strings. |
| `RunicInterpolationDelimiter` / `@string.special.symbol.runic` | `${` / closing `}` | `Delimiter` | Visually brackets interpolated expressions. |
| `RunicInterpolationExpr` / `@embedded.runic` | Expressions inside `${...}` | `SpecialChar` | Linked to `SpecialChar` to gently dim inline expressions. |
| `RunicRuneLiteral` / `@character.runic` | Planned rune / char literals (future `'a'`) | `Character` | Stubbed now; regex reserves the group once runes ship. |
| `RunicCaptureBinding` / `@keyword.capture.runic` | Capture clauses `|value, idx|` attached to `if`, `match`, `catch`, `for` | `Special` | Highlights binding bars plus identifiers to make captures pop. |
| `RunicBuiltinCmd` / `@function.builtin.runic` | Built-in commands/functions once enumerated in Lua (`echo`, `upper`, etc.) | `Function` | Lua helper will read the builtin list and assign this group via keyword match lists. |
| `RunicTypeName` / `@type.identifier.runic` | Explicit type identifiers following `:` or `fn name(...) Type` | `Type` | Regex heuristics look for identifiers after `:`; Tree-sitter capture fires on AST nodes. |

### Operators & Punctuation

| Runic Group / Capture | Applies To | Fallback Link | Notes |
| --- | --- | --- | --- |
| `RunicPipelineOperator` / `@operator.pipeline.runic` | `pipe`, `pipe_pipe`, `amp`, `amp_amp` | `Operator` | Distinguishes structural pipeline separators from math ops. |
| `RunicProcessOperator` / `@operator.process.runic` | Redirection tokens (`1>`, `2>`, `<`, `>>`) once the lexer surfaces them | `Operator` | Reserved for pending IO redirection tokens; regex uses `\d\?>\+`. |
| `RunicLogicOperator` / `@operator.logical.runic` | `bang`, `amp_amp`, `pipe_pipe` | `Operator` | Overlaps with pipeline logical variants but exists for pure expressions. |
| `RunicAssignmentOperator` / `@operator.assignment.runic` | `assign` | `Operator` | Tree-sitter capture used to match `=` tokens not part of comparisons. |
| `RunicComparisonOperator` / `@operator.comparison.runic` | `equal_equal`, `bang_equal`, `less`, `less_equal`, `greater`, `greater_equal` | `Operator` | Standard comparison palette. |
| `RunicRangeOperator` / `@operator.range.runic` | `range` (`..`), `ellipsis` (`...`) | `Operator` | Dedicated group for slicing/range sugar. |
| `RunicArrowOperator` / `@operator.arrow.runic` | `arrow`, `fat_arrow` | `Operator` | Used for return types and match arms. |
| `RunicSigil` / `@operator.sigil.runic` | `question`, `caret`, leading `bang` in signatures | `SpecialChar` | Highlights optional/promise/error sigils in type annotations. |
| `RunicDelimiter` / `@punctuation.bracket.runic` | `l_paren`, `r_paren`, `l_brace`, `r_brace`, `l_bracket`, `r_bracket` | `Delimiter` | Shared across every structural delimiter. |
| `RunicComma` / `@punctuation.delimiter.runic` | `comma`, `colon`, `semicolons`, `dot` used as separators | `Delimiter` | `dot` stays here to underline member access punctuation. |

### Comments & Docstrings

| Runic Group / Capture | Applies To | Fallback Link | Notes |
| --- | --- | --- | --- |
| `RunicLineComment` / `@comment.line.runic` | `# ...` and `// ...` | `Comment` | Regex declares two separate matches tied to the same group. |
| `RunicBlockComment` / `@comment.block.runic` | `/* ... */` | `Comment` | Supports nested comments per lexer behavior. |
| `RunicDocComment` / `@comment.documentation.runic` | `/// ...` and `/** ... */` | `SpecialComment` | Communicates docstrings distinctly from ordinary comments. |

### Structural Helpers

| Runic Group / Capture | Applies To | Fallback Link | Notes |
| --- | --- | --- | --- |
| `RunicCommandHead` / `@function.call.runic` | First bare identifier in each pipeline stage | `Function` | Regex uses `^\s*\%(\w\|\.\)\+` heuristics; Tree-sitter capture hits AST nodes with `StageRole.command`. |
| `RunicStageSeparator` / `@punctuation.special.runic` | Newlines/semicolons that terminate pipelines | `Delimiter` | Kept distinct so we can fade statement boundaries if desired. |
| `RunicHereDocFence` / `@string.special.runic` | Planned heredoc fences (not implemented yet) | `Special` | Stub entry ensures both regex + Tree-sitter tables remain compatible if heredocs land. |

## Implementation Notes

1. `lua/runic_highlight/highlights.lua` should export the tables exactly as
   listed. Example:

   ```lua
   return {
     keywords = {
       { group = "RunicDeclKeyword", capture = "@keyword.declaration.runic", link = "Keyword",
         tokens = { "let", "mut", "fn" } },
       -- ...
     },
   }
   ```

   The regex syntax file will iterate through each entry, issuing `syn keyword`
   or `syn match` statements and linking `Runic*` to the fallback `link`.
2. Tree-sitter queries reference only the capture column. Example capture:

   ```scm
   (keyword_declaration) @keyword.declaration.runic
   ```

   The Lua loader will automatically call `vim.treesitter.highlighter.hl_map`
   (or `vim.api.nvim_set_hl`) to link `@keyword.declaration.runic` to
   `RunicDeclKeyword`.
3. Color overrides belong in `after/syntax/runic.vim` so users may customize
   `Runic*` groups without editing the core syntax file.
4. Whenever the Runic lexer adds a new token, update this document first to
   decide whether it belongs in an existing group or requires a fresh `Runic*`
   entry. This keeps regex, Tree-sitter, and docs synchronized.
