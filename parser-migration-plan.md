# Parser Migration Plan

## Objective
- Replace `module_parser_deprecated.zig` by extending `src/frontend/parser.zig` so a single parser handles both script bodies and module documents.
- Preserve the public API that `cmd/runic/main-utils.zig` relies on today: `ModuleParseError`, `ModuleDocument`, the `ModuleDeclaration` unions, and `sliceForSpan`/`sliceForRange`.
- Reach behavioral parity with the deprecated parser before deleting it, then unblock follow-up work that can parse full function/value bodies with the richer AST.

## Current State
- `Parser` currently handles scripts (`parseScript`, statements, expressions) but exposes no module-level entrypoints.
- `module_parser_deprecated.zig` performs a streaming scan that only extracts spans and identifiers. It supports:
  - `async fn` and `fn` declarations with parameter lists, optional return types, and braced bodies (tracked via `SourceRange`).
  - `let` value declarations (top-level only) capturing initializer slices.
  - `manifest { ... }` blocks treated as opaque ranges.
- Downstream callers (ModuleRegistry) depend on its error enums and the ability to slice source text using stored spans/ranges.

## Plan of Work

### 1. Mirror legacy API on the new parser
- Copy or redefine `ModuleParseError`, `ModuleDocument`, `ModuleDeclaration`, and related structs near the top of `parser.zig` so they live beside `Parser`.
- Add `sliceForSpan`/`sliceForRange` methods on `Parser` (mirroring the deprecated parser) because the CLI still extracts strings directly from source slices.
- Introduce a `SourceRange` helper in `parser.zig` so consumers do not have to import two modules.

### 2. Implement module-aware entrypoints
- Add `parseModuleDocument` to `Parser` that follows the same signature (`ModuleParseError || std.mem.Allocator.Error`).
- Inside this method, reuse `skipNewlines`, the tokenizer, and arena allocation strategy already present in `Parser`.
- Track `scope_depth` exactly like the deprecated parser so we only inspect declarations at the top level while skipping nested braces.

### 3. Port declaration-specific parsing logic
- **Functions:** Build `parseModuleFunctionDecl(is_async: bool)` that consumes `fn`, records the identifier span, parses parameter lists, optional return type spans, and captures `{ ... }` body ranges using a generalized `captureDelimitedRange` helper (leveraging `skipNestedStructure` already present).
- **Parameters:** Reuse/extend `parseBindingPattern` infrastructure to parse names, but preserve the legacy behavior of accepting optional `mut`, type annotations, and default initializers even if we only capture spans. Store the resulting `ModuleParam` data in arena-backed slices for stability.
- **Values:** Implement `parseModuleValueDecl` to consume `let`, identifier, `=`, then capture the initializer until newline/semicolon/EOF while honoring nested brackets/braces. Return `ModuleValue`.
- **Manifest:** Parse the `manifest` identifier and capture the braced block using the same range helper, returning `ModuleManifest`.
- **Async detection:** Keep the outer loop that recognizes `kw_async` before `kw_fn`, calling `parseModuleFunctionDecl(true)` when present.

### 4. Align error handling
- Ensure each helper returns the exact legacy errors (`MissingFunctionName`, `UnterminatedParameterList`, etc.) so downstream user-facing error messages remain unchanged.
- Add adapter methods like `expectTokenTagModule` that wrap `self.stream.next()` but translate lexer errors into `ModuleParseError.InvalidSyntax`, matching the previous behavior.

### 5. Testing and validation
- Move the module parser tests in `src/frontend/parser_tests.zig` to target the new implementation. Expand coverage to include:
  - `async fn` parsing
  - Parameters with `mut`, default values, and type annotations
  - Value declarations with nested expressions (to validate range tracking)
  - Manifests following other declarations and in files with nested braces
  - Negative cases for every `ModuleParseError`
- Run `zig test src/frontend/parser_tests.zig` to confirm both the script parser and the new module parser entrypoints pass.

### 6. Cleanup and integration
- Update `cmd/runic/main-utils.zig` to reference `parser.Parser` (no longer importing `parser.module_parser`).
- Remove `module_parser_deprecated.zig` once callers only use the new parser and all tests pass.
- Drop the transitional `module_parser` alias from `parser.zig` and delete any unused helpers from the deprecated file.

## Risks and Follow-Ups
- While parity requires only spans/ranges, once the migration is complete we can iterate toward building real AST nodes for function/value bodies. Keep helpers (like `captureDelimitedRange`) generic enough to drive richer parsing later.
- Watch allocator usage: module parsing now shares the same arena as script parsing, so we need to ensure we reset it between invocations or document that the parser instance is one-shot per compilation unit.
