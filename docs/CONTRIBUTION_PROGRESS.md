# Runic Contribution Progress

This document tracks our progress on implementing missing language features.

## Target Features (from docs/plan.md)

| Feature | Status | Priority | Notes |
|---------|--------|----------|-------|
| Lambda/anonymous functions | 🔄 In Progress | High | Parser TODO at line 2617 |
| Struct/tuple destructuring | ⏳ Pending | High | Parser TODO at line 1826 |
| Generic function types | ⏳ Pending | Medium | Parser TODO at line 2782 |
| Async/await improvements | ⏳ Pending | Medium | Keywords exist, implementation incomplete |

---

## Research Notes

### 1. Lambda/Anonymous Functions

**Current State:**
- Parser has TODO at `src/frontend/parser.zig:2617`
- Function declarations work via `fn` keyword
- Anonymous functions need new AST node and parsing logic

**Required Changes:**
- Add `lambda` variant to `Expression` union in `ast.zig`
- Add parsing logic in `parser.zig` to handle anonymous function syntax
- Add type inference for lambdas in `type-checker.zig`
- Add IR compilation in `compiler.zig`

**Proposed Syntax:**
```runic
const add = |x: Int, y: Int| -> Int { x + y }
// or
const add = fn(x: Int, y: Int) Int { x + y }
```

### 2. Struct/Tuple Destructuring

**Current State:**
- Parser has TODO at `src/frontend/parser.zig:1826`
- Basic binding patterns work for function parameters

**Required Changes:**
- Add destructuring support in `parseBindingPattern()` 
- Support patterns like `const { a, b } = obj`
- Add tuple index access for tuple destructuring

### 3. Generic Function Types

**Current State:**
- Parser has TODO at `src/frontend/parser.zig:2782`
- `TypeExpr.FunctionType` exists but doesn't support generics

**Required Changes:**
- Add generic type parameters to `FunctionType` in `ast.zig`
- Add parsing for generic syntax like `fn[T](x: T) T`
- Add type parameter inference in type checker

### 4. Async/Await Improvements

**Current State:**
- Keywords `async` and `await` exist in tokens
- `Promise` type exists in type system
- TODO in compiler at lines 4104, 4106

**Required Changes:**
- Fix async function calling (multiple calls overwriting results)
- Implement proper async function compilation
- Add streaming input for async blocks

---

## Implementation Log

### 2026-03-30 - Initial Exploration
- Explored codebase structure
- Identified 45+ TODO comments across codebase
- Created this progress document
- Selected lambda/anonymous functions as starting feature

### 2026-03-30 - Lambda Implementation
- Added `Lambda` struct to `src/frontend/ast.zig` (after FunctionDecl)
- Added `lambda` variant to `Expression` union in `src/frontend/ast.zig`
- Added `parseLambda()` function in `src/frontend/parser.zig`
- Added lambda routing in `parseExpression()` switch
- Added `runLambda()` in `src/semantic/type-checker.zig`
- Added `compileLambda()` in `src/ir/compiler.zig`
- Added lambda case in `cmd/runic/run_script.zig`
- Fixed pre-existing Zig 0.15 compatibility issue in `src/process.zig`
- All tests pass (`zig build test`)
- Lambda syntax works: `|x: Int| x + 1`

**Test Results:**
```runic
const add = |x: Int| x + 1
echo "${add(1)}"  # Works!
```

**Note:** Multi-argument function calls have a pre-existing bug (unrelated to lambda implementation).

---

## Related Files

| File | Purpose |
|------|---------|
| `src/frontend/parser.zig` | Parse function declarations (line 2617 lambda TODO) |
| `src/frontend/ast.zig` | AST node definitions |
| `src/semantic/type-checker.zig` | Type inference |
| `src/ir/compiler.zig` | AST to IR lowering |
| `src/ir/evaluator.zig` | IR execution |
