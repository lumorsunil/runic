# Contributing to Runic

Runic is still evolving quickly. Contributions are welcome, but the project is
not at the stage where "just add the feature" is a good default. The most
useful contributions are the ones that stay aligned with the current roadmap,
come with regression coverage, and keep the repo easier to maintain.

## Before You Start

- Read [README.md](README.md) for the current
  language/runtime status and the canonical build/test commands.
- Read [docs/plan.md](docs/plan.md) for current
  project priorities.
- Read [todo.md](todo.md) for lower-level backlog
  items, bugs, and implementation notes.
- If your work touches the language server, also read
  [docs/lsp.md](docs/lsp.md).

If a proposed change conflicts with the roadmap or depends on speculative
language design, open the discussion first instead of sending a large PR.

## What Kinds of Changes Help Most

- Bug fixes with a focused regression test.
- Documentation updates that match the current implementation.
- Roadmap-aligned language/runtime improvements.
- LSP/editor improvements that are backed by protocol-level tests.
- Cleanup that removes dead code, stale docs, or misleading behavior.

These are usually less helpful without prior discussion:

- large speculative syntax additions
- broad refactors with no user-visible payoff
- tooling churn unrelated to an active problem
- features based on outdated docs instead of the current implementation

## Development Setup

Runic is implemented in Zig and currently targets Zig `0.15.1`.

Typical setup:

1. Install Zig `0.15.1`.
2. Verify with:

```bash
zig version
```

3. Build the CLI:

```bash
zig build
```

4. Optionally build the language server:

```bash
zig build runic-lsp
```

## Workflow

The fastest way to stay aligned with the repo is:

1. Make the smallest coherent change that solves one problem.
2. Add or update regression tests in the same change.
3. Run the relevant local verification.
4. Update docs if behavior or contributor workflow changed.

Prefer narrowly scoped PRs. If a branch touches language semantics, runtime,
LSP, editor tooling, packaging, and docs all at once, it becomes hard to review
and easy to merge the wrong thing.

## Testing Expectations

At a minimum, run the tests that cover your change. Before opening a PR, prefer
running the full local pass.

Common commands:

```bash
zig build
zig build test
bash tests/cli_smoke.sh
bash tests/cli_diagnostics.sh
zig build run -- scripts/run_ci.rn
```

Guidelines:

- New language/runtime fixes should include a regression under `tests/features/`
  or `tests/diagnostics/`.
- LSP changes should include protocol-level tests where practical. The current
  harness lives in [tests/lsp_protocol.zig](tests/lsp_protocol.zig).
- If you fix a parser/runtime bug, prefer a small targeted regression over only
  relying on broad smoke coverage.
- If the test suite exposes a regression caused by your change, keep going until
  the regression is fixed or clearly documented.

## Documentation Expectations

If your change affects user-visible behavior, update the relevant docs in the
same PR.

Typical places:

- [README.md](README.md) for high-level usage and
  contributor workflow
- [docs/features.md](docs/features.md) for current
  language behavior
- [docs/lsp.md](docs/lsp.md) for LSP status and
  roadmap
- [docs/plan.md](docs/plan.md) if priorities or
  roadmap framing changed
- [docs/CHANGELOG.md](docs/CHANGELOG.md) for
  notable user-visible changes

Do not treat old planning notes as source of truth if they disagree with the
code. Update the docs to match the implementation, not the other way around,
unless the change is an intentional roadmap decision.

## Coding Guidelines

- Follow the current Zig style in the repo.
- Use `zig fmt`.
- Keep functions and patches small when possible.
- Prefer removing stale code over leaving commented-out legacy blocks around.
- Do not mix unrelated cleanups into feature fixes unless they are required.

For file layout:

- `src/` contains reusable runtime/language modules
- `cmd/` contains executables
- `tests/` contains regression/integration coverage
- `docs/` contains reference and roadmap material
- `editor/` contains editor integration files such as Neovim support

## Commit and PR Expectations

Use a short conventional-style prefix, for example:

- `runtime: fix env result coercion`
- `parser: preserve fd redirect ordering`
- `lsp: add navigation regressions`
- `docs: refresh roadmap docs`

PRs should include:

- the motivation for the change
- a short summary of implementation choices
- the commands you used to verify it
- any known limitations or intentionally deferred follow-up work

If a PR changes scope while you work on it, rename the title and description so
they still describe the actual branch.

## Good First Areas

Contributors looking for smaller, useful work should usually start with:

  bugs listed in [todo.md](todo.md)
- stale or missing documentation
- LSP regressions and editor-support gaps
- missing diagnostics coverage
- cleanup of obviously dead or misleading code

## Draft Status

This document is intentionally a draft. It should evolve as the contributor
workflow becomes clearer and as the project settles on stricter expectations for
reviews, roadmap alignment, and release process.
