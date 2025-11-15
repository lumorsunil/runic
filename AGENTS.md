# Repository Guidelines

## Project Structure & Module Organization
Keep top-level folders predictable: place interpreter sources and shared utilities under `src/`, CLI front-ends under `cmd/`, integration fixtures in `examples/`, and human-readable specs inside `docs/`. Create `tests/` for integration-style suites and reserve `assets/` for longer sample scripts, diagrams, or benchmark data. Even though the repo currently ships a single `README.md`, follow this layout when adding files so newcomers can locate runtime, parser, and documentation modules instantly.

## Build, Test, and Development Commands
Use the native toolchain of the implementation language for formatting, linting, building, and testing. Document the canonical commands in the README (e.g., formatter → linter → build → test) so contributors can follow the same workflow. When working on experimental Runic programs inside `examples/`, prefer invoking the emerging CLI or REPL (`cmd/` binaries) the same way users eventually will, e.g., `path/to/repl -- path/to/script.rn`, so behavior mirrors production expectations.

## Coding Style & Naming Conventions
Follow the idioms of the chosen language (folder structure, naming, indentation) and keep functions short and domain-specific. Extract parser or runtime helpers into submodules inside `src/` to avoid monolithic files. Always run the language-appropriate formatter before opening a PR, treat lint warnings as errors, name Runic files with the `.rn` extension, and keep command invocations column-aligned for readability.

## Testing Guidelines
Place unit tests alongside their modules when the language supports it and rely on `tests/` for public API and CLI smoke suites. Favor table-driven cases for quoting and pipeline semantics. Every new feature should include at least one regression test covering both success and failure flows; when behavior spans multiple binaries, prefer snapshot-style tests under `tests/cli_*`. Run the full test suite locally before pushing and note any ignored tests in the PR body.

## Commit & Pull Request Guidelines
Adopt a Conventional Commits prefix (`parser:`, `runtime:`, `docs:`) plus an imperative summary under 72 characters, e.g., `runtime: enforce exit code propagation`. Each PR should describe motivation, implementation notes, and verification steps; link issues with `Fixes #ID` and attach logs or terminal transcripts demonstrating the change. Keep PRs narrowly scoped and request reviews only after formatting, linting, and tests pass.
