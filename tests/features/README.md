# Runic Runtime Regression Suite

`tests/features/` now tracks only the subset of Runic that is implemented and
stable enough to exercise regularly on the current runtime.

Run the suite with:

```sh
zig build run -- tests/features/runtime_regression.rn
```

This script is intentionally output-driven rather than aspirational. When the
runtime grows, expand this suite with new sections only after the behavior is
actually supported end to end.
