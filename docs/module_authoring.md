# Module Authoring Guide

Runic modules package reusable functions and values that other scripts can `import`. The runtime resolves module specs like `net/http` relative to the importing script's directory (plus any additional `--module-path` directories) and reads a manifest to learn which APIs the module exports. This guide captures the current conventions so you can publish libraries the CLI and upcoming interpreter will understand.

## Layout and naming

- Place module sources next to the scripts that consume them (or inside directories you add via `--module-path`). A spec of `net/http` maps to `<script_dir>/net/http.rn`.
- Keep specs lowercase with `/` separators. Each segment may include letters, digits, `_`, `-`, or `.`.
- Every source file must be accompanied by a JSON manifest that shares the filename plus the `.module.json` suffix, e.g. `<script_dir>/net/http.rn.module.json`.
- When experimenting outside the main tree, add directories to the loader search path with `runic --module-path ./lib -- script.rn`.

## Writing the module file

Author your reusable functions, constants, and helper types inside `<script_dir>/<spec>.rn`. Modules behave the same as regular Runic scripts, so you can define `fn` blocks, declare `let`/`mut` bindings, and spawn commands. The module loader does not currently inspect the file contents; it trusts the manifest to describe what is publicly available. Keep the source of record in the `.rn` file and treat the manifest as structured documentation.

## Describing exports via the manifest

The manifest root object contains a single `exports` array. Each entry advertises either a function or a value:

```json
{
  "exports": [
    {
      "kind": "function",
      "name": "get",
      "is_async": true,
      "params": [
        { "name": "url", "type": { "kind": "primitive", "name": "string" } },
        {
          "name": "headers",
          "type": {
            "kind": "map",
            "key": { "kind": "primitive", "name": "string" },
            "value": { "kind": "primitive", "name": "string" }
          }
        }
      ],
      "return_type": {
        "kind": "optional",
        "child": { "kind": "primitive", "name": "bytes" }
      }
    },
    {
      "kind": "value",
      "name": "default_headers",
      "type": {
        "kind": "array",
        "element": { "kind": "primitive", "name": "string" }
      }
    }
  ]
}
```

| Field | Description |
| --- | --- |
| `kind` | `"function"` or `"value"`. |
| `name` | Identifier exposed to consumers (e.g. `http.get`). |
| `is_async` | Optional flag for functions that resolve via promises/`await`. Defaults to `false`. |
| `params` | Ordered list of parameters. Each item provides a `name` and `type`. Empty arrays are allowed. |
| `return_type` | Required for functions. Uses the same type descriptor schema as parameters. |
| `type` | Required for values. Describes the value's type descriptor. |

## Supported type descriptors

Manifests use a compact JSON schema to describe public types. The loader currently understands:

- `primitive` — `{ "kind": "primitive", "name": "<tag>" }` where `<tag>` is one of `void`, `bool`, `int`, `float`, `string`, `bytes`, `pid`, or `exit_status`.
- `array` — `{ "kind": "array", "element": <type> }`.
- `map` — `{ "kind": "map", "key": <type>, "value": <type> }`.
- `optional` — `{ "kind": "optional", "child": <type> }`.
- `promise` — `{ "kind": "promise", "payload": <type> }`.

Nest these descriptors freely to model shapes such as `?Map(Str, Int)` or `^!ProcessHandle`. Future interpreter milestones will extend the schema with functions, error sets, and process handles; keep manifests minimal today so they continue to parse when more kinds arrive.

## Verifying a module

1. Create or update the `.rn` file and manifest.
2. Run `zig build run -- path/to/script.rn` from the repository root with a small script that imports your module:

   ```rn
   import http from "net/http"

   let ping = http.get("https://example.com")
   await(ping) |resp| {
     echo resp.code
   }
   ```

3. Execute the integration suite with `zig build test` (or `./scripts/run_ci.sh`) to ensure manifests remain valid and loader tests pass.

For additional context, see `features.md` for the user-facing module behavior and `src/runtime/module_loader.zig` for the manifest parser the CLI calls today.
