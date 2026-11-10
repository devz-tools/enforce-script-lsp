# enforce-script-lsp

A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server for
**Enforce Script** — the scripting language of the Enfusion engine, used for modding DayZ.

This is a fork of [`devz-tools/enforce-script-lsp`](https://github.com/devz-tools/enforce-script-lsp),
extended from a single-file analyzer into a **project-aware** language server: it discovers a
DayZ mod's `config.cpp` files, indexes the whole project (and, optionally, the vanilla engine
scripts and other dependency roots), resolves symbols across files and up the inheritance chain,
and lints for Enforce-specific pitfalls that the engine's own tooling misses.

## Features

- **config.cpp discovery** — walks the workspace, finds every `config.cpp`, parses
  `dir`/`files[]`/`defines[]`, and derives the set of compiled directories (both the
  "prefix" style `my_mod/3_Game` and the full-path style `SomeMod/Core/Scripts/3_Game`).
- **"excluded from compilation" warning** — flags a `.c` file that is not under any
  module's `files[]` (the classic "forgot to add the directory to config.cpp" footgun).
- **defines-aware `#ifdef`** — keeps/skips `#ifdef X`/`#ifndef X`/`#else`/`#endif` branches
  according to the module's `defines[]` from `config.cpp`.
- **Cross-file symbol index** — indexes all compiled `.c` files and powers
  *go-to-definition*, *completion* and *hover* across files (not just the current document).
- **Cross-file resolution** — resolves global symbols (enums, classes, functions, constants)
  and **inherited members** (fields/methods of parent classes) so that e.g. `m_SomeField`
  in `MyIntent : BaseIntent` is no longer reported as "undefined".
- **`dependencyRoots`** — indexes extra source roots (the vanilla engine scripts and other
  dependency roots) "lightly" (signatures straight from the AST), so engine globals like
  `Math`, `vector`, `GetGame` resolve too.
- **Enforce-specific lint** (`lint.rs`):
  - long `+` chain (string concatenation, >15) — *Formula too complex*;
  - long `||`/`&&` chain (>8);
  - ternary operator `cond ? a : b` — not supported by Enforce;
  - line break after `.` (`A.` newline `B()`);
  - a variable declared more than once in the same method (Enforce has no block scope);
  - a field/variable named after a built-in class (`Surface`, `Object`, `Game`, …) — the engine
    crashes on startup and AddonBuilder doesn't catch it.

## Building

Prerequisites: Rust 1.85+ (the crate uses edition 2024).

```bash
cargo build --release   # → target/release/enforce-script-lsp
cargo test              # run the test suite
```

## Usage

The server speaks LSP over stdio. Wire it into an editor:

### VS Code / Cursor

`editor/vscode/` contains a small local client. Build and install it:

```bash
cd editor/vscode
npm install
npx vsce package
code --install-extension enforce-script-lsp-local-0.1.0.vsix
```

Settings (workspace `.vscode/settings.json`):

```jsonc
{
  "enforceScriptLsp.serverPath": "/path/to/enforce-script-lsp",
  "enforceScriptLsp.dependencyRoots": [
    "/path/to/DayZ/scripts",           // vanilla engine scripts
    "/path/to/another-mod-Scripts"
  ],
  "files.associations": { "*.c": "enforcescript" }
}
```

### OpenCode

In `opencode.jsonc`:

```jsonc
{
  "lsp": {
    "enforce-script": {
      "command": ["/path/to/enforce-script-lsp"],
      "extensions": [".c"],
      "initialization": {
        "dependencyRoots": [
          "/path/to/DayZ/scripts",
          "/path/to/another-mod-Scripts"
        ]
      }
    }
  }
}
```

## Configuration (`initializationOptions`)

| Key | Type | Default | Meaning |
|---|---|---|---|
| `dependencyRoots` | `string[]` | `[]` | Extra source roots to index (e.g. the vanilla engine scripts). All `.c` under them are indexed "lightly". |

The project root is taken from the LSP `rootUri` (the opened folder) — no configuration needed
for a single mod.

## Deploying (operational notes)

Build, then atomically replace the binary and **restart the running server process**:

```bash
cargo build --release
cp target/release/enforce-script-lsp /opt/enf-lsp/enforce-script-lsp.new
mv /opt/enf-lsp/enforce-script-lsp.new /opt/enf-lsp/enforce-script-lsp
# IMPORTANT: the editor/LSP client keeps the OLD inode. Kill the running process so
# the client relaunches against the fresh binary:
pkill -f 'enforce-script-lsp --stdio'   # OpenCode relaunches it lazily
```

**Why the restart is required.** `mv` swaps the path to a new inode, but a process that
already loaded the binary keeps the old inode in memory. Symptoms of a stale server: recent
fixes don't take effect, `exe` shows `(deleted)` in `ls -li /proc/<PID>/exe`. Check:

```bash
ls -li /proc/$(pgrep -f 'enforce-script-lsp --stdio')/exe   # "(deleted)" = stale
```

## Architecture

Modules in `src/`:

- **`lexer.rs`** — tokenizer + preprocessor (`#ifdef`/`#ifndef`/`#else`/`#endif` evaluated
  against active defines; empty set = skip all `#ifdef`).
- **`parser.rs`** — recursive-descent AST; handles module-level `static const`, C-style casts
  `(float)x`, expression statements, optional `;` after `class`/`enum`.
- **`semantic.rs`** — per-file symbol table + `GlobalEnvironment` (globals, class hierarchy,
  members) used to resolve cross-file identifiers.
- **`workspace.rs`** — `config.cpp` discovery, prefix map, compiled directories, `defines_for`.
- **`index.rs`** — cross-file `SymbolIndex`: name → occurrences across the project + deps,
  plus the class hierarchy.
- **`lint.rs`** — Enforce-specific lint checks (thresholds in `LintConfig`).
- **`lsp.rs`** — the tower-lsp `Backend`: captures `rootUri`/`dependencyRoots`, builds the
  workspace + index, wires diagnostics/completion/hover/go-to-definition to the index.

## Testing

```bash
cargo test                # all
cargo test lexer::tests   # one module
```

Tests are co-located with each module (`#[cfg(test)] mod tests`).

## Current limitations / roadmap

- **`references` (find references)** is still per-document — cross-file needs an index of
  *usages*, not just definitions.
- **Member completion after `.`** — completion returns everything; `obj.` should resolve the
  type and offer its members.
- **`modded class` merging** — `modded class X` is not yet merged into the base `class X`
  (needed to resolve members of vanilla classes we extend). This surfaces as
  `Symbol 'X' already defined` for classes declared across multiple files (a per-file scope
  limitation, not a parser gap).
- **C++ natives** — engine-only symbols (e.g. a native method with no script declaration) are
  reported as `undefined identifier` (a warning, not an error). There's no hardcoded "known
  engine globals" list yet.
- The index is built synchronously in `initialized`; fine for a mod, but large dependency
  roots warrant `spawn_blocking`.

### Parser coverage

The parser was validated against real-world DayZ codebases (the vanilla engine scripts plus a
large mod tree). It handles: nested generics (`map<string, ref array<int>>`), `[attr]`
registration attributes, primitive types as objects/values (`string.Empty`, `!= int`),
`Type name();` default-construction, multi-variable declarations, generic base classes
(`class X : array<T>`), `(ClassName)expr` and `(Type){...}` casts, `.!` negated member access,
compound assignment (`^=`), `reference`/`event`/`local`/`sealed` modifiers, trailing commas,
and the `default` soft keyword. Remaining errors on large codebases are mostly genuine source
bugs (e.g. a missing `;`), not parser gaps.

Note: **bitwise operators (`&` `|` `~` `<<` `>>`) are supported in Enforce** — the old claim
that they give `Unknown operator` was wrong.

## License

GPL-3.0 — see [LICENSE](LICENSE).
