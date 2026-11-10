# enforce-script-lsp — Developer Guide

A Language Server Protocol server for **Enforce Script** (DayZ / Enfusion modding), written in
Rust on `tower-lsp`. Forked from `devz-tools/enforce-script-lsp` and extended from a
single-file analyzer into a **project-aware** language server.

## Project structure

```
enforce-script-lsp/
├── Cargo.toml               # deps: tower-lsp, tokio, serde, dashmap, regex
├── README.md                # user-facing docs
├── AGENTS.md                # this file
├── ENFORCE-SCRIPT-SYNTAX.md # language reference (syntax, not implementation)
├── editor/vscode/           # VS Code / Cursor client extension
└── src/
    ├── main.rs              # entry point
    ├── lexer.rs             # tokenizer + preprocessor (#ifdef/#ifndef/#else/#endif)
    ├── parser.rs            # recursive-descent AST
    ├── semantic.rs          # per-file symbol table + GlobalEnvironment
    ├── workspace.rs         # config.cpp discovery, prefix map, compiled dirs
    ├── index.rs             # cross-file SymbolIndex + class hierarchy
    ├── lint.rs              # Enforce-specific lint checks
    ├── lsp.rs               # tower-lsp Backend
    └── utils/               # position, error, diagnostics
```

## Data flow

```
initialize(rootUri, dependencyRoots)
  └─ Workspace::build(root, deps)   → configs, prefix_map, compiled_dirs, dependency_roots
initialized
  └─ SymbolIndex::build(&workspace) → by_name, class_parents  (AST-only, "light" indexing)
did_open(file)
  └─ lex (with module defines) → parse → per-file semantic (with GlobalEnvironment) → lint
```

## Key concepts

### `Workspace` (workspace.rs)
Discovers every `config.cpp` under the project root, parses `dir`/`files[]`/`defines[]`, and
builds:
- `prefix_map`: `files[]` prefix (`dm_cons`) → directory of the declaring `config.cpp`.
- `compiled_dirs`: absolute paths that actually enter compilation.
- `dependency_roots`: extra roots (vanilla/Expansion) indexed "lightly" (all `.c`, no config).
- `defines_for(path)`: walk up to the nearest `config.cpp`, return its `defines[]`.
- `is_compiled(path)`: is the file under a compiled directory?

Handles **two config.cpp styles**: prefix (`dm_cons/3_Game`) and full-path
(`DayZExpansion/Core/Scripts/3_Game`). See the plan for the layer/`modded class` model.

### `SymbolIndex` (index.rs)
`name → Vec<IndexedSymbol{uri, symbol}>` + `class_parents`. Built by walking compiled dirs +
dependency roots, parsing each `.c`, and extracting **signatures straight from the AST**
(no `SemanticAnalyzer` pass) — fast and error-tolerant on engine code.

`environment()` derives a `GlobalEnvironment` (globals + class hierarchy + members) used to
resolve cross-file identifiers.

### `GlobalEnvironment` (semantic.rs)
Cross-file resolution state passed to the per-file `SemanticAnalyzer`:
- `globals`: names of global symbols (parent == `None`).
- `class_parents`: `class X : Parent` map.
- `class_members`: `class → set of member names`.
- `has_member(class, name)`: walk the ancestry chain.

This is what removes `undefined identifier` for inherited fields (`m_Concurrency`) and global
enums (`dmBotIntentConcurrency`).

### Preprocessor (lexer.rs)
`#ifdef X` / `#ifndef X` / `#else` / `#endif` evaluated against the module's active defines
(a conditional stack; a token is emitted only when all levels are active). **Empty defines =
skip all `#ifdef`** (the default when no `config.cpp` is known, e.g. vanilla files).

### Lint (lint.rs)
Enforce-specific checks with thresholds in `LintConfig` (currently defaults from botorama's
`codeguide.md`): long `+` chain (>6), long `||`/`&&` chain (>8), ternary (forbidden), line
break after `.` (forbidden). Each finding is a `LintIssue { message, line, is_error }`.

## Conventions

- **No `eprintln!` in parser/semantic error recovery.** On vanilla code the parser produces
  thousands of recoverable errors; `eprintln!` floods stderr and **blocks** the pipe, hanging
  indexing. Collect errors into `self.errors`, never print them.
- Tests are co-located (`#[cfg(test)] mod tests` in each module). Add tests for new behavior.
- `Symbol` carries `parent: Option<String>`: `None` = global, `Some(class)` = member. The
  `environment()` split relies on this.
- Config is read from `initializationOptions` in `initialize` (currently `dependencyRoots`).

## Extending

- **New lint rule** → add a branch in `lint.rs::lint_expression`/`lint_statement` + a test.
- **New config option** → read it in `lsp.rs::initialize` (like `extract_dependency_roots`).
- **New LSP capability** → wire it in `lsp.rs`'s `Backend`, falling back to `self.symbol_index`
  when the current document's table misses.

## Testing

```bash
cargo test
```

## Housekeeping

When you change the implementation or architecture, update `README.md` (features/config) and
this file (modules/concepts) to match.
