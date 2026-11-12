# Enforce Script Language Server

A comprehensive Language Server Protocol (LSP) implementation for Enforce Script, the scripting language used by the Enfusion game engine and for modding video games such as DayZ.

It is a **project-aware** language server: it discovers a mod's `config.cpp` files, indexes the whole project (and, optionally, the vanilla engine scripts and other dependency roots), resolves symbols across files and up the inheritance chain, and lints for Enforce-specific pitfalls that the engine's own tooling misses.

## 🚀 Quick Start

```bash
# Build the project
cargo build --release

# Run tests
cargo test

# Start the LSP server
cargo run --release
```

## 📚 Documentation

- **[Tutorial](TUTORIAL.md)** - Comprehensive guide to using and extending the LSP
- **[Contributing](CONTRIBUTING.md)** - Guidelines for contributors
- **[Syntax Reference](ENFORCE-SCRIPT-SYNTAX.md)** - Complete Enforce Script language specification
- **[Implementation Details](IMPLEMENTATION.md)** - Architecture and design decisions
- **[Developer Guide](AGENTS.md)** - Guide for AI agents and developers

## Features

### ✅ Implemented Features

- **Lexer/Tokenizer** - Complete tokenization of Enforce Script source code
  - Keywords, operators, literals, identifiers, comments
  - Support for all Enforce Script syntax elements

- **Parser** - Full AST generation for Enforce Script
  - Classes (with inheritance, modded classes, templates)
  - Functions and methods (with modifiers: private, protected, static, override, etc.)
  - Variables and fields (with modifiers: const, ref, autoptr, etc.)
  - Enums (with inheritance and custom values)
  - Control structures (if, else, switch, for, foreach, while)
  - Expressions (binary, unary, member access, indexing, new, etc.)
  - Typedefs
  - Nested generics (`map<string, ref array<int>>`), `[attr]` registration attributes,
    primitive types as objects/values (`string.Empty`, `!= int`), `Type name();`
    default-construction, generic base classes (`class X : array<T>`), C-style and class casts,
    `.!` negated member access, compound assignment (`^=`), and the `default` soft keyword.

- **Semantic Analyzer** - Symbol table and semantic analysis
  - Symbol resolution across scopes
  - Class hierarchy tracking
  - Type inference for variables
  - Undefined symbol detection
  - Cross-file resolution of global symbols and inherited members

- **Diagnostics** - Real-time error and warning detection with accurate positions
  - Syntax errors from parser with line/column information
  - Semantic errors from analyzer
  - Undefined variable warnings

- **Completion** - IntelliSense support
  - All Enforce Script keywords
  - Symbols from symbol table (classes, methods, fields, variables)
  - Context-aware completions
  - Member completion after `.` — resolves `obj.` (or `this.`/`super.`) to the type and offers
    its fields/methods, including inherited ones

- **Hover Information** - Rich symbol information on hover
  - Symbol type and kind display
  - Markdown-formatted code blocks
  - Definition location highlighting

- **Go to Definition** - Navigate to symbol definitions
  - Jump from usage to definition
  - Works with classes, functions, methods, variables
  - Cross-file via the project index

- **Find References** - Find all usages of a symbol, scope-aware and cross-file
  - Resolves the symbol to a canonical identity — global, class member (field/method),
    or local variable — so `m_X` in one class isn't conflated with `m_X` in another
  - Member access (`obj.method`) resolves through the object's declared type
  - Returns every matching usage across the whole project, not just the current file

- **Signature Help** - Function/method parameter hints
  - Basic signature information
  - Shows function names and signatures

- **Document Symbols** - Outline view
  - Classes with fields and methods
  - Functions
  - Enums with variants
  - Hierarchical symbol tree

- **Folding Ranges** - Code folding support
  - Classes and methods
  - Code blocks

### 🧩 Project-aware features

- **config.cpp discovery** — walks the workspace, finds every `config.cpp`, parses
  `dir`/`files[]`/`defines[]`, and derives the set of compiled directories (both the
  "prefix" style `my_mod/3_Game` and the full-path style `SomeMod/Core/Scripts/3_Game`).
- **"excluded from compilation" warning** — flags a `.c` file that is not under any
  module's `files[]` (the classic "forgot to add the directory to config.cpp" footgun).
- **defines-aware `#ifdef`** — keeps/skips `#ifdef X`/`#ifndef X`/`#else`/`#endif` branches
  according to the module's `defines[]` from `config.cpp`, plus the `defaultDefines` option.
- **Cross-file symbol index** — indexes all compiled `.c` files and powers go-to-definition,
  completion, hover, and scope-aware find-references across files (not just the current document).
  Built asynchronously on a blocking thread so the server stays responsive while it indexes.
- **`dependencyRoots`** — indexes extra source roots (the vanilla engine scripts and other
  dependency roots) "lightly" (signatures straight from the AST), so engine globals like
  `Math`, `vector`, `GetGame` resolve too.
- **`lenientMemberScope`** — opt-in looser scoping where a bare name is treated as defined if
  it's a member of any class (diagnostics/navigation only).
- **Engine constants** — a built-in list of engine constants that exist without a script
  declaration (e.g. `DT_CUSTOM`, input-action constants) is suppressed so they don't read as
  `undefined identifier`.
- **Enforce-specific lint** (`lint.rs`) — long `+`/`||`/`&&` chains, a `&&`/`||` nested inside a
  string `+` expression (a native engine crash), the ternary operator, a line break after `.`,
  a variable declared more than once in a method, and a field/variable named after a built-in
  class.

### 🚧 Partially Implemented

- **Member completion after `.`** — resolves `obj.`/`this.`/`super.` to the type's members
  (including inherited ones), but does not yet resolve the return type of a method call
  (`GetWeapon().`) or primitive members (`vector.Zero`).

Stubs (registered but not implemented yet):

- **Workspace Symbols** - Global symbol search
- **Go to Implementation** - Navigate to implementations
- **Go to Type Definition** - Navigate to type definitions
- **Code Actions** - Quick fixes and refactorings
- **Rename** - Symbol renaming
- **Inlay Hints** - Inline type/parameter hints

## Building

### Prerequisites

- Rust 1.85+ (the crate uses edition 2024) (https://rustup.rs/)
- Cargo (comes with Rust)

### Build Instructions

```bash
# Clone the repository
git clone <repository-url>
cd enforce-script-lsp

# Build the project
cargo build --release

# Run tests
cargo test

# Run the LSP server
cargo run --release
```

The compiled binary will be available at `target/release/enforce-script-lsp` (or `.exe` on Windows).

## Usage

The LSP server communicates over stdin/stdout following the Language Server Protocol specification.

### VS Code Integration

To use with VS Code, you'll need to create or update a VS Code extension that uses this LSP server. Example configuration:

```json
{
  "command": "path/to/enforce-script-lsp",
  "args": []
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
| `dependencyRoots` | `string[]` | `[]` | Extra source roots to index (e.g. the vanilla engine scripts). All `.c` under them are indexed "lightly" (signatures from the AST). |
| `defaultDefines` | `string[]` | `[]` | Preprocessor defines applied to every file (engine feature flags that no `config.cpp` declares, e.g. `FEATURE_NETWORK_RECONCILIATION`). Merged into the per-module `defines[]`. |
| `lenientMemberScope` | `boolean` | `false` | Treat a bare name as defined if it's a member of *any* class. Affects diagnostics and navigation only — completion always stays strict. |

The project root is taken from the LSP `rootUri` (the opened folder) — no configuration needed
for a single mod.

## Deploying (operational notes)

Build, then atomically replace the binary and **restart the running server process**:

```bash
cargo build --release
cp target/release/enforce-script-lsp /path/to/enforce-script-lsp.new
mv /path/to/enforce-script-lsp.new /path/to/enforce-script-lsp
# IMPORTANT: the editor/LSP client keeps the OLD inode. Kill the running process so
# the client relaunches against the fresh binary:
pkill -f 'enforce-script-lsp --stdio'   # the editor relaunches it lazily
```

**Why the restart is required.** `mv` swaps the path to a new inode, but a process that
already loaded the binary keeps the old inode in memory. Symptoms of a stale server: recent
fixes don't take effect, `exe` shows `(deleted)` in `ls -li /proc/<PID>/exe`. Check:

```bash
ls -li /proc/$(pgrep -f 'enforce-script-lsp --stdio')/exe   # "(deleted)" = stale
```

## Architecture

### Modules

- **`lexer.rs`** - Tokenizes Enforce Script source code into tokens (plus the
  `#ifdef`/`#ifndef`/`#else`/`#endif` preprocessor)
- **`parser.rs`** - Parses tokens into an Abstract Syntax Tree (AST)
- **`semantic.rs`** - Performs semantic analysis and builds symbol tables (per-file) +
  `GlobalEnvironment` for cross-file resolution
- **`workspace.rs`** - `config.cpp` discovery, prefix map, compiled directories, `defines_for`
- **`index.rs`** - cross-file `SymbolIndex` (name → occurrences) + class hierarchy
- **`lint.rs`** - Enforce-specific lint checks
- **`lsp.rs`** - LSP server implementation using tower-lsp
- **`main.rs`** - Entry point that starts the LSP server

### Key Data Structures

- `Token` - Represents a lexical token with type, lexeme, and position
- `Expression` - Represents Enforce Script expressions in the AST
- `Statement` - Represents Enforce Script statements in the AST
- `Declaration` - Represents top-level declarations (classes, enums, functions, typedefs)
- `Symbol` - Represents a symbol in the symbol table with scope and type information
- `SymbolTable` - Maintains symbols across scopes for semantic analysis

## Testing

The project includes comprehensive unit tests for all major components:

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test module
cargo test lexer::tests
cargo test parser::tests
cargo test semantic::tests
```

### Test Coverage

- ✅ Lexer: Keywords, identifiers, numbers, strings, operators, comments
- ✅ Parser: Classes, enums, functions, expressions, statements
- ✅ Semantic: Symbol resolution, undefined variable detection
- ✅ Integration: End-to-end parsing and analysis

## Enforce Script Syntax Reference

The implementation follows the official Enforce Script syntax as documented in `ENFORCE-SCRIPT-SYNTAX.md`, which covers:

- Basic syntax (code blocks, variables, functions, comments)
- Operators (arithmetic, assignment, relational, logical, bitwise)
- Keywords and modifiers
- Types (primitives, objects, enums, templates, arrays)
- Control structures (if/else, switch, for, foreach, while)
- Object-oriented features (classes, inheritance, constructors/destructors)
- Managed classes and automatic reference counting
- Modding features (modded classes, modded constants)

## Current limitations / roadmap

- **`modded class` merging** — the members of `modded class X` are indexed under `X` (so they
  resolve), but `class_parents` is not merged: a `modded class X` without `extends` overwrites
  the base `class X : Parent`, breaking the inheritance chain. Separately, `Symbol 'X' already
  defined` is a per-file scope error (the same name declared twice in one file — e.g. a global
  function overload), not a cross-file merge issue.
- **Engine-only functions** — engine constants are covered by a built-in list, but engine
  *functions* (`IsInherited`, `IsKindOf`, `GetGame`, …) are not, and still read as
  `undefined identifier` (a warning, not an error).
- **Persistent index cache** — the index is rebuilt from source on every start; a disk cache
  with per-file `mtime`/`size` fingerprints would skip re-parsing unchanged files on cold starts.

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Priority Areas

1. **Complete LSP Features** - Finish implementing goto definition, find references, rename, etc.
2. **Enhanced Type Checking** - More sophisticated type inference and checking
3. **Better Error Recovery** - Improve parser error recovery for better diagnostics
4. **Performance** - Optimize parsing and semantic analysis for large files
5. **Documentation** - Add more inline documentation and examples
6. **Tests** - Expand test coverage for edge cases

### Development

```bash
# Run tests
cargo test

# Format code
cargo fmt

# Run linter
cargo clippy

# Build documentation
cargo doc --open
```

## License

See LICENSE file for details.

## Acknowledgments

- Bohemia Interactive for the Enforce Script language specification
- The tower-lsp library maintainers for the excellent LSP framework
- The Rust community for the robust tooling ecosystem
