# Enforce Script Language Server Developer Agent

This project contains an implementation of an LSP (language server protocol) for the Enforce Script programming language used with the Enfusion game engine and for modding video games such as DayZ.
The LSP is implemented using Rust and the `tower-lsp` library, primarily for use within Visual Studio Code via the `devz-tools` extension.

This fork extends the upstream single-file analyzer into a **project-aware** language server: it discovers a mod's `config.cpp` files, indexes the whole project (and, optionally, the vanilla engine scripts and other dependency roots), and resolves symbols across files and up the inheritance chain.

## Project Structure

```
enforce-script-lsp/
├── Cargo.toml              # Rust package manifest with dependencies
├── AGENTS.md               # This file - developer guide for AI agents
├── ENFORCE-SCRIPT-SYNTAX.md # Complete Enforce Script language specification
├── README.md               # User-facing documentation with quick start
└── src/
    ├── main.rs            # Entry point (~15 lines)
    ├── lexer.rs           # Tokenizer + preprocessor (#ifdef/#ifndef/#else/#endif)
    ├── parser.rs          # AST generator (recursive descent)
    ├── semantic.rs        # Per-file symbol tables + GlobalEnvironment (cross-file)
    ├── workspace.rs       # config.cpp discovery, prefix map, compiled dirs
    ├── index.rs           # cross-file SymbolIndex + class hierarchy
    ├── lint.rs            # Enforce-specific lint checks
    ├── lsp.rs             # LSP server implementation (tower-lsp)
    └── utils/             # Utility modules
        ├── mod.rs         # Module declarations with doc comments
        ├── position.rs    # Position and Span types
        ├── error.rs       # Error types and handling
        └── diagnostics.rs # Diagnostic utilities
```

## Core Components

### 1. Lexer (`src/lexer.rs`)
- **Purpose**: Tokenizes Enforce Script source code into typed tokens
- **Key Types**: `TokenType`, `Token`, `Lexer`
- **Functionality**: 
  - Handles all Enforce Script keywords (class, enum, modded, proto, etc.)
  - Operators and delimiters
  - String literals with escape sequences
  - Integer, float, and hex literals
  - Single-line (`//`) and multi-line (`/* */`) comments
  - Vector literals (e.g., `"1 2 3"`)
  - The `#ifdef`/`#ifndef`/`#else`/`#endif` preprocessor (evaluated against active defines)
  - Primitive types as objects/values: `string`/`float`/`int`/`bool`/`vector`/`typename`
    followed by `.` are emitted as identifiers so `string.Empty` is a member access.

### 2. Parser (`src/parser.rs`)
- **Purpose**: Parses tokens into Abstract Syntax Tree (AST)
- **Key Types**: `Parser`, `Declaration`, `Class`, `Enum`, `Method`, `Field`, `Statement`, `Expression`, `TypeRef`, `Parameter`
- **Functionality**:
  - Recursive descent parser with operator precedence
  - Parses classes (with inheritance, modded, proto modifiers)
  - Parses enums (with extends)
  - Methods with parameters, return types, modifiers (static, private, protected, etc.)
  - Full expression parsing (binary, unary, call, member access, array access, new, cast, ternary)
  - All statement types (if, switch, for, foreach, while, return, break, continue, delete)
  - Variable declarations with type inference (`auto`)
  - Template types (array<T>, map<K,V>), including nested generics (`map<string, ref array<int>>`)
  - `[attr]` registration attributes, `(ClassName)expr`/`(Type){...}` casts, `.!` negated
    member access, compound assignment (`^=`), and the `default` soft keyword.

### 3. Semantic Analyzer (`src/semantic.rs`)
- **Purpose**: Builds symbol tables and performs semantic analysis
- **Key Types**: `SemanticAnalyzer`, `SymbolTable`, `Symbol`, `SymbolKind`, `GlobalEnvironment`
- **Functionality**:
  - Two-pass analysis (declaration registration, then body analysis)
  - Symbol resolution across scopes
  - Type checking and inference
  - Undefined variable/function detection
  - Duplicate declaration detection
  - Scope management (global, class, method, block)
  - Class hierarchy tracking
  - Cross-file resolution via `GlobalEnvironment` (globals, class hierarchy, inherited members)

### 4. Workspace (`src/workspace.rs`)
Discovers every `config.cpp` under the project root, parses `dir`/`files[]`/`defines[]`, and
builds:
- `prefix_map`: `files[]` prefix (`my_mod`) → directory of the declaring `config.cpp`.
- `compiled_dirs`: absolute paths that actually enter compilation.
- `dependency_roots`: extra roots (e.g. the vanilla engine scripts) indexed "lightly".
- `defines_for(path)`: walk up to the nearest `config.cpp`, return its `defines[]`.
- `is_compiled(path)`: is the file under a compiled directory?

Handles **two config.cpp styles**: prefix (`my_mod/3_Game`) and full-path
(`SomeMod/Core/Scripts/3_Game`).

### 5. Symbol Index (`src/index.rs`)
`name → Vec<IndexedSymbol{uri, symbol}>` + `class_parents`. Built by walking compiled dirs +
dependency roots, parsing each `.c`, and extracting **signatures straight from the AST** (no
`SemanticAnalyzer` pass) — fast and error-tolerant on engine code. `environment()` derives a
`GlobalEnvironment` used to resolve cross-file identifiers.

### 6. Lint (`src/lint.rs`)
Enforce-specific checks with thresholds in `LintConfig` (tuned to Enforce engine limits): long
`+` chain (>15), long `||`/`&&` chain (>8), ternary (forbidden), line break after `.`
(forbidden), duplicate variable declaration in a method (Enforce has no block scope),
field/variable shadowing a built-in class name. Each finding is a
`LintIssue { message, line, is_error }`.

Note: **bitwise operators (`&` `|` `~` `<<` `>>`) ARE supported in Enforce** — there is no
bitwise lint.

### 7. LSP Server (`src/lsp.rs`)
- **Purpose**: Implements Language Server Protocol for editor integration
- **Key Types**: `Backend`, `DocumentData`
- **Functionality**:
  - Document synchronization (open, change, close)
  - Real-time diagnostics with accurate position tracking
  - Completions for keywords and symbols
  - Hover information showing symbol types and definitions
  - Go-to-definition navigation (cross-file via the index)
  - Find-references across the document
  - Signature help for functions/methods
  - Document symbols (hierarchical outline)
  - Folding ranges for classes
  - Stub implementations for: workspace-symbols, go-to-implementation, go-to-type-definition, code-actions, rename, inlay-hints

## Data flow

```
initialize(rootUri, dependencyRoots)
  └─ Workspace::build(root, deps)   → configs, prefix_map, compiled_dirs, dependency_roots
initialized
  └─ SymbolIndex::build(&workspace) → by_name, class_parents  (AST-only, "light" indexing)
did_open(file)
  └─ lex (with module defines) → parse → per-file semantic (with GlobalEnvironment) → lint
```

## LSP Features

### Core Features (Implemented)
- **Diagnostics** - Real-time error and warning detection with accurate line/column positions
- **Completion** - IntelliSense for classes, methods, variables, and Enforce Script keywords
- **Hover Information** - Display symbol types, kinds, and definitions with Markdown formatting
- **Go to Definition** - Navigate to symbol definitions using symbol table lookups (cross-file)
- **Find References** - Find all occurrences of symbols in the current document
- **Signature Help** - Basic parameter hints for functions and methods
- **Document Symbols** - Hierarchical outline view of classes, methods, enums, and fields
- **Folding Ranges** - Support for collapsing/expanding classes

### Stub Implementations (Ready for Enhancement)
- **Workspace Symbols** - Search for symbols across all Enforce Script files
- **Go to Implementation** - Navigate to method implementations in derived classes
- **Go to Type Definition** - Jump to type definitions for variables and parameters
- **Code Actions** - Quick fixes for common errors and refactoring suggestions
- **Rename** - Rename symbols with automatic updates across all files
- **Inlay Hints** - Display inferred types and parameter names inline

## Dependencies (`Cargo.toml`)

- **tower-lsp** 0.20 - LSP framework providing JSON-RPC communication
- **tokio** 1.35 (full features) - Async runtime for LSP server
- **dashmap** 5.5 - Concurrent hash map for thread-safe document storage
- **serde** 1.0 + **serde_json** - JSON serialization for LSP protocol
- **regex** 1.10 - Pattern matching support

## Enforce Script Syntax Reference

- `/ENFORCE-SCRIPT-SYNTAX.md` - Comprehensive reference for Enforce Script syntax and constructs that MUST be followed when implementing features

## Testing

**Location**: Tests are co-located with modules using `#[cfg(test)]` sections

**Current Test Coverage**: 147 tests total.

**Running Tests**: 
```bash
cargo test                # Run all tests
cargo test --release      # Run tests in release mode
cargo test lexer::tests   # Run only lexer tests
```

**Test Requirements**:
- You must write thorough tests for any new features or bug fixes
- Tests should cover various scenarios and edge cases
- Use descriptive test names and include comments explaining test purpose
- Test both success cases and error conditions
- For parser tests, verify AST structure matches expected output
- For semantic tests, verify symbol tables are built correctly and errors are detected

## Building and Running

```bash
# Development build
cargo build

# Release build (optimized)
cargo build --release

# Run the LSP server (connects via stdin/stdout)
cargo run

# Run with logging
RUST_LOG=debug cargo run

# Check code without building
cargo check
```

## Conventions

- **No `eprintln!` in parser/semantic error recovery.** On engine code the parser produces
  thousands of recoverable errors; `eprintln!` floods stderr and **blocks** the pipe, hanging
  indexing. Collect errors into `self.errors`, never print them.
- `Symbol` carries `parent: Option<String>`: `None` = global, `Some(class)` = member. The
  `environment()` split relies on this.
- Config is read from `initializationOptions` in `initialize` (currently `dependencyRoots`).
- The parser is validated against real-world DayZ codebases; new syntax must not regress
  `cargo test` nor re-introduce parse errors on real code.

## Architecture Notes

- **Lexer → Parser → Semantic → LSP**: Pipeline architecture with clear separation of concerns
- **Position Tracking**: All tokens, AST nodes, and symbols track source positions (line, column, offset) for accurate diagnostics and navigation
- **Concurrent Document Storage**: `DashMap` allows thread-safe access to parsed documents
- **Two-Pass Semantic Analysis**: First pass registers all declarations, second pass analyzes bodies (allows forward references)
- **Error Recovery**: Parser includes basic error recovery to continue parsing after errors
- **Span Tracking**: `Span` struct tracks start/end positions for all AST nodes (useful for folding, selection, etc.)

## Code Quality

### Documentation Standards
- All public APIs have comprehensive doc comments with examples
- Module-level documentation explains purpose and structure
- Complex algorithms include inline explanatory comments
- All utility functions are documented

### Maintainability
- Code is organized into focused modules (lexer, parser, semantic, workspace, index, lint, lsp, utils)
- Common utilities extracted into reusable components
- Consistent error handling with custom error types
- Position tracking throughout the pipeline for accurate diagnostics

### Test Quality
- 147 comprehensive tests with descriptive names
- Tests cover all major features and edge cases
- Both success and failure scenarios tested

### Best Practices
- Follows Rust idioms and conventions
- Uses type-safe abstractions (Position, Span, Diagnostic)
- Efficient data structures (HashMap, DashMap)
- Proper separation of concerns

## Operational notes

- **After deploying the binary (`cp … .new && mv`), restart the running server process.** `mv`
  swaps the path to a new inode, but an already-running process keeps the old inode in memory.
  `pkill -f 'enforce-script-lsp --stdio'` — OpenCode relaunches it lazily. Verify with
  `ls -li /proc/<PID>/exe`: a `(deleted)` suffix means stale.

## Notes

You must update the `AGENTS.md` and `README.md` files to reflect any changes made to the implementation or architecture.
