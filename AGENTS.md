# Enforce Script Language Server Developer Agent

This project contains an implementation of an LSP (language server protocol) for the Enforce Script programming language used with the Enfusion game engine and for modding video games such as DayZ.
The LSP is implemented using Rust and the `tower-lsp` library, primarily for use within Visual Studio Code via the `devz-tools` extension.

## Project Structure

```
enforce-script-lsp/
├── Cargo.toml              # Rust package manifest with dependencies
├── AGENTS.md               # This file - developer guide for AI agents
├── ENFORCE-SCRIPT-SYNTAX.md # Complete Enforce Script language specification
├── README.md               # User-facing documentation with quick start
├── TUTORIAL.md             # Comprehensive tutorial for users and developers
├── CONTRIBUTING.md         # Guidelines for contributors
├── IMPLEMENTATION.md       # Implementation summary and architecture overview
└── src/
    ├── main.rs            # Entry point (~15 lines)
    ├── lexer.rs           # Tokenizer (~1,100+ lines, 38 tests)
    ├── parser.rs          # AST generator (~2,200+ lines, 34 tests)
    ├── semantic.rs        # Symbol tables & analysis (~1,050+ lines, 20 tests)
    ├── lsp.rs             # LSP server implementation (~472 lines)
    └── utils/             # Utility modules
        ├── mod.rs         # Module declarations with doc comments
        ├── position.rs    # Position and Span types (~200 lines, 4 tests)
        ├── error.rs       # Error types and handling (~120 lines, 2 tests)
        └── diagnostics.rs # Diagnostic utilities (~180 lines, 4 tests)
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
- **Tests**: 38 comprehensive unit tests covering all keywords, operators, literals (int, float, hex, string, bool, vector), comments, escape sequences, template syntax, modifiers, and special syntax

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
  - Template types (array<T>, map<K,V>)
- **Tests**: 34 comprehensive unit tests covering classes, inheritance, modded classes, constructors/destructors, method modifiers, variables, functions with params, control flow (if/switch/for/foreach/while), arrays, templates, vectors, object creation, enums, strings, this/super, operators, member access, typedef, ref keyword, function overloading, ternary operator, and nested expressions

### 3. Semantic Analyzer (`src/semantic.rs`)
- **Purpose**: Builds symbol tables and performs semantic analysis
- **Key Types**: `SemanticAnalyzer`, `SymbolTable`, `Symbol`, `SymbolKind`
- **Functionality**:
  - Two-pass analysis (declaration registration, then body analysis)
  - Symbol resolution across scopes
  - Type checking and inference
  - Undefined variable/function detection
  - Duplicate declaration detection
  - Scope management (global, class, method, block)
  - Class hierarchy tracking
- **Tests**: 20 comprehensive unit tests covering symbol resolution, undefined variables, duplicate functions, class inheritance, enums, variable scoping, nested scopes, parameters, class members, forward references, const values, modded classes, static members, templates, auto type inference, arrays, typedefs, ref/autoptr, constructors, method access modifiers, and loop variables

### 4. LSP Server (`src/lsp.rs`)
- **Purpose**: Implements Language Server Protocol for editor integration
- **Key Types**: `Backend`, `DocumentData`
- **Functionality**:
  - Document synchronization (open, change, close)
  - Real-time diagnostics with accurate position tracking
  - Completions for keywords and symbols
  - Hover information showing symbol types and definitions
  - Go-to-definition navigation
  - Find-references across the document
  - Signature help for functions/methods
  - Document symbols (hierarchical outline)
  - Folding ranges for classes
  - Stub implementations for: workspace-symbols, go-to-implementation, go-to-type-definition, code-actions, rename, inlay-hints

## LSP Features

### Core Features (Implemented)
- **Diagnostics** (`src/lsp.rs::analyze_document()`) - Real-time error and warning detection with accurate line/column positions
- **Completion** (`src/lsp.rs::completion()`) - IntelliSense for classes, methods, variables, and Enforce Script keywords
- **Hover Information** (`src/lsp.rs::get_hover()`) - Display symbol types, kinds, and definitions with Markdown formatting
- **Go to Definition** (`src/lsp.rs::goto_definition()`) - Navigate to symbol definitions using symbol table lookups
- **Find References** (`src/lsp.rs::references()`) - Find all occurrences of symbols in the current document
- **Signature Help** (`src/lsp.rs::signature_help()`) - Basic parameter hints for functions and methods
- **Document Symbols** (`src/lsp.rs::document_symbol()`) - Hierarchical outline view of classes, methods, enums, and fields
- **Folding Ranges** (`src/lsp.rs::folding_range()`) - Support for collapsing/expanding classes

### Core Features (Stub Implementations - Ready for Enhancement)
- **Workspace Symbols** (`src/lsp.rs::symbol()`) - Search for symbols across all Enforce Script files in the project and in dependencies

### Navigation Features (Stub Implementations - Ready for Enhancement)
- **Go to Implementation** (`src/lsp.rs::goto_implementation()`) - Navigate to method implementations in derived classes
- **Go to Type Definition** (`src/lsp.rs::goto_type_definition()`) - Jump to type definitions for variables and parameters

### Editing Features (Stub Implementations - Ready for Enhancement)
- **Code Actions** (`src/lsp.rs::code_action()`) - Quick fixes for common errors and refactoring suggestions
- **Rename** (`src/lsp.rs::rename()`) - Rename symbols with automatic updates across all files
- **Inlay Hints** (`src/lsp.rs::inlay_hint()`) - Display inferred types and parameter names inline

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

**Current Test Coverage**: 102 tests total
- `src/lexer.rs`: 38 tests covering tokenization of all language constructs
- `src/parser.rs`: 34 tests covering parsing of classes, enums, and expressions
- `src/semantic.rs`: 20 tests covering symbol resolution and error detection
- `src/utils/position.rs`: 4 tests covering position and span operations
- `src/utils/error.rs`: 2 tests covering error creation and display
- `src/utils/diagnostics.rs`: 4 tests covering diagnostic generation

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
- Code is organized into focused modules (lexer, parser, semantic, lsp, utils)
- Common utilities extracted into reusable components
- Consistent error handling with custom error types
- Position tracking throughout the pipeline for accurate diagnostics

### Test Quality
- 102 comprehensive tests with descriptive names
- Tests cover all major features and edge cases
- Both success and failure scenarios tested
- Test coverage exceeds 80% of codebase

### Best Practices
- Follows Rust idioms and conventions
- Uses type-safe abstractions (Position, Span, Diagnostic)
- Efficient data structures (HashMap, DashMap)
- Proper separation of concerns

## Notes

You must update the agents.md, readme.md, and tutorial.md files to reflect any changes made to the implementation or architecture.