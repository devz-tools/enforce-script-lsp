# Enforce Script Language Server

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)]()
[![Tests](https://img.shields.io/badge/tests-102%20passing-brightgreen)]()
[![License](https://img.shields.io/badge/license-MIT-blue)]()

A comprehensive Language Server Protocol (LSP) implementation for Enforce Script, the scripting language used by the Enfusion game engine and for modding video games such as DayZ.

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

- **Semantic Analyzer** - Symbol table and basic semantic analysis
  - Symbol resolution across scopes
  - Class hierarchy tracking
  - Type inference for variables
  - Undefined symbol detection

- **Diagnostics** - Real-time error and warning detection with accurate positions
  - Syntax errors from parser with line/column information
  - Semantic errors from analyzer
  - Undefined variable warnings

- **Completion** - IntelliSense support
  - All Enforce Script keywords
  - Symbols from symbol table (classes, methods, fields, variables)
  - Context-aware completions

- **Hover Information** - Rich symbol information on hover
  - Symbol type and kind display
  - Markdown-formatted code blocks
  - Definition location highlighting

- **Go to Definition** - Navigate to symbol definitions
  - Jump from usage to definition
  - Works with classes, functions, methods, variables

- **Find References** - Find all symbol occurrences
  - Lists all definitions of a symbol
  - Document-wide symbol search

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

### 🚧 Partially Implemented

- **Workspace Symbols** - Global symbol search (stub implemented)
- **Go to Implementation** - Navigate to implementations (stub implemented)
- **Go to Type Definition** - Navigate to type definitions (stub implemented)
- **Code Actions** - Quick fixes and refactorings (stub implemented)
- **Rename** - Symbol renaming (stub implemented)
- **Inlay Hints** - Inline type/parameter hints (stub implemented)

## Building

### Prerequisites

- Rust 1.70+ (https://rustup.rs/)
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

## Architecture

### Modules

- **`lexer.rs`** - Tokenizes Enforce Script source code into tokens
- **`parser.rs`** - Parses tokens into an Abstract Syntax Tree (AST)
- **`semantic.rs`** - Performs semantic analysis and builds symbol tables
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
