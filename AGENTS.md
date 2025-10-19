# Enforce Script Language Server Developer Agent

This project contains an implementation of an LSP (language server protocol) for the Enforce Script programming language used with the Enfusion game engine and for modding video games such as DayZ.
The LSP is implemented using Rust and the `tower-lsp` library, primarily for use within Visual Studio Code via the `devz-tools` extension.

## LSP Features

### Core Features
- **Diagnostics** - Real-time error and warning detection for syntax and semantic issues
- **Completion** - IntelliSense for classes, methods, variables, and Enforce Script keywords
- **Hover Information** - Display documentation, type information, and function signatures
- **Signature Help** - Parameter hints when calling functions and methods
- **Go to Definition** - Navigate to class, method, and variable definitions
- **Find References** - Find all usages of symbols across the workspace
- **Document Symbols** - Outline view showing classes, methods, and variables in the current file
- **Workspace Symbols** - Search for symbols across all Enforce Script files in the project and in dependencies

### Navigation Features
- **Go to Implementation** - Navigate to method implementations in derived classes
- **Go to Type Definition** - Jump to type definitions for variables and parameters

### Editing Features
- **Code Actions** - Quick fixes for common errors and refactoring suggestions
- **Rename** - Rename symbols with automatic updates across all files
- **Folding Ranges** - Support for collapsing/expanding code blocks, classes, and methods
- **Inlay Hints** - Display inferred types and parameter names inline