//! Error handling utilities for the Enforce Script LSP
//!
//! This module provides error types and utilities for consistent error handling
//! throughout the LSP implementation.

#![allow(dead_code)] // Public API module - will be fully used by LSP features

use std::fmt;

/// Result type alias for LSP operations
pub type Result<T> = std::result::Result<T, LspError>;

/// Errors that can occur in the LSP
#[derive(Debug, Clone)]
pub enum LspError {
    /// Error during lexical analysis
    LexerError {
        message: String,
        line: usize,
        column: usize,
    },
    /// Error during parsing
    ParserError {
        message: String,
        line: usize,
        column: usize,
    },
    /// Error during semantic analysis
    SemanticError {
        message: String,
        line: usize,
        column: usize,
    },
    /// IO error
    IoError(String),
    /// Generic error
    Other(String),
}

impl LspError {
    /// Creates a new lexer error
    pub fn lexer(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::LexerError {
            message: message.into(),
            line,
            column,
        }
    }

    /// Creates a new parser error
    pub fn parser(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::ParserError {
            message: message.into(),
            line,
            column,
        }
    }

    /// Creates a new semantic error
    pub fn semantic(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self::SemanticError {
            message: message.into(),
            line,
            column,
        }
    }

    /// Creates a generic error
    pub fn other(message: impl Into<String>) -> Self {
        Self::Other(message.into())
    }
}

impl fmt::Display for LspError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LspError::LexerError {
                message,
                line,
                column,
            } => write!(f, "Lexer error at {}:{}: {}", line, column, message),
            LspError::ParserError {
                message,
                line,
                column,
            } => write!(f, "Parser error at {}:{}: {}", line, column, message),
            LspError::SemanticError {
                message,
                line,
                column,
            } => write!(f, "Semantic error at {}:{}: {}", line, column, message),
            LspError::IoError(msg) => write!(f, "IO error: {}", msg),
            LspError::Other(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl std::error::Error for LspError {}

impl From<std::io::Error> for LspError {
    fn from(err: std::io::Error) -> Self {
        LspError::IoError(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = LspError::parser("Expected ';'", 10, 5);
        assert!(matches!(err, LspError::ParserError { .. }));
        assert!(err.to_string().contains("Expected ';'"));
        assert!(err.to_string().contains("10:5"));
    }

    #[test]
    fn test_error_display() {
        let err = LspError::lexer("Unexpected character", 1, 10);
        let display = format!("{}", err);
        assert!(display.contains("Lexer error"));
        assert!(display.contains("Unexpected character"));
    }

    #[test]
    fn test_semantic_error_creation() {
        let err = LspError::semantic("Type mismatch: expected int, found string", 15, 8);
        assert!(matches!(err, LspError::SemanticError { .. }));
        assert!(err.to_string().contains("Type mismatch"));
        assert!(err.to_string().contains("15:8"));
    }

    #[test]
    fn test_other_error_creation() {
        let err = LspError::other("Unknown internal error occurred");
        assert!(matches!(err, LspError::Other(_)));
        assert!(err.to_string().contains("Unknown internal error"));
    }

    #[test]
    fn test_error_conversion_chain() {
        // Test that errors can be converted and displayed properly
        let lexer_err = LspError::lexer("Invalid token", 5, 3);
        let parser_err = LspError::parser("Syntax error", 10, 2);
        let semantic_err = LspError::semantic("Undefined variable", 20, 4);

        // All should be displayable
        assert!(!lexer_err.to_string().is_empty());
        assert!(!parser_err.to_string().is_empty());
        assert!(!semantic_err.to_string().is_empty());
    }

    #[test]
    fn test_error_messages_contain_location() {
        let err = LspError::parser("Missing semicolon", 42, 17);
        let msg = err.to_string();
        assert!(msg.contains("42"));
        assert!(msg.contains("17"));
    }
}
