//! Diagnostic generation utilities for LSP
//!
//! This module provides utilities for creating LSP diagnostics from errors
//! detected during lexing, parsing, and semantic analysis.

#![allow(dead_code)] // Public API module - will be fully used by LSP features

use crate::utils::position::Span;
use serde::{Deserialize, Serialize};

/// Severity level of a diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    /// Reports an error
    Error,
    /// Reports a warning
    Warning,
    /// Reports an information message
    Information,
    /// Reports a hint
    Hint,
}

/// A diagnostic message produced by the LSP
///
/// Diagnostics are used to report errors, warnings, and other issues
/// to the user in their editor.
///
/// # Examples
///
/// ```
/// use enforce_script_lsp::utils::diagnostics::{Diagnostic, DiagnosticSeverity};
/// use enforce_script_lsp::utils::position::{Position, Span};
///
/// let diagnostic = Diagnostic::new(
///     Span::new(Position::new(1, 5, 5), Position::new(1, 10, 10)),
///     DiagnosticSeverity::Error,
///     "Undefined variable 'foo'".to_string(),
/// );
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    /// The source code range this diagnostic applies to
    pub span: Span,
    /// The severity level
    pub severity: DiagnosticSeverity,
    /// The diagnostic message
    pub message: String,
    /// Optional diagnostic code
    pub code: Option<String>,
    /// Optional related information
    pub related: Vec<RelatedInformation>,
}

impl Diagnostic {
    /// Creates a new diagnostic
    pub fn new(span: Span, severity: DiagnosticSeverity, message: String) -> Self {
        Self {
            span,
            severity,
            message,
            code: None,
            related: Vec::new(),
        }
    }

    /// Creates a new error diagnostic
    pub fn error(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticSeverity::Error, message)
    }

    /// Creates a new warning diagnostic
    pub fn warning(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticSeverity::Warning, message)
    }

    /// Creates a new information diagnostic
    pub fn info(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticSeverity::Information, message)
    }

    /// Creates a new hint diagnostic
    pub fn hint(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticSeverity::Hint, message)
    }

    /// Sets the diagnostic code
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Adds related information
    pub fn with_related(mut self, related: RelatedInformation) -> Self {
        self.related.push(related);
        self
    }
}

/// Related information for a diagnostic
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RelatedInformation {
    /// The span of the related information
    pub span: Span,
    /// The message describing the relationship
    pub message: String,
}

impl RelatedInformation {
    /// Creates new related information
    pub fn new(span: Span, message: String) -> Self {
        Self { span, message }
    }
}

/// A collection of diagnostics
#[derive(Debug, Default, Clone)]
pub struct DiagnosticCollection {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticCollection {
    /// Creates a new empty diagnostic collection
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    /// Adds a diagnostic to the collection
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Adds an error diagnostic
    pub fn error(&mut self, span: Span, message: String) {
        self.add(Diagnostic::error(span, message));
    }

    /// Adds a warning diagnostic
    pub fn warning(&mut self, span: Span, message: String) {
        self.add(Diagnostic::warning(span, message));
    }

    /// Adds an info diagnostic
    pub fn info(&mut self, span: Span, message: String) {
        self.add(Diagnostic::info(span, message));
    }

    /// Adds a hint diagnostic
    pub fn hint(&mut self, span: Span, message: String) {
        self.add(Diagnostic::hint(span, message));
    }

    /// Returns all diagnostics in the collection
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Returns the number of diagnostics
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Returns true if the collection is empty
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Clears all diagnostics
    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }

    /// Returns the number of errors
    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .count()
    }

    /// Returns the number of warnings
    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::position::Position;

    #[test]
    fn test_diagnostic_creation() {
        let span = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));
        let diagnostic = Diagnostic::error(span, "Test error".to_string());

        assert_eq!(diagnostic.severity, DiagnosticSeverity::Error);
        assert_eq!(diagnostic.message, "Test error");
        assert!(diagnostic.code.is_none());
    }

    #[test]
    fn test_diagnostic_with_code() {
        let span = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));
        let diagnostic = Diagnostic::error(span, "Test error".to_string()).with_code("E001");

        assert_eq!(diagnostic.code, Some("E001".to_string()));
    }

    #[test]
    fn test_diagnostic_collection() {
        let mut collection = DiagnosticCollection::new();
        let span = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));

        collection.error(span, "Error 1".to_string());
        collection.warning(span, "Warning 1".to_string());
        collection.error(span, "Error 2".to_string());

        assert_eq!(collection.len(), 3);
        assert_eq!(collection.error_count(), 2);
        assert_eq!(collection.warning_count(), 1);
    }

    #[test]
    fn test_related_information() {
        let span1 = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));
        let span2 = Span::new(Position::new(2, 0, 10), Position::new(2, 5, 15));

        let related = RelatedInformation::new(span2, "Defined here".to_string());
        let diagnostic =
            Diagnostic::error(span1, "Undefined variable".to_string()).with_related(related);

        assert_eq!(diagnostic.related.len(), 1);
        assert_eq!(diagnostic.related[0].message, "Defined here");
    }

    #[test]
    fn test_info_and_hint_diagnostics() {
        let span = Span::new(Position::new(1, 0, 0), Position::new(1, 10, 10));

        let info = Diagnostic::info(span, "Info message".to_string());
        assert_eq!(info.severity, DiagnosticSeverity::Information);
        assert_eq!(info.message, "Info message");

        let hint = Diagnostic::hint(span, "Hint message".to_string());
        assert_eq!(hint.severity, DiagnosticSeverity::Hint);
        assert_eq!(hint.message, "Hint message");
    }

    #[test]
    fn test_diagnostic_collection_utilities() {
        let span = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));
        let mut collection = DiagnosticCollection::new();

        assert!(collection.is_empty());
        assert_eq!(collection.len(), 0);

        collection.info(span, "Info message".to_string());
        assert!(!collection.is_empty());
        assert_eq!(collection.len(), 1);

        collection.hint(span, "Hint message".to_string());
        assert_eq!(collection.len(), 2);

        // Test diagnostics getter
        let diagnostics = collection.diagnostics();
        assert_eq!(diagnostics.len(), 2);

        collection.clear();
        assert!(collection.is_empty());
        assert_eq!(collection.len(), 0);
    }

    #[test]
    fn test_diagnostic_collection_mixed_severities() {
        let span = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));
        let mut collection = DiagnosticCollection::new();

        collection.error(span, "Error".to_string());
        collection.warning(span, "Warning".to_string());
        collection.info(span, "Info".to_string());
        collection.hint(span, "Hint".to_string());

        assert_eq!(collection.len(), 4);
        assert_eq!(collection.error_count(), 1);
        assert_eq!(collection.warning_count(), 1);

        let diagnostics = collection.diagnostics();
        assert_eq!(diagnostics[0].severity, DiagnosticSeverity::Error);
        assert_eq!(diagnostics[1].severity, DiagnosticSeverity::Warning);
        assert_eq!(diagnostics[2].severity, DiagnosticSeverity::Information);
        assert_eq!(diagnostics[3].severity, DiagnosticSeverity::Hint);
    }

    #[test]
    fn test_diagnostic_with_code_and_related() {
        let span1 = Span::new(Position::new(1, 0, 0), Position::new(1, 5, 5));
        let span2 = Span::new(Position::new(2, 0, 10), Position::new(2, 5, 15));

        let related = RelatedInformation::new(span2, "See here".to_string());
        let diagnostic = Diagnostic::error(span1, "Complex error".to_string())
            .with_code("E100")
            .with_related(related);

        assert_eq!(diagnostic.code, Some("E100".to_string()));
        assert_eq!(diagnostic.related.len(), 1);
        assert_eq!(diagnostic.message, "Complex error");
    }
}
