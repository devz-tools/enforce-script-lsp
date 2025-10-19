//! Position and span utilities for tracking source code locations.
//!
//! This module provides types and utilities for representing positions and ranges
//! in source code, which are essential for error reporting, diagnostics, and
//! navigation features in the LSP.

#![allow(dead_code)] // Public API module - will be fully used by LSP features

use serde::{Deserialize, Serialize};

/// Represents a position in source code (line and column)
///
/// # Examples
///
/// ```
/// use enforce_script_lsp::utils::position::Position;
///
/// let pos = Position::new(10, 5, 150);
/// assert_eq!(pos.line, 10);
/// assert_eq!(pos.column, 5);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Position {
    /// Line number (0-indexed)
    pub line: usize,
    /// Column number (0-indexed)
    pub column: usize,
    /// Byte offset from the start of the file
    pub offset: usize,
}

impl Position {
    /// Creates a new Position
    ///
    /// # Arguments
    ///
    /// * `line` - The line number (0-indexed)
    /// * `column` - The column number (0-indexed)
    /// * `offset` - The byte offset from the start of the file
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }

    /// Creates a Position at the start of a file
    pub fn start() -> Self {
        Self::new(0, 0, 0)
    }

    /// Advances the position by one character
    ///
    /// # Arguments
    ///
    /// * `c` - The character to advance by (used to determine if it's a newline)
    pub fn advance(&mut self, c: char) {
        self.offset += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::start()
    }
}

/// Represents a span (range) in source code
///
/// A span has a start and end position, representing a continuous region
/// of source code. This is used for tracking the location of tokens, AST nodes,
/// and for error reporting.
///
/// # Examples
///
/// ```
/// use enforce_script_lsp::utils::position::{Position, Span};
///
/// let start = Position::new(1, 0, 10);
/// let end = Position::new(1, 5, 15);
/// let span = Span::new(start, end);
///
/// assert_eq!(span.length(), 5);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    /// Start position of the span
    pub start: Position,
    /// End position of the span
    pub end: Position,
}

impl Span {
    /// Creates a new Span
    ///
    /// # Arguments
    ///
    /// * `start` - The starting position
    /// * `end` - The ending position
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    /// Creates a zero-width span at a given position
    pub fn at(pos: Position) -> Self {
        Self::new(pos, pos)
    }

    /// Returns the length of the span in bytes
    pub fn length(&self) -> usize {
        self.end.offset.saturating_sub(self.start.offset)
    }

    /// Checks if this span contains a given position
    pub fn contains(&self, pos: Position) -> bool {
        pos.offset >= self.start.offset && pos.offset <= self.end.offset
    }

    /// Checks if this span overlaps with another span
    pub fn overlaps(&self, other: &Span) -> bool {
        self.start.offset <= other.end.offset && self.end.offset >= other.start.offset
    }

    /// Merges two spans into a single span covering both
    pub fn merge(&self, other: &Span) -> Span {
        let start = if self.start.offset < other.start.offset {
            self.start
        } else {
            other.start
        };
        let end = if self.end.offset > other.end.offset {
            self.end
        } else {
            other.end
        };
        Span::new(start, end)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::at(Position::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_advance() {
        let mut pos = Position::start();
        pos.advance('a');
        assert_eq!(pos.line, 0);
        assert_eq!(pos.column, 1);
        assert_eq!(pos.offset, 1);

        pos.advance('\n');
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 0);
        assert_eq!(pos.offset, 2);
    }

    #[test]
    fn test_span_contains() {
        let span = Span::new(Position::new(1, 0, 10), Position::new(1, 5, 15));
        assert!(span.contains(Position::new(1, 2, 12)));
        assert!(!span.contains(Position::new(2, 0, 20)));
    }

    #[test]
    fn test_span_overlaps() {
        let span1 = Span::new(Position::new(1, 0, 10), Position::new(1, 5, 15));
        let span2 = Span::new(Position::new(1, 3, 13), Position::new(1, 8, 18));
        assert!(span1.overlaps(&span2));
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(Position::new(1, 0, 10), Position::new(1, 5, 15));
        let span2 = Span::new(Position::new(2, 0, 20), Position::new(2, 3, 23));
        let merged = span1.merge(&span2);

        assert_eq!(merged.start.offset, 10);
        assert_eq!(merged.end.offset, 23);
    }

    #[test]
    fn test_span_length() {
        let span1 = Span::new(Position::new(1, 0, 10), Position::new(1, 5, 15));
        assert_eq!(span1.length(), 5);

        let span2 = Span::new(Position::new(1, 0, 0), Position::new(5, 10, 100));
        assert_eq!(span2.length(), 100);

        let span3 = Span::new(Position::new(0, 0, 0), Position::new(0, 0, 0));
        assert_eq!(span3.length(), 0);
    }
}
