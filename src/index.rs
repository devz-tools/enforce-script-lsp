use std::collections::HashMap;
use std::path::Path;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::semantic::{SemanticAnalyzer, Symbol};
use crate::workspace::Workspace;

/// A symbol occurrence together with its defining file.
#[derive(Debug, Clone)]
pub struct IndexedSymbol {
    pub uri: String,
    pub symbol: Symbol,
}

/// Cross-file symbol index: name → occurrences across the whole project.
#[derive(Debug, Default)]
pub struct SymbolIndex {
    pub by_name: HashMap<String, Vec<IndexedSymbol>>,
}

impl SymbolIndex {
    /// Parse every compiled `.c` file and collect its symbols.
    pub fn build(workspace: &Workspace) -> SymbolIndex {
        let mut index = SymbolIndex::default();
        for dir in &workspace.compiled_dirs {
            index_dir(dir, workspace, &mut index);
        }
        index
    }

    pub fn lookup(&self, name: &str) -> &[IndexedSymbol] {
        self.by_name.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }
}

fn index_dir(dir: &Path, workspace: &Workspace, index: &mut SymbolIndex) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            index_dir(&path, workspace, index);
        } else if path.extension().and_then(|e| e.to_str()) == Some("c") {
            index_file(&path, workspace, index);
        }
    }
}

fn index_file(path: &Path, workspace: &Workspace, index: &mut SymbolIndex) {
    let Ok(text) = std::fs::read_to_string(path) else {
        return;
    };

    let mut lexer = Lexer::new(&text);
    lexer.set_active_defines(&workspace.defines_for(path));
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let Ok(program) = parser.parse() else {
        return;
    };

    let mut analyzer = SemanticAnalyzer::new();
    let _ = analyzer.analyze(&program);

    let uri = format!("file://{}", path.display());
    for symbol in analyzer.get_symbol_table().get_all_symbols() {
        // Only top-level symbols and class members (scope 0); skip locals/params.
        if symbol.scope != 0 {
            continue;
        }
        index
            .by_name
            .entry(symbol.name.clone())
            .or_default()
            .push(IndexedSymbol {
                uri: uri.clone(),
                symbol: symbol.clone(),
            });
    }
}
