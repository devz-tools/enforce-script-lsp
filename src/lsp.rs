use dashmap::DashMap;
use std::collections::HashSet;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::sync::RwLock;
/// LSP Server implementation for Enforce Script
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::lexer::Lexer;
use crate::index::{IndexedSymbol, SymbolIndex};
use crate::lint::{lint, LintConfig, LintIssue};
use crate::parser::{type_ref_base_name, Class, Declaration, Parser, Program, Span};
use crate::semantic::{SemanticAnalyzer, Symbol, SymbolKind, SymbolTable};
use crate::workspace::Workspace;

pub struct Backend {
    client: Client,
    documents: Arc<DashMap<String, DocumentData>>,
    workspace: Arc<RwLock<Option<Workspace>>>,
    symbol_index: Arc<RwLock<Option<Arc<SymbolIndex>>>>,
    lenient_member_scope: Arc<AtomicBool>,
}

struct DocumentData {
    text: String,
    program: Option<Program>,
    symbol_table: Option<SymbolTable>,
    #[allow(dead_code)] // Will be used for diagnostic caching in LSP
    diagnostics: Vec<Diagnostic>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Backend {
            client,
            documents: Arc::new(DashMap::new()),
            workspace: Arc::new(RwLock::new(None)),
            symbol_index: Arc::new(RwLock::new(None)),
            lenient_member_scope: Arc::new(AtomicBool::new(false)),
        }
    }

    async fn analyze_document(&self, uri: &str, text: &str) {
        // Determine active defines from the file's module config.cpp, so #ifdef
        // branches are kept/skipped correctly.
        let defines: HashSet<String> = Url::parse(uri)
            .ok()
            .and_then(|u| u.to_file_path().ok())
            .and_then(|path| {
                self.workspace
                    .read()
                    .unwrap()
                    .as_ref()
                    .map(|ws| ws.defines_for(&path))
            })
            .unwrap_or_default();

        let mut lexer = Lexer::new(text);
        lexer.set_active_defines(&defines);
        let tokens = lexer.tokenize();
        let tokens_for_lint = tokens.clone();

        let mut diagnostics = Vec::new();

        let mut parser = Parser::new(tokens);
        let program = match parser.parse() {
            Ok(prog) => {
                // Collect parser errors even when parse succeeds (due to error recovery)
                for error in parser.get_errors() {
                    diagnostics.push(create_diagnostic(error.clone(), DiagnosticSeverity::ERROR));
                }
                // Missing-`;` automatic-semicolon-insertion warnings.
                for warning in parser.get_warnings() {
                    diagnostics.push(create_diagnostic(warning.clone(), DiagnosticSeverity::WARNING));
                }
                Some(prog)
            }
            Err(e) => {
                let message = format!("Parse error: {}", e);
                diagnostics.push(create_diagnostic(message, DiagnosticSeverity::ERROR));
                // Also collect any accumulated errors
                for error in parser.get_errors() {
                    diagnostics.push(create_diagnostic(error.clone(), DiagnosticSeverity::ERROR));
                }
                for warning in parser.get_warnings() {
                    diagnostics.push(create_diagnostic(warning.clone(), DiagnosticSeverity::WARNING));
                }
                None
            }
        };

        let symbol_table = if let Some(ref prog) = program {
            let mut analyzer = SemanticAnalyzer::new();
            // Resolve cross-file identifiers (globals, inherited members).
            if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
                analyzer.set_global_environment(index.environment());
            }
            analyzer.set_lenient_member_scope(self.lenient_member_scope.load(Ordering::Relaxed));
            match analyzer.analyze(prog) {
                Ok(_) => {
                    // Add warnings as diagnostics
                    for warning in analyzer.get_warnings() {
                        diagnostics.push(create_diagnostic(
                            warning.clone(),
                            DiagnosticSeverity::WARNING,
                        ));
                    }
                    Some(analyzer.get_symbol_table().clone())
                }
                Err(errors) => {
                    for error in errors {
                        diagnostics.push(create_diagnostic(error, DiagnosticSeverity::ERROR));
                    }
                    Some(analyzer.get_symbol_table().clone())
                }
            }
        } else {
            None
        };

        // Enforce-specific lint checks (chained operators, line break after '.', ...).
        if let Some(ref prog) = program {
            for issue in lint(prog, &tokens_for_lint, &LintConfig::default()) {
                diagnostics.push(lint_issue_to_diagnostic(issue));
            }
        }

        self.documents.insert(
            uri.to_string(),
            DocumentData {
                text: text.to_string(),
                program,
                symbol_table,
                diagnostics: diagnostics.clone(),
            },
        );

        // Warn when a script file is not under any compiled directory (config.cpp files[]).
        if let Some(ws) = self.workspace.read().unwrap().as_ref() {
            if let Ok(url) = Url::parse(uri) {
                if let Ok(path) = url.to_file_path() {
                    let is_config =
                        path.file_name().and_then(|n| n.to_str()) == Some("config.cpp");
                    let is_script = path.extension().and_then(|e| e.to_str()) == Some("c");
                    if is_script && !is_config && !ws.is_compiled(&path) {
                        diagnostics.push(create_diagnostic(
                            "File is not included in any module's files[] — excluded from compilation"
                                .to_string(),
                            DiagnosticSeverity::WARNING,
                        ));
                    }
                }
            }
        }

        // Publish diagnostics
        let uri_parsed = Url::parse(uri).unwrap_or_else(|_| Url::parse("file:///unknown").unwrap());
        self.client
            .publish_diagnostics(uri_parsed, diagnostics, None)
            .await;
    }

    fn get_completions(&self, uri: &str, position: Position) -> Vec<CompletionItem> {
        // Member access: `obj.` → offer the members of `obj`'s type (not everything).
        if let Some(doc) = self.documents.get(uri) {
            let member_items = self.member_completions(&doc, position);
            if !member_items.is_empty() {
                return member_items;
            }
        }

        let mut items = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        // Add keywords
        for keyword in &[
            "class",
            "extends",
            "modded",
            "enum",
            "typedef",
            "void",
            "int",
            "float",
            "bool",
            "string",
            "vector",
            "typename",
            "auto",
            "autoptr",
            "ref",
            "const",
            "static",
            "private",
            "protected",
            "override",
            "proto",
            "native",
            "out",
            "inout",
            "new",
            "delete",
            "this",
            "super",
            "return",
            "null",
            "true",
            "false",
            "thread",
            "if",
            "else",
            "switch",
            "case",
            "default",
            "for",
            "foreach",
            "while",
            "break",
            "continue",
        ] {
            seen.insert(keyword.to_string());
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("Keyword".to_string()),
                ..Default::default()
            });
        }

        // Determine the enclosing class and its inheritance chain. Completion
        // shows the current class's members first, then its ancestors', then
        // globals — members of unrelated classes are excluded (strict namespace).
        let chain: Vec<String> = {
            let doc = self.documents.get(uri);
            let class = doc
                .as_ref()
                .and_then(|d| d.program.as_ref())
                .and_then(|p| enclosing_class(p, position));
            match class {
                Some(c) => match self.symbol_index.read().unwrap().as_ref() {
                    Some(index) => ancestry_chain(&c, index),
                    None => vec![c],
                },
                None => Vec::new(),
            }
        };

        // 1. Class members: the current class, then each ancestor, in order.
        for class in &chain {
            if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
                for occs in index.by_name.values() {
                    if let Some(occ) = occs.iter().find(|o| is_class_member_of(&o.symbol, class))
                        && seen.insert(occ.symbol.name.clone())
                    {
                        items.push(symbol_to_completion(&occ.symbol));
                    }
                }
            }
            if let Some(doc) = self.documents.get(uri)
                && let Some(st) = doc.symbol_table.as_ref()
            {
                for s in st.get_all_symbols() {
                    if is_class_member_of(s, class) && seen.insert(s.name.clone()) {
                        items.push(symbol_to_completion(s));
                    }
                }
            }
        }

        // 2. Globals (classes, enums, typedefs, functions, globals, enum variants).
        if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
            for (name, occs) in &index.by_name {
                if let Some(occ) = occs.iter().find(|o| o.symbol.parent.is_none())
                    && seen.insert(name.clone())
                {
                    items.push(symbol_to_completion(&occ.symbol));
                }
            }
        }
        if let Some(doc) = self.documents.get(uri)
            && let Some(st) = doc.symbol_table.as_ref()
        {
            for s in st.get_all_symbols() {
                if s.parent.is_none() && seen.insert(s.name.clone()) {
                    items.push(symbol_to_completion(s));
                }
            }
        }

        // 3. Local variables/parameters of the enclosing method (span-scoped).
        let doc = self.documents.get(uri);
        let method_span = doc
            .as_ref()
            .and_then(|d| d.program.as_ref())
            .and_then(|p| enclosing_method(p, position));
        let symbol_table = doc.as_ref().and_then(|d| d.symbol_table.as_ref());
        if let (Some(method_span), Some(st)) = (method_span, symbol_table) {
            for s in st.get_all_symbols() {
                if matches!(s.kind, SymbolKind::Variable | SymbolKind::Parameter)
                    && span_within(&method_span, &s.span)
                    && seen.insert(s.name.clone())
                {
                    items.push(symbol_to_completion(s));
                }
            }
        }

        items
    }

    /// Member access completion: `obj.` resolves `obj`'s type and offers its
    /// fields/methods (including inherited), instead of returning everything.
    fn member_completions(&self, doc: &DocumentData, position: Position) -> Vec<CompletionItem> {
        let Some((object, partial)) = find_member_context(&doc.text, position) else {
            return Vec::new();
        };

        let index_guard = self.symbol_index.read().unwrap();
        let Some(index) = index_guard.as_ref() else {
            return Vec::new();
        };

        // Resolve the object's type name.
        let type_name = if object == "this" || object == "super" {
            let enclosing = doc
                .program
                .as_ref()
                .and_then(|p| enclosing_class(p, position));
            match (object.as_str(), enclosing) {
                ("super", Some(class)) => index.class_parents.get(&class).cloned().flatten(),
                (_, Some(class)) => Some(class),
                _ => None,
            }
        } else {
            let enclosing = doc
                .program
                .as_ref()
                .and_then(|p| enclosing_class(p, position));
            resolve_type_name(&object, doc, index, enclosing.as_deref())
        };
        let Some(type_name) = type_name else {
            return Vec::new();
        };

        collect_members(&type_name, index, doc, &partial)
    }

    fn get_hover(&self, uri: &str, position: Position) -> Option<Hover> {
        let doc = self.documents.get(uri)?;
        let symbol_name = find_symbol_at_position(&doc.text, position)?;

        let lenient = self.lenient_member_scope.load(Ordering::Relaxed);

        // 1. Member access (`obj.name`): resolve via the OBJECT's type.
        if let Some((object, _partial)) = find_member_context(&doc.text, position) {
            if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
                let type_name = if object == "this" || object == "super" {
                    let enclosing = doc
                        .program
                        .as_ref()
                        .and_then(|p| enclosing_class(p, position));
                    match (object.as_str(), enclosing) {
                        ("super", Some(c)) => index.class_parents.get(&c).cloned().flatten(),
                        (_, Some(c)) => Some(c),
                        _ => None,
                    }
                } else {
                    let enclosing = doc
                        .program
                        .as_ref()
                        .and_then(|p| enclosing_class(p, position));
                    resolve_type_name(&object, &doc, index, enclosing.as_deref())
                };
                if let Some(type_name) = type_name {
                    if let Some(symbol_table) = doc.symbol_table.as_ref()
                        && let Some(symbol) = symbol_table.lookup_in_class(&type_name, &symbol_name)
                    {
                        return Some(symbol_to_hover(symbol));
                    }
                    if let Some(occ) = find_member_in_hierarchy(&type_name, &symbol_name, index) {
                        return Some(symbol_to_hover(&occ.symbol));
                    }
                }
            }
            return None;
        }

        // 2. Bare identifier: the enclosing class (and ancestors), then globals.
        if let Some(class) = doc
            .program
            .as_ref()
            .and_then(|p| enclosing_class(p, position))
        {
            if let Some(symbol_table) = doc.symbol_table.as_ref()
                && let Some(symbol) = symbol_table.lookup_in_class(&class, &symbol_name)
            {
                return Some(symbol_to_hover(symbol));
            }
            if let Some(index) = self.symbol_index.read().unwrap().as_ref()
                && let Some(occ) = find_member_in_hierarchy(&class, &symbol_name, index)
            {
                return Some(symbol_to_hover(&occ.symbol));
            }
        }

        // 3. Local symbol table.
        if let Some(symbol_table) = doc.symbol_table.as_ref()
            && let Some(symbol) = symbol_table.lookup(&symbol_name)
        {
            return Some(symbol_to_hover(symbol));
        }

        // 4. Cross-file: a global symbol; in lenient mode, also a same-named
        //    member of another class.
        if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
            let matches = index.lookup(&symbol_name);
            let best = matches
                .iter()
                .find(|m| is_definition_kind(&m.symbol.kind))
                .or_else(|| if lenient { matches.first() } else { None });
            if let Some(m) = best {
                return Some(symbol_to_hover(&m.symbol));
            }
        }

        None
    }

    fn get_document_symbols(&self, uri: &str) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();

        if let Some(doc) = self.documents.get(uri)
            && let Some(ref program) = doc.program
        {
            for declaration in &program.declarations {
                match declaration {
                    Declaration::Class(class) => {
                        symbols.push(self.class_to_document_symbol(class));
                    }
                    Declaration::Enum(enum_decl) => {
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: enum_decl.name.clone(),
                            detail: None,
                            kind: tower_lsp::lsp_types::SymbolKind::ENUM,
                            tags: None,
                            deprecated: None,
                            range: span_to_range(&enum_decl.span),
                            selection_range: span_to_range(&enum_decl.span),
                            children: Some(
                                enum_decl
                                    .variants
                                    .iter()
                                    .map(|v| {
                                        #[allow(deprecated)]
                                        DocumentSymbol {
                                            name: v.name.clone(),
                                            detail: v.value.map(|val| val.to_string()),
                                            kind: tower_lsp::lsp_types::SymbolKind::ENUM_MEMBER,
                                            tags: None,
                                            deprecated: None,
                                            range: span_to_range(&v.span),
                                            selection_range: span_to_range(&v.span),
                                            children: None,
                                        }
                                    })
                                    .collect(),
                            ),
                        });
                    }
                    Declaration::Function(function) => {
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: function.name.clone(),
                            detail: Some(format!("{:?}", function.return_type)),
                            kind: tower_lsp::lsp_types::SymbolKind::FUNCTION,
                            tags: None,
                            deprecated: None,
                            range: span_to_range(&function.span),
                            selection_range: span_to_range(&function.span),
                            children: None,
                        });
                    }
                    _ => {}
                }
            }
        }

        symbols
    }

    fn class_to_document_symbol(&self, class: &Class) -> DocumentSymbol {
        let mut children = Vec::new();

        // Add fields
        for field in &class.fields {
            #[allow(deprecated)]
            children.push(DocumentSymbol {
                name: field.name.clone(),
                detail: Some(format!("{:?}", field.type_ref)),
                kind: tower_lsp::lsp_types::SymbolKind::FIELD,
                tags: None,
                deprecated: None,
                range: span_to_range(&field.span),
                selection_range: span_to_range(&field.span),
                children: None,
            });
        }

        // Add methods
        for method in &class.methods {
            #[allow(deprecated)]
            children.push(DocumentSymbol {
                name: method.name.clone(),
                detail: Some(format!("{:?}", method.return_type)),
                kind: tower_lsp::lsp_types::SymbolKind::METHOD,
                tags: None,
                deprecated: None,
                range: span_to_range(&method.span),
                selection_range: span_to_range(&method.span),
                children: None,
            });
        }

        #[allow(deprecated)]
        DocumentSymbol {
            name: class.name.clone(),
            detail: class.extends.as_ref().map(|e| format!("extends {}", e)),
            kind: tower_lsp::lsp_types::SymbolKind::CLASS,
            tags: None,
            deprecated: None,
            range: span_to_range(&class.span),
            selection_range: span_to_range(&class.span),
            children: Some(children),
        }
    }
}

fn span_to_range(span: &crate::parser::Span) -> Range {
    Range::new(
        Position::new((span.start.line - 1) as u32, (span.start.column - 1) as u32),
        Position::new((span.end.line - 1) as u32, (span.end.column - 1) as u32),
    )
}

/// Whether a symbol kind is a top-level "definition" (as opposed to a member).
fn is_definition_kind(kind: &SymbolKind) -> bool {
    matches!(
        kind,
        SymbolKind::Class | SymbolKind::Enum | SymbolKind::Function | SymbolKind::Variable
    )
}

/// Convert a semantic symbol to a completion item.
fn symbol_to_completion(symbol: &crate::semantic::Symbol) -> CompletionItem {
    let kind = match symbol.kind {
        SymbolKind::Class => CompletionItemKind::CLASS,
        SymbolKind::Enum => CompletionItemKind::ENUM,
        SymbolKind::Function | SymbolKind::Method => CompletionItemKind::FUNCTION,
        SymbolKind::Field => CompletionItemKind::FIELD,
        SymbolKind::Variable => CompletionItemKind::VARIABLE,
        SymbolKind::Parameter => CompletionItemKind::VARIABLE,
        SymbolKind::EnumVariant => CompletionItemKind::ENUM_MEMBER,
    };
    CompletionItem {
        label: symbol.name.clone(),
        kind: Some(kind),
        detail: symbol.type_ref.as_ref().map(|t| format!("{:?}", t)),
        ..Default::default()
    }
}

/// Convert a semantic symbol to a hover.
fn symbol_to_hover(symbol: &crate::semantic::Symbol) -> Hover {
    let kind_str = match symbol.kind {
        SymbolKind::Class => "class",
        SymbolKind::Enum => "enum",
        SymbolKind::Function => "function",
        SymbolKind::Method => "method",
        SymbolKind::Field => "field",
        SymbolKind::Variable => "variable",
        SymbolKind::Parameter => "parameter",
        SymbolKind::EnumVariant => "enum variant",
    };
    let type_str = match &symbol.type_ref {
        Some(t) => format!("{:?}", t),
        None => "unknown".to_string(),
    };
    let value = format!(
        "```enforcescript\n({}) {}: {}\n```",
        kind_str, symbol.name, type_str
    );
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(span_to_range(&symbol.span)),
    }
}

/// Convert a lint issue to an LSP diagnostic.
fn lint_issue_to_diagnostic(issue: LintIssue) -> Diagnostic {
    let line = issue.line.saturating_sub(1) as u32;
    Diagnostic {
        range: Range::new(Position::new(line, 0), Position::new(line, 100)),
        severity: Some(if issue.is_error {
            DiagnosticSeverity::ERROR
        } else {
            DiagnosticSeverity::WARNING
        }),
        code: None,
        code_description: None,
        source: Some("enforce-script-lsp (lint)".to_string()),
        message: issue.message,
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Extract dependency roots (vanilla/Expansion dirs) from initializationOptions.
fn extract_dependency_roots(opts: &Option<serde_json::Value>) -> Vec<std::path::PathBuf> {
    opts.as_ref()
        .and_then(|v| v.get("dependencyRoots"))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|s| s.as_str())
                .map(std::path::PathBuf::from)
                .collect()
        })
        .unwrap_or_default()
}

/// Extract `lenientMemberScope` from initialization options (default `false`).
fn extract_lenient_member_scope(opts: &Option<serde_json::Value>) -> bool {
    opts.as_ref()
        .and_then(|v| v.get("lenientMemberScope"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

/// Extract line number from error message (format: "... at line N")
fn extract_line_from_error(message: &str) -> Option<usize> {
    if let Some(idx) = message.rfind("at line ") {
        let line_part = &message[idx + 8..];
        if let Some(end) = line_part.find(|c: char| !c.is_numeric()) {
            line_part[..end].parse().ok()
        } else {
            line_part.parse().ok()
        }
    } else {
        None
    }
}

/// Create diagnostic with proper position from error message
fn create_diagnostic(message: String, severity: DiagnosticSeverity) -> Diagnostic {
    let line = extract_line_from_error(&message).unwrap_or(1);
    // Lines are 1-indexed in error messages but 0-indexed in LSP
    let lsp_line = if line > 0 { line - 1 } else { 0 };

    Diagnostic {
        range: Range::new(
            Position::new(lsp_line as u32, 0),
            Position::new(lsp_line as u32, 100), // Highlight the whole line
        ),
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("enforce-script-lsp".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Find symbol name at given position in text
fn find_symbol_at_position(text: &str, position: Position) -> Option<String> {
    let line = text.lines().nth(position.line as usize)?;

    // Work in character space, not byte space: a byte-index slice into a line
    // containing multi-byte UTF-8 (e.g. Cyrillic comments) would panic on a
    // non-char boundary.
    let chars: Vec<char> = line.chars().collect();
    let col = position.character as usize;
    if col >= chars.len() {
        return None;
    }

    // Find the start of the identifier
    let mut start = col;
    while start > 0 {
        let ch = chars[start - 1];
        if ch.is_alphanumeric() || ch == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    // Find the end of the identifier
    let mut end = col;
    while end < chars.len() {
        let ch = chars[end];
        if ch.is_alphanumeric() || ch == '_' {
            end += 1;
        } else {
            break;
        }
    }

    if start < end {
        Some(chars[start..end].iter().collect())
    } else {
        None
    }
}

/// Detect a member-access completion context `obj.<partial>` at `position`.
/// Returns `(object, partial_member)` where `object` is the trailing identifier
/// before the last `.` and `partial_member` is the (possibly empty) prefix after it.
fn find_member_context(text: &str, position: Position) -> Option<(String, String)> {
    let lines: Vec<&str> = text.lines().collect();
    let line = *lines.get(position.line as usize)?;
    let col = position.character as usize;
    let prefix: String = line.chars().take(col).collect();

    let dot = prefix.rfind('.')?;
    let partial = prefix[dot + 1..].trim().to_string();

    // The object is the trailing word (identifier / this / super) before the dot.
    let before = prefix[..dot].trim_end();
    let obj_start = before
        .rfind(|c: char| !(c.is_alphanumeric() || c == '_'))
        .map(|i| i + 1)
        .unwrap_or(0);
    let object = &before[obj_start..];
    if object.is_empty() {
        return None;
    }

    // The partial must be empty or a valid identifier prefix.
    if !partial.is_empty() && !partial.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return None;
    }

    Some((object.to_string(), partial))
}

/// Find the class whose span contains `position` (for `this`/`super` resolution).
/// Whether the 1-based `(line, column)` position lies within `span` (inclusive).
fn span_contains(span: &Span, line: usize, column: usize) -> bool {
    let after_start = line > span.start.line || (line == span.start.line && column >= span.start.column);
    let before_end = line < span.end.line || (line == span.end.line && column <= span.end.column);
    after_start && before_end
}

/// Whether `inner` lies entirely within `outer` (line-based; parser spans are 1-based).
fn span_within(outer: &Span, inner: &Span) -> bool {
    inner.start.line >= outer.start.line && inner.end.line <= outer.end.line
}

/// Find the (0-based line, 0-based char column) of `name` within `span`, by
/// searching the lines of `text` covered by the span for the first occurrence.
fn find_name_position(text: &str, span: &Span, name: &str) -> Option<(u32, u32)> {
    let lines: Vec<&str> = text.lines().collect();
    // Parser spans use 1-based lines; convert to 0-based.
    let start = span.start.line.saturating_sub(1);
    let end = span.end.line.min(lines.len());
    for idx in start..end {
        let line = lines[idx];
        if let Some(byte_col) = line.find(name) {
            let col = line[..byte_col].chars().count() as u32;
            return Some((idx as u32, col));
        }
    }
    None
}

fn enclosing_class(program: &Program, position: Position) -> Option<String> {
    // LSP lines/characters are 0-based; parser spans are 1-based.
    let line = position.line as usize + 1;
    let column = position.character as usize + 1;
    for decl in &program.declarations {
        if let Declaration::Class(class) = decl {
            if span_contains(&class.span, line, column) {
                return Some(class.name.clone());
            }
        }
    }
    None
}

/// Find the method (or module-level function) whose span contains `position`, so
/// local variables can be scoped to their enclosing method.
fn enclosing_method(program: &Program, position: Position) -> Option<Span> {
    let line = position.line as usize + 1;
    let column = position.character as usize + 1;
    for decl in &program.declarations {
        match decl {
            Declaration::Class(class) => {
                for method in &class.methods {
                    if span_contains(&method.span, line, column) {
                        return Some(method.span.clone());
                    }
                }
            }
            Declaration::Function(method) => {
                if span_contains(&method.span, line, column) {
                    return Some(method.span.clone());
                }
            }
            _ => {}
        }
    }
    None
}

/// Whether `name` is a known class type (has a parent entry or a Class symbol).
fn is_class_type(name: &str, index: &SymbolIndex) -> bool {
    if index.class_parents.contains_key(name) {
        return true;
    }
    index
        .lookup(name)
        .iter()
        .any(|occ| occ.symbol.kind == SymbolKind::Class)
}

/// Whether `name` is a known enum type (has an Enum symbol).
fn is_enum_type(name: &str, index: &SymbolIndex) -> bool {
    index
        .lookup(name)
        .iter()
        .any(|occ| occ.symbol.kind == SymbolKind::Enum)
}

/// Resolve the type name of a member-access object (a local/field/parameter with
/// a class type, or a class/enum name used for static access).
fn resolve_type_name(
    object: &str,
    doc: &DocumentData,
    index: &SymbolIndex,
    enclosing: Option<&str>,
) -> Option<String> {
    // 1. Object is a field/variable/parameter → its declared type.
    if let Some(st) = &doc.symbol_table {
        for s in st.get_all_symbols() {
            if s.name == object
                && matches!(
                    s.kind,
                    SymbolKind::Field | SymbolKind::Variable | SymbolKind::Parameter
                )
                && let Some(t) = &s.type_ref
            {
                let name = type_ref_base_name(t);
                if is_class_type(&name, index) {
                    return Some(name);
                }
            }
        }
    }

    // 2. Object is a class or enum name (static member access).
    if is_class_type(object, index) || is_enum_type(object, index) {
        return Some(object.to_string());
    }

    // 3. Object is a field/variable/parameter inherited from an ancestor class
    //    (declared in another file, so absent from this document's symbol table).
    if let Some(class) = enclosing {
        for ancestor in ancestry_chain(class, index) {
            if let Some(occ) = index.lookup(object).iter().find(|occ| {
                matches!(
                    occ.symbol.kind,
                    SymbolKind::Field | SymbolKind::Variable | SymbolKind::Parameter
                ) && occ.symbol.parent.as_deref() == Some(ancestor.as_str())
            }) && let Some(t) = &occ.symbol.type_ref
            {
                let name = type_ref_base_name(t);
                if is_class_type(&name, index) || is_enum_type(&name, index) {
                    return Some(name);
                }
            }
        }
    }

    None
}

/// Build the inheritance chain `[class, parent, grandparent, …]`, guarding
/// against cycles (via a step cap).
fn ancestry_chain(class: &str, index: &SymbolIndex) -> Vec<String> {
    let mut chain = Vec::new();
    let mut current = Some(class.to_string());
    let mut guard = 0;
    while let Some(c) = current {
        chain.push(c.clone());
        current = index.class_parents.get(&c).cloned().flatten();
        guard += 1;
        if guard > 100 {
            break;
        }
    }
    chain
}

/// Whether `symbol` is a field/method declared directly in `class` (as opposed
/// to a global, a local variable, or a member of another class).
fn is_class_member_of(symbol: &Symbol, class: &str) -> bool {
    matches!(symbol.kind, SymbolKind::Field | SymbolKind::Method)
        && symbol.parent.as_deref() == Some(class)
}

/// Build a completion item for a member. Enum variants are indexed with a
/// dotted `Enum.Variant` name; strip the enum prefix so the completion offers
/// the bare `Variant`.
fn member_completion_item(symbol: &Symbol) -> CompletionItem {
    let mut item = symbol_to_completion(symbol);
    if symbol.kind == SymbolKind::EnumVariant
        && let Some(parent) = &symbol.parent
    {
        let prefix = format!("{}.", parent);
        item.label = symbol
            .name
            .strip_prefix(&prefix)
            .unwrap_or(&symbol.name)
            .to_string();
    }
    item
}

/// Collect the members of `type_name` (walking the inheritance chain) as
/// completion items, filtered by `partial`. Sources: the cross-file index
/// (for inherited/other-file members) and the current document's symbol table
/// (for members being edited right now).
fn collect_members(
    type_name: &str,
    index: &SymbolIndex,
    doc: &DocumentData,
    partial: &str,
) -> Vec<CompletionItem> {
    // Build the ancestry chain (type + all ancestors), guarding against cycles.
    let chain = ancestry_chain(type_name, index);

    let mut seen: HashSet<String> = HashSet::new();
    let mut items = Vec::new();

    // 1. From the cross-file index.
    for occs in index.by_name.values() {
        for occ in occs {
            if matches!(
                occ.symbol.kind,
                SymbolKind::Field | SymbolKind::Method | SymbolKind::EnumVariant
            ) && let Some(parent) = &occ.symbol.parent
                && chain.contains(parent)
                && seen.insert(occ.symbol.name.clone())
            {
                items.push(member_completion_item(&occ.symbol));
            }
        }
    }

    // 2. From the current document (fresh members, in case the file is mid-edit).
    if let Some(st) = &doc.symbol_table {
        for s in st.get_all_symbols() {
            if matches!(
                s.kind,
                SymbolKind::Field | SymbolKind::Method | SymbolKind::EnumVariant
            ) && let Some(parent) = &s.parent
                && chain.contains(parent)
                && seen.insert(s.name.clone())
            {
                items.push(member_completion_item(s));
            }
        }
    }

    // Filter by the partial prefix.
    if !partial.is_empty() {
        items.retain(|i| i.label.starts_with(partial));
    }

    items
}

/// Find the member `name` declared in `class` or any of its ancestors (walking
/// `class_parents`). Used to resolve a field/method to its definition, including
/// inherited members that live in another file.
fn find_member_in_hierarchy<'a>(
    class: &str,
    name: &str,
    index: &'a SymbolIndex,
) -> Option<&'a IndexedSymbol> {
    let chain = ancestry_chain(class, index);
    index
        .lookup(name)
        .iter()
        .find(|occ| occ.symbol.parent.as_ref().is_some_and(|p| chain.contains(p)))
        .or_else(|| index.lookup(&format!("{}.{}", class, name)).first())
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Discover config.cpp files and build the workspace model from rootUri.
        if let Some(uri) = params.root_uri {
            if let Ok(root) = uri.to_file_path() {
                let dependency_roots = extract_dependency_roots(&params.initialization_options);
                let workspace = Workspace::build(&root, dependency_roots);
                *self.workspace.write().unwrap() = Some(workspace);
            }
        }
        self.lenient_member_scope.store(
            extract_lenient_member_scope(&params.initialization_options),
            Ordering::Relaxed,
        );

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "enforce-script-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                rename_provider: Some(OneOf::Left(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                        work_done_progress_options: Default::default(),
                        legend: SemanticTokensLegend {
                            token_types: vec![
                                SemanticTokenType::CLASS,
                                SemanticTokenType::ENUM,
                                SemanticTokenType::FUNCTION,
                                SemanticTokenType::METHOD,
                                SemanticTokenType::PROPERTY,
                                SemanticTokenType::VARIABLE,
                                SemanticTokenType::PARAMETER,
                                SemanticTokenType::ENUM_MEMBER,
                            ],
                            token_modifiers: vec![SemanticTokenModifier::DECLARATION],
                        },
                        range: Some(false),
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                    }),
                ),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // Build the cross-file symbol index from the workspace (blocking, but
        // the project is small; optimize with spawn_blocking later if needed).
        let indexed = {
            let ws = self.workspace.read().unwrap();
            match ws.as_ref() {
                Some(ws) => {
                    let index = SymbolIndex::build(ws);
                    let n = index.by_name.len();
                    *self.symbol_index.write().unwrap() = Some(Arc::new(index));
                    Some(n)
                }
                None => None,
            }
        };

        let msg = match indexed {
            Some(n) => format!("Enforce Script LSP initialized ({} indexed symbols)", n),
            None => "Enforce Script LSP initialized".to_string(),
        };
        self.client.log_message(MessageType::INFO, msg).await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text;
        self.analyze_document(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(change) = params.content_changes.into_iter().next() {
            self.analyze_document(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.documents.remove(&uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let items = self.get_completions(&uri, position);
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };
        let Some(st) = doc.symbol_table.as_ref() else {
            return Ok(None);
        };

        // Map each symbol's name to a (line, col, len, token_type) tuple.
        let mut raw: Vec<(u32, u32, u32, u32)> = Vec::new();
        for s in st.get_all_symbols() {
            let type_index = match s.kind {
                SymbolKind::Class => 0,
                SymbolKind::Enum => 1,
                SymbolKind::Function => 2,
                SymbolKind::Method => 3,
                SymbolKind::Field => 4,
                SymbolKind::Variable => 5,
                SymbolKind::Parameter => 6,
                SymbolKind::EnumVariant => 7,
            };
            let Some((line, col)) = find_name_position(&doc.text, &s.span, &s.name) else {
                continue;
            };
            let len = s.name.chars().count() as u32;
            raw.push((line, col, len, type_index));
        }

        // Delta-encode (sorted by position).
        raw.sort_by_key(|&(l, c, _, _)| (l, c));
        let mut data = Vec::with_capacity(raw.len());
        let (mut prev_line, mut prev_col) = (0u32, 0u32);
        for (line, col, len, type_index) in raw {
            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 { col - prev_col } else { col };
            data.push(SemanticToken {
                delta_line,
                delta_start,
                length: len,
                token_type: type_index,
                token_modifiers_bitset: 1, // declaration
            });
            prev_line = line;
            prev_col = col;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        Ok(self.get_hover(&uri, position))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(&uri) {
            // Find symbol name at position (would need better parsing to find function being called)
            if let Some(symbol_name) = find_symbol_at_position(&doc.text, position)
                && let Some(ref symbol_table) = doc.symbol_table
                && let Some(symbol) = symbol_table.lookup(&symbol_name)
            {
                // Only provide signature help for functions and methods
                if matches!(
                    symbol.kind,
                    crate::semantic::SymbolKind::Function | crate::semantic::SymbolKind::Method
                ) {
                    // For now, provide basic signature info
                    // A full implementation would parse parameters from the symbol
                    let signature = SignatureInformation {
                        label: format!("{}()", symbol.name),
                        documentation: None,
                        parameters: None,
                        active_parameter: None,
                    };

                    return Ok(Some(SignatureHelp {
                        signatures: vec![signature],
                        active_signature: Some(0),
                        active_parameter: None,
                    }));
                }
            }
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let uri_str = uri.to_string();

        let Some(doc) = self.documents.get(&uri_str) else {
            return Ok(None);
        };
        let Some(symbol_name) = find_symbol_at_position(&doc.text, position) else {
            return Ok(None);
        };

        let lenient = self.lenient_member_scope.load(Ordering::Relaxed);

        // 1. Member access (`obj.name`): resolve the member via the OBJECT's type
        //    (not the enclosing class), including inherited members.
        if let Some((object, _partial)) = find_member_context(&doc.text, position) {
            if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
                let type_name = if object == "this" || object == "super" {
                    let enclosing = doc
                        .program
                        .as_ref()
                        .and_then(|p| enclosing_class(p, position));
                    match (object.as_str(), enclosing) {
                        ("super", Some(c)) => index.class_parents.get(&c).cloned().flatten(),
                        (_, Some(c)) => Some(c),
                        _ => None,
                    }
                } else {
                    let enclosing = doc
                        .program
                        .as_ref()
                        .and_then(|p| enclosing_class(p, position));
                    resolve_type_name(&object, &doc, index, enclosing.as_deref())
                };
                if let Some(type_name) = type_name {
                    if let Some(symbol_table) = doc.symbol_table.as_ref()
                        && let Some(symbol) = symbol_table.lookup_in_class(&type_name, &symbol_name)
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range: span_to_range(&symbol.span),
                        })));
                    }
                    if let Some(occ) = find_member_in_hierarchy(&type_name, &symbol_name, index)
                        && let Ok(url) = Url::parse(&occ.uri)
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: url,
                            range: span_to_range(&occ.symbol.span),
                        })));
                    }
                }
            }
            // A member access we couldn't resolve — don't fall through to a
            // same-named global or unrelated member.
            return Ok(None);
        }

        // 2. Bare identifier: the enclosing class (and ancestors), then globals.
        if let Some(class) = doc
            .program
            .as_ref()
            .and_then(|p| enclosing_class(p, position))
        {
            if let Some(symbol_table) = doc.symbol_table.as_ref()
                && let Some(symbol) = symbol_table.lookup_in_class(&class, &symbol_name)
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range: span_to_range(&symbol.span),
                })));
            }
            if let Some(index) = self.symbol_index.read().unwrap().as_ref()
                && let Some(occ) = find_member_in_hierarchy(&class, &symbol_name, index)
                && let Ok(url) = Url::parse(&occ.uri)
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: url,
                    range: span_to_range(&occ.symbol.span),
                })));
            }
        }

        // 3. Local: current document's symbol table (globals).
        if let Some(symbol_table) = doc.symbol_table.as_ref()
            && let Some(symbol) = symbol_table.lookup(&symbol_name)
        {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: span_to_range(&symbol.span),
            })));
        }

        // 4. Cross-file: a global symbol (class/enum/function/variable). Only in
        //    lenient mode do we also fall back to a same-named member of another class.
        if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
            let matches = index.lookup(&symbol_name);
            let best = matches
                .iter()
                .find(|m| is_definition_kind(&m.symbol.kind))
                .or_else(|| if lenient { matches.first() } else { None });
            if let Some(m) = best
                && let Ok(url) = Url::parse(&m.uri)
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: url,
                    range: span_to_range(&m.symbol.span),
                })));
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let uri_str = uri.to_string();

        if let Some(doc) = self.documents.get(&uri_str) {
            // Find symbol name at position
            if let Some(symbol_name) = find_symbol_at_position(&doc.text, position) {
                // For now, just return all symbols with the same name
                // A more sophisticated implementation would track actual usage
                if let Some(ref symbol_table) = doc.symbol_table {
                    let mut locations = Vec::new();

                    // Get all symbols with this name
                    for symbol in symbol_table.get_all_symbols() {
                        if symbol.name == symbol_name {
                            locations.push(Location {
                                uri: uri.clone(),
                                range: span_to_range(&symbol.span),
                            });
                        }
                    }

                    if !locations.is_empty() {
                        return Ok(Some(locations));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri.to_string();
        let symbols = self.get_document_symbols(&uri);

        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn symbol(
        &self,
        _params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        // TODO: Implement workspace symbols
        Ok(None)
    }

    async fn goto_implementation(
        &self,
        _params: request::GotoImplementationParams,
    ) -> Result<Option<request::GotoImplementationResponse>> {
        // TODO: Implement go to implementation
        Ok(None)
    }

    async fn goto_type_definition(
        &self,
        _params: request::GotoTypeDefinitionParams,
    ) -> Result<Option<request::GotoTypeDefinitionResponse>> {
        // TODO: Implement go to type definition
        Ok(None)
    }

    async fn code_action(&self, _params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        // TODO: Implement code actions
        Ok(None)
    }

    async fn rename(&self, _params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        // TODO: Implement rename
        Ok(None)
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        // TODO: Implement folding ranges
        let uri = params.text_document.uri.to_string();
        let mut ranges = Vec::new();

        if let Some(doc) = self.documents.get(&uri)
            && let Some(ref program) = doc.program
        {
            for declaration in &program.declarations {
                if let Declaration::Class(class) = declaration {
                    ranges.push(FoldingRange {
                        start_line: (class.span.start.line - 1) as u32,
                        start_character: Some((class.span.start.column - 1) as u32),
                        end_line: (class.span.end.line - 1) as u32,
                        end_character: Some((class.span.end.column - 1) as u32),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    });
                }
            }
        }

        Ok(Some(ranges))
    }

    async fn inlay_hint(&self, _params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        // TODO: Implement inlay hints
        Ok(None)
    }
}

pub async fn start_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pos(line: u32, character: u32) -> Position {
        Position::new(line, character)
    }

    #[test]
    fn test_find_member_context_object_dot() {
        // "obj." — cursor right after the dot.
        let (object, partial) = find_member_context("obj.", pos(0, 4)).unwrap();
        assert_eq!(object, "obj");
        assert_eq!(partial, "");
    }

    #[test]
    fn test_find_member_context_partial_member() {
        // "obj.foo" — cursor after "foo".
        let (object, partial) = find_member_context("obj.foo", pos(0, 7)).unwrap();
        assert_eq!(object, "obj");
        assert_eq!(partial, "foo");
    }

    #[test]
    fn test_find_member_context_this() {
        let (object, partial) = find_member_context("this.", pos(0, 5)).unwrap();
        assert_eq!(object, "this");
        assert_eq!(partial, "");
    }

    #[test]
    fn test_find_member_context_no_dot() {
        assert!(find_member_context("player", pos(0, 6)).is_none());
    }

    #[test]
    fn test_find_member_context_leading_code() {
        // "void F() { player." — the object is the identifier right before the dot.
        let text = "void F() { player.";
        let col = text.len() as u32; // cursor at end, right after '.'
        let (object, partial) = find_member_context(text, pos(0, col)).unwrap();
        assert_eq!(object, "player");
        assert_eq!(partial, "");
    }

    #[test]
    fn test_enclosing_class() {
        let code = "class A {\n  void F() { this.X(); }\n}\nclass B {\n  void G() {}\n}";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        // Line 1 (0-based) is inside class A.
        let name = enclosing_class(&program, pos(1, 10)).unwrap();
        assert_eq!(name, "A");
        // Line 4 is inside class B.
        let name = enclosing_class(&program, pos(4, 10)).unwrap();
        assert_eq!(name, "B");
    }

    #[test]
    fn test_enclosing_method() {
        let code = "class A {\n  void F() {\n    int x;\n  }\n  void G() {\n    int y;\n  }\n}";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        // Line 2 (0-based) is inside method F.
        let m = enclosing_method(&program, pos(2, 5)).unwrap();
        assert_eq!(m.start.line, 2); // F() spans lines 2..4 (1-based)
        // Line 5 (0-based) is inside method G.
        let m = enclosing_method(&program, pos(5, 5)).unwrap();
        assert_eq!(m.start.line, 5);
        // Line 0 (0-based) is the class header, not inside any method.
        assert!(enclosing_method(&program, pos(0, 5)).is_none());
    }

    #[test]
    fn test_find_name_position() {
        use crate::parser::{Position, Span};

        // Cyrillic comment above the class — exercises char-vs-byte handling
        // (must not panic on a non-char boundary, and must locate the name).
        let text = "//! комментарий про бота\nclass dmAISurvivor\n{\n}";
        let span = Span::new(Position::new(2, 1, 0), Position::new(4, 1, 0));
        let (line, col) = find_name_position(text, &span, "dmAISurvivor").unwrap();
        assert_eq!(line, 1); // 0-based line 1
        assert_eq!(col, 6); // after "class "
    }

    #[test]
    fn test_find_symbol_at_position_cyrillic() {
        // A line whose identifier is preceded by multi-byte UTF-8 must not panic
        // and must resolve the identifier (not a byte-index slice).
        let text = "//! комментарий про бота: m_Weapon.";
        // "m_Weapon" starts after "//! комментарий про бота: " = 29 chars.
        let name = find_symbol_at_position(text, pos(0, 32)).unwrap();
        assert_eq!(name, "m_Weapon");
    }

    #[test]
    fn test_find_symbol_at_position_cyrillic_only() {
        // Identifier that is itself Cyrillic.
        let text = "// бот_идёт";
        // "бот_идёт" starts at char 3 ("// "), cursor in the middle.
        let name = find_symbol_at_position(text, pos(0, 5)).unwrap();
        assert_eq!(name, "бот_идёт");
    }

    #[test]
    fn test_find_member_in_hierarchy() {
        use crate::parser::{Position, Span};

        let mut index = SymbolIndex::default();
        index
            .class_parents
            .insert("Child".to_string(), Some("Base".to_string()));
        index.by_name.insert(
            "m_value".to_string(),
            vec![IndexedSymbol {
                uri: "file:///base.c".to_string(),
                symbol: crate::semantic::Symbol {
                    name: "m_value".to_string(),
                    kind: SymbolKind::Field,
                    type_ref: None,
                    span: Span::new(Position::new(1, 1, 0), Position::new(1, 2, 1)),
                    scope: 0,
                    modifiers: Vec::new(),
                    parent: Some("Base".to_string()),
                },
            }],
        );

        // A member inherited from `Base` resolves for `Child` (walk the hierarchy).
        let found = find_member_in_hierarchy("Child", "m_value", &index).unwrap();
        assert_eq!(found.symbol.parent.as_deref(), Some("Base"));
        // Not part of the hierarchy → None.
        assert!(find_member_in_hierarchy("Unrelated", "m_value", &index).is_none());
    }

    #[test]
    fn test_is_class_member_of() {
        use crate::parser::{Position, Span};

        let mk = |name: &str, kind: SymbolKind, parent: Option<&str>| Symbol {
            name: name.to_string(),
            kind,
            type_ref: None,
            span: Span::new(Position::new(0, 0, 0), Position::new(0, 1, 1)),
            scope: 0,
            modifiers: Vec::new(),
            parent: parent.map(|s| s.to_string()),
        };

        // A field/method of the current class → a class member.
        assert!(is_class_member_of(&mk("m_x", SymbolKind::Field, Some("A")), "A"));
        assert!(is_class_member_of(&mk("Do", SymbolKind::Method, Some("A")), "A"));
        // A member of an unrelated class → NOT a member of `A`.
        assert!(!is_class_member_of(&mk("m_x", SymbolKind::Field, Some("B")), "A"));
        // Globals and locals are handled separately (not class members).
        assert!(!is_class_member_of(&mk("F", SymbolKind::Function, None), "A"));
        assert!(!is_class_member_of(&mk("local", SymbolKind::Variable, Some("A")), "A"));
    }
}
