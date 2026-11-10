use dashmap::DashMap;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::RwLock;
/// LSP Server implementation for Enforce Script
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::lexer::Lexer;
use crate::index::SymbolIndex;
use crate::parser::{Class, Declaration, Parser, Program};
use crate::semantic::{SemanticAnalyzer, SymbolKind, SymbolTable};
use crate::workspace::Workspace;

pub struct Backend {
    client: Client,
    documents: Arc<DashMap<String, DocumentData>>,
    workspace: Arc<RwLock<Option<Workspace>>>,
    symbol_index: Arc<RwLock<Option<Arc<SymbolIndex>>>>,
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

        let mut diagnostics = Vec::new();

        let mut parser = Parser::new(tokens);
        let program = match parser.parse() {
            Ok(prog) => {
                // Collect parser errors even when parse succeeds (due to error recovery)
                for error in parser.get_errors() {
                    diagnostics.push(create_diagnostic(error.clone(), DiagnosticSeverity::ERROR));
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
                None
            }
        };

        let symbol_table = if let Some(ref prog) = program {
            let mut analyzer = SemanticAnalyzer::new();
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

    fn get_completions(&self, uri: &str, _position: Position) -> Vec<CompletionItem> {
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

        // Add symbols from the current document's symbol table.
        if let Some(doc) = self.documents.get(uri)
            && let Some(ref symbol_table) = doc.symbol_table
        {
            for symbol in symbol_table.get_all_symbols() {
                if seen.insert(symbol.name.clone()) {
                    items.push(symbol_to_completion(symbol));
                }
            }
        }

        // Add symbols from the cross-file index.
        if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
            for (name, occs) in &index.by_name {
                if seen.insert(name.clone())
                    && let Some(occ) = occs.first()
                {
                    items.push(symbol_to_completion(&occ.symbol));
                }
            }
        }

        items
    }

    fn get_hover(&self, uri: &str, position: Position) -> Option<Hover> {
        let symbol_name = self
            .documents
            .get(uri)
            .and_then(|doc| find_symbol_at_position(&doc.text, position))?;

        // Local symbol table.
        if let Some(doc) = self.documents.get(uri)
            && let Some(ref symbol_table) = doc.symbol_table
            && let Some(symbol) = symbol_table.lookup(&symbol_name)
        {
            return Some(symbol_to_hover(symbol));
        }

        // Cross-file index.
        if let Some(index) = self.symbol_index.read().unwrap().as_ref()
            && let Some(occ) = index.lookup(&symbol_name).first()
        {
            return Some(symbol_to_hover(&occ.symbol));
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
    let lines: Vec<&str> = text.lines().collect();
    let line_idx = position.line as usize;
    let col_idx = position.character as usize;

    if line_idx >= lines.len() {
        return None;
    }

    let line = lines[line_idx];
    if col_idx >= line.len() {
        return None;
    }

    // Find the start of the identifier
    let mut start = col_idx;
    while start > 0 {
        let ch = line.chars().nth(start - 1)?;
        if ch.is_alphanumeric() || ch == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    // Find the end of the identifier
    let mut end = col_idx;
    while end < line.len() {
        let ch = line.chars().nth(end)?;
        if ch.is_alphanumeric() || ch == '_' {
            end += 1;
        } else {
            break;
        }
    }

    if start < end {
        Some(line[start..end].to_string())
    } else {
        None
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Discover config.cpp files and build the workspace model from rootUri.
        if let Some(uri) = params.root_uri {
            if let Ok(root) = uri.to_file_path() {
                let workspace = Workspace::build(&root);
                *self.workspace.write().unwrap() = Some(workspace);
            }
        }

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

        let symbol_name = self
            .documents
            .get(&uri_str)
            .and_then(|doc| find_symbol_at_position(&doc.text, position));
        let Some(symbol_name) = symbol_name else {
            return Ok(None);
        };

        // 1. Local: current document's symbol table.
        if let Some(doc) = self.documents.get(&uri_str)
            && let Some(ref symbol_table) = doc.symbol_table
            && let Some(symbol) = symbol_table.lookup(&symbol_name)
        {
            let location = Location {
                uri,
                range: span_to_range(&symbol.span),
            };
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }

        // 2. Cross-file: global index (prefer a definition in another file).
        if let Some(index) = self.symbol_index.read().unwrap().as_ref() {
            let matches = index.lookup(&symbol_name);
            let best = matches
                .iter()
                .find(|m| m.uri != uri_str && is_definition_kind(&m.symbol.kind))
                .or_else(|| matches.iter().find(|m| m.uri != uri_str))
                .or_else(|| matches.first());
            if let Some(m) = best
                && let Ok(url) = Url::parse(&m.uri)
            {
                let location = Location {
                    uri: url,
                    range: span_to_range(&m.symbol.span),
                };
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
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
