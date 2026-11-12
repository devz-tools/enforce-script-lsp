use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::lexer::Lexer;
use crate::parser::{
    type_ref_base_name, Declaration, Expression, Parser, Program, Span, Statement, TypeRef,
};
use crate::semantic::{GlobalEnvironment, Symbol, SymbolKind};
use crate::workspace::Workspace;

/// A symbol occurrence together with its defining file.
#[derive(Debug, Clone)]
pub struct IndexedSymbol {
    pub uri: String,
    pub symbol: Symbol,
}

/// Role of an identifier occurrence within the AST.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageRole {
    /// A bare identifier in an expression (e.g. `foo` in `foo + 1`, `GetGame()`).
    Bare,
    /// The object of a member access (e.g. `obj` in `obj.member`).
    MemberObject,
    /// The member of a member access (e.g. `member` in `obj.member`).
    MemberMember,
    /// An identifier in a type position (parameter/field/return/base type, enum-extends).
    Type,
}

/// A single identifier usage: where it appears and its resolution context.
#[derive(Debug, Clone)]
#[allow(dead_code)] // Populated now; read by scope-aware find-references (next step).
pub struct IndexedUsage {
    /// The identifier text — keyed under this name in `SymbolIndex::by_usage`.
    pub name: String,
    pub uri: String,
    pub span: Span,
    pub role: UsageRole,
    /// Enclosing class name (None at module level).
    pub enclosing_class: Option<String>,
    /// Start line of the enclosing method/function (None at module level) —
    /// used later to disambiguate same-named locals across methods.
    pub enclosing_method_line: Option<usize>,
    /// The object identifier of a `MemberMember` usage (`obj` in `obj.member`),
    /// so the member's declaring class can be resolved from the object's type.
    /// `None` for other roles and when the object is not a plain identifier.
    pub object_name: Option<String>,
}

/// Cross-file symbol index: name → occurrences across the whole project.
#[derive(Debug, Default)]
pub struct SymbolIndex {
    pub by_name: HashMap<String, Vec<IndexedSymbol>>,
    /// class name → parent class name (from `class X : Parent`).
    pub class_parents: HashMap<String, Option<String>>,
    /// Identifier usages (references), keyed by name — collected from the AST.
    /// Populated during `index_file` alongside `by_name`.
    #[allow(dead_code)] // Read by scope-aware find-references (next step).
    pub by_usage: HashMap<String, Vec<IndexedUsage>>,
    /// Canonical scope-aware identity → occurrences `(uri, span)`. Identity is
    /// `g:<name>` (global), `m:<class>:<name>` (member) or
    /// `l:<uri>:<method_line>:<name>` (local). Populated during `index_file`.
    #[allow(dead_code)] // Read by scope-aware find-references (next step).
    pub by_identity: HashMap<String, Vec<(String, Span)>>,
    /// Per-file local variable/parameter types (uri → name → base type name),
    /// used to resolve a `MemberMember` object's type during the second pass.
    by_local_types: HashMap<String, HashMap<String, String>>,
}

/// Primitive types are not indexed as `g:` identities.
fn is_primitive_type(name: &str) -> bool {
    matches!(name, "int" | "float" | "bool" | "string" | "vector" | "void")
}

/// Precomputed lookup structures derived from `by_name` once, so per-usage
/// identity resolution is O(chain depth) with O(1) member lookups instead of
/// linear scans over every occurrence of a name.
struct ResolveContext {
    /// class/enum name → member bare names (fields, methods, enum variants).
    class_members: HashMap<String, HashSet<String>>,
    /// Global names (classes, enums, functions, global variables).
    globals: HashSet<String>,
    /// Class/enum names (static-access targets).
    types: HashSet<String>,
    /// class name → field name → base type name.
    class_field_types: HashMap<String, HashMap<String, String>>,
}

impl SymbolIndex {
    /// Parse every compiled `.c` file (project + dependency roots) and collect
    /// its symbols ("лайтово": signatures straight from the AST, no semantic pass).
    pub fn build(workspace: &Workspace) -> SymbolIndex {
        let mut index = SymbolIndex::default();
        for dir in &workspace.compiled_dirs {
            index_dir(dir, workspace, &mut index);
        }
        for root in &workspace.dependency_roots {
            index_dir(root, workspace, &mut index);
        }
        // Second pass: resolve usage identities now that the whole index exists.
        index.resolve_identities();
        index
    }

    fn insert(&mut self, uri: &str, symbol: Symbol) {
        self.by_name
            .entry(symbol.name.clone())
            .or_default()
            .push(IndexedSymbol {
                uri: uri.to_string(),
                symbol,
            });
    }

    pub fn lookup(&self, name: &str) -> &[IndexedSymbol] {
        self.by_name.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Derive the cross-file environment (globals + class hierarchy + members)
    /// used to resolve identifiers outside the current file.
    pub fn environment(&self) -> GlobalEnvironment {
        let mut env = GlobalEnvironment::default();
        for (name, occs) in &self.by_name {
            for occ in occs {
                match &occ.symbol.parent {
                    None => {
                        env.globals.insert(name.clone());
                    }
                    Some(parent) => {
                        env.class_members
                            .entry(parent.clone())
                            .or_default()
                            .insert(name.clone());
                        env.all_members.insert(name.clone());
                    }
                }
            }
        }
        env.class_parents = self.class_parents.clone();
        env
    }

    /// Build the resolve context: precomputed member/global/type/field maps
    /// derived from `by_name`, so per-usage resolution is O(chain depth) with
    /// O(1) lookups instead of scanning every occurrence of a name.
    fn build_resolve_context(&self) -> ResolveContext {
        let mut class_members: HashMap<String, HashSet<String>> = HashMap::new();
        let mut globals: HashSet<String> = HashSet::new();
        let mut types: HashSet<String> = HashSet::new();
        let mut class_field_types: HashMap<String, HashMap<String, String>> = HashMap::new();
        for (name, occs) in &self.by_name {
            for occ in occs {
                match &occ.symbol.parent {
                    Some(parent) => match occ.symbol.kind {
                        SymbolKind::EnumVariant => {
                            // Indexed as "Enum.Variant"; register the bare name.
                            let bare = name
                                .strip_prefix(&format!("{}.", parent))
                                .unwrap_or(name.as_str())
                                .to_string();
                            class_members.entry(parent.clone()).or_default().insert(bare);
                        }
                        SymbolKind::Field | SymbolKind::Method => {
                            class_members
                                .entry(parent.clone())
                                .or_default()
                                .insert(name.clone());
                        }
                        _ => {}
                    },
                    None => match occ.symbol.kind {
                        SymbolKind::Class | SymbolKind::Enum => {
                            globals.insert(name.clone());
                            types.insert(name.clone());
                        }
                        SymbolKind::Function | SymbolKind::Variable => {
                            globals.insert(name.clone());
                        }
                        _ => {}
                    },
                }
                if let Some(parent) = &occ.symbol.parent
                    && matches!(occ.symbol.kind, SymbolKind::Field | SymbolKind::Variable)
                    && let Some(t) = &occ.symbol.type_ref
                {
                    class_field_types
                        .entry(parent.clone())
                        .or_default()
                        .insert(name.clone(), type_ref_base_name(t));
                }
            }
        }
        ResolveContext {
            class_members,
            globals,
            types,
            class_field_types,
        }
    }

    /// Compute the canonical identity for one usage (public entry point; builds
    /// the resolve context on demand — used by the single-usage handler).
    pub fn resolve_usage_identity(
        &self,
        usage: &IndexedUsage,
        local_types: &HashMap<String, String>,
    ) -> Option<String> {
        let ctx = self.build_resolve_context();
        self.resolve_usage_identity_ctx(usage, local_types, &ctx)
    }

    fn resolve_usage_identity_ctx(
        &self,
        usage: &IndexedUsage,
        local_types: &HashMap<String, String>,
        ctx: &ResolveContext,
    ) -> Option<String> {
        match usage.role {
            UsageRole::Type => {
                if is_primitive_type(&usage.name) {
                    None
                } else {
                    Some(format!("g:{}", usage.name))
                }
            }
            UsageRole::Bare | UsageRole::MemberObject => self.resolve_bare_identity(usage, ctx),
            UsageRole::MemberMember => self.resolve_member_identity(usage, local_types, ctx),
        }
    }

    /// Bare / object identifier: member of enclosing class → global → local.
    fn resolve_bare_identity(&self, usage: &IndexedUsage, ctx: &ResolveContext) -> Option<String> {
        if let Some(class) = &usage.enclosing_class {
            if let Some(declaring) = self.find_declaring_class(class, &usage.name, ctx) {
                return Some(format!("m:{}:{}", declaring, usage.name));
            }
        }
        if ctx.globals.contains(&usage.name) {
            return Some(format!("g:{}", usage.name));
        }
        Some(format!(
            "l:{}:{}:{}",
            usage.uri,
            usage.enclosing_method_line.unwrap_or(0),
            usage.name
        ))
    }

    /// Member access: resolve the object's type, then find the member's
    /// declaring class in that type's ancestry.
    fn resolve_member_identity(
        &self,
        usage: &IndexedUsage,
        local_types: &HashMap<String, String>,
        ctx: &ResolveContext,
    ) -> Option<String> {
        let object = usage.object_name.as_deref()?;
        let object_type = self.resolve_object_type(object, usage, local_types, ctx)?;
        let declaring = self.find_declaring_class(&object_type, &usage.name, ctx)?;
        Some(format!("m:{}:{}", declaring, usage.name))
    }

    /// Resolve the type of a member-access object: a class/enum name, a local
    /// variable/parameter type, or a field of the enclosing class/ancestors.
    fn resolve_object_type(
        &self,
        object: &str,
        usage: &IndexedUsage,
        local_types: &HashMap<String, String>,
        ctx: &ResolveContext,
    ) -> Option<String> {
        if self.class_parents.contains_key(object) || ctx.types.contains(object) {
            return Some(object.to_string());
        }
        if let Some(t) = local_types.get(object) {
            return Some(t.clone());
        }
        if let Some(class) = &usage.enclosing_class {
            if let Some(t) = self.find_field_type(class, object, ctx) {
                return Some(t);
            }
        }
        None
    }

    /// Find the nearest ancestor of `class` that declares a member `name`
    /// (field, method, or enum variant); return that declaring class/enum.
    fn find_declaring_class(
        &self,
        class: &str,
        name: &str,
        ctx: &ResolveContext,
    ) -> Option<String> {
        let mut current = Some(class.to_string());
        let mut guard = 0;
        while let Some(c) = current {
            if ctx.class_members.get(&c).is_some_and(|m| m.contains(name)) {
                return Some(c);
            }
            current = self.class_parents.get(&c).cloned().flatten();
            guard += 1;
            if guard > 100 {
                break;
            }
        }
        None
    }

    /// Find the type of a field `name` declared in `class` or its ancestors.
    fn find_field_type(&self, class: &str, name: &str, ctx: &ResolveContext) -> Option<String> {
        let mut current = Some(class.to_string());
        let mut guard = 0;
        while let Some(c) = current {
            if let Some(t) = ctx.class_field_types.get(&c).and_then(|f| f.get(name)) {
                return Some(t.clone());
            }
            current = self.class_parents.get(&c).cloned().flatten();
            guard += 1;
            if guard > 100 {
                break;
            }
        }
        None
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
    let Ok(bytes) = std::fs::read(path) else {
        return;
    };
    // Tolerate non-UTF-8 bytes (some vanilla files use Windows-1252) — replace
    // them with U+FFFD instead of skipping the whole file.
    let text = String::from_utf8_lossy(&bytes).into_owned();

    let mut lexer = Lexer::new(&text);
    lexer.set_active_defines(&workspace.defines_for(path));
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let Ok(program) = parser.parse() else {
        return;
    };

    // URL-encode the path (e.g. spaces in "DayZ Projects") so the indexed uri
    // matches the client's `Url` form; otherwise `l:` local identities (which
    // embed the uri) mismatch for vanilla files with spaces in their path.
    let uri = url::Url::from_file_path(path)
        .map(|u| u.to_string())
        .unwrap_or_else(|_| format!("file://{}", path.display()));
    index_program(&program, &uri, index);
}

/// Index one parsed file: register definitions (`by_name`, `class_parents`),
/// collect identifier usages (`by_usage`), and resolve each usage's canonical
/// scope-aware identity (`by_identity`).
fn index_program(program: &Program, uri: &str, index: &mut SymbolIndex) {
    for decl in &program.declarations {
        match decl {
            Declaration::Class(class) => {
                // A `modded class X` without `extends` must not overwrite the base
                // `class X : Parent`'s parent (the modded class just adds members).
                let is_modded = class.modifiers.iter().any(|m| m == "modded");
                if is_modded && class.extends.is_none() {
                    index.class_parents.entry(class.name.clone()).or_insert(None);
                } else {
                    index
                        .class_parents
                        .insert(class.name.clone(), class.extends.clone());
                }
                index.insert(
                    uri,
                    Symbol {
                        name: class.name.clone(),
                        kind: SymbolKind::Class,
                        type_ref: Some(TypeRef::Class(class.name.clone())),
                        span: class.span.clone(),
                        scope: 0,
                        modifiers: class.modifiers.clone(),
                        parent: None,
                    },
                );
                for field in &class.fields {
                    index.insert(
                        uri,
                        Symbol {
                            name: field.name.clone(),
                            kind: SymbolKind::Field,
                            type_ref: Some(field.type_ref.clone()),
                            span: field.span.clone(),
                            scope: 0,
                            modifiers: field.modifiers.clone(),
                            parent: Some(class.name.clone()),
                        },
                    );
                }
                for method in &class.methods {
                    index.insert(
                        uri,
                        Symbol {
                            name: method.name.clone(),
                            kind: SymbolKind::Method,
                            type_ref: Some(method.return_type.clone()),
                            span: method.span.clone(),
                            scope: 0,
                            modifiers: method.modifiers.clone(),
                            parent: Some(class.name.clone()),
                        },
                    );
                }
            }
            Declaration::Enum(enum_decl) => {
                index.insert(
                    uri,
                    Symbol {
                        name: enum_decl.name.clone(),
                        kind: SymbolKind::Enum,
                        type_ref: Some(TypeRef::Primitive("int".to_string())),
                        span: enum_decl.span.clone(),
                        scope: 0,
                        modifiers: Vec::new(),
                        parent: None,
                    },
                );
                for variant in &enum_decl.variants {
                    index.insert(
                        uri,
                        Symbol {
                            name: format!("{}.{}", enum_decl.name, variant.name),
                            kind: SymbolKind::EnumVariant,
                            type_ref: Some(TypeRef::Primitive("int".to_string())),
                            span: variant.span.clone(),
                            scope: 0,
                            modifiers: Vec::new(),
                            parent: Some(enum_decl.name.clone()),
                        },
                    );
                    // DayZ also exposes each variant as a C++-style global
                    // `EnumNameVariantName` (e.g. ObjIntersectView = ObjIntersect.View).
                    index.insert(
                        uri,
                        Symbol {
                            name: format!("{}{}", enum_decl.name, variant.name),
                            kind: SymbolKind::EnumVariant,
                            type_ref: Some(TypeRef::Primitive("int".to_string())),
                            span: variant.span.clone(),
                            scope: 0,
                            modifiers: Vec::new(),
                            parent: None,
                        },
                    );
                }
            }
            Declaration::Typedef(typedef) => {
                // A `typedef ClassName Alias` is a class alias — register the
                // target in class_parents so the inheritance chain resolves
                // through the alias (e.g. `typedef ItemBase InventoryItemSuper`,
                // where `Weapon extends InventoryItemSuper`).
                if let TypeRef::Class(target) = &typedef.type_ref {
                    index
                        .class_parents
                        .entry(typedef.name.clone())
                        .or_insert(Some(target.clone()));
                }
                index.insert(
                    uri,
                    Symbol {
                        name: typedef.name.clone(),
                        kind: SymbolKind::Class, // typedef acts as an alias
                        type_ref: Some(typedef.type_ref.clone()),
                        span: typedef.span.clone(),
                        scope: 0,
                        modifiers: Vec::new(),
                        parent: None,
                    },
                );
            }
            Declaration::Function(method) => {
                index.insert(
                    uri,
                    Symbol {
                        name: method.name.clone(),
                        kind: SymbolKind::Function,
                        type_ref: Some(method.return_type.clone()),
                        span: method.span.clone(),
                        scope: 0,
                        modifiers: method.modifiers.clone(),
                        parent: None,
                    },
                );
            }
            Declaration::Variable(field) => {
                index.insert(
                    uri,
                    Symbol {
                        name: field.name.clone(),
                        kind: SymbolKind::Variable,
                        type_ref: Some(field.type_ref.clone()),
                        span: field.span.clone(),
                        scope: 0,
                        modifiers: field.modifiers.clone(),
                        parent: None,
                    },
                );
            }
        }
    }

    // Collect identifier usages (references) and local variable/parameter types
    // alongside the definitions above. Identity resolution is deferred to a
    // second pass (after the whole project is indexed), so cross-file member
    // references resolve regardless of index order.
    let mut usages = Vec::new();
    let mut local_types = HashMap::new();
    collect_usages(program, None, None, &mut local_types, &mut usages);

    index.by_local_types.insert(uri.to_string(), local_types);
    for usage in usages {
        let mut u = usage;
        u.uri = uri.to_string();
        index.by_usage.entry(u.name.clone()).or_default().push(u);
    }
}

impl SymbolIndex {
    /// Second pass: resolve every usage's identity now that the full index is
    /// built, and populate `by_identity`. Must run after all files are indexed.
    pub fn resolve_identities(&mut self) {
        let ctx = self.build_resolve_context();
        let empty = HashMap::new();
        let mut by_identity: HashMap<String, Vec<(String, Span)>> = HashMap::new();
        for usages in self.by_usage.values() {
            for usage in usages {
                let local_types = self.by_local_types.get(&usage.uri).unwrap_or(&empty);
                if let Some(identity) = self.resolve_usage_identity_ctx(usage, local_types, &ctx) {
                    by_identity
                        .entry(identity)
                        .or_default()
                        .push((usage.uri.clone(), usage.span.clone()));
                }
            }
        }
        self.by_identity = by_identity;
    }
}

/// Walk the AST and collect every identifier usage (references) with context,
/// plus the type of each local variable/parameter (name → base type name) for
/// later member-type resolution.
fn collect_usages(
    program: &Program,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
    local_types: &mut HashMap<String, String>,
    out: &mut Vec<IndexedUsage>,
) {
    for decl in &program.declarations {
        match decl {
            Declaration::Class(class) => {
                let class_name = Some(class.name.as_str());
                if let Some(parent) = &class.extends {
                    push_usage(
                        out,
                        parent,
                        UsageRole::Type,
                        class.span.clone(),
                        class_name,
                        None,
                    );
                }
                for field in &class.fields {
                    collect_type_usages(&field.type_ref, &field.span, class_name, None, out);
                    if let Some(init) = &field.initializer {
                        collect_expr_usages(init, class_name, None, out);
                    }
                }
                for method in &class.methods {
                    let method_line = Some(method.span.start.line);
                    collect_type_usages(
                        &method.return_type,
                        &method.span,
                        class_name,
                        method_line,
                        out,
                    );
                    for param in &method.parameters {
                        collect_type_usages(&param.type_ref, &param.span, class_name, method_line, out);
                        local_types.insert(
                            param.name.clone(),
                            type_ref_base_name(&param.type_ref),
                        );
                        if let Some(default) = &param.default_value {
                            collect_expr_usages(default, class_name, method_line, out);
                        }
                    }
                    if let Some(body) = &method.body {
                        collect_stmt_usages(body, class_name, method_line, local_types, out);
                    }
                }
            }
            Declaration::Enum(enum_decl) => {
                // Variants are definitions, not usages; only the base enum
                // (extends) is a type usage.
                if let Some(parent) = &enum_decl.extends {
                    push_usage(
                        out,
                        parent,
                        UsageRole::Type,
                        enum_decl.span.clone(),
                        enclosing_class,
                        None,
                    );
                }
            }
            Declaration::Typedef(typedef) => {
                collect_type_usages(
                    &typedef.type_ref,
                    &typedef.span,
                    enclosing_class,
                    enclosing_method_line,
                    out,
                );
            }
            Declaration::Function(method) => {
                let method_line = Some(method.span.start.line);
                collect_type_usages(&method.return_type, &method.span, None, method_line, out);
                for param in &method.parameters {
                    collect_type_usages(&param.type_ref, &param.span, None, method_line, out);
                    local_types.insert(
                        param.name.clone(),
                        type_ref_base_name(&param.type_ref),
                    );
                    if let Some(default) = &param.default_value {
                        collect_expr_usages(default, None, method_line, out);
                    }
                }
                if let Some(body) = &method.body {
                    collect_stmt_usages(body, None, method_line, local_types, out);
                }
            }
            Declaration::Variable(field) => {
                collect_type_usages(
                    &field.type_ref,
                    &field.span,
                    enclosing_class,
                    enclosing_method_line,
                    out,
                );
                if let Some(init) = &field.initializer {
                    collect_expr_usages(init, enclosing_class, enclosing_method_line, out);
                }
            }
        }
    }
}

fn push_usage(
    out: &mut Vec<IndexedUsage>,
    name: &str,
    role: UsageRole,
    span: Span,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
) {
    out.push(IndexedUsage {
        name: name.to_string(),
        uri: String::new(),
        span,
        role,
        enclosing_class: enclosing_class.map(|s| s.to_string()),
        enclosing_method_line,
        object_name: None,
    });
}

fn push_member_usage(
    out: &mut Vec<IndexedUsage>,
    member: &str,
    object_name: Option<&str>,
    span: Span,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
) {
    out.push(IndexedUsage {
        name: member.to_string(),
        uri: String::new(),
        span,
        role: UsageRole::MemberMember,
        enclosing_class: enclosing_class.map(|s| s.to_string()),
        enclosing_method_line,
        object_name: object_name.map(|s| s.to_string()),
    });
}

fn collect_type_usages(
    type_ref: &TypeRef,
    span: &Span,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
    out: &mut Vec<IndexedUsage>,
) {
    match type_ref {
        TypeRef::Primitive(_) | TypeRef::Auto => {}
        TypeRef::Class(name) => {
            push_usage(
                out,
                name,
                UsageRole::Type,
                span.clone(),
                enclosing_class,
                enclosing_method_line,
            );
        }
        TypeRef::Template(name, params) => {
            push_usage(
                out,
                name,
                UsageRole::Type,
                span.clone(),
                enclosing_class,
                enclosing_method_line,
            );
            for param in params {
                collect_type_usages(param, span, enclosing_class, enclosing_method_line, out);
            }
        }
        TypeRef::Array(inner, _) => {
            collect_type_usages(inner, span, enclosing_class, enclosing_method_line, out);
        }
    }
}

fn collect_stmt_usages(
    stmt: &Statement,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
    local_types: &mut HashMap<String, String>,
    out: &mut Vec<IndexedUsage>,
) {
    match stmt {
        Statement::VariableDeclaration { type_ref, declarators, span, .. } => {
            collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
            let base = type_ref_base_name(type_ref);
            for (name, init) in declarators {
                local_types.insert(name.clone(), base.clone());
                if let Some(e) = init {
                    collect_expr_usages(e, enclosing_class, enclosing_method_line, out);
                }
            }
        }
        Statement::Expression { expression, .. } => {
            collect_expr_usages(expression, enclosing_class, enclosing_method_line, out);
        }
        Statement::Block { statements, .. } => {
            for s in statements {
                collect_stmt_usages(s, enclosing_class, enclosing_method_line, local_types, out);
            }
        }
        Statement::If { condition, then_branch, else_branch, .. } => {
            collect_expr_usages(condition, enclosing_class, enclosing_method_line, out);
            collect_stmt_usages(then_branch, enclosing_class, enclosing_method_line, local_types, out);
            if let Some(e) = else_branch {
                collect_stmt_usages(e, enclosing_class, enclosing_method_line, local_types, out);
            }
        }
        Statement::Switch { expression, cases, .. } => {
            collect_expr_usages(expression, enclosing_class, enclosing_method_line, out);
            for (case_expr, statements) in cases {
                if let Some(e) = case_expr {
                    collect_expr_usages(e, enclosing_class, enclosing_method_line, out);
                }
                for s in statements {
                    collect_stmt_usages(s, enclosing_class, enclosing_method_line, local_types, out);
                }
            }
        }
        Statement::For { initializer, condition, increment, body, .. } => {
            if let Some(init) = initializer {
                collect_stmt_usages(init, enclosing_class, enclosing_method_line, local_types, out);
            }
            if let Some(c) = condition {
                collect_expr_usages(c, enclosing_class, enclosing_method_line, out);
            }
            if let Some(inc) = increment {
                collect_expr_usages(inc, enclosing_class, enclosing_method_line, out);
            }
            collect_stmt_usages(body, enclosing_class, enclosing_method_line, local_types, out);
        }
        Statement::Foreach { key_var, value_var, iterable, body, span, .. } => {
            if let Some((type_ref, _)) = key_var {
                collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
            }
            collect_type_usages(&value_var.0, span, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(iterable, enclosing_class, enclosing_method_line, out);
            collect_stmt_usages(body, enclosing_class, enclosing_method_line, local_types, out);
        }
        Statement::While { condition, body, .. } => {
            collect_expr_usages(condition, enclosing_class, enclosing_method_line, out);
            collect_stmt_usages(body, enclosing_class, enclosing_method_line, local_types, out);
        }
        Statement::Return { value, .. } => {
            if let Some(v) = value {
                collect_expr_usages(v, enclosing_class, enclosing_method_line, out);
            }
        }
        Statement::Delete { expression, .. } => {
            collect_expr_usages(expression, enclosing_class, enclosing_method_line, out);
        }
        Statement::Break(_) | Statement::Continue(_) => {}
    }
}

fn collect_expr_usages(
    expr: &Expression,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
    out: &mut Vec<IndexedUsage>,
) {
    match expr {
        Expression::Identifier(name, span) => {
            push_usage(
                out,
                name,
                UsageRole::Bare,
                span.clone(),
                enclosing_class,
                enclosing_method_line,
            );
        }
        Expression::MemberAccess { object, member, span } => {
            // The object is a reference (MemberObject) when it is a plain
            // identifier; otherwise recurse (e.g. `a.b.c` → `a` is the object).
            let mut object_name = None;
            if let Expression::Identifier(name, object_span) = &**object {
                push_usage(
                    out,
                    name,
                    UsageRole::MemberObject,
                    object_span.clone(),
                    enclosing_class,
                    enclosing_method_line,
                );
                object_name = Some(name.as_str());
            } else {
                collect_expr_usages(object, enclosing_class, enclosing_method_line, out);
            }
            // The member is a reference (MemberMember). The exact member span is
            // not tracked by the parser yet, so fall back to the whole
            // member-access span; refine later via `find_name_position`.
            push_member_usage(
                out,
                member,
                object_name,
                span.clone(),
                enclosing_class,
                enclosing_method_line,
            );
        }
        Expression::Call { callee, arguments, .. } => {
            collect_expr_usages(callee, enclosing_class, enclosing_method_line, out);
            for arg in arguments {
                collect_expr_usages(arg, enclosing_class, enclosing_method_line, out);
            }
        }
        Expression::Binary { left, right, .. } => {
            collect_expr_usages(left, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(right, enclosing_class, enclosing_method_line, out);
        }
        Expression::Unary { operand, .. } => {
            collect_expr_usages(operand, enclosing_class, enclosing_method_line, out);
        }
        Expression::Index { object, index, .. } => {
            collect_expr_usages(object, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(index, enclosing_class, enclosing_method_line, out);
        }
        Expression::ArrayLiteral { elements, .. } => {
            for element in elements {
                collect_expr_usages(element, enclosing_class, enclosing_method_line, out);
            }
        }
        Expression::New { type_ref, arguments, span } => {
            collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
            for arg in arguments {
                collect_expr_usages(arg, enclosing_class, enclosing_method_line, out);
            }
        }
        Expression::Cast { type_ref, expression, span } => {
            collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(expression, enclosing_class, enclosing_method_line, out);
        }
        Expression::TypeRef(type_ref, span) => {
            collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
        }
        Expression::Ternary { condition, then_expr, else_expr, .. } => {
            collect_expr_usages(condition, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(then_expr, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(else_expr, enclosing_class, enclosing_method_line, out);
        }
        Expression::IntLiteral(..)
        | Expression::FloatLiteral(..)
        | Expression::StringLiteral(..)
        | Expression::BoolLiteral(..)
        | Expression::NullLiteral(..)
        | Expression::VectorLiteral(..)
        | Expression::This(..)
        | Expression::Super(..) => {}
    }
}

#[cfg(test)]
mod collect_undefined {
    use std::collections::BTreeMap;
    use std::path::{Path, PathBuf};

    use super::SymbolIndex;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticAnalyzer;
    use crate::workspace::Workspace;

    fn walk_c_files(dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else { return };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk_c_files(&path, out);
            } else if path.extension().and_then(|e| e.to_str()) == Some("c") {
                out.push(path);
            }
        }
    }

    fn undefined_name(w: &str) -> Option<String> {
        let rest = w.strip_prefix("Undefined identifier '")?;
        let end = rest.find('\'')?;
        Some(rest[..end].to_string())
    }

    /// Bulk-collect the unique "undefined identifier" names across the vanilla
    /// (Steam pack) dependency root. Run with:
    ///   cargo test --release collect_undefined -- --ignored --nocapture
    #[test]
    #[ignore]
    fn collect_undefined() {
        let root = Path::new("/home/devalio/dayz/Work/botorama");
        let steam = Path::new("/mnt/deep-space/Steam/steamapps/compatdata/830640/pfx/drive_c/users/steamuser/Documents/DayZ Projects/scripts");
        let expansion = Path::new("/home/devalio/dayz/Work/DayZ-Expansion-Scripts");
        let deps = vec![steam.to_path_buf(), expansion.to_path_buf()];
        let ws = Workspace::build(root, deps, vec!["FEATURE_NETWORK_RECONCILIATION".to_string()]);
        let index = SymbolIndex::build(&ws);
        let env = index.environment();

        let mut files = Vec::new();
        walk_c_files(steam, &mut files);

        let mut undefined: BTreeMap<String, usize> = BTreeMap::new();
        let mut analyzed = 0usize;
        for path in &files {
            let Ok(bytes) = std::fs::read(path) else { continue };
            let text = String::from_utf8_lossy(&bytes).into_owned();
            let mut lexer = Lexer::new(&text);
            lexer.set_active_defines(&ws.defines_for(path));
            let tokens = lexer.tokenize();
            let mut parser = Parser::new(tokens);
            let Ok(program) = parser.parse() else { continue };
            let mut analyzer = SemanticAnalyzer::new();
            analyzer.set_global_environment(env.clone());
            let _ = analyzer.analyze(&program);
            analyzed += 1;
            for w in analyzer.get_warnings() {
                if let Some(name) = undefined_name(w) {
                    *undefined.entry(name).or_insert(0) += 1;
                }
            }
        }

        println!("--- analyzed {} files ---", analyzed);
        for (name, count) in &undefined {
            println!("{}  x{}", name, count);
        }
        println!("--- {} unique undefined identifiers ---", undefined.len());
    }
}

#[cfg(test)]
mod collect_usages_tests {
    use super::{collect_usages, UsageRole};
    use crate::lexer::Lexer;
    use crate::parser::{Parser, Program};
    use std::collections::HashMap;

    fn parse(code: &str) -> Program {
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        parser.parse().expect("snippet should parse")
    }

    #[test]
    fn collects_bare_member_and_type_usages() {
        let code = r#"
class Foo
{
    MyType m_field;
    void Method(ParamType p)
    {
        int x = someVar;
        obj.Member();
    }
}
"#;
        let program = parse(code);
        let mut usages = Vec::new();
        collect_usages(&program, None, None, &mut HashMap::new(), &mut usages);

        // Field type, with enclosing class context.
        let field = usages
            .iter()
            .find(|u| u.name == "MyType")
            .expect("field type usage");
        assert_eq!(field.role, UsageRole::Type);
        assert_eq!(field.enclosing_class.as_deref(), Some("Foo"));

        // Parameter type.
        assert!(usages.iter().any(|u| u.name == "ParamType" && u.role == UsageRole::Type));

        // Bare identifier in an expression.
        assert!(usages.iter().any(|u| u.name == "someVar" && u.role == UsageRole::Bare));

        // Member access: object and member.
        assert!(usages.iter().any(|u| u.name == "obj" && u.role == UsageRole::MemberObject));
        assert!(usages.iter().any(|u| u.name == "Member" && u.role == UsageRole::MemberMember));
    }

    #[test]
    fn enum_variants_are_not_collected() {
        let code = r#"
enum MyEnum : BaseEnum
{
    Alfa,
    Beta
}
"#;
        let program = parse(code);
        let mut usages = Vec::new();
        collect_usages(&program, None, None, &mut HashMap::new(), &mut usages);

        assert!(usages.iter().any(|u| u.name == "BaseEnum" && u.role == UsageRole::Type));
        assert!(!usages.iter().any(|u| u.name == "Alfa" || u.name == "Beta"));
    }
}

#[cfg(test)]
mod identity_tests {
    use super::{index_program, IndexedUsage, SymbolIndex, UsageRole};
    use crate::lexer::Lexer;
    use crate::parser::{Parser, Position, Span};
    use std::collections::HashMap;

    fn index_snippet(code: &str) -> SymbolIndex {
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().expect("snippet should parse");
        let mut index = SymbolIndex::default();
        index_program(&program, "file:///test.c", &mut index);
        index.resolve_identities();
        index
    }

    #[test]
    fn resolves_bare_member_local_type_and_member_member() {
        let code = r#"
class MyClass
{
    void DoSomething();
}

class Foo
{
    int m_value;
    void MethodA()
    {
        MyClass obj;
        obj.DoSomething();
        int x;
        x = m_value;
    }
    void MethodB()
    {
        int x;
        x = 1;
    }
}

class Bar
{
    int m_value;
    void MethodC()
    {
        int y = m_value;
    }
}
"#;
        let index = index_snippet(code);

        // A bare member inside the class resolves to its declaring class.
        assert!(index.by_identity.contains_key("m:Foo:m_value"));
        assert!(index.by_identity.contains_key("m:Bar:m_value"));

        // A type usage resolves to a global identity.
        assert!(index.by_identity.contains_key("g:MyClass"));

        // Member access through a local variable resolves via its type.
        assert!(index.by_identity.contains_key("m:MyClass:DoSomething"));

        // The local `x` in two different methods gets two distinct identities.
        let local_x: Vec<&String> = index
            .by_identity
            .keys()
            .filter(|k| k.starts_with("l:") && k.ends_with(":x"))
            .collect();
        assert_eq!(local_x.len(), 2, "expected two distinct local x identities");

        // Primitives are not indexed as globals.
        assert!(!index.by_identity.contains_key("g:int"));
        assert!(!index.by_identity.contains_key("g:void"));
    }

    #[test]
    fn member_member_without_identifier_object_resolves_none() {
        let index = SymbolIndex::default();
        let local_types: HashMap<String, String> = HashMap::new();
        let usage = IndexedUsage {
            name: "Member".to_string(),
            uri: "file:///test.c".to_string(),
            span: Span::new(Position::new(1, 0, 0), Position::new(1, 6, 6)),
            role: UsageRole::MemberMember,
            enclosing_class: Some("Foo".to_string()),
            enclosing_method_line: Some(1),
            object_name: None,
        };
        assert_eq!(index.resolve_usage_identity(&usage, &local_types), None);
    }

    #[test]
    fn primitive_type_usage_resolves_none() {
        let index = SymbolIndex::default();
        let local_types: HashMap<String, String> = HashMap::new();
        let usage = IndexedUsage {
            name: "int".to_string(),
            uri: "file:///test.c".to_string(),
            span: Span::new(Position::new(1, 0, 0), Position::new(1, 3, 3)),
            role: UsageRole::Type,
            enclosing_class: None,
            enclosing_method_line: None,
            object_name: None,
        };
        assert_eq!(index.resolve_usage_identity(&usage, &local_types), None);
    }
}
