use std::collections::HashMap;
use std::path::Path;

use crate::lexer::Lexer;
use crate::parser::{Declaration, Expression, Parser, Program, Span, Statement, TypeRef};
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

    let uri = format!("file://{}", path.display());
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
                    &uri,
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
                        &uri,
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
                        &uri,
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
                    &uri,
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
                        &uri,
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
                        &uri,
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
                    &uri,
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
                    &uri,
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
                    &uri,
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

    // Collect identifier usages (references) alongside the definitions above.
    let mut usages = Vec::new();
    collect_usages(&program, None, None, &mut usages);
    for mut usage in usages {
        usage.uri = uri.clone();
        index
            .by_usage
            .entry(usage.name.clone())
            .or_default()
            .push(usage);
    }
}

/// Walk the AST and collect every identifier usage (references) with context.
fn collect_usages(
    program: &Program,
    enclosing_class: Option<&str>,
    enclosing_method_line: Option<usize>,
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
                        if let Some(default) = &param.default_value {
                            collect_expr_usages(default, class_name, method_line, out);
                        }
                    }
                    if let Some(body) = &method.body {
                        collect_stmt_usages(body, class_name, method_line, out);
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
                    if let Some(default) = &param.default_value {
                        collect_expr_usages(default, None, method_line, out);
                    }
                }
                if let Some(body) = &method.body {
                    collect_stmt_usages(body, None, method_line, out);
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
    out: &mut Vec<IndexedUsage>,
) {
    match stmt {
        Statement::VariableDeclaration { type_ref, declarators, span, .. } => {
            collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
            for (_, init) in declarators {
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
                collect_stmt_usages(s, enclosing_class, enclosing_method_line, out);
            }
        }
        Statement::If { condition, then_branch, else_branch, .. } => {
            collect_expr_usages(condition, enclosing_class, enclosing_method_line, out);
            collect_stmt_usages(then_branch, enclosing_class, enclosing_method_line, out);
            if let Some(e) = else_branch {
                collect_stmt_usages(e, enclosing_class, enclosing_method_line, out);
            }
        }
        Statement::Switch { expression, cases, .. } => {
            collect_expr_usages(expression, enclosing_class, enclosing_method_line, out);
            for (case_expr, statements) in cases {
                if let Some(e) = case_expr {
                    collect_expr_usages(e, enclosing_class, enclosing_method_line, out);
                }
                for s in statements {
                    collect_stmt_usages(s, enclosing_class, enclosing_method_line, out);
                }
            }
        }
        Statement::For { initializer, condition, increment, body, .. } => {
            if let Some(init) = initializer {
                collect_stmt_usages(init, enclosing_class, enclosing_method_line, out);
            }
            if let Some(c) = condition {
                collect_expr_usages(c, enclosing_class, enclosing_method_line, out);
            }
            if let Some(inc) = increment {
                collect_expr_usages(inc, enclosing_class, enclosing_method_line, out);
            }
            collect_stmt_usages(body, enclosing_class, enclosing_method_line, out);
        }
        Statement::Foreach { key_var, value_var, iterable, body, span, .. } => {
            if let Some((type_ref, _)) = key_var {
                collect_type_usages(type_ref, span, enclosing_class, enclosing_method_line, out);
            }
            collect_type_usages(&value_var.0, span, enclosing_class, enclosing_method_line, out);
            collect_expr_usages(iterable, enclosing_class, enclosing_method_line, out);
            collect_stmt_usages(body, enclosing_class, enclosing_method_line, out);
        }
        Statement::While { condition, body, .. } => {
            collect_expr_usages(condition, enclosing_class, enclosing_method_line, out);
            collect_stmt_usages(body, enclosing_class, enclosing_method_line, out);
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
            if let Expression::Identifier(name, object_span) = &**object {
                push_usage(
                    out,
                    name,
                    UsageRole::MemberObject,
                    object_span.clone(),
                    enclosing_class,
                    enclosing_method_line,
                );
            } else {
                collect_expr_usages(object, enclosing_class, enclosing_method_line, out);
            }
            // The member is a reference (MemberMember). The exact member span is
            // not tracked by the parser yet, so fall back to the whole
            // member-access span; refine later via `find_name_position`.
            push_usage(
                out,
                member,
                UsageRole::MemberMember,
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
        collect_usages(&program, None, None, &mut usages);

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
        collect_usages(&program, None, None, &mut usages);

        assert!(usages.iter().any(|u| u.name == "BaseEnum" && u.role == UsageRole::Type));
        assert!(!usages.iter().any(|u| u.name == "Alfa" || u.name == "Beta"));
    }
}
