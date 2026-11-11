use std::collections::HashMap;
use std::path::Path;

use crate::lexer::Lexer;
use crate::parser::{Declaration, Parser, TypeRef};
use crate::semantic::{GlobalEnvironment, Symbol, SymbolKind};
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
    /// class name → parent class name (from `class X : Parent`).
    pub class_parents: HashMap<String, Option<String>>,
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
}
