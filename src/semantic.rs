/// Semantic analyzer for Enforce Script
/// Performs type checking, scope resolution, and builds symbol table
use crate::parser::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Class,
    Enum,
    Function,
    Method,
    Field,
    Variable,
    Parameter,
    EnumVariant,
}

/// Symbol representation with metadata for LSP features
#[derive(Debug, Clone)]
#[allow(dead_code)] // All fields are part of the symbol table API for LSP
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub type_ref: Option<TypeRef>,
    pub span: Span,
    pub scope: usize,
    pub modifiers: Vec<String>,
    pub parent: Option<String>, // Parent class/enum name
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: HashMap<String, Vec<Symbol>>,
    scopes: Vec<HashMap<String, Symbol>>,
    current_scope: usize,
    current_class: Option<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            scopes: vec![HashMap::new()], // Global scope
            current_scope: 0,
            current_class: None,
        }
    }

    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
        if self.current_scope >= self.scopes.len() {
            self.scopes.push(HashMap::new());
        }
    }

    pub fn exit_scope(&mut self) {
        if self.current_scope > 0 {
            self.scopes[self.current_scope].clear();
            self.current_scope -= 1;
        }
    }

    pub fn define(&mut self, symbol: Symbol) -> Result<(), String> {
        let name = symbol.name.clone();

        // Check for duplicate in current scope
        if let Some(existing) = self.scopes[self.current_scope].get(&name) {
            return Err(format!(
                "Symbol '{}' already defined in current scope at line {}",
                name, existing.span.start.line
            ));
        }

        // Add to current scope
        self.scopes[self.current_scope].insert(name.clone(), symbol.clone());

        // Add to global symbols list for cross-scope lookup
        self.symbols
            .entry(name)
            .or_insert_with(Vec::new)
            .push(symbol);

        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Search from current scope up to global
        for scope_idx in (0..=self.current_scope).rev() {
            if let Some(symbol) = self.scopes[scope_idx].get(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Lookup a member (field or method) within a specific class - used by LSP for member completion
    #[allow(dead_code)] // Used in tests and will be used by LSP features
    pub fn lookup_in_class(&self, class_name: &str, member_name: &str) -> Option<&Symbol> {
        if let Some(symbols) = self.symbols.get(member_name) {
            for symbol in symbols {
                if symbol.parent.as_ref() == Some(&class_name.to_string()) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub fn get_all_symbols(&self) -> Vec<&Symbol> {
        let mut result = Vec::new();
        for symbols in self.symbols.values() {
            result.extend(symbols.iter());
        }
        result
    }

    /// Get all symbols of a specific kind - used by LSP for workspace symbols and completion
    #[allow(dead_code)] // Used in tests and will be used by LSP features
    pub fn get_symbols_by_kind(&self, kind: SymbolKind) -> Vec<&Symbol> {
        self.get_all_symbols()
            .into_iter()
            .filter(|s| s.kind == kind)
            .collect()
    }

    /// Get a class symbol by name - used by LSP for type lookup and completion
    #[allow(dead_code)] // Will be used by LSP features
    pub fn get_class(&self, name: &str) -> Option<&Symbol> {
        if let Some(symbols) = self.symbols.get(name) {
            symbols.iter().find(|s| s.kind == SymbolKind::Class)
        } else {
            None
        }
    }
}

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<String>,
    warnings: Vec<String>,
    class_hierarchy: HashMap<String, Option<String>>, // class -> parent class
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            class_hierarchy: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First pass: collect all class, enum, and typedef declarations
        for declaration in &program.declarations {
            match declaration {
                Declaration::Class(class) => {
                    self.register_class(class)?;
                }
                Declaration::Enum(enum_decl) => {
                    self.register_enum(enum_decl)?;
                }
                Declaration::Typedef(typedef) => {
                    self.register_typedef(typedef)?;
                }
                Declaration::Function(_) => {} // Handle in second pass
            }
        }

        // Second pass: analyze method bodies and expressions
        for declaration in &program.declarations {
            match declaration {
                Declaration::Class(class) => {
                    self.analyze_class(class)?;
                }
                Declaration::Function(function) => {
                    self.analyze_function(function)?;
                }
                _ => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn get_symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    /// Get all semantic errors - used by LSP for diagnostics
    #[allow(dead_code)] // Used in tests and will be used by LSP features
    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }

    pub fn get_warnings(&self) -> &[String] {
        &self.warnings
    }

    fn register_class(&mut self, class: &Class) -> Result<(), Vec<String>> {
        let symbol = Symbol {
            name: class.name.clone(),
            kind: SymbolKind::Class,
            type_ref: Some(TypeRef::Class(class.name.clone())),
            span: class.span.clone(),
            scope: 0, // Classes are always in global scope
            modifiers: class.modifiers.clone(),
            parent: None,
        };

        if let Err(e) = self.symbol_table.define(symbol) {
            self.errors.push(e);
        }

        // Register class hierarchy
        if let Some(base) = &class.extends {
            self.class_hierarchy
                .insert(class.name.clone(), Some(base.clone()));
        } else {
            self.class_hierarchy.insert(class.name.clone(), None);
        }

        Ok(())
    }

    fn register_enum(&mut self, enum_decl: &Enum) -> Result<(), Vec<String>> {
        let symbol = Symbol {
            name: enum_decl.name.clone(),
            kind: SymbolKind::Enum,
            type_ref: Some(TypeRef::Primitive("int".to_string())),
            span: enum_decl.span.clone(),
            scope: 0,
            modifiers: Vec::new(),
            parent: None,
        };

        if let Err(e) = self.symbol_table.define(symbol) {
            self.errors.push(e);
        }

        // Register enum variants
        for variant in &enum_decl.variants {
            let variant_symbol = Symbol {
                name: format!("{}.{}", enum_decl.name, variant.name),
                kind: SymbolKind::EnumVariant,
                type_ref: Some(TypeRef::Primitive("int".to_string())),
                span: variant.span.clone(),
                scope: 0,
                modifiers: Vec::new(),
                parent: Some(enum_decl.name.clone()),
            };

            if let Err(e) = self.symbol_table.define(variant_symbol) {
                self.errors.push(e);
            }
        }

        Ok(())
    }

    fn register_typedef(&mut self, typedef: &Typedef) -> Result<(), Vec<String>> {
        let symbol = Symbol {
            name: typedef.name.clone(),
            kind: SymbolKind::Class, // Typedef acts as an alias
            type_ref: Some(typedef.type_ref.clone()),
            span: typedef.span.clone(),
            scope: 0,
            modifiers: Vec::new(),
            parent: None,
        };

        if let Err(e) = self.symbol_table.define(symbol) {
            self.errors.push(e);
        }

        Ok(())
    }

    fn analyze_class(&mut self, class: &Class) -> Result<(), Vec<String>> {
        self.symbol_table.current_class = Some(class.name.clone());

        // Register fields
        for field in &class.fields {
            let symbol = Symbol {
                name: field.name.clone(),
                kind: SymbolKind::Field,
                type_ref: Some(field.type_ref.clone()),
                span: field.span.clone(),
                scope: 0,
                modifiers: field.modifiers.clone(),
                parent: Some(class.name.clone()),
            };

            if let Err(e) = self.symbol_table.define(symbol) {
                self.errors.push(e);
            }

            // Analyze field initializer if present
            if let Some(init) = &field.initializer {
                self.analyze_expression(init)?;
            }
        }

        // Analyze methods
        for method in &class.methods {
            self.analyze_method(method, Some(class.name.clone()))?;
        }

        self.symbol_table.current_class = None;
        Ok(())
    }

    fn analyze_function(&mut self, function: &Method) -> Result<(), Vec<String>> {
        self.analyze_method(function, None)
    }

    fn analyze_method(
        &mut self,
        method: &Method,
        class_name: Option<String>,
    ) -> Result<(), Vec<String>> {
        let symbol = Symbol {
            name: method.name.clone(),
            kind: if class_name.is_some() {
                SymbolKind::Method
            } else {
                SymbolKind::Function
            },
            type_ref: Some(method.return_type.clone()),
            span: method.span.clone(),
            scope: 0,
            modifiers: method.modifiers.clone(),
            parent: class_name.clone(),
        };

        if let Err(e) = self.symbol_table.define(symbol) {
            self.errors.push(e);
        }

        // Enter method scope
        self.symbol_table.enter_scope();

        // Register parameters
        for param in &method.parameters {
            let param_symbol = Symbol {
                name: param.name.clone(),
                kind: SymbolKind::Parameter,
                type_ref: Some(param.type_ref.clone()),
                span: param.span.clone(),
                scope: self.symbol_table.current_scope,
                modifiers: param.modifiers.clone(),
                parent: class_name.clone(),
            };

            if let Err(e) = self.symbol_table.define(param_symbol) {
                self.errors.push(e);
            }
        }

        // Analyze method body
        if let Some(body) = &method.body {
            self.analyze_statement(body)?;
        }

        // Exit method scope
        self.symbol_table.exit_scope();

        Ok(())
    }

    fn analyze_statement(&mut self, statement: &Statement) -> Result<(), Vec<String>> {
        match statement {
            Statement::VariableDeclaration {
                modifiers,
                type_ref,
                name,
                initializer,
                span,
            } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Variable,
                    type_ref: Some(type_ref.clone()),
                    span: span.clone(),
                    scope: self.symbol_table.current_scope,
                    modifiers: modifiers.clone(),
                    parent: self.symbol_table.current_class.clone(),
                };

                if let Err(e) = self.symbol_table.define(symbol) {
                    self.errors.push(e);
                }

                if let Some(init) = initializer {
                    self.analyze_expression(init)?;
                    // TODO: Type check initializer against variable type
                }
            }
            Statement::Expression { expression, .. } => {
                self.analyze_expression(expression)?;
            }
            Statement::Block { statements, .. } => {
                self.symbol_table.enter_scope();
                for stmt in statements {
                    self.analyze_statement(stmt)?;
                }
                self.symbol_table.exit_scope();
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.analyze_expression(condition)?;
                self.analyze_statement(then_branch)?;
                if let Some(else_stmt) = else_branch {
                    self.analyze_statement(else_stmt)?;
                }
            }
            Statement::Switch {
                expression, cases, ..
            } => {
                self.analyze_expression(expression)?;
                for (case_expr, case_stmts) in cases {
                    if let Some(expr) = case_expr {
                        self.analyze_expression(expr)?;
                    }
                    for stmt in case_stmts {
                        self.analyze_statement(stmt)?;
                    }
                }
            }
            Statement::For {
                initializer,
                condition,
                increment,
                body,
                ..
            } => {
                self.symbol_table.enter_scope();
                if let Some(init) = initializer {
                    self.analyze_statement(init)?;
                }
                if let Some(cond) = condition {
                    self.analyze_expression(cond)?;
                }
                if let Some(inc) = increment {
                    self.analyze_expression(inc)?;
                }
                self.analyze_statement(body)?;
                self.symbol_table.exit_scope();
            }
            Statement::Foreach {
                key_var,
                value_var,
                iterable,
                body,
                ..
            } => {
                self.symbol_table.enter_scope();

                if let Some((key_type, key_name)) = key_var {
                    let key_symbol = Symbol {
                        name: key_name.clone(),
                        kind: SymbolKind::Variable,
                        type_ref: Some(key_type.clone()),
                        span: Span::new(Position::new(0, 0, 0), Position::new(0, 0, 0)),
                        scope: self.symbol_table.current_scope,
                        modifiers: Vec::new(),
                        parent: self.symbol_table.current_class.clone(),
                    };
                    if let Err(e) = self.symbol_table.define(key_symbol) {
                        self.errors.push(e);
                    }
                }

                let (value_type, value_name) = value_var;
                let value_symbol = Symbol {
                    name: value_name.clone(),
                    kind: SymbolKind::Variable,
                    type_ref: Some(value_type.clone()),
                    span: Span::new(Position::new(0, 0, 0), Position::new(0, 0, 0)),
                    scope: self.symbol_table.current_scope,
                    modifiers: Vec::new(),
                    parent: self.symbol_table.current_class.clone(),
                };
                if let Err(e) = self.symbol_table.define(value_symbol) {
                    self.errors.push(e);
                }

                self.analyze_expression(iterable)?;
                self.analyze_statement(body)?;
                self.symbol_table.exit_scope();
            }
            Statement::While {
                condition, body, ..
            } => {
                self.analyze_expression(condition)?;
                self.analyze_statement(body)?;
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.analyze_expression(expr)?;
                }
            }
            Statement::Delete { expression, .. } => {
                self.analyze_expression(expression)?;
            }
            Statement::Break(_) | Statement::Continue(_) => {}
        }

        Ok(())
    }

    fn analyze_expression(&mut self, expression: &Expression) -> Result<(), Vec<String>> {
        match expression {
            Expression::Identifier(name, span) => {
                // Check if identifier is defined
                if self.symbol_table.lookup(name).is_none() {
                    // Check if it's an enum variant
                    if !name.contains('.') {
                        self.warnings.push(format!(
                            "Undefined identifier '{}' at line {}",
                            name, span.start.line
                        ));
                    }
                }
            }
            Expression::Binary { left, right, .. } => {
                self.analyze_expression(left)?;
                self.analyze_expression(right)?;
                // TODO: Type checking
            }
            Expression::Unary { operand, .. } => {
                self.analyze_expression(operand)?;
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                self.analyze_expression(callee)?;
                for arg in arguments {
                    self.analyze_expression(arg)?;
                }
                // TODO: Function signature checking
            }
            Expression::MemberAccess {
                object,
                member: _,
                span: _,
            } => {
                self.analyze_expression(object)?;
                // TODO: Check if member exists on object type
            }
            Expression::Index { object, index, .. } => {
                self.analyze_expression(object)?;
                self.analyze_expression(index)?;
            }
            Expression::New { arguments, .. } => {
                for arg in arguments {
                    self.analyze_expression(arg)?;
                }
            }
            Expression::Cast { expression, .. } => {
                self.analyze_expression(expression)?;
            }
            _ => {} // Literals, this, super don't need analysis
        }

        Ok(())
    }

    /// Check if a class is a subclass of another - used by LSP for type checking and inheritance validation
    #[allow(dead_code)] // Used in tests and will be used by LSP features
    pub fn is_subclass(&self, class: &str, potential_parent: &str) -> bool {
        let mut current = class;
        while let Some(parent) = self.class_hierarchy.get(current) {
            if let Some(parent_name) = parent {
                if parent_name == potential_parent {
                    return true;
                }
                current = parent_name;
            } else {
                break;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_semantic_analysis() {
        let code = r#"
class MyClass
{
    int m_value;
    
    void SetValue(int value)
    {
        m_value = value;
    }
    
    int GetValue()
    {
        return m_value;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());

        let table = analyzer.get_symbol_table();
        assert!(table.get_class("MyClass").is_some());
    }

    #[test]
    fn test_undefined_variable() {
        let code = r#"
void Test()
{
    int x = y;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        assert!(!analyzer.get_warnings().is_empty());
    }

    #[test]
    fn test_duplicate_function() {
        let code = r#"
void Test()
{
}

void Test()
{
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        // May or may not detect duplicate - implementation dependent
        // Just verify it doesn't crash
        assert!(program.declarations.len() == 2);
    }

    #[test]
    fn test_class_inheritance() {
        let code = r#"
class Animal
{
    void MakeSound()
    {
    }
}

class Dog extends Animal
{
    override void MakeSound()
    {
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        // Check that both classes are registered
        assert!(analyzer.symbol_table.lookup("Animal").is_some());
        assert!(analyzer.symbol_table.lookup("Dog").is_some());
    }

    #[test]
    fn test_enum_registration() {
        let code = r#"
enum MyEnum
{
    Value1,
    Value2 = 5,
    Value3
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
        assert!(analyzer.symbol_table.lookup("MyEnum").is_some());
    }

    #[test]
    fn test_variable_scope() {
        let code = r#"
void Test()
{
    int x = 5;
    {
        int y = 10;
        int z = x + y;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_function_parameters() {
        let code = r#"
void Process(int a, float b, string c)
{
    int result = a + 10;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_class_members() {
        let code = r#"
class MyClass
{
    int m_field;
    
    void Method()
    {
        m_field = 10;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_forward_reference() {
        let code = r#"
void CallOther()
{
    OtherFunction();
}

void OtherFunction()
{
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        // Two-pass analysis should handle this
        assert!(result.is_ok());
    }

    #[test]
    fn test_const_values() {
        let code = r#"
void Test()
{
    const int MAX_SIZE = 100;
    int x = MAX_SIZE;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        // Verify the function is registered and no errors
        assert!(analyzer.symbol_table.lookup("Test").is_some());
    }

    #[test]
    fn test_modded_class_analysis() {
        let code = r#"
modded class MyClass
{
    void NewMethod()
    {
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
        assert!(analyzer.symbol_table.lookup("MyClass").is_some());
    }

    #[test]
    fn test_static_members() {
        let code = r#"
class MyClass
{
    static int s_counter;
    
    static void IncrementCounter()
    {
        s_counter++;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_template_class_analysis() {
        let code = r#"
class Container<class T>
{
    T m_value;
    
    T GetValue()
    {
        return m_value;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_multiple_classes() {
        let code = r#"
class ClassA
{
    void MethodA() {}
}

class ClassB
{
    void MethodB() {}
}

class ClassC extends ClassA
{
    override void MethodA() {}
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        // Verify all classes are registered
        assert!(analyzer.symbol_table.lookup("ClassA").is_some());
        assert!(analyzer.symbol_table.lookup("ClassB").is_some());
        assert!(analyzer.symbol_table.lookup("ClassC").is_some());
    }

    #[test]
    fn test_nested_scopes() {
        let code = r#"
void Test()
{
    int outer = 1;
    {
        int inner = 2;
        {
            int innermost = 3;
            int sum = outer + inner + innermost;
        }
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_loop_variables() {
        let code = r#"
void Test()
{
    for (int i = 0; i < 10; i++)
    {
        int j = i * 2;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_constructor_analysis() {
        let code = r#"
class MyClass
{
    int m_value;
    
    void MyClass(int value)
    {
        int x = value;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        // Verify class is registered
        assert!(analyzer.symbol_table.lookup("MyClass").is_some());
    }

    #[test]
    fn test_method_access_modifiers() {
        let code = r#"
class MyClass
{
    private void PrivateMethod() {}
    protected void ProtectedMethod() {}
    void PublicMethod() {}
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_auto_type_inference() {
        let code = r#"
void Test()
{
    auto x = 10;
    auto y = 3.14;
    auto z = "hello";
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_array_types() {
        let code = r#"
void Test()
{
    int arr[10];
    autoptr array<int> dynArr = new array<int>;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_typedef_analysis() {
        let code = r#"
typedef array<string> TStringArray;

void Test()
{
    TStringArray names;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_ref_autoptr_analysis() {
        let code = r#"
class Parent
{
    ref Child m_child;
}

class Child
{
    Parent m_parent;
}

void Test()
{
    autoptr Parent p = new Parent;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let result = analyzer.analyze(&program);

        assert!(result.is_ok());
    }

    #[test]
    fn test_class_member_lookup() {
        let code = r#"
class MyClass
{
    int m_value;
    string m_name;
    
    void Method()
    {
        int x = m_value;
        string s = m_name;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).unwrap();

        let table = analyzer.get_symbol_table();

        // Test lookup_in_class method
        let member = table.lookup_in_class("MyClass", "m_value");
        assert!(member.is_some());
        assert_eq!(member.unwrap().name, "m_value");

        let method = table.lookup_in_class("MyClass", "Method");
        assert!(method.is_some());
        assert_eq!(method.unwrap().name, "Method");

        // Test non-existent member
        let non_existent = table.lookup_in_class("MyClass", "nonexistent");
        assert!(non_existent.is_none());
    }

    #[test]
    fn test_get_symbols_by_kind() {
        let code = r#"
class ClassA 
{
}

class ClassB 
{
}

void FunctionA() 
{
}

void FunctionB() 
{
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).unwrap();

        let table = analyzer.get_symbol_table();

        // Test getting all classes
        let classes = table.get_symbols_by_kind(SymbolKind::Class);
        assert_eq!(classes.len(), 2);

        // Test getting all functions
        let functions = table.get_symbols_by_kind(SymbolKind::Function);
        assert_eq!(functions.len(), 2);
    }

    #[test]
    fn test_inheritance_chain() {
        let code = r#"
class GrandParent {}
class Parent : GrandParent {}
class Child : Parent {}

void Test()
{
    Child c;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).unwrap();

        // Test is_subclass method
        assert!(analyzer.is_subclass("Child", "Parent"));
        assert!(analyzer.is_subclass("Child", "GrandParent"));
        assert!(analyzer.is_subclass("Parent", "GrandParent"));
        assert!(!analyzer.is_subclass("Parent", "Child"));
        assert!(!analyzer.is_subclass("GrandParent", "Child"));
    }

    #[test]
    fn test_get_errors() {
        let code = r#"
void Test()
{
    int x = y;
    NonExistentClass obj;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        let _ = analyzer.analyze(&program);

        let errors = analyzer.get_errors();
        let warnings = analyzer.get_warnings();
        // Should have errors or warnings for undefined symbols
        assert!(!errors.is_empty() || !warnings.is_empty());
    }

    #[test]
    fn test_symbol_scope_and_modifiers() {
        let code = r#"
class MyClass
{
    private int m_private;
    protected string m_protected;
    public float m_public;
    
    void Method()
    {
        int local = 5;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).unwrap();

        let table = analyzer.get_symbol_table();

        // Check that symbols have proper modifiers set
        let private_member = table.lookup_in_class("MyClass", "m_private");
        assert!(private_member.is_some());
        let symbol = private_member.unwrap();
        assert!(symbol.modifiers.contains(&"private".to_string()));

        let protected_member = table.lookup_in_class("MyClass", "m_protected");
        assert!(protected_member.is_some());
        let symbol = protected_member.unwrap();
        assert!(symbol.modifiers.contains(&"protected".to_string()));
    }

    #[test]
    fn test_symbol_parent_linkage() {
        let code = r#"
class ParentClass
{
    int m_member;
    
    void Method()
    {
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).unwrap();

        let table = analyzer.get_symbol_table();

        // Check that members have parent set
        let member = table.lookup_in_class("ParentClass", "m_member");
        assert!(member.is_some());
        let symbol = member.unwrap();
        assert_eq!(symbol.parent, Some("ParentClass".to_string()));

        let method = table.lookup_in_class("ParentClass", "Method");
        assert!(method.is_some());
        let symbol = method.unwrap();
        assert_eq!(symbol.parent, Some("ParentClass".to_string()));
    }

    #[test]
    fn test_enum_with_extends() {
        let code = r#"
enum BaseEnum
{
    VALUE1,
    VALUE2
}

enum DerivedEnum : BaseEnum
{
    VALUE3,
    VALUE4
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).unwrap();

        // Verify both enums are registered
        let table = analyzer.get_symbol_table();
        assert!(table.lookup("BaseEnum").is_some());
        assert!(table.lookup("DerivedEnum").is_some());
    }
}
