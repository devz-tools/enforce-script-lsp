/// Parser for Enforce Script
/// Builds an Abstract Syntax Tree (AST) from tokens
use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Position {
            line,
            column,
            offset,
        }
    }

    pub fn from_token(token: &Token) -> Self {
        Position::new(token.line, token.column, token.offset)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }

    pub fn from_token(token: &Token) -> Self {
        let start = Position::from_token(token);
        let end = Position::new(
            token.line,
            token.column + token.lexeme.len(),
            token.offset + token.lexeme.len(),
        );
        Span::new(start, end)
    }
}

/// Type reference in the AST - fields are part of the data model and will be used by LSP features
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum TypeRef {
    Primitive(String),                  // int, float, bool, string, vector, void
    Class(String),                      // ClassName
    Template(String, Vec<TypeRef>),     // array<int>, map<string, int>
    Array(Box<TypeRef>, Option<usize>), // int[10], string[]
    Auto,                               // auto
}

/// Function parameter - default_value will be used for completion/signature help in LSP
#[derive(Debug, Clone)]
pub struct Parameter {
    pub modifiers: Vec<String>, // out, inout, ref
    pub type_ref: TypeRef,
    pub name: String,
    #[allow(dead_code)] // Will be used for signature help in LSP
    pub default_value: Option<Expression>,
    pub span: Span,
}

/// Expression nodes - fields contain the actual values and are part of the AST data model
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expression {
    IntLiteral(i64, Span),
    FloatLiteral(f64, Span),
    StringLiteral(String, Span),
    BoolLiteral(bool, Span),
    NullLiteral(Span),
    VectorLiteral(String, Span),
    Identifier(String, Span),
    ArrayLiteral {
        elements: Vec<Expression>,
        span: Span,
    },
    Binary {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
        span: Span,
    },
    Unary {
        operator: String,
        operand: Box<Expression>,
        span: Span,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },
    MemberAccess {
        object: Box<Expression>,
        member: String,
        span: Span,
    },
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },
    New {
        type_ref: TypeRef,
        arguments: Vec<Expression>,
        span: Span,
    },
    This(Span),
    Super(Span),
    Cast {
        type_ref: TypeRef,
        expression: Box<Expression>,
        span: Span,
    },
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
        span: Span,
    },
}

impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::IntLiteral(_, span) => span,
            Expression::FloatLiteral(_, span) => span,
            Expression::StringLiteral(_, span) => span,
            Expression::BoolLiteral(_, span) => span,
            Expression::NullLiteral(span) => span,
            Expression::VectorLiteral(_, span) => span,
            Expression::Identifier(_, span) => span,
            Expression::ArrayLiteral { span, .. } => span,
            Expression::Binary { span, .. } => span,
            Expression::Unary { span, .. } => span,
            Expression::Call { span, .. } => span,
            Expression::MemberAccess { span, .. } => span,
            Expression::Index { span, .. } => span,
            Expression::New { span, .. } => span,
            Expression::This(span) => span,
            Expression::Super(span) => span,
            Expression::Cast { span, .. } => span,
            Expression::Ternary { span, .. } => span,
        }
    }
}

/// Statement nodes - span fields are part of the data model for LSP features like folding
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Statement {
    VariableDeclaration {
        modifiers: Vec<String>,
        type_ref: TypeRef,
        name: String,
        initializer: Option<Expression>,
        span: Span,
    },
    Expression {
        expression: Expression,
        span: Span,
    },
    Block {
        statements: Vec<Statement>,
        span: Span,
    },
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
        span: Span,
    },
    Switch {
        expression: Expression,
        cases: Vec<(Option<Expression>, Vec<Statement>)>, // None for default case
        span: Span,
    },
    For {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Box<Statement>,
        span: Span,
    },
    Foreach {
        key_var: Option<(TypeRef, String)>,
        value_var: (TypeRef, String),
        iterable: Expression,
        body: Box<Statement>,
        span: Span,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
        span: Span,
    },
    Return {
        value: Option<Expression>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
    Delete {
        expression: Expression,
        span: Span,
    },
}

impl Statement {
    /// Get the span of this statement - will be used for LSP features like folding ranges
    #[allow(dead_code)]
    pub fn span(&self) -> &Span {
        match self {
            Statement::VariableDeclaration { span, .. } => span,
            Statement::Expression { span, .. } => span,
            Statement::Block { span, .. } => span,
            Statement::If { span, .. } => span,
            Statement::Switch { span, .. } => span,
            Statement::For { span, .. } => span,
            Statement::Foreach { span, .. } => span,
            Statement::While { span, .. } => span,
            Statement::Return { span, .. } => span,
            Statement::Break(span) => span,
            Statement::Continue(span) => span,
            Statement::Delete { span, .. } => span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub modifiers: Vec<String>, // private, protected, static, override, proto, native
    pub return_type: TypeRef,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Option<Statement>, // None for proto/native methods
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub modifiers: Vec<String>, // private, protected, static, const, ref, autoptr
    pub type_ref: TypeRef,
    pub name: String,
    pub initializer: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub modifiers: Vec<String>, // modded
    pub name: String,
    pub extends: Option<String>,
    #[allow(dead_code)] // Will be used for generic class support in LSP
    pub template_params: Vec<String>,
    pub methods: Vec<Method>,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<i64>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    #[allow(dead_code)] // Will be used for enum inheritance validation in LSP
    pub extends: Option<String>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Typedef {
    pub name: String,
    pub type_ref: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Class(Class),
    Enum(Enum),
    Typedef(Typedef),
    Function(Method), // Global functions
}

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Parser {
            tokens,
            position: 0,
            errors: Vec::new(),
        };
        parser.skip_comments();
        parser
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut declarations = Vec::new();

        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(decl) => declarations.push(decl),
                Err(e) => {
                    // Error recovery: skip to next likely declaration start
                    eprintln!("Parse error: {}", e);
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        Ok(Program { declarations })
    }

    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }

    fn parse_declaration(&mut self) -> Result<Declaration, String> {
        let mut modifiers = Vec::new();

        // Parse modifiers
        while self.match_token(&[
            TokenType::Modded,
            TokenType::Private,
            TokenType::Protected,
            TokenType::Static,
            TokenType::Override,
            TokenType::Proto,
            TokenType::Native,
            TokenType::Const,
            TokenType::Ref,
            TokenType::Autoptr,
        ]) {
            if let Some(token) = self.previous() {
                modifiers.push(token.lexeme.clone());
            }
        }

        if self.check(&TokenType::Class) {
            self.parse_class(modifiers)
        } else if self.check(&TokenType::Enum) {
            self.parse_enum()
        } else if self.check(&TokenType::Typedef) {
            self.parse_typedef()
        } else {
            // Try to parse as function
            self.parse_function(modifiers)
        }
    }

    fn parse_class(&mut self, modifiers: Vec<String>) -> Result<Declaration, String> {
        let start = Position::from_token(self.current());
        self.consume(&TokenType::Class, "Expected 'class'")?;

        let name = self.consume_identifier("Expected class name")?;

        // Parse template parameters
        let template_params = if self.match_token(&[TokenType::Less]) {
            self.parse_template_parameters()?
        } else {
            Vec::new()
        };

        // Parse extends
        let extends =
            if self.match_token(&[TokenType::Colon]) || self.match_token(&[TokenType::Extends]) {
                Some(self.consume_identifier("Expected base class name")?)
            } else {
                None
            };

        // Check for forward declaration (class Name;)
        if self.match_token(&[TokenType::Semicolon]) {
            let end = Position::from_token(self.previous().unwrap());
            let span = Span::new(start, end);
            // Return empty class for forward declaration
            return Ok(Declaration::Class(Class {
                modifiers,
                name,
                extends,
                template_params,
                methods: Vec::new(),
                fields: Vec::new(),
                span,
            }));
        }

        self.consume(&TokenType::LeftBrace, "Expected '{'")?;

        let mut methods = Vec::new();
        let mut fields = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_class_member() {
                Ok((member_methods, member_fields)) => {
                    methods.extend(member_methods);
                    fields.extend(member_fields);
                }
                Err(e) => {
                    eprintln!("Error parsing class member: {}", e);
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        let end_line = self.current().line;
        let end_column = self.current().column;
        let end_offset = self.current().offset;
        self.consume(&TokenType::RightBrace, "Expected '}'")?;

        // Optional semicolon after class closing brace (config.cpp style)
        self.match_token(&[TokenType::Semicolon]);

        let end = Position::new(end_line, end_column, end_offset);
        let span = Span::new(start, end);

        Ok(Declaration::Class(Class {
            modifiers,
            name,
            extends,
            template_params,
            methods,
            fields,
            span,
        }))
    }

    fn parse_template_parameters(&mut self) -> Result<Vec<String>, String> {
        let mut params = Vec::new();

        loop {
            self.consume(&TokenType::Class, "Expected 'class' in template parameter")?;
            params.push(self.consume_identifier("Expected template parameter name")?);

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        self.consume(&TokenType::Greater, "Expected '>'")?;
        Ok(params)
    }

    fn parse_class_member(&mut self) -> Result<(Vec<Method>, Vec<Field>), String> {
        let mut modifiers = Vec::new();

        // Parse modifiers
        while self.match_token(&[
            TokenType::Private,
            TokenType::Protected,
            TokenType::Static,
            TokenType::Override,
            TokenType::Proto,
            TokenType::Native,
            TokenType::Const,
            TokenType::Ref,
            TokenType::Autoptr,
        ]) {
            if let Some(token) = self.previous() {
                modifiers.push(token.lexeme.clone());
            }
        }

        // Check for nested class declaration
        if self.check(&TokenType::Class) {
            let _class_decl = self.parse_class(modifiers)?;
            // For now, we ignore nested classes (config.cpp pattern)
            // They'll be stored as declarations but not as class members
            return Ok((vec![], vec![]));
        }

        // Check if this looks like a config.cpp style field (identifier followed by [ or =)
        // e.g., fieldName[]=value; or fieldName=value;
        if let Some(token) = self.current_opt() {
            if let TokenType::Identifier(first_name) = &token.token_type {
                let first_name_clone = first_name.clone();
                self.advance();

                // Check what comes after the identifier
                if self.check(&TokenType::LeftBracket) || self.check(&TokenType::Assign) {
                    // This is a config-style field without explicit type
                    // Treat the identifier as the field name with Auto type
                    let is_array_field = self.match_token(&[TokenType::LeftBracket]);
                    if is_array_field {
                        self.consume(&TokenType::RightBracket, "Expected ']' after '['")?;
                    }
                    let field =
                        self.parse_field_rest(modifiers, TypeRef::Auto, first_name_clone)?;
                    return Ok((vec![], vec![field]));
                }

                // Not a config-style field, so the first identifier was a type
                // We need to parse it as a type and then get the actual member name
                // Rewind one step to re-parse as type
                self.position -= 1;
            }
        }

        // Standard Enforce Script: type name = value; or type name(...) { }
        let type_ref = self.parse_type()?;
        let name = self.consume_identifier("Expected member name")?;

        // Check for array brackets (e.g., int myArray[10];)
        let is_array_field = self.match_token(&[TokenType::LeftBracket]);
        if is_array_field {
            self.consume(&TokenType::RightBracket, "Expected ']' after '['")?;
        }

        // Check if it's a method (has parentheses)
        if self.check(&TokenType::LeftParen) {
            let method = self.parse_method_rest(modifiers, type_ref, name)?;
            Ok((vec![method], vec![]))
        } else {
            // It's a field
            let field = self.parse_field_rest(modifiers, type_ref, name)?;
            Ok((vec![], vec![field]))
        }
    }

    fn parse_method_rest(
        &mut self,
        modifiers: Vec<String>,
        return_type: TypeRef,
        name: String,
    ) -> Result<Method, String> {
        let start = Position::new(0, 0, 0); // TODO: Track properly

        self.consume(&TokenType::LeftParen, "Expected '('")?;

        let parameters = if !self.check(&TokenType::RightParen) {
            self.parse_parameters()?
        } else {
            Vec::new()
        };

        self.consume(&TokenType::RightParen, "Expected ')'")?;

        // Check for body
        let body = if self.check(&TokenType::LeftBrace) {
            Some(self.parse_block()?)
        } else if self.match_token(&[TokenType::Semicolon]) {
            None // Proto/native method
        } else {
            return Err("Expected '{' or ';' after method declaration".to_string());
        };

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Method {
            modifiers,
            return_type,
            name,
            parameters,
            body,
            span,
        })
    }

    fn parse_field_rest(
        &mut self,
        modifiers: Vec<String>,
        type_ref: TypeRef,
        name: String,
    ) -> Result<Field, String> {
        let start = Position::new(0, 0, 0); // TODO: Track properly

        let initializer = if self.match_token(&[TokenType::Assign]) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(
            &TokenType::Semicolon,
            "Expected ';' after field declaration",
        )?;

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Field {
            modifiers,
            type_ref,
            name,
            initializer,
            span,
        })
    }

    fn parse_enum(&mut self) -> Result<Declaration, String> {
        let start = Position::from_token(self.current());
        self.consume(&TokenType::Enum, "Expected 'enum'")?;

        let name = self.consume_identifier("Expected enum name")?;

        let extends = if self.match_token(&[TokenType::Colon]) {
            Some(self.consume_identifier("Expected base enum name")?)
        } else {
            None
        };

        self.consume(&TokenType::LeftBrace, "Expected '{'")?;

        let mut variants = Vec::new();
        let mut next_value = 0i64;

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let variant_start = Position::from_token(self.current());
            let variant_name = self.consume_identifier("Expected enum variant name")?;

            let value = if self.match_token(&[TokenType::Assign]) {
                let expr = self.parse_primary()?;
                if let Expression::IntLiteral(val, _) = expr {
                    next_value = val + 1;
                    Some(val)
                } else {
                    return Err("Enum variant value must be an integer".to_string());
                }
            } else {
                let val = next_value;
                next_value += 1;
                Some(val)
            };

            let variant_end = Position::from_token(self.previous().unwrap());
            variants.push(EnumVariant {
                name: variant_name,
                value,
                span: Span::new(variant_start, variant_end),
            });

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        let end_line = self.current().line;
        let end_column = self.current().column;
        let end_offset = self.current().offset;
        self.consume(&TokenType::RightBrace, "Expected '}'")?;

        let end = Position::new(end_line, end_column, end_offset);
        let span = Span::new(start, end);

        Ok(Declaration::Enum(Enum {
            name,
            extends,
            variants,
            span,
        }))
    }

    fn parse_typedef(&mut self) -> Result<Declaration, String> {
        let start = Position::from_token(self.current());
        self.consume(&TokenType::Typedef, "Expected 'typedef'")?;

        let type_ref = self.parse_type()?;
        let name = self.consume_identifier("Expected typedef name")?;

        self.consume(&TokenType::Semicolon, "Expected ';'")?;

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Declaration::Typedef(Typedef {
            name,
            type_ref,
            span,
        }))
    }

    fn parse_function(&mut self, modifiers: Vec<String>) -> Result<Declaration, String> {
        let return_type = self.parse_type()?;
        let name = self.consume_identifier("Expected function name")?;

        let method = self.parse_method_rest(modifiers, return_type, name)?;
        Ok(Declaration::Function(method))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut parameters = Vec::new();

        loop {
            let mut modifiers = Vec::new();

            while self.match_token(&[TokenType::Out, TokenType::Inout, TokenType::Ref]) {
                if let Some(token) = self.previous() {
                    modifiers.push(token.lexeme.clone());
                }
            }

            let start = Position::from_token(self.current());
            let type_ref = self.parse_type()?;
            let name = self.consume_identifier("Expected parameter name")?;

            let default_value = if self.match_token(&[TokenType::Assign]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let end = Position::from_token(self.previous().unwrap());
            let span = Span::new(start, end);

            parameters.push(Parameter {
                modifiers,
                type_ref,
                name,
                default_value,
                span,
            });

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        Ok(parameters)
    }

    fn parse_type(&mut self) -> Result<TypeRef, String> {
        if self.match_token(&[TokenType::Auto]) {
            return Ok(TypeRef::Auto);
        }

        let mut modifiers = Vec::new();
        while self.match_token(&[TokenType::Ref, TokenType::Autoptr]) {
            if let Some(token) = self.previous() {
                modifiers.push(token.lexeme.clone());
            }
        }

        let base_type = if self.match_token(&[
            TokenType::Void,
            TokenType::Int,
            TokenType::Float,
            TokenType::Bool,
            TokenType::String,
            TokenType::Vector,
            TokenType::Typename,
        ]) {
            let token = self.previous().unwrap();
            TypeRef::Primitive(token.lexeme.clone())
        } else if let Some(token) = self.current_opt() {
            if let TokenType::Identifier(name) = &token.token_type {
                let type_name = name.clone();
                self.advance();

                // Check for template parameters
                if self.match_token(&[TokenType::Less]) {
                    let params = self.parse_type_parameters()?;
                    self.consume(&TokenType::Greater, "Expected '>'")?;
                    TypeRef::Template(type_name, params)
                } else {
                    TypeRef::Class(type_name)
                }
            } else {
                return Err(format!("Expected type, got {:?}", token.token_type));
            }
        } else {
            return Err("Expected type".to_string());
        };

        // Check for array brackets
        if self.match_token(&[TokenType::LeftBracket]) {
            let size = if !self.check(&TokenType::RightBracket) {
                if let Some(token) = self.current_opt() {
                    if let TokenType::IntLiteral(n) = token.token_type {
                        self.advance();
                        Some(n as usize)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };

            self.consume(&TokenType::RightBracket, "Expected ']'")?;
            Ok(TypeRef::Array(Box::new(base_type), size))
        } else {
            Ok(base_type)
        }
    }

    fn parse_type_parameters(&mut self) -> Result<Vec<TypeRef>, String> {
        let mut params = Vec::new();

        loop {
            params.push(self.parse_type()?);

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.current());
        self.consume(&TokenType::LeftBrace, "Expected '{'")?;

        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    eprintln!("Error parsing statement: {}", e);
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        let end_line = self.current().line;
        let end_column = self.current().column;
        let end_offset = self.current().offset;
        self.consume(&TokenType::RightBrace, "Expected '}'")?;

        let end = Position::new(end_line, end_column, end_offset);
        let span = Span::new(start, end);

        Ok(Statement::Block { statements, span })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.match_token(&[TokenType::If]) {
            self.parse_if_statement()
        } else if self.match_token(&[TokenType::Switch]) {
            self.parse_switch_statement()
        } else if self.match_token(&[TokenType::For]) {
            self.parse_for_statement()
        } else if self.match_token(&[TokenType::Foreach]) {
            self.parse_foreach_statement()
        } else if self.match_token(&[TokenType::While]) {
            self.parse_while_statement()
        } else if self.match_token(&[TokenType::Return]) {
            self.parse_return_statement()
        } else if self.match_token(&[TokenType::Break]) {
            let start = Position::from_token(self.previous().unwrap());
            self.consume(&TokenType::Semicolon, "Expected ';'")?;
            let end = Position::from_token(self.previous().unwrap());
            Ok(Statement::Break(Span::new(start, end)))
        } else if self.match_token(&[TokenType::Continue]) {
            let start = Position::from_token(self.previous().unwrap());
            self.consume(&TokenType::Semicolon, "Expected ';'")?;
            let end = Position::from_token(self.previous().unwrap());
            Ok(Statement::Continue(Span::new(start, end)))
        } else if self.match_token(&[TokenType::Delete]) {
            self.parse_delete_statement()
        } else if self.check(&TokenType::LeftBrace) {
            self.parse_block()
        } else {
            self.parse_variable_declaration_or_expression()
        }
    }

    fn parse_variable_declaration_or_expression(&mut self) -> Result<Statement, String> {
        let start_pos = self.position;

        // Try to parse as variable declaration
        let mut modifiers = Vec::new();
        while self.match_token(&[
            TokenType::Const,
            TokenType::Static,
            TokenType::Ref,
            TokenType::Autoptr,
        ]) {
            if let Some(token) = self.previous() {
                modifiers.push(token.lexeme.clone());
            }
        }

        // Check if next is a type
        if self.is_type_start() {
            let start = Position::from_token(self.current());
            let type_ref = self.parse_type()?;
            let name = self.consume_identifier("Expected variable name")?;

            let initializer = if self.match_token(&[TokenType::Assign]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            self.consume(&TokenType::Semicolon, "Expected ';'")?;
            let end = Position::from_token(self.previous().unwrap());

            return Ok(Statement::VariableDeclaration {
                modifiers,
                type_ref,
                name,
                initializer,
                span: Span::new(start, end),
            });
        }

        // Otherwise parse as expression statement
        self.position = start_pos; // Backtrack
        let start = Position::from_token(self.current());
        let expression = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';'")?;
        let end = Position::from_token(self.previous().unwrap());

        Ok(Statement::Expression {
            expression,
            span: Span::new(start, end),
        })
    }

    fn is_type_start(&self) -> bool {
        if let Some(token) = self.current_opt() {
            matches!(
                token.token_type,
                TokenType::Void
                    | TokenType::Int
                    | TokenType::Float
                    | TokenType::Bool
                    | TokenType::String
                    | TokenType::Vector
                    | TokenType::Typename
                    | TokenType::Auto
                    | TokenType::Identifier(_)
            )
        } else {
            false
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());

        self.consume(&TokenType::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')'")?;

        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if self.match_token(&[TokenType::Else]) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
            span,
        })
    }

    fn parse_switch_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());

        self.consume(&TokenType::LeftParen, "Expected '(' after 'switch'")?;
        let expression = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')'")?;
        self.consume(&TokenType::LeftBrace, "Expected '{'")?;

        let mut cases = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            if self.match_token(&[TokenType::Case]) {
                let case_expr = self.parse_expression()?;
                self.consume(&TokenType::Colon, "Expected ':' after case")?;

                let mut case_statements = Vec::new();
                while !self.check(&TokenType::Case)
                    && !self.check(&TokenType::Default)
                    && !self.check(&TokenType::RightBrace)
                {
                    case_statements.push(self.parse_statement()?);
                }

                cases.push((Some(case_expr), case_statements));
            } else if self.match_token(&[TokenType::Default]) {
                self.consume(&TokenType::Colon, "Expected ':' after default")?;

                let mut case_statements = Vec::new();
                while !self.check(&TokenType::Case)
                    && !self.check(&TokenType::Default)
                    && !self.check(&TokenType::RightBrace)
                {
                    case_statements.push(self.parse_statement()?);
                }

                cases.push((None, case_statements));
            } else {
                break;
            }
        }

        let end_line = self.current().line;
        let end_column = self.current().column;
        let end_offset = self.current().offset;
        self.consume(&TokenType::RightBrace, "Expected '}'")?;

        let end = Position::new(end_line, end_column, end_offset);
        let span = Span::new(start, end);

        Ok(Statement::Switch {
            expression,
            cases,
            span,
        })
    }

    fn parse_for_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());

        self.consume(&TokenType::LeftParen, "Expected '(' after 'for'")?;

        let initializer = if self.match_token(&[TokenType::Semicolon]) {
            None
        } else {
            Some(Box::new(self.parse_variable_declaration_or_expression()?))
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume(&TokenType::Semicolon, "Expected ';'")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume(&TokenType::RightParen, "Expected ')'")?;

        let body = Box::new(self.parse_statement()?);

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Statement::For {
            initializer,
            condition,
            increment,
            body,
            span,
        })
    }

    fn parse_foreach_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());

        self.consume(&TokenType::LeftParen, "Expected '(' after 'foreach'")?;

        // Parse loop variables
        let first_type = self.parse_type()?;
        let first_name = self.consume_identifier("Expected variable name")?;

        let (key_var, value_var) = if self.match_token(&[TokenType::Comma]) {
            // Two variables: key and value
            let value_type = self.parse_type()?;
            let value_name = self.consume_identifier("Expected variable name")?;
            (Some((first_type, first_name)), (value_type, value_name))
        } else {
            // One variable: just value
            (None, (first_type, first_name))
        };

        self.consume(&TokenType::Colon, "Expected ':' in foreach")?;
        let iterable = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')'")?;

        let body = Box::new(self.parse_statement()?);

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Statement::Foreach {
            key_var,
            value_var,
            iterable,
            body,
            span,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());

        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')'")?;

        let body = Box::new(self.parse_statement()?);

        let end = Position::from_token(self.previous().unwrap());
        let span = Span::new(start, end);

        Ok(Statement::While {
            condition,
            body,
            span,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());

        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon, "Expected ';'")?;
        let end = Position::from_token(self.previous().unwrap());

        Ok(Statement::Return {
            value,
            span: Span::new(start, end),
        })
    }

    fn parse_delete_statement(&mut self) -> Result<Statement, String> {
        let start = Position::from_token(self.previous().unwrap());
        let expression = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';'")?;
        let end = Position::from_token(self.previous().unwrap());

        Ok(Statement::Delete {
            expression,
            span: Span::new(start, end),
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expression, String> {
        let expr = self.parse_ternary()?;

        if self.match_token(&[
            TokenType::Assign,
            TokenType::PlusAssign,
            TokenType::MinusAssign,
            TokenType::StarAssign,
            TokenType::SlashAssign,
            TokenType::OrAssign,
            TokenType::AndAssign,
            TokenType::LeftShiftAssign,
            TokenType::RightShiftAssign,
        ]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_assignment()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());

            return Ok(Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            });
        }

        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<Expression, String> {
        let expr = self.parse_logical_or()?;

        if self.match_token(&[TokenType::Question]) {
            let then_expr = self.parse_expression()?;
            self.consume(&TokenType::Colon, "Expected ':' in ternary expression")?;
            let else_expr = self.parse_ternary()?;
            let span = Span::new(expr.span().start.clone(), else_expr.span().end.clone());

            return Ok(Expression::Ternary {
                condition: Box::new(expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
                span,
            });
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and()?;

        while self.match_token(&[TokenType::LogicalOr]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_logical_and()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_bitwise_or()?;

        while self.match_token(&[TokenType::LogicalAnd]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_bitwise_or()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_bitwise_xor()?;

        while self.match_token(&[TokenType::BitwiseOr]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_bitwise_xor()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_bitwise_and()?;

        while self.match_token(&[TokenType::BitwiseXor]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_bitwise_and()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality()?;

        while self.match_token(&[TokenType::BitwiseAnd]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_equality()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;

        while self.match_token(&[TokenType::Equal, TokenType::NotEqual]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_comparison()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_shift()?;

        while self.match_token(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_shift()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;

        while self.match_token(&[TokenType::LeftShift, TokenType::RightShift]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_term()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;

        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_factor()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;

        while self.match_token(&[TokenType::Star, TokenType::Slash, TokenType::Percent]) {
            let operator = self.previous().unwrap().lexeme.clone();
            let right = self.parse_unary()?;
            let span = Span::new(expr.span().start.clone(), right.span().end.clone());
            expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        if self.match_token(&[
            TokenType::Not,
            TokenType::Minus,
            TokenType::BitwiseNot,
            TokenType::Increment,
            TokenType::Decrement,
        ]) {
            let start = Position::from_token(self.previous().unwrap());
            let operator = self.previous().unwrap().lexeme.clone();
            let operand = Box::new(self.parse_unary()?);
            let end = operand.span().end.clone();
            let span = Span::new(start, end);
            return Ok(Expression::Unary {
                operator,
                operand,
                span,
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                let arguments = self.parse_arguments()?;
                let end_line = self.current().line;
                let end_column = self.current().column;
                let end_offset = self.current().offset;
                self.consume(&TokenType::RightParen, "Expected ')'")?;
                let span = Span::new(
                    expr.span().start.clone(),
                    Position::new(end_line, end_column, end_offset),
                );
                expr = Expression::Call {
                    callee: Box::new(expr),
                    arguments,
                    span,
                };
            } else if self.match_token(&[TokenType::Dot]) {
                let member = self.consume_identifier("Expected member name")?;
                let span = Span::new(
                    expr.span().start.clone(),
                    Position::from_token(self.previous().unwrap()),
                );
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    member,
                    span,
                };
            } else if self.match_token(&[TokenType::LeftBracket]) {
                let index = Box::new(self.parse_expression()?);
                let end_line = self.current().line;
                let end_column = self.current().column;
                let end_offset = self.current().offset;
                self.consume(&TokenType::RightBracket, "Expected ']'")?;
                let span = Span::new(
                    expr.span().start.clone(),
                    Position::new(end_line, end_column, end_offset),
                );
                expr = Expression::Index {
                    object: Box::new(expr),
                    index,
                    span,
                };
            } else if self.match_token(&[TokenType::Increment, TokenType::Decrement]) {
                let operator = self.previous().unwrap().lexeme.clone();
                let span = Span::new(
                    expr.span().start.clone(),
                    Position::from_token(self.previous().unwrap()),
                );
                expr = Expression::Unary {
                    operator: format!("post{}", operator),
                    operand: Box::new(expr),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut arguments = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                arguments.push(self.parse_expression()?);

                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        Ok(arguments)
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        if self.match_token(&[TokenType::True]) {
            let token = self.previous().unwrap();
            return Ok(Expression::BoolLiteral(true, Span::from_token(token)));
        }

        if self.match_token(&[TokenType::False]) {
            let token = self.previous().unwrap();
            return Ok(Expression::BoolLiteral(false, Span::from_token(token)));
        }

        if self.match_token(&[TokenType::Null]) {
            let token = self.previous().unwrap();
            return Ok(Expression::NullLiteral(Span::from_token(token)));
        }

        if self.match_token(&[TokenType::This]) {
            let token = self.previous().unwrap();
            return Ok(Expression::This(Span::from_token(token)));
        }

        if self.match_token(&[TokenType::Super]) {
            let token = self.previous().unwrap();
            return Ok(Expression::Super(Span::from_token(token)));
        }

        if let Some(token) = self.current_opt() {
            match &token.token_type {
                TokenType::IntLiteral(n) => {
                    let value = *n;
                    let span = Span::from_token(token);
                    self.advance();
                    return Ok(Expression::IntLiteral(value, span));
                }
                TokenType::FloatLiteral(f) => {
                    let value = *f;
                    let span = Span::from_token(token);
                    self.advance();
                    return Ok(Expression::FloatLiteral(value, span));
                }
                TokenType::StringLiteral(s) => {
                    let value = s.clone();
                    let span = Span::from_token(token);
                    self.advance();

                    // Check if this might be a vector literal
                    if value.split_whitespace().count() == 3 {
                        return Ok(Expression::VectorLiteral(value, span));
                    }
                    return Ok(Expression::StringLiteral(value, span));
                }
                TokenType::Identifier(name) => {
                    let identifier = name.clone();
                    let span = Span::from_token(token);
                    self.advance();
                    return Ok(Expression::Identifier(identifier, span));
                }
                _ => {}
            }
        }

        if self.match_token(&[TokenType::New]) {
            let start = Position::from_token(self.previous().unwrap());
            let type_ref = self.parse_type()?;

            let arguments = if self.match_token(&[TokenType::LeftParen]) {
                let args = self.parse_arguments()?;
                self.consume(&TokenType::RightParen, "Expected ')'")?;
                args
            } else {
                Vec::new()
            };

            let end = Position::from_token(self.previous().unwrap());
            let span = Span::new(start, end);

            return Ok(Expression::New {
                type_ref,
                arguments,
                span,
            });
        }

        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.parse_expression()?;
            self.consume(&TokenType::RightParen, "Expected ')'")?;
            return Ok(expr);
        }

        // Array/object initializer (config.cpp style): {value1, value2, ...}
        if self.match_token(&[TokenType::LeftBrace]) {
            let start = Position::from_token(self.previous().unwrap());
            let mut elements = Vec::new();

            while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                elements.push(self.parse_expression()?);
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }

            let end_line = self.current().line;
            let end_column = self.current().column;
            let end_offset = self.current().offset;
            self.consume(&TokenType::RightBrace, "Expected '}'")?;

            let end = Position::new(end_line, end_column, end_offset);
            let span = Span::new(start, end);

            return Ok(Expression::ArrayLiteral { elements, span });
        }

        Err(format!("Unexpected token: {:?}", self.current_opt()))
    }

    // Helper methods
    fn current(&self) -> &Token {
        &self.tokens[self.position]
    }

    fn current_opt(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    fn previous(&self) -> Option<&Token> {
        if self.position > 0 {
            Some(&self.tokens[self.position - 1])
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        if let Some(token) = self.current_opt() {
            matches!(token.token_type, TokenType::Eof)
        } else {
            true
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.position += 1;
            self.skip_comments();
        }
    }

    fn skip_comments(&mut self) {
        while !self.is_at_end() {
            if let Some(token) = self.current_opt() {
                if matches!(token.token_type, TokenType::Comment(_)) {
                    self.position += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if let Some(token) = self.current_opt() {
            std::mem::discriminant(&token.token_type) == std::mem::discriminant(token_type)
        } else {
            false
        }
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<(), String> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            Err(format!("{} at line {}", message, self.current().line))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, String> {
        if let Some(token) = self.current_opt() {
            if let TokenType::Identifier(name) = &token.token_type {
                let result = name.clone();
                self.advance();
                return Ok(result);
            }
        }
        Err(format!("{} at line {}", message, self.current().line))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some(prev) = self.previous() {
                if matches!(prev.token_type, TokenType::Semicolon) {
                    return;
                }
            }

            if let Some(curr) = self.current_opt() {
                if matches!(
                    curr.token_type,
                    TokenType::Class
                        | TokenType::Enum
                        | TokenType::Typedef
                        | TokenType::Void
                        | TokenType::Int
                        | TokenType::Float
                        | TokenType::Bool
                        | TokenType::String
                        | TokenType::For
                        | TokenType::If
                        | TokenType::While
                        | TokenType::Return
                ) {
                    return;
                }
            }

            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_parse_simple_class() {
        let code = r#"
class MyClass
{
    void Method()
    {
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.declarations.len(), 1);
    }

    #[test]
    fn test_parse_enum() {
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
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_expressions() {
        let code = r#"
void Test()
{
    int x = 5 + 3 * 2;
    bool y = x > 10 && x < 20;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_class_with_inheritance() {
        let code = r#"
class Dog extends AnimalClass
{
    override void MakeSound()
    {
        Print("Wof! Wof!");
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.declarations.len(), 1);
    }

    #[test]
    fn test_parse_modded_class() {
        let code = r#"
modded class ModMe
{
    override void Say()
    {
        super.Say();
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_constructor_destructor() {
        let code = r#"
class MyClass
{
    void MyClass()
    {
        Print("Constructor");
    }
    
    void ~MyClass()
    {
        Print("Destructor");
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_method_modifiers() {
        let code = r#"
class TestClass
{
    private void PrivateMethod() {}
    protected void ProtectedMethod() {}
    static void StaticMethod() {}
    proto native void NativeMethod();
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_variables() {
        let code = r#"
void Test()
{
    int a;
    a = 5;
    int b = 9;
    auto c = 10;
    const int MAX = 100;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_functions_with_params() {
        let code = r#"
int GiveMeTen()
{
    return 10;
}

void GiveMeElevenAndTwelve(out int val1, out int val2, int val3)
{
    val1 = 11;
    val2 = 12;
    val3 = 13;
}

void PrintNum(int a = 0)
{
    Print(a);
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_if_statement() {
        let code = r#"
void Method()
{
    int a = 4;
    int b = 5;
    
    if (a > 0)
    {
        Print("A is greater than zero!");
    }
    else if (a > 5)
    {
        Print("A is bigger than 5");
    }
    else
    {
        Print("A is not greater than zero!");
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_switch_statement() {
        let code = r#"
void Method()
{
    int a = 2;
    switch(a)
    {
        case 1:
            Print("a is 1");
            break;
        case 2:
            Print("a is 2");
            break;
        default:
            Print("it's something else");
            break;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_for_loop() {
        let code = r#"
void Method()
{
    for (int i = 0; i < 3; i++)
    {
        Print(i);
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_foreach_loop() {
        let code = r#"
void TestFn()
{
    int pole1[] = {7,3,6,8};
    
    foreach(int v: pole1)
    {
        Print(v);
    }
    
    foreach(int i, string j: pole2)
    {
        Print("pole[" + i + "] = " + j);
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_while_loop() {
        let code = r#"
void Method()
{
    int i = 0;
    while (i < 3)
    {
        Print(i);
        i++;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_static_arrays() {
        let code = r#"
void MethodA()
{
    int numbersArray[3];
    numbersArray[0] = 54;
    numbersArray[1] = 82;
    numbersArray[2] = 7;
    
    int anotherArray[3] = {53, 90, 7};
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_template_types() {
        let code = r#"
class Item<class T>
{
    T m_data;
    
    void Item(T data)
    {
        m_data = data;
    }
    
    T GetData()
    {
        return m_data;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_vectors() {
        let code = r#"
void Method()
{
    vector up = "0 1 0";
    vector down;
    down = up;
    down[1] = -1;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_object_creation() {
        let code = r#"
void MethodA()
{
    MyClass o;
    o = new MyClass;
    o.Say();
    delete o;
}

void MethodB()
{
    autoptr MyClass o;
    o = new MyClass;
    o.Say();
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_enum_with_extends() {
        let code = r#"
enum MyEnumBase
{
    Alfa = 5,
    Beta,
    Gamma
}

enum MyEnum extends MyEnumBase
{
    Blue,
    Yellow,
    Green = 20,
    Orange
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_string_operations() {
        let code = r#"
void Method()
{
    string a = "Hello";
    string b = " world!";
    string c = a + b;
    Print(c);
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_this_super() {
        let code = r#"
class HoneyBadger extends AnimalClass
{
    override void Hello()
    {
        Print("HoneyBadger.Hello()");
    }
    
    void Test()
    {
        Hello();
        this.Hello();
        super.Hello();
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_operators() {
        let code = r#"
void Test()
{
    int a = 10;
    int b = 5;
    
    int sum = a + b;
    int diff = a - b;
    int prod = a * b;
    int quot = a / b;
    int mod = a % b;
    
    bool eq = a == b;
    bool neq = a != b;
    bool gt = a > b;
    bool lt = a < b;
    bool gte = a >= b;
    bool lte = a <= b;
    
    bool and_result = (a > 0) && (b > 0);
    bool or_result = (a > 0) || (b > 0);
    bool not_result = !eq;
    
    int bitwise_and = a & b;
    int bitwise_or = a | b;
    int bitwise_xor = a ^ b;
    int bitwise_not = ~a;
    int left_shift = a << 2;
    int right_shift = a >> 2;
    
    a += 5;
    b -= 2;
    a *= 3;
    b /= 2;
    a |= 1;
    b &= 0xFF;
    a <<= 1;
    b >>= 1;
    
    ++a;
    --b;
    a++;
    b--;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_member_access() {
        let code = r#"
void Test()
{
    MyClass obj = new MyClass;
    obj.method();
    obj.property = 5;
    int val = obj.arr[0];
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_typedef() {
        let code = r#"
typedef array<string> TStringArray;
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_ref_keyword() {
        let code = r#"
class Parent
{
    ref Child m_child;
}

class Child
{
    Parent m_parent;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function_overloading() {
        let code = r#"
float Sum(float a, float b)
{
    return a + b;
}

float Sum(int a, int b)
{
    return a + b;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_return_continue_break() {
        let code = r#"
void Test()
{
    for (int i = 0; i < 10; i++)
    {
        if (i == 5)
            continue;
        if (i == 8)
            break;
        Print(i);
    }
    return;
}

int GetValue()
{
    return 42;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_ternary_operator() {
        let code = r#"
void Test()
{
    int x = 5;
    int y = (x > 0) ? 10 : 20;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_complex_class() {
        let code = r#"
class ComplexClass extends BaseClass
{
    private int m_privateField;
    protected string m_protectedField;
    static int s_staticField;
    ref array<int> m_refArray;
    autoptr MyClass m_autoPtr;
    
    void ComplexClass(int param)
    {
        m_privateField = param;
    }
    
    void ~ComplexClass()
    {
    }
    
    private void PrivateMethod()
    {
    }
    
    protected override void ProtectedOverride()
    {
        super.ProtectedOverride();
    }
    
    static void StaticMethod()
    {
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_nested_expressions() {
        let code = r#"
void Test()
{
    int result = ((a + b) * (c - d)) / ((e + f) * (g - h));
    bool complex = (x > 0 && y < 10) || (z >= 5 && w <= 15);
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function_calls() {
        let code = r#"
void Test()
{
    Print("Hello");
    int result = Calculate(5, 10);
    obj.Method(arg1, arg2, arg3);
    thread SomeFunction();
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_const_declarations() {
        let code = r#"
const int MONTHS_COUNT = 12;
const float PI = 3.14159;

class TestConst
{
    const int CONST_BASE = 4;
    const int CONST_TEST = 5;
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_with_comments() {
        let code = r#"
// This is a file comment
modded class StaminaHandler
{
    // This function is called every frame to update the stamina state.
    // By overriding it with an empty function, we prevent any stamina
    // calculations from happening.
    override void Update(float deltaT, int pCurrentCommandID)
    {
        // Stamina update logic is intentionally left blank.
        // This stops stamina from draining or regenerating based on player actions.
        // The stamina bar will appear full at all times.
    }

    /* This function is called when an action should deplete stamina.
       We override it to do nothing. */
    override void DepleteStamina(EStaminaModifiers modifier, float dT = -1)
    {
        // Stamina depletion logic is intentionally left blank.
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        if let Err(e) = &result {
            eprintln!("Parse error: {}", e);
        }
        for error in parser.get_errors() {
            eprintln!("Parser error: {}", error);
        }

        assert!(result.is_ok());
        assert_eq!(
            parser.get_errors().len(),
            0,
            "Parser should not have any errors"
        );
    }

    #[test]
    fn test_parse_config_cpp_style() {
        let code = r#"
class CfgPatches
{
    class MyMod
    {
        requiredAddons[]={"DZ_Characters_Vests"};
    };
};

class CfgVehicles
{
    class Clothing;
    class SmershVest: Clothing
    {
        inventorySlot[]=
        {
            "Vest",
            "Hips"
        };
        itemSize[]={2,2};
    };
};
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        if let Err(e) = &result {
            eprintln!("Parse error: {}", e);
        }
        for error in parser.get_errors() {
            eprintln!("Parser error: {}", error);
        }

        assert!(result.is_ok());
        assert_eq!(
            parser.get_errors().len(),
            0,
            "Config.cpp style should parse without errors"
        );
    }
}
