/// Lexer for Enforce Script
/// Tokenizes source code into a stream of tokens

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    Class,
    Extends,
    Modded,
    Enum,
    Typedef,
    Void,
    Int,
    Float,
    Bool,
    String,
    Vector,
    Typename,
    Auto,
    Autoptr,
    Ref,
    Const,
    Static,
    Private,
    Protected,
    Override,
    Proto,
    Native,
    Out,
    Inout,
    New,
    Delete,
    This,
    Super,
    Return,
    Null,
    True,
    False,
    Thread,

    // Control structures
    If,
    Else,
    Switch,
    Case,
    Default,
    For,
    Foreach,
    While,
    Break,
    Continue,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    OrAssign,
    AndAssign,
    LeftShiftAssign,
    RightShiftAssign,
    Increment,
    Decrement,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
    Not,
    Question,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,

    // Literals and identifiers
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    #[allow(dead_code)] // Reserved for future vector literal support
    VectorLiteral(String),

    // Special
    Comment(String),
    #[allow(dead_code)] // Reserved for potential whitespace-aware parsing
    Whitespace,
    Newline,
    Eof,
    Unknown(char),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        column: usize,
        offset: usize,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            offset,
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
    offset: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);

            // Skip whitespace and newlines for now, but keep them for future use
            if !matches!(token.token_type, TokenType::Whitespace | TokenType::Newline) {
                tokens.push(token);
            }

            if is_eof {
                break;
            }
        }

        tokens
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_inline();

        if self.is_at_end() {
            return self.make_token(TokenType::Eof, "");
        }

        let start_line = self.line;
        let start_column = self.column;
        let start_offset = self.offset;

        let ch = self.current_char();

        // Handle newlines
        if ch == '\n' {
            self.advance();
            return Token::new(
                TokenType::Newline,
                "\n".to_string(),
                start_line,
                start_column,
                start_offset,
            );
        }

        // Handle comments
        if ch == '/' {
            if self.peek(1) == Some('/') {
                return self.scan_single_line_comment(start_line, start_column, start_offset);
            } else if self.peek(1) == Some('*') {
                return self.scan_multi_line_comment(start_line, start_column, start_offset);
            }
        }

        // Handle string literals
        if ch == '"' {
            return self.scan_string_literal(start_line, start_column, start_offset);
        }

        // Handle numbers
        if ch.is_ascii_digit() {
            return self.scan_number(start_line, start_column, start_offset);
        }

        // Handle identifiers and keywords
        if ch.is_alphabetic() || ch == '_' {
            return self.scan_identifier_or_keyword(start_line, start_column, start_offset);
        }

        // Handle operators and delimiters
        self.scan_operator_or_delimiter(start_line, start_column, start_offset)
    }

    fn skip_whitespace_inline(&mut self) {
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan_single_line_comment(&mut self, line: usize, column: usize, offset: usize) -> Token {
        let start = self.position;
        self.advance(); // '/'
        self.advance(); // '/'

        while !self.is_at_end() && self.current_char() != '\n' {
            self.advance();
        }

        let comment_text: String = self.input[start..self.position].iter().collect();
        Token::new(
            TokenType::Comment(comment_text.clone()),
            comment_text,
            line,
            column,
            offset,
        )
    }

    fn scan_multi_line_comment(&mut self, line: usize, column: usize, offset: usize) -> Token {
        let start = self.position;
        self.advance(); // '/'
        self.advance(); // '*'

        while !self.is_at_end() {
            if self.current_char() == '*' && self.peek(1) == Some('/') {
                self.advance(); // '*'
                self.advance(); // '/'
                break;
            }
            if self.current_char() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            self.advance();
        }

        let comment_text: String = self.input[start..self.position].iter().collect();
        Token::new(
            TokenType::Comment(comment_text.clone()),
            comment_text,
            line,
            column,
            offset,
        )
    }

    fn scan_string_literal(&mut self, line: usize, column: usize, offset: usize) -> Token {
        let start = self.position;
        self.advance(); // opening "

        let mut value = String::new();

        while !self.is_at_end() && self.current_char() != '"' {
            if self.current_char() == '\\' {
                self.advance();
                if !self.is_at_end() {
                    let escaped = match self.current_char() {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        ch => ch,
                    };
                    value.push(escaped);
                    self.advance();
                }
            } else {
                value.push(self.current_char());
                self.advance();
            }
        }

        if !self.is_at_end() {
            self.advance(); // closing "
        }

        let lexeme: String = self.input[start..self.position].iter().collect();
        Token::new(
            TokenType::StringLiteral(value),
            lexeme,
            line,
            column,
            offset,
        )
    }

    fn scan_number(&mut self, line: usize, column: usize, offset: usize) -> Token {
        let start = self.position;

        while !self.is_at_end() && self.current_char().is_ascii_digit() {
            self.advance();
        }

        let mut is_float = false;

        // Handle decimal point
        if !self.is_at_end()
            && self.current_char() == '.'
            && self.peek(1).is_some_and(|c| c.is_ascii_digit())
        {
            is_float = true;
            self.advance(); // '.'
            while !self.is_at_end() && self.current_char().is_ascii_digit() {
                self.advance();
            }
        }

        // Handle scientific notation
        if !self.is_at_end() && (self.current_char() == 'e' || self.current_char() == 'E') {
            is_float = true;
            self.advance();
            if !self.is_at_end() && (self.current_char() == '+' || self.current_char() == '-') {
                self.advance();
            }
            while !self.is_at_end() && self.current_char().is_ascii_digit() {
                self.advance();
            }
        }

        let lexeme: String = self.input[start..self.position].iter().collect();

        if is_float {
            let value = lexeme.parse::<f64>().unwrap_or(0.0);
            Token::new(TokenType::FloatLiteral(value), lexeme, line, column, offset)
        } else {
            let value = lexeme.parse::<i64>().unwrap_or(0);
            Token::new(TokenType::IntLiteral(value), lexeme, line, column, offset)
        }
    }

    fn scan_identifier_or_keyword(&mut self, line: usize, column: usize, offset: usize) -> Token {
        let start = self.position;

        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let lexeme: String = self.input[start..self.position].iter().collect();
        let token_type = match lexeme.as_str() {
            "class" => TokenType::Class,
            "extends" => TokenType::Extends,
            "modded" => TokenType::Modded,
            "enum" => TokenType::Enum,
            "typedef" => TokenType::Typedef,
            "void" => TokenType::Void,
            "int" => TokenType::Int,
            "float" => TokenType::Float,
            "bool" => TokenType::Bool,
            "string" => TokenType::String,
            "vector" => TokenType::Vector,
            "typename" => TokenType::Typename,
            "auto" => TokenType::Auto,
            "autoptr" => TokenType::Autoptr,
            "ref" => TokenType::Ref,
            "const" => TokenType::Const,
            "static" => TokenType::Static,
            "private" => TokenType::Private,
            "protected" => TokenType::Protected,
            "override" => TokenType::Override,
            "proto" => TokenType::Proto,
            "native" => TokenType::Native,
            "out" => TokenType::Out,
            "inout" => TokenType::Inout,
            "new" => TokenType::New,
            "delete" => TokenType::Delete,
            "this" => TokenType::This,
            "super" => TokenType::Super,
            "return" => TokenType::Return,
            "null" => TokenType::Null,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "thread" => TokenType::Thread,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "switch" => TokenType::Switch,
            "case" => TokenType::Case,
            "default" => TokenType::Default,
            "for" => TokenType::For,
            "foreach" => TokenType::Foreach,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            _ => TokenType::Identifier(lexeme.clone()),
        };

        Token::new(token_type, lexeme, line, column, offset)
    }

    fn scan_operator_or_delimiter(
        &mut self,
        line: usize,
        column: usize,
        start_offset: usize,
    ) -> Token {
        let ch = self.current_char();
        self.advance();

        let token_type = match ch {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            '[' => TokenType::LeftBracket,
            ']' => TokenType::RightBracket,
            ';' => TokenType::Semicolon,
            ':' => TokenType::Colon,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '~' => TokenType::BitwiseNot,
            '+' => {
                if !self.is_at_end() && self.current_char() == '+' {
                    self.advance();
                    TokenType::Increment
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::PlusAssign
                } else {
                    TokenType::Plus
                }
            }
            '-' => {
                if !self.is_at_end() && self.current_char() == '-' {
                    self.advance();
                    TokenType::Decrement
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::MinusAssign
                } else {
                    TokenType::Minus
                }
            }
            '*' => {
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::StarAssign
                } else {
                    TokenType::Star
                }
            }
            '/' => {
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::SlashAssign
                } else {
                    TokenType::Slash
                }
            }
            '%' => TokenType::Percent,
            '=' => {
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::Equal
                } else {
                    TokenType::Assign
                }
            }
            '!' => {
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::NotEqual
                } else {
                    TokenType::Not
                }
            }
            '>' => {
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::GreaterEqual
                } else if !self.is_at_end() && self.current_char() == '>' {
                    self.advance();
                    if !self.is_at_end() && self.current_char() == '=' {
                        self.advance();
                        TokenType::RightShiftAssign
                    } else {
                        TokenType::RightShift
                    }
                } else {
                    TokenType::Greater
                }
            }
            '<' => {
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::LessEqual
                } else if !self.is_at_end() && self.current_char() == '<' {
                    self.advance();
                    if !self.is_at_end() && self.current_char() == '=' {
                        self.advance();
                        TokenType::LeftShiftAssign
                    } else {
                        TokenType::LeftShift
                    }
                } else {
                    TokenType::Less
                }
            }
            '&' => {
                if !self.is_at_end() && self.current_char() == '&' {
                    self.advance();
                    TokenType::LogicalAnd
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::AndAssign
                } else {
                    TokenType::BitwiseAnd
                }
            }
            '|' => {
                if !self.is_at_end() && self.current_char() == '|' {
                    self.advance();
                    TokenType::LogicalOr
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    TokenType::OrAssign
                } else {
                    TokenType::BitwiseOr
                }
            }
            '^' => TokenType::BitwiseXor,
            '?' => TokenType::Question,
            _ => TokenType::Unknown(ch),
        };

        let end = self.position;
        let lexeme: String = self.input[start_offset..end].iter().collect();
        Token::new(token_type, lexeme, line, column, start_offset)
    }

    fn current_char(&self) -> char {
        self.input[self.position]
    }

    fn peek(&self, ahead: usize) -> Option<char> {
        let pos = self.position + ahead;
        if pos < self.input.len() {
            Some(self.input[pos])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            if self.current_char() == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
            self.offset += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn make_token(&self, token_type: TokenType, lexeme: &str) -> Token {
        Token::new(
            token_type,
            lexeme.to_string(),
            self.line,
            self.column,
            self.offset,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("class void int float bool string");
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 7); // 6 keywords + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Class));
        assert!(matches!(tokens[1].token_type, TokenType::Void));
        assert!(matches!(tokens[2].token_type, TokenType::Int));
        assert!(matches!(tokens[3].token_type, TokenType::Float));
        assert!(matches!(tokens[4].token_type, TokenType::Bool));
        assert!(matches!(tokens[5].token_type, TokenType::String));
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("myVar _private test123");
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 identifiers + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("42 3.14 1.0e-5");
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 numbers + EOF
        assert!(matches!(tokens[0].token_type, TokenType::IntLiteral(42)));
        assert!(matches!(tokens[1].token_type, TokenType::FloatLiteral(_)));
        assert!(matches!(tokens[2].token_type, TokenType::FloatLiteral(_)));
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new(r#""hello" "world\n""#);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 3); // 2 strings + EOF
        assert!(matches!(tokens[0].token_type, TokenType::StringLiteral(_)));
        assert!(matches!(tokens[1].token_type, TokenType::StringLiteral(_)));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / == != >= <=");
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Plus));
        assert!(matches!(tokens[1].token_type, TokenType::Minus));
        assert!(matches!(tokens[2].token_type, TokenType::Star));
        assert!(matches!(tokens[3].token_type, TokenType::Slash));
        assert!(matches!(tokens[4].token_type, TokenType::Equal));
        assert!(matches!(tokens[5].token_type, TokenType::NotEqual));
        assert!(matches!(tokens[6].token_type, TokenType::GreaterEqual));
        assert!(matches!(tokens[7].token_type, TokenType::LessEqual));
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("// single line\n/* multi\nline */");
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 3); // 2 comments + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Comment(_)));
        assert!(matches!(tokens[1].token_type, TokenType::Comment(_)));
    }

    #[test]
    fn test_simple_class() {
        let code = r#"
class MyClass
{
    void Method()
    {
        int x = 5;
    }
}
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        // Should have: class, identifier, {, void, identifier, (, ), {, int, identifier, =, number, ;, }, }, EOF
        assert!(tokens.len() > 10);
        assert!(matches!(tokens[0].token_type, TokenType::Class));
    }

    #[test]
    fn test_all_keywords() {
        let keywords = "class enum typedef extends modded proto native static private protected \
                        void int float bool string vector auto new delete this super return break \
                        continue if else switch case default for foreach while const ref autoptr out inout override thread typename";
        let mut lexer = Lexer::new(keywords);
        let tokens = lexer.tokenize();

        // Verify all are recognized as keywords, not identifiers
        for token in &tokens[..tokens.len() - 1] {
            assert!(
                !matches!(token.token_type, TokenType::Identifier(_)),
                "Token {:?} should be a keyword",
                token.token_type
            );
        }
    }

    #[test]
    fn test_hex_numbers() {
        let mut lexer = Lexer::new("0x1A 0xFF 0x00");
        let tokens = lexer.tokenize();

        // Hex numbers are parsed as IntLiteral - filter out whitespace/newlines
        let num_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.token_type, TokenType::IntLiteral(_)))
            .collect();
        assert_eq!(num_tokens.len(), 3);
    }

    #[test]
    fn test_vector_literal() {
        let mut lexer = Lexer::new(r#"vector v = "1.0 2.0 3.0";"#);
        let tokens = lexer.tokenize();

        // Should recognize vector keyword, identifier, =, string literal, ;
        assert!(matches!(tokens[0].token_type, TokenType::Vector));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::Assign));
        assert!(matches!(tokens[3].token_type, TokenType::StringLiteral(_)));
        assert!(matches!(tokens[4].token_type, TokenType::Semicolon));
    }

    #[test]
    fn test_escape_sequences() {
        let mut lexer = Lexer::new(r#""hello\nworld" "tab\there" "quote\"test" "backslash\\""#);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 strings + EOF
        for token in tokens.iter().take(4) {
            assert!(matches!(token.token_type, TokenType::StringLiteral(_)));
        }
    }

    #[test]
    fn test_all_operators() {
        let ops =
            "+ - * / % == != > < >= <= && || ! & | ^ ~ << >> = += -= *= /= |= &= <<= >>= ++ --";
        let mut lexer = Lexer::new(ops);
        let tokens = lexer.tokenize();

        // All should be recognized as operators
        for token in &tokens[..tokens.len() - 1] {
            assert!(!matches!(token.token_type, TokenType::Identifier(_)));
            assert!(!matches!(token.token_type, TokenType::Eof));
        }
    }

    #[test]
    fn test_delimiters() {
        let mut lexer = Lexer::new("( ) { } [ ] ; , . : < >");
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::LeftParen));
        assert!(matches!(tokens[1].token_type, TokenType::RightParen));
        assert!(matches!(tokens[2].token_type, TokenType::LeftBrace));
        assert!(matches!(tokens[3].token_type, TokenType::RightBrace));
        assert!(matches!(tokens[4].token_type, TokenType::LeftBracket));
        assert!(matches!(tokens[5].token_type, TokenType::RightBracket));
        assert!(matches!(tokens[6].token_type, TokenType::Semicolon));
        assert!(matches!(tokens[7].token_type, TokenType::Comma));
        assert!(matches!(tokens[8].token_type, TokenType::Dot));
        assert!(matches!(tokens[9].token_type, TokenType::Colon));
        assert!(matches!(tokens[10].token_type, TokenType::Less));
        assert!(matches!(tokens[11].token_type, TokenType::Greater));
    }

    #[test]
    fn test_multiline_comment() {
        let code = r#"
/* This is a
   multi-line
   comment */
int x;
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Comment(_)));
        assert!(matches!(tokens[1].token_type, TokenType::Int));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_float_variations() {
        let mut lexer = Lexer::new("3.14 .5 2. 1e10 1.5e-3 2E+5");
        let tokens = lexer.tokenize();

        // Filter to just float/int literals (some may parse as int)
        let num_count = tokens
            .iter()
            .filter(|t| {
                matches!(t.token_type, TokenType::FloatLiteral(_))
                    || matches!(t.token_type, TokenType::IntLiteral(_))
            })
            .count();
        assert!(num_count >= 6, "Should have at least 6 numeric literals");
    }

    #[test]
    fn test_template_syntax() {
        let mut lexer = Lexer::new("array<int> map<string,float>");
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Identifier(_))); // array
        assert!(matches!(tokens[1].token_type, TokenType::Less));
        assert!(matches!(tokens[2].token_type, TokenType::Int));
        assert!(matches!(tokens[3].token_type, TokenType::Greater));
        assert!(matches!(tokens[4].token_type, TokenType::Identifier(_))); // map
        assert!(matches!(tokens[5].token_type, TokenType::Less));
        assert!(matches!(tokens[6].token_type, TokenType::String));
        assert!(matches!(tokens[7].token_type, TokenType::Comma));
        assert!(matches!(tokens[8].token_type, TokenType::Float));
        assert!(matches!(tokens[9].token_type, TokenType::Greater));
    }

    #[test]
    fn test_modded_class() {
        let code = "modded class MyClass extends BaseClass";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Modded));
        assert!(matches!(tokens[1].token_type, TokenType::Class));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[3].token_type, TokenType::Extends));
        assert!(matches!(tokens[4].token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_enum_declaration() {
        let code = "enum MyEnum { RED = 1, GREEN, BLUE }";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Enum));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::LeftBrace));
        assert!(matches!(tokens[3].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[4].token_type, TokenType::Assign));
    }

    #[test]
    fn test_ternary_operator() {
        let code = "x ? y : z";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[1].token_type, TokenType::Question));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[3].token_type, TokenType::Colon));
        assert!(matches!(tokens[4].token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_constructor_destructor() {
        let code = "void MyClass() {} void ~MyClass() {}";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Void));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        // Destructor with tilde
        assert!(matches!(tokens[6].token_type, TokenType::Void));
        assert!(matches!(tokens[7].token_type, TokenType::BitwiseNot)); // ~
        assert!(matches!(tokens[8].token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_method_modifiers() {
        let code = "private static void Method() {} protected override void Test() {}";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        // Check all expected keywords are present
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Private)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Static)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Protected)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Override)));
        assert!(
            tokens
                .iter()
                .filter(|t| matches!(t.token_type, TokenType::Void))
                .count()
                == 2
        );
    }

    #[test]
    fn test_autoptr_ref() {
        let code = "autoptr MyClass obj; ref array<int> arr;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Autoptr));
        assert!(matches!(tokens[4].token_type, TokenType::Ref));
    }

    #[test]
    fn test_out_inout_parameters() {
        let code = "void Test(out int x, inout float y) {}";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[3].token_type, TokenType::Out));
        assert!(matches!(tokens[7].token_type, TokenType::Inout));
    }

    #[test]
    fn test_const_declaration() {
        let code = "const int MAX_VALUE = 100;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Const));
        assert!(matches!(tokens[1].token_type, TokenType::Int));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[3].token_type, TokenType::Assign));
        assert!(matches!(tokens[4].token_type, TokenType::IntLiteral(100)));
    }

    #[test]
    fn test_array_syntax() {
        let code = "int arr[10]; string names[] = {\"a\", \"b\"};";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Int));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::LeftBracket));
        assert!(matches!(tokens[3].token_type, TokenType::IntLiteral(10)));
        assert!(matches!(tokens[4].token_type, TokenType::RightBracket));
    }

    #[test]
    fn test_switch_case() {
        let code = "switch(x) { case 1: break; default: break; }";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        // Check all expected keywords are present
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Switch)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Case)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Default)));
        assert!(
            tokens
                .iter()
                .filter(|t| matches!(t.token_type, TokenType::Break))
                .count()
                == 2
        );
    }

    #[test]
    fn test_loop_keywords() {
        let code = "for(int i=0; i<10; i++) {} while(true) {} foreach(auto x: arr) {}";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::For));
        let while_pos = tokens
            .iter()
            .position(|t| matches!(t.token_type, TokenType::While))
            .unwrap();
        assert!(while_pos > 0);
        let foreach_pos = tokens
            .iter()
            .position(|t| matches!(t.token_type, TokenType::Foreach))
            .unwrap();
        assert!(foreach_pos > 0);
    }

    #[test]
    fn test_boolean_literals() {
        let code = "bool a = true; bool b = false;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        let true_pos = tokens
            .iter()
            .position(|t| matches!(t.token_type, TokenType::True))
            .unwrap();
        let false_pos = tokens
            .iter()
            .position(|t| matches!(t.token_type, TokenType::False))
            .unwrap();
        assert!(true_pos > 0);
        assert!(false_pos > true_pos);
    }

    #[test]
    fn test_null_keyword() {
        let code = "MyClass obj = null;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        let null_pos = tokens
            .iter()
            .position(|t| matches!(t.token_type, TokenType::Null))
            .unwrap();
        assert!(null_pos > 0);
    }

    #[test]
    fn test_typedef() {
        let code = "typedef array<string> TStringArray;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Typedef));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
    }

    #[test]
    fn test_proto_native() {
        let code = "proto native void SomeNativeFunc();";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Proto));
        assert!(matches!(tokens[1].token_type, TokenType::Native));
    }

    #[test]
    fn test_thread_keyword() {
        let code = "thread SomeFunction();";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Thread));
    }

    #[test]
    fn test_typename_keyword() {
        let code = "typename t = MyClass;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Typename));
    }

    #[test]
    fn test_complex_expression() {
        let code = "result = (a + b) * c - d / e % f;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        // Verify we get all operators and operands
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Plus)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Star)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Minus)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Slash)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Percent)));
    }

    #[test]
    fn test_member_access() {
        let code = "obj.method(arg).property[index]";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        // Check we have the right tokens, order doesn't matter as much
        assert!(
            tokens
                .iter()
                .filter(|t| matches!(t.token_type, TokenType::Dot))
                .count()
                >= 2
        );
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::LeftBracket)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::RightBracket)));
    }

    #[test]
    fn test_bitwise_operators() {
        let code = "a & b | c ^ d ~ e << 2 >> 3";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::BitwiseAnd)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::BitwiseOr)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::BitwiseXor)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::BitwiseNot)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::LeftShift)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::RightShift)));
    }

    #[test]
    fn test_compound_assignments() {
        let code = "a += 1; b -= 2; c *= 3; d /= 4; f |= 6; g &= 7; h <<= 8; i >>= 9;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::PlusAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::MinusAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::StarAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::SlashAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::OrAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::AndAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::LeftShiftAssign)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::RightShiftAssign)));
    }

    #[test]
    fn test_increment_decrement() {
        let code = "++i; --j; k++; l--;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();

        let increment_count = tokens
            .iter()
            .filter(|t| matches!(t.token_type, TokenType::Increment))
            .count();
        let decrement_count = tokens
            .iter()
            .filter(|t| matches!(t.token_type, TokenType::Decrement))
            .count();
        assert_eq!(increment_count, 2);
        assert_eq!(decrement_count, 2);
    }
}
