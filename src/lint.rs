use crate::lexer::{Token, TokenType};
use crate::parser::{Declaration, Expression, Program, Statement};

/// Configurable lint thresholds.
#[derive(Debug, Clone)]
pub struct LintConfig {
    /// Max `+` operators in a single expression (string concat / addition chain).
    pub max_concat: usize,
    /// Max `||`/`&&` operators in a single expression.
    pub max_logical: usize,
}

impl Default for LintConfig {
    fn default() -> Self {
        LintConfig {
            // Enforce "Formula too complex" — empirically ~15 `+` is still fine
            // (botorama has working 15-operator chains).
            max_concat: 15,
            // codeguide: длинная булева цепочка тоже падает (~8-10 слагаемых)
            max_logical: 8,
        }
    }
}

/// A single lint finding: message + 1-based line + whether it's an error.
#[derive(Debug, Clone)]
pub struct LintIssue {
    pub message: String,
    pub line: usize,
    pub is_error: bool,
}

/// Run all Enforce-specific lint checks over the parsed program and token stream.
pub fn lint(program: &Program, tokens: &[Token], config: &LintConfig) -> Vec<LintIssue> {
    let mut issues = Vec::new();
    for decl in &program.declarations {
        lint_declaration(decl, config, &mut issues);
    }
    lint_line_break_after_dot(tokens, &mut issues);
    issues
}

fn lint_declaration(decl: &Declaration, config: &LintConfig, issues: &mut Vec<LintIssue>) {
    match decl {
        Declaration::Class(class) => {
            for field in &class.fields {
                if let Some(init) = &field.initializer {
                    lint_expression(init, config, issues, None);
                }
            }
            for method in &class.methods {
                if let Some(body) = &method.body {
                    lint_statement(body, config, issues);
                }
            }
        }
        Declaration::Function(method) => {
            if let Some(body) = &method.body {
                lint_statement(body, config, issues);
            }
        }
        Declaration::Variable(field) => {
            if let Some(init) = &field.initializer {
                lint_expression(init, config, issues, None);
            }
        }
        Declaration::Enum(_) | Declaration::Typedef(_) => {}
    }
}

fn lint_statement(stmt: &Statement, config: &LintConfig, issues: &mut Vec<LintIssue>) {
    match stmt {
        Statement::VariableDeclaration { initializer, .. } => {
            if let Some(init) = initializer {
                lint_expression(init, config, issues, None);
            }
        }
        Statement::Expression { expression, .. } => {
            lint_expression(expression, config, issues, None);
        }
        Statement::Block { statements, .. } => {
            for s in statements {
                lint_statement(s, config, issues);
            }
        }
        Statement::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            lint_expression(condition, config, issues, None);
            lint_statement(then_branch, config, issues);
            if let Some(e) = else_branch {
                lint_statement(e, config, issues);
            }
        }
        Statement::Switch {
            expression, cases, ..
        } => {
            lint_expression(expression, config, issues, None);
            for (case_expr, case_stmts) in cases {
                if let Some(ce) = case_expr {
                    lint_expression(ce, config, issues, None);
                }
                for s in case_stmts {
                    lint_statement(s, config, issues);
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
            if let Some(i) = initializer {
                lint_statement(i, config, issues);
            }
            if let Some(c) = condition {
                lint_expression(c, config, issues, None);
            }
            if let Some(inc) = increment {
                lint_expression(inc, config, issues, None);
            }
            lint_statement(body, config, issues);
        }
        Statement::Foreach { iterable, body, .. } => {
            lint_expression(iterable, config, issues, None);
            lint_statement(body, config, issues);
        }
        Statement::While { condition, body, .. } => {
            lint_expression(condition, config, issues, None);
            lint_statement(body, config, issues);
        }
        Statement::Return { value, .. } => {
            if let Some(v) = value {
                lint_expression(v, config, issues, None);
            }
        }
        Statement::Delete { expression, .. } => {
            lint_expression(expression, config, issues, None);
        }
        Statement::Break(_) | Statement::Continue(_) => {}
    }
}

fn lint_expression(
    expr: &Expression,
    config: &LintConfig,
    issues: &mut Vec<LintIssue>,
    parent_op: Option<&str>,
) {
    match expr {
        Expression::Binary {
            left,
            operator,
            right,
            span,
        } => {
            if operator == "+" || operator == "||" || operator == "&&" {
                // Flag only at the root of a maximal same-operator chain.
                if parent_op != Some(operator.as_str()) {
                    let operands = chain_operand_count(expr, operator);
                    let n_ops = operands.saturating_sub(1);
                    let threshold = if operator == "+" {
                        config.max_concat
                    } else {
                        config.max_logical
                    };
                    if n_ops > threshold {
                        issues.push(LintIssue {
                            message: format!(
                                "Chained '{}' ({} operators) exceeds limit ({})",
                                operator, n_ops, threshold
                            ),
                            line: span.start.line,
                            is_error: true,
                        });
                    }
                }
                lint_expression(left, config, issues, Some(operator));
                lint_expression(right, config, issues, None);
            } else {
                lint_expression(left, config, issues, None);
                lint_expression(right, config, issues, None);
            }
        }
        Expression::MemberAccess { object, .. } => {
            lint_expression(object, config, issues, None);
        }
        Expression::Call {
            callee, arguments, ..
        } => {
            lint_expression(callee, config, issues, None);
            for a in arguments {
                lint_expression(a, config, issues, None);
            }
        }
        Expression::Unary { operand, .. } => lint_expression(operand, config, issues, None),
        Expression::Index { object, index, .. } => {
            lint_expression(object, config, issues, None);
            lint_expression(index, config, issues, None);
        }
        Expression::Cast { expression, .. } => lint_expression(expression, config, issues, None),
        Expression::Ternary {
            condition,
            then_expr,
            else_expr,
            span,
        } => {
            // codeguide: ternary `cond ? a : b` is NOT supported in Enforce Script.
            issues.push(LintIssue {
                message: "Ternary operator is not supported in Enforce Script".to_string(),
                line: span.start.line,
                is_error: true,
            });
            lint_expression(condition, config, issues, None);
            lint_expression(then_expr, config, issues, None);
            lint_expression(else_expr, config, issues, None);
        }
        Expression::ArrayLiteral { elements, .. } => {
            for e in elements {
                lint_expression(e, config, issues, None);
            }
        }
        Expression::New { arguments, .. } => {
            for a in arguments {
                lint_expression(a, config, issues, None);
            }
        }
        _ => {}
    }
}

/// Number of operands in a flat same-operator chain rooted at `expr`.
fn chain_operand_count(expr: &Expression, op: &str) -> usize {
    match expr {
        Expression::Binary {
            left,
            operator,
            right,
            ..
        } if operator == op => {
            chain_operand_count(left, op) + chain_operand_count(right, op)
        }
        _ => 1,
    }
}

/// Flag a `.` whose next token (member) is on a later line — a real syntax error
/// in Enforce (`A.` newline `B()`).
fn lint_line_break_after_dot(tokens: &[Token], issues: &mut Vec<LintIssue>) {
    for i in 0..tokens.len() {
        if !matches!(tokens[i].token_type, TokenType::Dot) {
            continue;
        }
        let mut j = i + 1;
        while j < tokens.len() && matches!(tokens[j].token_type, TokenType::Comment(_)) {
            j += 1;
        }
        if j < tokens.len() && tokens[j].line > tokens[i].line {
            issues.push(LintIssue {
                message: "Line break after '.'".to_string(),
                line: tokens[i].line,
                is_error: true,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn lint_code(code: &str) -> Vec<LintIssue> {
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens.clone());
        let program = parser.parse().unwrap();
        lint(&program, &tokens, &LintConfig::default())
    }

    #[test]
    fn test_long_concat() {
        // 16 '+' operators — over the threshold of 15.
        let code = r#"void F() { string s = "a"+"b"+"c"+"d"+"e"+"f"+"g"+"h"+"i"+"j"+"k"+"l"+"m"+"n"+"o"+"p"+"q"; }"#;
        let issues = lint_code(code);
        assert!(issues.iter().any(|i| i.message.contains("Chained '+'")));
    }

    #[test]
    fn test_long_logical() {
        // 9 '||' operators — over the threshold of 8.
        let code = "void F(bool a,bool b,bool c,bool d,bool e,bool f,bool g,bool h,bool i,bool j) { if (a||b||c||d||e||f||g||h||i||j) {} }";
        let issues = lint_code(code);
        assert!(issues.iter().any(|i| i.message.contains("Chained '||'")));
    }

    #[test]
    fn test_ternary() {
        let code = r#"void F(bool c) { string s = c ? "a" : "b"; }"#;
        let issues = lint_code(code);
        assert!(issues.iter().any(|i| i.message.contains("Ternary")));
    }

    #[test]
    fn test_line_break_after_dot() {
        let code = "void F() { int x = obj.\n  field; }";
        let issues = lint_code(code);
        assert!(issues.iter().any(|i| i.message.contains("Line break")));
    }

    #[test]
    fn test_short_concat_not_flagged() {
        let code = r#"void F() { string s = "a" + "b" + "c"; }"#; // 2 '+', under threshold
        let issues = lint_code(code);
        assert!(!issues.iter().any(|i| i.message.contains("Chained '+'")));
    }
}
