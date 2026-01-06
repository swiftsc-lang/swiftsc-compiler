use swiftsc_frontend::ast::*;
use swiftsc_frontend::{parser::Parser, tokenize};

// Helper to parse single function
fn parse_func(input: &str) -> Function {
    let tokens = tokenize(input);
    let mut parser = Parser::new(tokens.into_iter(), input.to_string());
    // Hack: wrap in Items then unwrap
    // Or just expose parser internals? For integration test, use public API
    match parser.parse_program().unwrap().items.pop().unwrap() {
        Item::Function(f) => f,
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_parse_simple_function() {
    let input = "fn add(a: u64, b: u64) -> u64 { return a + b; }";
    let func = parse_func(input);
    assert_eq!(func.name, "add");
    // Check Parsing success is enough for MVP
}

#[test]
fn test_precedence() {
    // 1 + 2 * 3 should be 1 + (2 * 3)
    let input = "fn main() { 1 + 2 * 3; }";
    let func = parse_func(input);

    let stmt = &func.body.stmts[0];
    if let StatementKind::Expr(expr) = &stmt.kind {
        if let ExpressionKind::Binary { left, op, right } = &expr.kind {
            assert_eq!(*op, BinaryOp::Add); // Root must be +
            match &left.kind {
                ExpressionKind::Literal(Literal::Int(1)) => {}
                _ => panic!("Left should be 1"),
            }
            match &right.kind {
                ExpressionKind::Binary {
                    left: _l2,
                    op: op2,
                    right: _r2,
                } => {
                    assert_eq!(*op2, BinaryOp::Mul); // Inner must be *
                                                     // 2 * 3
                }
                _ => panic!("Right should be Binary"),
            }
        } else {
            panic!("Expression should be Binary");
        }
    } else {
        panic!("Top level should be expression");
    }
}
