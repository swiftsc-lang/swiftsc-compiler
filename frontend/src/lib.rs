pub mod ast;
pub mod module_resolver;
pub mod parser;
pub mod sema;
pub mod token;

use ast::Program;
use logos::Logos;
use parser::Parser;
use token::Token;

pub fn tokenize(input: &str) -> Vec<(Token, std::ops::Range<usize>)> {
    Token::lexer(input)
        .spanned()
        .map(|(token, span)| match token {
            Ok(t) => (t, span),
            Err(_) => (Token::Error, span),
        })
        .collect()
}

pub fn parse(input: &str) -> Result<Program, parser::ParseError> {
    let tokens = tokenize(input);
    let iterator = tokens.into_iter(); // Vec into iterator
    let mut parser = Parser::new(iterator);
    parser.parse_program()
}
pub fn analyze(
    program: &ast::Program,
    root: Option<std::path::PathBuf>,
) -> Result<(), sema::analyzer::SemanticError> {
    let mut analyzer = sema::analyzer::Analyzer::new();
    if let Some(r) = root {
        analyzer = analyzer.with_root(r);
    }
    analyzer.check_program(program)
}

pub fn analyze_and_link(
    program: &ast::Program,
    root: Option<std::path::PathBuf>,
) -> Result<ast::Program, sema::analyzer::SemanticError> {
    let mut analyzer = sema::analyzer::Analyzer::new();
    if let Some(r) = root {
        analyzer = analyzer.with_root(r);
    }
    analyzer.check_program(program)?;
    Ok(ast::Program {
        items: analyzer.get_linked_items(),
    })
}
