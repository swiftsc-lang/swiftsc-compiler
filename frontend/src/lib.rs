pub mod ast;
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
pub fn analyze(program: &ast::Program) -> Result<(), sema::analyzer::SemanticError> {
    let mut analyzer = sema::analyzer::Analyzer::new();
    analyzer.check_program(program)
}
