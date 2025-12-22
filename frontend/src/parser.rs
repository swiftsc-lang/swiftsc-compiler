use crate::ast::*;
use crate::token::Token;
use logos::Span;
use std::iter::Peekable;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token: {0:?} at {1:?}")]
    UnexpectedToken(Token, Span),
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Expected {0} but found {1:?} at {2:?}")]
    Expected(String, Token, Span),
}

pub struct Parser<I>
where
    I: Iterator<Item = (Token, Span)>,
{
    lexer: Peekable<I>,
    last_span: Span,
}

impl<I> Parser<I>
where
    I: Iterator<Item = (Token, Span)>,
{
    pub fn new(lexer: I) -> Self {
        Self {
            lexer: lexer.peekable(),
            last_span: 0..0,
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek().map(|(t, _)| t)
    }

    fn advance(&mut self) -> Option<Token> {
        let (token, span) = self.lexer.next()?;
        self.last_span = span;
        Some(token)
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        let token_mismatch = match self.peek() {
            Some(t) if t == &expected => None,
            Some(t) => Some(t.clone()),
            None => return Err(ParseError::UnexpectedEOF),
        };

        if let Some(found_token) = token_mismatch {
            let span = self
                .lexer
                .peek()
                .map(|(_, s)| s.clone())
                .unwrap_or(self.last_span.clone());
            eprintln!(
                "DEBUG: Expected {:?} but found {:?} at {:?}",
                expected, found_token, span
            );
            return Err(ParseError::Expected(
                format!("{:?}", expected),
                found_token,
                span,
            ));
        }

        self.advance();
        Ok(())
    }

    // --- High Level ---

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while self.peek().is_some() {
            items.push(self.parse_item()?);
        }
        Ok(Program { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek() {
            Some(Token::Contract) => Ok(Item::Contract(self.parse_contract()?)),
            Some(Token::Fn) | Some(Token::Pub) => Ok(Item::Function(self.parse_function()?)),

            // Skip other top-level items for MVP to allow parsing success
            Some(Token::Use) => {
                self.advance();
                while self.peek() != Some(&Token::Semicolon) {
                    self.advance();
                }
                self.expect(Token::Semicolon)?;
                // Return dummy item or filter out?
                // For MVP AST, let's just recurse or return a dummy.
                // Actually, Program::items expects Item. Let's add a Dummy/Skipped variant to AST?
                // Or better, just loop in parse_program until we find a real item?
                // See parse_program loop.
                // Refactor: parse_item should return Option<Item>?
                // No, let's implement basic parsing for them.
                Ok(Item::Dummy)
            }
            Some(Token::Struct) | Some(Token::Resource) | Some(Token::Enum) => {
                // Parse Name { ... }
                self.advance();
                match self.advance() {
                    Some(Token::Identifier(_)) => {}
                    _ => {}
                } // Name
                if self.peek() == Some(&Token::LBrace) {
                    self.advance();
                    while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                        self.advance();
                    }
                    self.expect(Token::RBrace)?;
                } else if self.peek() == Some(&Token::Semicolon) {
                    self.advance(); // struct Foo;
                }
                Ok(Item::Dummy)
            }
            Some(Token::Const) | Some(Token::Type) => {
                self.advance();
                while self.peek() != Some(&Token::Semicolon) {
                    self.advance();
                }
                self.expect(Token::Semicolon)?;
                Ok(Item::Dummy)
            }

            Some(t) => Err(ParseError::UnexpectedToken(
                t.clone(),
                self.last_span.clone(),
            )),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_contract(&mut self) -> Result<Contract, ParseError> {
        self.advance(); // eat 'contract'

        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        self.expect(Token::LBrace)?;

        let mut members = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            // Handle storage block
            if self.peek() == Some(&Token::Storage) {
                self.advance(); // eat 'storage'
                self.expect(Token::LBrace)?;
                // Skip storage fields for MVP
                while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                    self.advance();
                }
                self.expect(Token::RBrace)?;
                continue;
            }

            // For MVP, simplified: assumes functions inside
            if let Some(Token::Fn) | Some(Token::Pub) | Some(Token::Init) = self.peek() {
                if self.peek() == Some(&Token::Init) {
                    self.advance(); // eat 'init'

                    self.expect(Token::LParen)?;
                    // Parse params (same as function)
                    let mut params = Vec::new();
                    if self.peek() != Some(&Token::RParen) {
                        loop {
                            let name = match self.advance() {
                                Some(Token::Identifier(s)) => s,
                                _ => break,
                            };
                            self.expect(Token::Colon)?;
                            let ty = match self.advance() {
                                Some(Token::Identifier(s)) => Type::Path(s),
                                Some(Token::U64) => Type::Path("u64".into()),
                                _ => Type::Path("unknown".into()),
                            };
                            params.push(Param { name, ty });
                            if self.peek() == Some(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;

                    let body = self.parse_block()?;
                    members.push(ContractMember::Init(Function {
                        name: "init".to_string(),
                        params,
                        return_type: None,
                        body,
                        is_pub: false,
                    }));
                } else {
                    members.push(ContractMember::Function(self.parse_function()?));
                }
            } else {
                // Skip unknown tokens to prevent infinite loop in dev
                self.advance();
            }
        }

        self.expect(Token::RBrace)?;
        Ok(Contract { name, members })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        let is_pub = if self.peek() == Some(&Token::Pub) {
            self.advance();
            true
        } else {
            false
        };

        self.expect(Token::Fn)?;

        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        self.expect(Token::LParen)?;
        // Parse params
        let mut params = Vec::new();
        if self.peek() != Some(&Token::RParen) {
            loop {
                let name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                    None => return Err(ParseError::UnexpectedEOF),
                };

                self.expect(Token::Colon)?;

                let ty = match self.advance() {
                    Some(Token::Identifier(s)) => Type::Path(s),
                    Some(Token::U64) => Type::Path("u64".into()),
                    Some(Token::Address) => Type::Path("Address".into()),
                    Some(Token::Bool) => Type::Path("bool".into()),
                    Some(Token::StringType) => Type::Path("String".into()),
                    Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                    None => return Err(ParseError::UnexpectedEOF),
                };

                params.push(Param { name, ty });

                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;

        // Return type?
        let return_type = if self.peek() == Some(&Token::Arrow) {
            self.advance();
            let ty = match self.advance() {
                Some(Token::Identifier(s)) => Type::Path(s),
                Some(Token::U64) => Type::Path("u64".into()),
                Some(Token::Address) => Type::Path("Address".into()),
                Some(Token::Bool) => Type::Path("bool".into()),
                Some(Token::StringType) => Type::Path("String".into()),
                _ => Type::Path("unknown".into()),
            };

            // Skip generic parameters if present
            if self.peek() == Some(&Token::Lt) {
                self.advance();
                let mut depth = 1;
                while depth > 0 && self.peek().is_some() {
                    match self.peek() {
                        Some(Token::Lt) => depth += 1,
                        Some(Token::Gt) => depth -= 1,
                        _ => {}
                    }
                    self.advance();
                }
            }

            Some(ty)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Function {
            name,
            params,
            return_type,
            body,
            is_pub,
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            stmts.push(self.parse_statement()?);
        }
        self.expect(Token::RBrace)?;
        Ok(Block { stmts })
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            Some(Token::Let) => {
                self.advance(); // eat let
                let is_mut = if self.peek() == Some(&Token::Mut) {
                    self.advance();
                    true
                } else {
                    false
                };
                let name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    _ => return Err(ParseError::UnexpectedEOF), // Simplified
                };

                let mut destruct_names = Vec::new();
                // Destructuring pattern if present (e.g. let Token { amount } = ...)
                if self.peek() == Some(&Token::LBrace) {
                    self.advance();
                    while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                        if let Some(Token::Identifier(s)) = self.peek() {
                            destruct_names.push(s.clone());
                        }
                        self.advance();
                    }
                    self.expect(Token::RBrace)?;
                }

                // Type annotation?
                let ty = if self.peek() == Some(&Token::Colon) {
                    self.advance();
                    Some(match self.advance() {
                        Some(Token::Identifier(s)) => Type::Path(s),
                        Some(Token::U64) => Type::Path("u64".into()),
                        Some(Token::Address) => Type::Path("Address".into()),
                        Some(Token::Bool) => Type::Path("bool".into()),
                        Some(Token::StringType) => Type::Path("String".into()),
                        Some(t) => {
                            return Err(ParseError::UnexpectedToken(t, self.last_span.clone()))
                        }
                        None => return Err(ParseError::UnexpectedEOF),
                    })
                } else {
                    None
                };

                self.expect(Token::Eq)?;
                let init = self.parse_expression(0, true)?;
                self.expect(Token::Semicolon)?;

                Ok(Statement::Let {
                    name,
                    destruct_names,
                    ty,
                    init,
                    is_mut,
                })
            }
            Some(Token::Return) => {
                self.advance();
                let expr = if self.peek() != Some(&Token::Semicolon) {
                    Some(self.parse_expression(0, true)?)
                } else {
                    None
                };
                self.expect(Token::Semicolon)?;
                Ok(Statement::Return(expr))
            }
            Some(Token::If) => {
                self.advance(); // eat 'if'
                let _cond = self.parse_expression(0, false)?; // Parse condition (no struct init)
                let _then_block = self.parse_block()?;

                // Optional else
                if self.peek() == Some(&Token::Else) {
                    self.advance();
                    let _else_block = self.parse_block()?;
                }

                // For MVP, return dummy expression statement
                Ok(Statement::Expr(Expression::Literal(Literal::Bool(true))))
            }
            Some(Token::Emit) => {
                self.advance(); // eat 'emit'
                let _expr = self.parse_expression(0, true)?;
                if self.peek() == Some(&Token::Semicolon) {
                    self.advance();
                }
                Ok(Statement::Expr(Expression::Literal(Literal::Bool(true))))
            }
            Some(Token::While) => {
                self.advance(); // eat 'while'
                let _cond = self.parse_expression(0, false)?;
                let _body = self.parse_block()?;
                Ok(Statement::Expr(Expression::Literal(Literal::Bool(true))))
            }
            Some(Token::For) => {
                self.advance(); // eat 'for'
                let _binding = self.advance(); // name
                if self.peek() == Some(&Token::In) {
                    self.advance();
                }
                let _expr = self.parse_expression(0, false)?;
                let _body = self.parse_block()?;
                Ok(Statement::Expr(Expression::Literal(Literal::Bool(true))))
            }
            Some(Token::Break) | Some(Token::Continue) => {
                self.advance();
                if self.peek() == Some(&Token::Semicolon) {
                    self.advance();
                }
                Ok(Statement::Expr(Expression::Literal(Literal::Bool(true))))
            }
            _ => {
                // Try to parse as assignment or expression
                let expr = self.parse_expression(0, true)?;

                // Check for assignment operators
                if matches!(
                    self.peek(),
                    Some(Token::Eq)
                        | Some(Token::PlusEq)
                        | Some(Token::MinusEq)
                        | Some(Token::StarEq)
                        | Some(Token::SlashEq)
                ) {
                    self.advance(); // eat assignment op
                    let rhs = self.parse_expression(0, true)?;
                    if self.peek() == Some(&Token::Semicolon) {
                        self.advance();
                    }
                    // For MVP, treat as expression statement (assignment is expression in AST)
                    Ok(Statement::Expr(Expression::Binary {
                        left: Box::new(expr),
                        op: BinaryOp::Add, // Placeholder
                        right: Box::new(rhs),
                    }))
                } else {
                    if self.peek() == Some(&Token::Semicolon) {
                        self.advance();
                    }
                    Ok(Statement::Expr(expr))
                }
            }
        }
    }

    // Pratt Parser
    fn parse_expression(
        &mut self,
        min_bp: u8,
        allow_struct_init: bool,
    ) -> Result<Expression, ParseError> {
        let mut lhs = match self.advance() {
            Some(Token::Integer(i)) => Expression::Literal(Literal::Int(i)),
            Some(Token::StringLit(s)) => Expression::Literal(Literal::String(s)),
            Some(Token::True) => Expression::Literal(Literal::Bool(true)),
            Some(Token::False) => Expression::Literal(Literal::Bool(false)),
            Some(Token::Identifier(s)) => {
                if allow_struct_init && self.peek() == Some(&Token::LBrace) {
                    self.advance(); // eat '{'
                                    // Parse struct fields
                    while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                        self.advance();
                    }
                    self.expect(Token::RBrace)?;
                    Expression::Literal(Literal::Bool(true)) // Dummy
                } else {
                    Expression::Identifier(s)
                }
            }
            Some(Token::SelfKw) => Expression::Identifier("self".to_string()),
            Some(Token::LParen) => {
                if self.peek() == Some(&Token::RParen) {
                    self.advance();
                    Expression::Literal(Literal::Unit)
                } else {
                    let expr = self.parse_expression(0, true)?; // Inside parens, struct init allowed
                    self.expect(Token::RParen)?;
                    expr
                }
            }
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        loop {
            // Handle postfix operators first
            match self.peek() {
                Some(Token::Dot) => {
                    self.advance();
                    let field = match self.advance() {
                        Some(Token::Identifier(s)) => s,
                        _ => return Err(ParseError::UnexpectedEOF),
                    };
                    lhs = Expression::FieldAccess {
                        expr: Box::new(lhs),
                        field,
                    };
                    continue;
                }
                Some(Token::LBracket) => {
                    self.advance();
                    let index = self.parse_expression(0, true)?;
                    self.expect(Token::RBracket)?;
                    lhs = Expression::Index {
                        expr: Box::new(lhs),
                        index: Box::new(index),
                    };
                    continue;
                }
                Some(Token::LParen) => {
                    // Function call
                    self.advance();
                    let mut args = Vec::new();
                    if self.peek() != Some(&Token::RParen) {
                        loop {
                            args.push(self.parse_expression(0, true)?);
                            if self.peek() == Some(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;
                    lhs = Expression::Call {
                        func: Box::new(lhs),
                        args,
                    };
                    continue;
                }
                _ => {}
            }

            // Handle binary operators
            let op = match self.peek() {
                Some(Token::Plus) => BinaryOp::Add,
                Some(Token::Minus) => BinaryOp::Sub,
                Some(Token::Star) => BinaryOp::Mul,
                Some(Token::Slash) => BinaryOp::Div,
                Some(Token::EqEq) => BinaryOp::Eq,
                Some(Token::NotEq) => BinaryOp::Ne,
                Some(Token::Lt) => BinaryOp::Lt,
                Some(Token::Gt) => BinaryOp::Gt,
                Some(Token::LtEq) => BinaryOp::Le,
                Some(Token::GtEq) => BinaryOp::Ge,
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.advance(); // eat op
            let rhs = self.parse_expression(r_bp, allow_struct_init)?;

            lhs = Expression::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: &BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Gt => (10, 20), // Comparison
        BinaryOp::Add | BinaryOp::Sub => (30, 40),
        BinaryOp::Mul | BinaryOp::Div => (50, 60),
        _ => (0, 0),
    }
}
