use crate::ast::*;
use crate::token::Token;
use logos::Span;
use std::iter::Peekable;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token: {0:?} at {1:?}")]
    UnexpectedToken(Token, std::ops::Range<usize>),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Custom error: {0}")]
    Custom(String),
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
        let is_pub = if self.peek() == Some(&Token::Pub) {
            self.advance();
            true
        } else {
            false
        };

        match self.peek() {
            Some(Token::Contract) => Ok(Item::Contract(self.parse_contract()?)),
            Some(Token::Fn) => Ok(Item::Function(self.parse_function(is_pub)?)),
            Some(Token::Use) => {
                if is_pub {
                    return Err(ParseError::Custom("use declaration cannot be pub".into()));
                }
                Ok(Item::Use(self.parse_use_declaration()?))
            }
            Some(Token::Struct) => Ok(Item::Struct(self.parse_struct(is_pub)?)),
            Some(Token::Enum) => Ok(Item::Enum(self.parse_enum(is_pub)?)),
            Some(Token::Resource) => {
                self.advance(); // eat 'resource'
                let name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                    None => return Err(ParseError::UnexpectedEOF),
                };

                self.expect(Token::LBrace)?;
                let mut fields = Vec::new();
                while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                    let field_name = match self.advance() {
                        Some(Token::Identifier(s)) => s,
                        Some(t) => {
                            return Err(ParseError::UnexpectedToken(t, self.last_span.clone()))
                        }
                        None => return Err(ParseError::UnexpectedEOF),
                    };
                    self.expect(Token::Colon)?;
                    let ty = self.parse_type()?;
                    fields.push(Field {
                        name: field_name,
                        ty,
                    });
                    if self.peek() == Some(&Token::Comma) {
                        self.advance();
                    }
                }
                self.expect(Token::RBrace)?;
                Ok(Item::Resource(Struct {
                    name,
                    fields,
                    generics: vec![],
                    is_pub,
                }))
            }
            Some(Token::Const) | Some(Token::Type) => {
                self.advance();
                while self.peek() != Some(&Token::Semicolon) {
                    self.advance();
                }
                self.expect(Token::Semicolon)?;
                Ok(Item::Dummy)
            }
            Some(Token::Trait) => Ok(Item::Trait(self.parse_trait(is_pub)?)),
            Some(Token::Impl) => {
                if is_pub {
                    return Err(ParseError::Custom("impl block cannot be pub".into()));
                }
                Ok(Item::Impl(self.parse_impl()?))
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
                let mut storage_fields = Vec::new();
                while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                    if let Some(Token::Identifier(field_name)) = self.advance() {
                        self.expect(Token::Colon)?;
                        let ty = self.parse_type()?;
                        self.expect(Token::Semicolon)?;
                        storage_fields.push(Field {
                            name: field_name,
                            ty,
                        });
                    }
                }
                self.expect(Token::RBrace)?;
                members.push(ContractMember::Storage(storage_fields));
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
                        while let Some(Token::Identifier(name)) = self.advance() {
                            self.expect(Token::Colon)?;
                            let ty = self.parse_type()?;
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
                        generics: vec![],
                    }));
                } else {
                    let is_pub = if self.peek() == Some(&Token::Pub) {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    members.push(ContractMember::Function(self.parse_function(is_pub)?));
                }
            } else {
                // Skip unknown tokens to prevent infinite loop in dev
                self.advance();
            }
        }

        self.expect(Token::RBrace)?;
        Ok(Contract { name, members })
    }

    fn parse_use_declaration(&mut self) -> Result<UseDeclaration, ParseError> {
        self.expect(Token::Use)?;

        // Parse path: std::collections::Map
        let mut path = vec![];
        loop {
            match self.advance() {
                Some(Token::Identifier(s)) => path.push(s),
                Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                None => return Err(ParseError::UnexpectedEOF),
            }

            if self.peek() != Some(&Token::DoubleColon) {
                break;
            }
            self.advance(); // consume ::
        }

        if path.is_empty() {
            return Err(ParseError::Custom("Empty use path".to_string()));
        }

        // Handle {Map, Vec} or 'as Alias'
        let (items, alias) = if self.peek() == Some(&Token::LBrace) {
            // use std::{Map, Vec}
            self.advance(); // consume {
            let mut items = vec![];
            while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                if let Some(Token::Identifier(s)) = self.advance() {
                    items.push(s);
                }
                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::RBrace)?;
            (Some(items), None)
        } else if self.peek() == Some(&Token::As) {
            // use std::Map as M
            self.advance(); // consume 'as'
            let alias = match self.advance() {
                Some(Token::Identifier(s)) => Some(s),
                Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                None => return Err(ParseError::UnexpectedEOF),
            };
            (None, alias)
        } else {
            (None, None)
        };

        self.expect(Token::Semicolon)?;
        Ok(UseDeclaration { path, items, alias })
    }

    fn parse_struct(&mut self, is_pub: bool) -> Result<Struct, ParseError> {
        self.advance(); // eat 'struct'
        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        let mut generics = Vec::new();
        if self.peek() == Some(&Token::Lt) {
            self.advance(); // eat '<'
            while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                if let Some(Token::Identifier(sub_s)) = self.advance() {
                    generics.push(sub_s);
                }
                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::Gt)?;
        }

        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            let field_name = match self.advance() {
                Some(Token::Identifier(s)) => s,
                Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                None => return Err(ParseError::UnexpectedEOF),
            };
            self.expect(Token::Colon)?;
            let ty = self.parse_type()?;
            fields.push(Field {
                name: field_name,
                ty,
            });
            if self.peek() == Some(&Token::Comma) {
                self.advance();
            }
        }
        self.expect(Token::RBrace)?;

        Ok(Struct {
            name,
            fields,
            generics,
            is_pub,
        })
    }

    fn parse_enum(&mut self, is_pub: bool) -> Result<Enum, ParseError> {
        self.advance(); // eat 'enum'
        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        self.expect(Token::LBrace)?;
        let mut variants = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            let variant_name = match self.advance() {
                Some(Token::Identifier(s)) => s,
                Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                None => return Err(ParseError::UnexpectedEOF),
            };
            variants.push(variant_name);
            if self.peek() == Some(&Token::Comma) {
                self.advance();
            }
        }
        self.expect(Token::RBrace)?;
        Ok(Enum {
            name,
            variants,
            is_pub,
        })
    }

    fn parse_trait(&mut self, is_pub: bool) -> Result<Trait, ParseError> {
        self.expect(Token::Trait)?;
        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };
        self.expect(Token::LBrace)?;
        let mut methods = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            methods.push(self.parse_function_signature()?);
            self.expect(Token::Semicolon)?;
        }
        self.expect(Token::RBrace)?;
        Ok(Trait {
            name,
            methods,
            is_pub,
        })
    }

    fn parse_impl(&mut self) -> Result<Impl, ParseError> {
        self.expect(Token::Impl)?;

        let mut generics = Vec::new();
        if self.peek() == Some(&Token::Lt) {
            self.advance(); // eat '<'
            while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                if let Some(Token::Identifier(sub_s)) = self.advance() {
                    generics.push(sub_s);
                }
                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::Gt)?;
        }

        let first_name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        // Optional generic args for first name: identifier<T>
        if self.peek() == Some(&Token::Lt) {
            self.advance(); // eat '<'
            while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                self.parse_type()?; // parse and discard
                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::Gt)?;
        }

        let (trait_name, target_name) = if self.peek() == Some(&Token::For) {
            self.advance(); // consume 'for'
            let target = match self.advance() {
                Some(Token::Identifier(s)) => s,
                Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                None => return Err(ParseError::UnexpectedEOF),
            };
            if self.peek() == Some(&Token::Lt) {
                self.advance(); // eat '<'
                while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                    self.parse_type()?; // parse and discard
                    if self.peek() == Some(&Token::Comma) {
                        self.advance();
                    }
                }
                self.expect(Token::Gt)?;
            }
            (Some(first_name), target)
        } else {
            (None, first_name)
        };

        self.expect(Token::LBrace)?;
        let mut methods = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            let is_pub = if self.peek() == Some(&Token::Pub) {
                self.advance();
                true
            } else {
                false
            };
            methods.push(self.parse_function(is_pub)?);
        }
        self.expect(Token::RBrace)?;

        Ok(Impl {
            trait_name,
            target_name,
            methods,
            generics,
        })
    }

    fn parse_function_signature(&mut self) -> Result<FunctionSignature, ParseError> {
        // This is shared logic for fn signature (trait methods)
        self.expect(Token::Fn)?;

        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        };

        // Parse generics <T, U>
        let mut generics = Vec::new();
        if self.peek() == Some(&Token::Lt) {
            self.advance();
            while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                let g_name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            self.peek().cloned().unwrap_or(Token::Error),
                            self.last_span.clone(),
                        ))
                    }
                };
                generics.push(g_name);
                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::Gt)?;
        }

        self.expect(Token::LParen)?;
        // Parse params
        let mut params = Vec::new();
        if self.peek() != Some(&Token::RParen) {
            loop {
                let p_name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                    None => return Err(ParseError::UnexpectedEOF),
                };

                self.expect(Token::Colon)?;

                let ty = self.parse_type()?;

                params.push(Param { name: p_name, ty });

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
            let ty = self.parse_type()?;
            Some(ty)
        } else {
            None
        };

        Ok(FunctionSignature {
            name,
            params,
            return_type,
            generics,
        })
    }

    fn parse_function(&mut self, is_pub: bool) -> Result<Function, ParseError> {
        let sig = self.parse_function_signature()?;
        let body = self.parse_block()?;

        Ok(Function {
            name: sig.name,
            params: sig.params,
            return_type: sig.return_type,
            body,
            is_pub,
            generics: sig.generics,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.advance() {
            Some(Token::Identifier(s)) => {
                if self.peek() == Some(&Token::Lt) {
                    self.advance(); // eat '<'
                    let mut type_args = Vec::new();
                    while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                        type_args.push(self.parse_type()?);
                        if self.peek() == Some(&Token::Comma) {
                            self.advance();
                        }
                    }
                    self.expect(Token::Gt)?;

                    if s == "Map" && type_args.len() == 2 {
                        Ok(Type::Map(
                            Box::new(type_args[0].clone()),
                            Box::new(type_args[1].clone()),
                        ))
                    } else {
                        Ok(Type::Generic(s, type_args))
                    }
                } else {
                    Ok(Type::Path(s))
                }
            }
            Some(Token::U64) => Ok(Type::Path("u64".into())),
            Some(Token::Address) => Ok(Type::Path("Address".into())),
            Some(Token::Bool) => Ok(Type::Path("bool".into())),
            Some(Token::StringType) => Ok(Type::Path("String".into())),
            Some(Token::LParen) => {
                if self.peek() == Some(&Token::RParen) {
                    self.advance();
                    Ok(Type::Path("Unit".into()))
                } else {
                    Err(ParseError::UnexpectedToken(
                        Token::LParen,
                        self.last_span.clone(),
                    ))
                }
            }
            Some(t) => Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
            None => Err(ParseError::UnexpectedEOF),
        }
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
                let condition = self.parse_expression(0, false)?;
                let then_branch = self.parse_block()?;

                let mut else_branch = None;
                if self.peek() == Some(&Token::Else) {
                    self.advance(); // eat 'else'
                                    // Support 'else if' or just 'else { ... }'
                    if self.peek() == Some(&Token::If) {
                        else_branch = Some(Block {
                            stmts: vec![self.parse_statement()?],
                        });
                    } else {
                        else_branch = Some(self.parse_block()?);
                    }
                }

                Ok(Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                })
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
                let condition = self.parse_expression(0, false)?;
                let body = self.parse_block()?;
                Ok(Statement::While { condition, body })
            }
            Some(Token::For) => {
                self.advance(); // eat 'for'
                let var_name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                    None => return Err(ParseError::UnexpectedEOF),
                };

                self.expect(Token::In)?;

                let start = self.parse_expression(0, false)?;
                self.expect(Token::Range)?; // For now only inclusive range ..
                let end = self.parse_expression(0, false)?;

                let body = self.parse_block()?;
                Ok(Statement::For {
                    var_name,
                    start,
                    end,
                    body,
                })
            }
            Some(Token::Match) => Ok(Statement::Expr(self.parse_match_expression()?)),
            Some(Token::Break) => {
                self.advance();
                if self.peek() == Some(&Token::Semicolon) {
                    self.advance();
                }
                Ok(Statement::Break)
            }
            Some(Token::Continue) => {
                self.advance();
                if self.peek() == Some(&Token::Semicolon) {
                    self.advance();
                }
                Ok(Statement::Continue)
            }
            _ => {
                // Try to parse as assignment or expression
                let expr = self.parse_expression(0, true)?;

                // Check for assignment operators
                let op_token = self.peek().cloned();
                if let Some(t) = op_token {
                    match t {
                        Token::Eq
                        | Token::PlusEq
                        | Token::MinusEq
                        | Token::StarEq
                        | Token::SlashEq => {
                            self.advance(); // eat assignment op
                            let mut rhs = self.parse_expression(0, true)?;

                            // Desugar: a += b  =>  a = a + b
                            match t {
                                Token::PlusEq => {
                                    rhs = Expression::Binary {
                                        left: Box::new(expr.clone()),
                                        op: BinaryOp::Add,
                                        right: Box::new(rhs),
                                    };
                                }
                                Token::MinusEq => {
                                    rhs = Expression::Binary {
                                        left: Box::new(expr.clone()),
                                        op: BinaryOp::Sub,
                                        right: Box::new(rhs),
                                    };
                                }
                                Token::StarEq => {
                                    rhs = Expression::Binary {
                                        left: Box::new(expr.clone()),
                                        op: BinaryOp::Mul,
                                        right: Box::new(rhs),
                                    };
                                }
                                Token::SlashEq => {
                                    rhs = Expression::Binary {
                                        left: Box::new(expr.clone()),
                                        op: BinaryOp::Div,
                                        right: Box::new(rhs),
                                    };
                                }
                                _ => {}
                            }

                            if self.peek() == Some(&Token::Semicolon) {
                                self.advance();
                            }
                            Ok(Statement::Expr(Expression::Binary {
                                left: Box::new(expr),
                                op: BinaryOp::Assign,
                                right: Box::new(rhs),
                            }))
                        }
                        _ => {
                            if self.peek() == Some(&Token::Semicolon) {
                                self.advance();
                            }
                            Ok(Statement::Expr(expr))
                        }
                    }
                } else {
                    Ok(Statement::Expr(expr))
                }
            }
        }
    }

    fn parse_match_expression(&mut self) -> Result<Expression, ParseError> {
        let value = self.parse_expression(0, false)?;
        self.expect(Token::LBrace)?;
        let mut arms = Vec::new();
        while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
            // Parse Pattern
            let pattern = if self.peek() == Some(&Token::Identifier("_".into())) {
                self.advance();
                Pattern::Wildcard
            } else if let Some(Token::Identifier(s)) = self.peek().cloned() {
                self.advance();
                // Check for ::
                if self.peek() == Some(&Token::DoubleColon) {
                    self.advance();
                    let variant = match self.advance() {
                        Some(Token::Identifier(v)) => v,
                        _ => {
                            return Err(ParseError::UnexpectedToken(
                                self.peek().cloned().unwrap_or(Token::Error),
                                self.last_span.clone(),
                            ))
                        }
                    };
                    Pattern::EnumVariant {
                        enum_name: s,
                        variant_name: variant,
                    }
                } else if s == "_" {
                    Pattern::Wildcard
                } else {
                    return Err(ParseError::Custom(
                        "Only Enum::Variant or _ patterns supported for now".into(),
                    ));
                }
            } else {
                return Err(ParseError::Custom(
                    "Invalid pattern, expected enum variant or _".into(),
                ));
            };

            self.expect(Token::FatArrow)?;
            let body = self.parse_expression(0, false)?;
            arms.push(MatchArm { pattern, body });

            if self.peek() == Some(&Token::Comma) {
                self.advance();
            }
        }
        self.expect(Token::RBrace)?;
        Ok(Expression::Match {
            value: Box::new(value),
            arms,
        })
    }

    fn parse_closure(&mut self) -> Result<Expression, ParseError> {
        // We already ate one '|' (via parse_expression)
        let mut params = Vec::new();
        if self.peek() != Some(&Token::Pipe) {
            loop {
                let name = match self.advance() {
                    Some(Token::Identifier(s)) => s,
                    Some(t) => return Err(ParseError::UnexpectedToken(t, self.last_span.clone())),
                    None => return Err(ParseError::UnexpectedEOF),
                };

                let ty = if self.peek() == Some(&Token::Colon) {
                    self.advance();
                    match self.advance() {
                        Some(Token::Identifier(s)) => Type::Path(s),
                        Some(Token::U64) => Type::Path("u64".into()),
                        Some(Token::Address) => Type::Path("Address".into()),
                        _ => Type::Path("unknown".into()),
                    }
                } else {
                    Type::Path("unknown".into())
                };

                params.push(Param { name, ty });

                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::Pipe)?;

        let body = self.parse_block()?;
        Ok(Expression::Closure {
            params,
            body: Box::new(body),
        })
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
                let mut expr = Expression::Identifier(s.clone());

                if self.peek() == Some(&Token::DoubleColon) {
                    self.advance(); // eat ::
                    if self.peek() == Some(&Token::Lt) {
                        self.advance(); // eat <
                        let mut type_args = Vec::new();
                        while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                            type_args.push(self.parse_type()?);
                            if self.peek() == Some(&Token::Comma) {
                                self.advance();
                            }
                        }
                        self.expect(Token::Gt)?;
                        expr = Expression::GenericInst {
                            target: Box::new(expr),
                            type_args,
                        };
                    } else {
                        let variant = match self.advance() {
                            Some(Token::Identifier(v)) => v,
                            t => {
                                return Err(ParseError::UnexpectedToken(
                                    t.unwrap_or(Token::Error),
                                    self.last_span.clone(),
                                ))
                            }
                        };
                        expr = Expression::EnumVariant {
                            enum_name: s,
                            variant_name: variant,
                        };
                    }
                }

                if allow_struct_init && self.peek() == Some(&Token::LBrace) {
                    self.advance(); // eat '{'
                    let mut fields = Vec::new();
                    while self.peek() != Some(&Token::RBrace) && self.peek().is_some() {
                        let field_name = match self.advance() {
                            Some(Token::Identifier(fn_name)) => fn_name,
                            Some(t) => {
                                return Err(ParseError::UnexpectedToken(t, self.last_span.clone()))
                            }
                            None => return Err(ParseError::UnexpectedEOF),
                        };
                        self.expect(Token::Colon)?;
                        let value = self.parse_expression(0, true)?;
                        fields.push((field_name, value));
                        if self.peek() == Some(&Token::Comma) {
                            self.advance();
                        }
                    }
                    self.expect(Token::RBrace)?;

                    match expr {
                        Expression::GenericInst { target, type_args } => {
                            if let Expression::Identifier(name) = *target {
                                Expression::StructInit {
                                    name,
                                    type_args,
                                    fields,
                                }
                            } else {
                                return Err(ParseError::UnexpectedToken(
                                    Token::LBrace,
                                    self.last_span.clone(),
                                ));
                            }
                        }
                        Expression::Identifier(name) => Expression::StructInit {
                            name,
                            type_args: vec![],
                            fields,
                        },
                        _ => {
                            return Err(ParseError::UnexpectedToken(
                                Token::LBrace,
                                self.last_span.clone(),
                            ))
                        }
                    }
                } else {
                    expr
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
            Some(Token::Match) => self.parse_match_expression()?,
            Some(Token::Pipe) => self.parse_closure()?,
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
                Some(Token::QuestionMark) => {
                    self.advance();
                    lhs = Expression::Try(Box::new(lhs));
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
                Some(Token::DoubleColon) => {
                    self.advance();
                    if self.peek() == Some(&Token::Lt) {
                        self.advance(); // eat <
                        let mut type_args = Vec::new();
                        while self.peek() != Some(&Token::Gt) && self.peek().is_some() {
                            type_args.push(self.parse_type()?);
                            if self.peek() == Some(&Token::Comma) {
                                self.advance();
                            }
                        }
                        self.expect(Token::Gt)?;
                        lhs = Expression::GenericInst {
                            target: Box::new(lhs),
                            type_args,
                        };
                    } else {
                        let field = match self.advance() {
                            Some(Token::Identifier(s)) => s,
                            _ => return Err(ParseError::UnexpectedEOF),
                        };
                        lhs = Expression::FieldAccess {
                            expr: Box::new(lhs),
                            field,
                        };
                    }
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

                    let (func, type_args) = match lhs {
                        Expression::GenericInst { target, type_args } => (target, type_args),
                        _ => (Box::new(lhs), vec![]),
                    };

                    lhs = Expression::Call {
                        func,
                        args,
                        type_args,
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
