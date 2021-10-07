use std::iter::Peekable;
use crate::ast::{ASTExpr, ASTFnDef, ASTStmt, ASTStmtBlock, ASTStmtKind, ASTIf, ASTWhile, ASTFor};
use crate::lexer::{Token, TokenKind};
use crate::report::{ExpectedTokenKinds, ParseError, ParseErrorKind};
use crate::span::{BytePos, Symbol, SessionGlobals};

pub mod parse_expr;

pub type PResult<T> = Result<T, ParseError>;

pub struct Parser<T: Iterator<Item = Token>> {
    pub tokens: Peekable<T>,
    pub expected_tokens: Vec<TokenKind>
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(iterator: T) -> Self {
        Self {
            tokens: iterator.peekable(),
            expected_tokens: vec![]
        }
    }

    fn peek(&mut self) -> &Token {
        if let Some(token) = self.tokens.peek() {
            token
        }
        else {
            &Token {
                kind: TokenKind::PostEof,
                span: (BytePos(0), BytePos(0))
            }
        }
    }

    fn err_unexpected(&mut self) -> ParseError {
        let token_kinds = self.expected_tokens.clone();
        ParseError {
            kind: ParseErrorKind::UnexpectedToken {
                got_token: self.bump(),
                expected: ExpectedTokenKinds {
                    token_kinds,
                    // TODO
                    explanation: None
                }
            }
        }
    }

    fn expect(&mut self, token_kind: TokenKind) -> PResult<Token> {
        if self.check(token_kind) {
            Ok(self.bump())
        }
        else {
            Err(self.err_unexpected())
        }
    }

    fn check(&mut self, token_kind: TokenKind) -> bool {
        let is_present = self.peek().kind == token_kind;

        if !is_present {
            self.expected_tokens.push(token_kind);
        }

        is_present
    }

    fn bump(&mut self) -> Token {
        self.expected_tokens.clear();
        if let Some(token) = self.tokens.next() {
            token
        }
        else {
            Token {
                kind: TokenKind::PostEof,
                span: (BytePos(0), BytePos(0))
            }
        }
    }

    fn span_start(&mut self) -> BytePos {
        self.peek().span.0
    }

    fn mk_span(&mut self, start: BytePos) -> (BytePos, BytePos) {
        (
            start,
            self.peek().span.1
        )
    }

    fn get_span_symbol(&mut self, span: (BytePos, BytePos)) -> Symbol {
        SessionGlobals::with(|session_globals| {
            let string = &session_globals.src[span.0.0..span.1.0];
            let mut interner = session_globals.interner.write().unwrap();
            interner.intern(string)
        })
    }

    fn parse_fn_params(&mut self) -> PResult<Vec<(Symbol, ASTExpr)>> {
        let mut result = vec![];

        let name = self.expect(TokenKind::Ident)?.span;
        let name = self.get_span_symbol(name);
        self.expect(TokenKind::Colon)?;
        let param_type = self.parse_expr()?;
        result.push((name, param_type));

        while self.check(TokenKind::Comma) {
            self.bump();

            // TODO : rewrite to avoid repetition
            let name = self.expect(TokenKind::Ident)?.span;
            let name = self.get_span_symbol(name);
            self.expect(TokenKind::Colon)?;
            let param_type = self.parse_expr()?;
            result.push((name, param_type));
        }

        Ok(result)
    }

    fn parse_statement_def(&mut self) -> PResult<ASTStmt> {
        let start = self.span_start();
        self.expect(TokenKind::KwDef)?;
        let name = self.expect(TokenKind::Ident)?.span;
        let name = self.get_span_symbol(name);
        self.expect(TokenKind::LParen)?;
        let params = if !self.check(TokenKind::RParent) {
            self.parse_fn_params()?
        } else {
            vec![]
        };
        self.expect(TokenKind::RParent)?;
        self.expect(TokenKind::Arrow)?;
        let return_type = self.parse_expr()?;
        self.expect(TokenKind::Colon)?;
        let block = self.parse_statement_block()?;

        Ok(ASTStmt {
            kind: ASTStmtKind::FnDef(ASTFnDef {
                params,
                name,
                return_type,
                block
            }),
            span: self.mk_span(start)
        })
    }

    fn parse_statement(&mut self) -> PResult<ASTStmt> {
        let start = self.span_start();
        if self.check(TokenKind::KwDef) {
            return self.parse_statement_def();
        }
        if self.check(TokenKind::KwAssert) {
            self.bump();
            let expr = self.parse_expr()?;
            return Ok(ASTStmt {
                kind: ASTStmtKind::Assert(expr),
                span: self.mk_span(start)
            });
        }
        if self.check(TokenKind::KwIf) {
            self.bump();
            let mut conditions_blocks = vec![];

            let expr = self.parse_expr()?;
            self.expect(TokenKind::Colon)?;
            let block = self.parse_statement_block()?;
            conditions_blocks.push((expr, block));

            while self.check(TokenKind::KwElif) {
                self.bump();

                let expr = self.parse_expr()?;
                self.expect(TokenKind::Colon)?;
                let block = self.parse_statement_block()?;
                conditions_blocks.push((expr, block));
            }

            let else_block = if self.check(TokenKind::KwElse) {
                self.bump();
                self.expect(TokenKind::Colon)?;

                let block = self.parse_statement_block()?;
                Some(block)
            }
            else {
                None
            };

            return Ok(ASTStmt {
                kind: ASTStmtKind::If(ASTIf {
                    conditions_blocks,
                    else_block
                }),
                span: self.mk_span(start)
            });
        }
        if self.check(TokenKind::KwWhile) {
            self.bump();
            let condition = self.parse_expr()?;
            self.expect(TokenKind::Colon)?;
            let block = self.parse_statement_block()?;
            return Ok(ASTStmt {
                kind: ASTStmtKind::While(ASTWhile {
                    condition,
                    block
                }),
                span: self.mk_span(start)
            })
        }
        if self.check(TokenKind::KwFor) {
            self.bump();
            let ident = self.expect(TokenKind::Ident)?.span;
            let ident = self.get_span_symbol(ident);
            self.expect(TokenKind::KwIn)?;
            let iter = self.parse_expr()?;
            self.expect(TokenKind::Colon)?;
            let block = self.parse_statement_block()?;
            return Ok(ASTStmt {
                kind: ASTStmtKind::For(ASTFor {
                    ident,
                    iter,
                    block
                }),
                span: self.mk_span(start)
            })
        }
        if self.check(TokenKind::KwBreak) {
            self.bump();
            return Ok(ASTStmt {
                kind: ASTStmtKind::Break,
                span: self.mk_span(start)
            });
        }
        if self.check(TokenKind::KwContinue) {
            self.bump();
            return Ok(ASTStmt {
                kind: ASTStmtKind::Continue,
                span: self.mk_span(start)
            });
        }
        if self.check(TokenKind::KwReturn) {
            self.bump();
            let expr = self.parse_expr()?;
            return Ok(ASTStmt {
                kind: ASTStmtKind::Return(expr),
                span: self.mk_span(start)
            });
        }
        let expr = self.parse_expr()?;
        Ok(ASTStmt {
            kind: ASTStmtKind::Expr(expr),
            span: self.mk_span(start)
        })
    }

    fn parse_statement_block(&mut self) -> PResult<ASTStmtBlock> {
        let start = self.span_start();
        self.expect(TokenKind::BlockStart)?;
        let mut stmts = vec![];
        while !self.check(TokenKind::BlockEnd) {
            stmts.push(self.parse_statement()?);
        }
        self.bump();
        Ok(ASTStmtBlock {
            stmts,
            span: self.mk_span(start)
        })
    }

    pub fn parse_root(&mut self) -> PResult<ASTStmtBlock> {
        self.parse_statement_block()
    }
}