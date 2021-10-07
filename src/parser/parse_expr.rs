use crate::ast::{ASTExpr, ASTExprKind, ASTOperator};
use crate::lexer::{Token, TokenKind};
use crate::parser::{Parser, PResult};

// TODO : Potentially refactor this so precedence is passed as a parameter to functions
impl<T: Iterator<Item = Token>> Parser<T> {
    pub(in super) fn parse_expr_primary(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        if self.check(TokenKind::Ident) |
            self.check(TokenKind::IntegerLiteral) {
            let token = self.bump();
            let sym = token.span;
            let sym = self.get_span_symbol(sym);
            return Ok(ASTExpr {
                kind: match token.kind {
                    TokenKind::Ident => ASTExprKind::Ident(sym),
                    TokenKind::IntegerLiteral => ASTExprKind::Integer(sym),
                    _ => unreachable!()
                },
                span: self.mk_span(start)
            });
        }
        if self.check(TokenKind::LParen) {
            self.bump();
            let expr = self.parse_expr()?;
            self.expect(TokenKind::RParent)?;
            return Ok(expr);
        }
        if self.check(TokenKind::BooleanTrue) {
            self.bump();
            return Ok(ASTExpr {
                kind: ASTExprKind::Boolean(true),
                span: self.mk_span(start)
            });
        }
        if self.check(TokenKind::BooleanFalse) {
            self.bump();
            return Ok(ASTExpr {
                kind: ASTExprKind::Boolean(false),
                span: self.mk_span(start)
            });
        }

        Err(self.err_unexpected())
    }

    pub(in super) fn parse_expr_access_and_call(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let mut lhs = self.parse_expr_primary()?;
        while self.check(TokenKind::Dot) |
            self.check(TokenKind::LSBracket) |
            self.check(TokenKind::LParen) {
            let kind = self.bump().kind;
            match kind {
                TokenKind::Dot => {
                    let ident = self.expect(TokenKind::Ident)?.span;
                    let ident = self.get_span_symbol(ident);
                    lhs = ASTExpr {
                        kind: ASTExprKind::MemberAccess(Box::new(lhs), ident),
                        span: self.mk_span(start)
                    };
                }
                TokenKind::LSBracket => {
                    let rhs = self.parse_expr()?;
                    self.expect(TokenKind::RSBracket)?;
                    lhs = ASTExpr {
                        kind: ASTExprKind::IndexAccess(Box::new(lhs), Box::new(rhs)),
                        span: self.mk_span(start)
                    };
                }
                TokenKind::LParen => {
                    let mut args = vec![];
                    if self.check(TokenKind::RParent) {
                        self.bump();
                    }
                    else {
                        args.push(self.parse_expr()?);
                        while self.check(TokenKind::Comma) {
                            self.bump();
                            args.push(self.parse_expr()?);
                        }
                        self.expect(TokenKind::RParent)?;
                    }
                    lhs = ASTExpr {
                        kind: ASTExprKind::Call(Box::new(lhs), args),
                        span: self.mk_span(start)
                    };
                }
                _ => unreachable!()
            }
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_exp(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_access_and_call()?;
        if self.check(TokenKind::And) {
            self.bump();
            let rhs = self.parse_expr_exp()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    ASTOperator::Exp,
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_prefix(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        if self.check(TokenKind::Plus) |
            self.check(TokenKind::Minus) {
            let kind = self.bump().kind;
            let rhs = self.parse_expr_prefix()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::PreOp(
                    match kind {
                        TokenKind::Plus => ASTOperator::Plus,
                        TokenKind::Minus => ASTOperator::Minus,
                        _ => unreachable!()
                    },
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        self.parse_expr_exp()
    }

    pub(in super) fn parse_expr_product(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_prefix()?;
        if self.check(TokenKind::Star) |
            self.check(TokenKind::Slash) {
            let kind = self.bump().kind;
            let rhs = self.parse_expr_product()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    match kind {
                        TokenKind::Star => ASTOperator::Mul,
                        TokenKind::Slash => ASTOperator::Div,
                        _ => unreachable!()
                    },
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_sum(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_product()?;
        if self.check(TokenKind::Plus) |
            self.check(TokenKind::Minus) {
            let kind = self.bump().kind;
            let rhs = self.parse_expr_sum()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    match kind {
                        TokenKind::Plus => ASTOperator::Plus,
                        TokenKind::Minus => ASTOperator::Minus,
                        _ => unreachable!()
                    },
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_comp(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_sum()?;
        if self.check(TokenKind::LABracket) |
            self.check(TokenKind::RABracket) |
            self.check(TokenKind::LsEq) |
            self.check(TokenKind::GtEq) |
            self.check(TokenKind::BangEq) |
            self.check(TokenKind::EqEq) {
            let kind = self.bump().kind;
            let rhs = self.parse_expr_comp()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    match kind {
                        TokenKind::LABracket => ASTOperator::Ls,
                        TokenKind::RABracket => ASTOperator::Gt,
                        TokenKind::LsEq => ASTOperator::LsOrEq,
                        TokenKind::GtEq => ASTOperator::GtOrEq,
                        TokenKind::BangEq => ASTOperator::NotEq,
                        TokenKind::EqEq => ASTOperator::Eq,
                        _ => unreachable!()
                    },
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_not(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        if self.check(TokenKind::Not) {
            self.bump();
            let rhs = self.parse_expr_not()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::PreOp(
                    ASTOperator::Not,
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        self.parse_expr_comp()
    }

    pub(in super) fn parse_expr_and(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_not()?;
        if self.check(TokenKind::And) {
            self.bump();
            let rhs = self.parse_expr_and()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    ASTOperator::And,
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_or(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_and()?;
        if self.check(TokenKind::Or) {
            self.bump();
            let rhs = self.parse_expr_or()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    ASTOperator::Or,
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr_assign(&mut self) -> PResult<ASTExpr> {
        let start = self.span_start();
        let lhs = self.parse_expr_or()?;
        if self.check(TokenKind::Eq) {
            self.bump();
            let rhs = self.parse_expr_assign()?;
            return Ok(ASTExpr {
                kind: ASTExprKind::BinOp(
                    Box::new(lhs),
                    ASTOperator::Assign,
                    Box::new(rhs)
                ),
                span: self.mk_span(start)
            })
        }
        Ok(lhs)
    }

    pub(in super) fn parse_expr(&mut self) -> PResult<ASTExpr> {
        self.parse_expr_assign()
    }
}