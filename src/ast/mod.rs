use crate::span::{BytePos, Symbol};

#[derive(Debug)]
pub enum ASTStmtKind {
    FnDef(ASTFnDef),
    Return(ASTExpr),
    Expr(ASTExpr)
}

#[derive(Debug)]
pub struct ASTStmtBlock {
    pub stmts: Vec<ASTStmt>,
    pub span: (BytePos, BytePos)
}

#[derive(Debug)]
pub struct ASTStmt {
    pub kind: ASTStmtKind,
    pub span: (BytePos, BytePos)
}

#[derive(Debug)]
pub struct ASTFnDef {
    pub name: Symbol,
    pub params: Vec<(Symbol, ASTExpr)>,
    // TODO : pub generic_params: Vec<Symbol>,
    pub return_type: ASTExpr,
    pub block: ASTStmtBlock
}

#[derive(Debug)]
pub enum ASTOperator {
    Assign,
    Or,
    And,
    Not,
    Ls,
    Gt,
    LsOrEq,
    GtOrEq,
    NotEq,
    Plus,
    Minus,
    Eq,
    Mul,
    Div,
    Mod,
    EucDiv,
    Exp,
}

#[derive(Debug)]
pub enum ASTExprKind {
    Ident(Symbol),
    Integer(Symbol),
    BinOp(Box<ASTExpr>, ASTOperator, Box<ASTExpr>),
    PreOp(ASTOperator, Box<ASTExpr>),
    MemberAccess(Box<ASTExpr>, Symbol),
    IndexAccess(Box<ASTExpr>, Box<ASTExpr>),
    Call(Box<ASTExpr>, Vec<Box<ASTExpr>>)
}

#[derive(Debug)]
pub struct ASTExpr {
    pub kind: ASTExprKind,
    pub span: (BytePos, BytePos)
}