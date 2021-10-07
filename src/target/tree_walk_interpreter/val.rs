use std::sync::{Arc, RwLock};
use crate::span::{Symbol};
use crate::ast::{ASTStmtBlock, ASTFnDef};
use crate::target::tree_walk_interpreter::{TreeWalkInterpreter, TWIResult};

#[derive(Eq, PartialEq)]
pub enum TyTWIVal {
    Any,

    Ty,
    Fn,
    Int,
    Bool,
    None
    // TODO : Class
}

pub enum FnTWIValCode<'a> {
    Native(Box<dyn Fn(&mut TreeWalkInterpreter<'a>) -> TWIResult<TWIValPtr<'a>>>),
    AST(&'a ASTStmtBlock)
}

pub struct FnTWIVal<'a> {
    // TODO : Enclosed scope (for closures)
    pub params: Vec<(Symbol, TWIValPtr<'a>)>,
    pub return_type: TWIValPtr<'a>,
    pub code: FnTWIValCode<'a>,
    pub name: Symbol
}

pub struct IntTWIVal {
    pub value: i64
}

pub struct BoolTWIVal {
    pub value: bool
}

pub enum TWIValKind<'a> {
    Ty(TyTWIVal),
    Fn(FnTWIVal<'a>),
    Int(IntTWIVal),
    Bool(BoolTWIVal),
    None
}

impl<'a> TWIValKind<'a> {
    pub fn string_rep(&self) -> String {
        match self {
            TWIValKind::Ty(_) => todo!(),
            TWIValKind::Fn(_) => todo!(),
            TWIValKind::Int(value) => value.value.to_string(),
            TWIValKind::Bool(_) => todo!(),
            TWIValKind::None => todo!()
        }
    }
}

pub struct TWIVal<'a> {
    pub kind: TWIValKind<'a>
}

impl<'a> TWIVal<'a> {
    pub fn ty(&self) -> TyTWIVal {
        match self.kind {
            TWIValKind::Ty(_) => TyTWIVal::Ty,
            TWIValKind::Fn(_) => TyTWIVal::Fn,
            TWIValKind::Int(_) => TyTWIVal::Int,
            TWIValKind::Bool(_) => TyTWIVal::Bool,
            TWIValKind::None => TyTWIVal::None
        }
    }

    pub fn from_fn_def(fn_def: &'a ASTFnDef, interpreter: &mut TreeWalkInterpreter<'a>) -> TWIResult<Self> {
        let mut params = vec![];
        for param in &fn_def.params {
            let ty_expr = interpreter.eval_expr(&param.1)?;
            params.push((param.0.clone(), ty_expr.evaluated))
        }
        Ok(Self {
            kind: TWIValKind::Fn(FnTWIVal {
                params,
                return_type: interpreter.eval_expr(&fn_def.return_type)?.evaluated.clone(),
                code: FnTWIValCode::AST(&fn_def.block),
                name: fn_def.name.clone()
            })
        })
    }

    pub fn string_rep(&self) -> String {
        self.kind.string_rep()
    }
}

pub type TWIValPtr<'a> = Arc<RwLock<TWIVal<'a>>>;

pub fn ptr(v: TWIVal) -> TWIValPtr {
    Arc::new(RwLock::new(v))
}

pub fn mk_int<'a>(value: i64) -> TWIValPtr<'a> {
    ptr(TWIVal {
        kind: TWIValKind::Int(IntTWIVal {
            value
        })
    })
}

pub fn mk_none<'a>() -> TWIValPtr<'a> {
    ptr(TWIVal { kind: TWIValKind::None })
}

pub fn mk_type<'a>(value: TyTWIVal) -> TWIValPtr<'a> {
    ptr(TWIVal {
        kind: TWIValKind::Ty(value)
    })
}