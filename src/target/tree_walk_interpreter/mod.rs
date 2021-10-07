use crate::ast::{ASTStmtBlock, ASTStmt, ASTStmtKind, ASTExpr, ASTExprKind, ASTOperator};
use std::collections::HashMap;
use crate::span::{Symbol, SessionGlobals};
use crate::target::tree_walk_interpreter::val::{TWIValPtr, TWIVal, ptr, TWIValKind, TyTWIVal, FnTWIVal, IntTWIVal, BoolTWIVal, mk_int, FnTWIValCode};

pub mod val;

#[derive(Debug)]
pub enum TWIErrorKind {
    UndeclaredIdentifier(Symbol),
    ExprIsNotCallable,
    ParamLenMismatch,
    TypeMismatch,
    ExprIsNotATy
}

impl TWIErrorKind {
    pub fn string_rep(&self) -> String {
        match &self {
            TWIErrorKind::UndeclaredIdentifier(symbol) => SessionGlobals::with_interner(|interner| {
                format!("Undeclared identifier : {}", interner.get(symbol).unwrap_or("ERR"))
            }),
            TWIErrorKind::ExprIsNotCallable => "Expression is not callable".to_owned(),
            TWIErrorKind::ParamLenMismatch => "Parameter and argument lists aren't of the same size".to_owned(),
            TWIErrorKind::TypeMismatch => "Types don't match".to_owned(),
            TWIErrorKind::ExprIsNotATy => "Expression is not a type".to_owned()
        }
    }
}

pub struct TWIError {
    pub kind: TWIErrorKind,
}

impl TWIError {
    pub fn string_rep(&self) -> String {
        self.kind.string_rep()
    }
}

pub type TWIResult<T> = Result<T, TWIError>;

pub struct TWIExprEvaluation<'a> {
    evaluated: TWIValPtr<'a>
}

impl<'a> TWIExprEvaluation<'a> {
    pub fn from_expr(evaluated: TWIValPtr<'a>) -> Self {
        Self {
            evaluated
        }
    }
}

pub struct TWIStmtInterpretation<'a> {
    pub returning: bool,
    pub continuing: bool,
    pub breaking: bool,
    pub returned: Option<TWIValPtr<'a>>
}

impl<'a> TWIStmtInterpretation<'a> {
    pub fn blank() -> Self {
        Self {
            returning: false,
            continuing: false,
            breaking: false,
            returned: None
        }
    }
}

pub struct TWIStackFrame<'a> {
    pub decl: HashMap<Symbol, TWIValPtr<'a>>,
    pub name: Symbol
}

pub struct TreeWalkInterpreter<'a> {
    pub stack: Vec<TWIStackFrame<'a>>
}

macro_rules! stmt_propagate (
    ($eval: expr) => {{
        let eval = $eval;
        if eval.returning || eval.continuing || eval.breaking {
            return Ok(eval);
        }
        eval
    }}
);

impl<'a> TreeWalkInterpreter<'a> {
    pub fn new() -> Self {
        SessionGlobals::with_interner_mut(|i| {
            let mut decl = HashMap::new();
            decl.insert(i.intern("int"), ptr(TWIVal {
                kind: TWIValKind::Ty(TyTWIVal::Int)
            }));
            decl.insert(i.intern("bool"), ptr(TWIVal {
                kind: TWIValKind::Ty(TyTWIVal::Bool)
            }));
            decl.insert(i.intern("none"), ptr(TWIVal {
                kind: TWIValKind::Ty(TyTWIVal::None)
            }));
            Self {
                stack: vec![TWIStackFrame {
                    decl,
                    name: i.intern("GLOBAL")
                }]
            }
        })
    }

    pub fn get_val_for_symbol_mut(&mut self, symbol: Symbol) -> Option<&mut TWIValPtr<'a>> {
        for frame in self.stack.iter_mut().rev() {
            if let Some(ptr) = frame.decl.get_mut(&symbol) {
                return Some(ptr);
            }
        }
        None
    }

    pub fn get_val_for_symbol(&mut self, symbol: Symbol) -> Option<&TWIValPtr<'a>> {
        for frame in self.stack.iter().rev() {
            if let Some(ptr) = frame.decl.get(&symbol) {
                return Some(ptr);
            }
        }
        None
    }

    pub fn assign(&mut self, symbol: Symbol, value: TWIValPtr<'a>) {
        if let Some(assing_to) = self.get_val_for_symbol_mut(symbol.clone()) {
            *assing_to = value;
        }
        else {
            self.stack.last_mut().unwrap().decl.insert(symbol, value);
        }
    }

    pub fn assign_new(&mut self, symbol: Symbol, value: TWIValPtr<'a>) {
        self.stack.last_mut().unwrap().decl.insert(symbol, value);
    }

    fn check_assignable(&self, type_expr: &TWIVal, expr_to_assign: &TWIVal) -> TWIResult<()> {
        if type_expr.ty() != TyTWIVal::Ty {
            return Err(TWIError {
                kind: TWIErrorKind::ExprIsNotATy
            });
        }
        match &type_expr.kind {
            TWIValKind::Ty(ty) => {
                if ty == &TyTWIVal::Any {
                    return Ok(())
                }
                if &expr_to_assign.ty() != ty {
                    return Err(TWIError {
                        kind: TWIErrorKind::TypeMismatch
                    })
                }
            },
            _ => unreachable!()
        }

        Ok(())
    }

    fn check_equal(&self, lhs: &TWIVal<'a>, rhs: &TWIVal<'a>) -> bool {
        if lhs.ty() != rhs.ty() {
            return false;
        }
        match (&lhs.kind, &rhs.kind) {
            (TWIValKind::Ty(l), TWIValKind::Ty(r)) => l == r,
            (TWIValKind::Fn(_), TWIValKind::Fn(_)) => {
                let lhs = lhs as *const TWIVal;
                let rhs = rhs as *const TWIVal;
                lhs == rhs
            }
            (TWIValKind::Int(l), TWIValKind::Int(r)) => l.value == r.value,
            (TWIValKind::Bool(l), TWIValKind::Bool(r)) => l.value == r.value,
            (TWIValKind::None, _) => true,
            _ => unreachable!()
        }
    }

    fn call_fn_val(&mut self, fn_val: &FnTWIVal<'a>, args: &[TWIValPtr<'a>]) -> TWIResult<TWIExprEvaluation<'a>> {
        self.stack.push(TWIStackFrame {
            name: fn_val.name.clone(),
            decl: HashMap::new()
        });
        if fn_val.params.len() != args.len() {
            return Err(TWIError {
                kind: TWIErrorKind::ParamLenMismatch
            });
        }
        for i in 0..fn_val.params.len() {
            let param_ty = fn_val.params[i].1.read().unwrap();
            let arg = args[i].read().unwrap();
            self.check_assignable(&*param_ty, &*arg)?;
            self.assign_new(fn_val.params[i].0.clone(), args[i].clone());
        }

        let to_return = match &fn_val.code {
            FnTWIValCode::Native(native) => {
                Ok(TWIExprEvaluation {
                    evaluated: native(self)?
                })
            }
            FnTWIValCode::AST(ast_code) => {
                let interpreted = self.interpret_block(ast_code)?;
                Ok(TWIExprEvaluation {
                    evaluated: if let Some(returned) = interpreted.returned {
                        returned
                    }
                    else {
                        ptr(TWIVal {
                            kind: TWIValKind::None
                        })
                    }
                })
            }
        };

        self.stack.pop();

        to_return
    }

    fn eval_expr(&mut self, expr: &'a ASTExpr) -> TWIResult<TWIExprEvaluation<'a>> {
        match &expr.kind {
            ASTExprKind::Ident(ident) => {
                Ok(TWIExprEvaluation::from_expr(
                    if let Some(e) = self.get_val_for_symbol(ident.clone()) {
                        e.clone()
                    } else {
                        return Err(TWIError {
                            kind: TWIErrorKind::UndeclaredIdentifier(ident.clone())
                        });
                    }
                ))
            }
            ASTExprKind::Integer(integer) => {
                SessionGlobals::with_interner(|interner| {
                    Ok(TWIExprEvaluation::from_expr(ptr(TWIVal {
                        kind: TWIValKind::Int(IntTWIVal {
                            value: interner.get(integer).unwrap().parse().unwrap()
                        })
                    })))
                })
            }
            ASTExprKind::Boolean(_) => {
                todo!()
            }
            ASTExprKind::BinOp(lhs, op, rhs) => {
                match op {
                    ASTOperator::Eq => {
                        let lhs = self.eval_expr(lhs)?;
                        let lhs = lhs.evaluated.read().unwrap();
                        let rhs = self.eval_expr(rhs)?;
                        let rhs = rhs.evaluated.read().unwrap();
                        Ok(TWIExprEvaluation::from_expr(ptr(TWIVal {
                            kind: TWIValKind::Bool(BoolTWIVal {
                                value: self.check_equal(&*lhs, &*rhs)
                            })
                        })))
                    }
                    ASTOperator::Plus | ASTOperator::Minus | ASTOperator::Mul | ASTOperator::Div | ASTOperator::Exp | ASTOperator::Mod | ASTOperator::EucDiv => {
                        let lhs = self.eval_expr(lhs)?;
                        let lhs = lhs.evaluated.read().unwrap();
                        let rhs = self.eval_expr(rhs)?;
                        let rhs = rhs.evaluated.read().unwrap();
                        if lhs.ty() != rhs.ty() {
                            return Err(TWIError {
                                kind: TWIErrorKind::TypeMismatch
                            });
                        }
                        Ok(TWIExprEvaluation::from_expr(match (&lhs.kind, &rhs.kind) {
                            (TWIValKind::Int(IntTWIVal{ value: l }), TWIValKind::Int(IntTWIVal{ value: r })) => match op {
                                ASTOperator::Plus => mk_int(l + r),
                                ASTOperator::Minus => mk_int(l - r),
                                ASTOperator::Mul => mk_int(l * r),
                                ASTOperator::Div => mk_int(l / r),
                                ASTOperator::Exp => mk_int(l.pow(*r as u32)),
                                ASTOperator::Mod => mk_int(l + r),
                                ASTOperator::EucDiv => mk_int(l + r),
                                _ => unreachable!()
                            }
                            _ => todo!()
                        }))
                    }
                    _ => todo!()
                }
            }
            ASTExprKind::PreOp(_, _) => {
                todo!()
            }
            ASTExprKind::MemberAccess(_, _) => {
                todo!()
            }
            ASTExprKind::IndexAccess(_, _) => {
                todo!()
            }
            ASTExprKind::Call(expr, args) => {
                let evaluated_expr = self.eval_expr(expr)?.evaluated;
                let evaluated_expr = evaluated_expr.read().unwrap();
                let mut evaluated_args = Vec::with_capacity(args.len());
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?.evaluated);
                }
                if evaluated_expr.ty() != TyTWIVal::Fn {
                    return Err(TWIError {
                        kind: TWIErrorKind::ExprIsNotCallable
                    });
                }
                match &evaluated_expr.kind {
                    TWIValKind::Fn(fn_val) => self.call_fn_val(fn_val, &evaluated_args),
                    _ => unreachable!()
                }
            }
        }
    }

    fn interpret_statement(&mut self, stmt: &'a ASTStmt) -> TWIResult<TWIStmtInterpretation<'a>> {
        Ok(match &stmt.kind {
            ASTStmtKind::FnDef(fn_def) => {
                let val = ptr(TWIVal::from_fn_def(fn_def, self)?);
                self.assign_new(fn_def.name.clone(), val);
                TWIStmtInterpretation::blank()
            }
            ASTStmtKind::Assert(_) => {
                todo!()
            }
            ASTStmtKind::If(if_stmt) => {
                for condition_block in &if_stmt.conditions_blocks {
                    let evaluated_expr = self.eval_expr(&condition_block.0)?;
                    match &evaluated_expr.evaluated.read().unwrap().kind {
                        TWIValKind::Bool(bool) => {
                            if bool.value {
                                stmt_propagate!(self.interpret_block(&condition_block.1)?);
                                return Ok(TWIStmtInterpretation::blank());
                            }
                        }
                        _ => return Err(TWIError {
                            kind: TWIErrorKind::TypeMismatch
                        })
                    };
                }
                if let Some(else_block) = &if_stmt.else_block {
                    stmt_propagate!(self.interpret_block(else_block)?);
                }
                return Ok(TWIStmtInterpretation::blank());
            }
            ASTStmtKind::While(_) => {
                todo!()
            }
            ASTStmtKind::For(_) => {
                todo!()
            }
            ASTStmtKind::Break => {
                todo!()
            }
            ASTStmtKind::Continue => {
                todo!()
            }
            ASTStmtKind::Return(expr) => {
                TWIStmtInterpretation {
                    returning: true,
                    continuing: false,
                    breaking: false,
                    returned: Some(self.eval_expr(expr)?.evaluated)
                }
            }
            ASTStmtKind::Expr(expr) => {
                self.eval_expr(expr)?;
                TWIStmtInterpretation::blank()
            }
        })
    }

    pub fn interpret_block(&mut self, block: &'a ASTStmtBlock) -> TWIResult<TWIStmtInterpretation<'a>> {
        for stmt in &block.stmts {
            stmt_propagate!(self.interpret_statement(stmt)?);
        }

        Ok(TWIStmtInterpretation {
            returning: false,
            continuing: false,
            breaking: false,
            returned: None
        })
    }
}