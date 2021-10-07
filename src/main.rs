use solid_python::lexer::{lex, Token};
use solid_python::parser::{Parser};
use solid_python::span::SessionGlobals;
use solid_python::target::tree_walk_interpreter::TreeWalkInterpreter;
use solid_python::target::tree_walk_interpreter::val::{ptr, TWIVal, TWIValKind, FnTWIVal, TyTWIVal, mk_type, FnTWIValCode, mk_none};

fn main() {
    let src = "\
def fib(n: int) -> int:
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)

print(fib(0))
print(fib(1))
print(fib(2))
print(fib(3))
print(fib(4))
print(fib(5))
print(fib(6))
";

    SessionGlobals::new_set(src, || {
        let tokens = lex(src);
        let mut tokens: Vec<Token> = tokens.collect();
        // println!("{:#?}", tokens.iter().map(|t| &t.kind).collect::<Vec<&TokenKind>>());
        let mut parser = Parser::new(tokens.drain(..));
        match parser.parse_root() {
            Err(e) => println!("{}", e.string_rep(src)),
            Ok(t) => {
                let mut interpreter = TreeWalkInterpreter::new();
                SessionGlobals::with_interner_mut(|i| {
                    let none_type = interpreter.get_val_for_symbol(i.intern("none")).unwrap().clone();
                    interpreter.assign_new(i.intern("print"), ptr(TWIVal {
                        kind: TWIValKind::Fn(FnTWIVal {
                            params: vec![(i.intern("to_print"), mk_type(TyTWIVal::Any))],
                            return_type: none_type,
                            code: FnTWIValCode::Native(Box::new(|interpreter| {
                                SessionGlobals::with_interner(|i| {
                                    let val = interpreter.get_val_for_symbol(i.get_sym("to_print").unwrap());
                                    if let Some(val) = val {
                                        println!("{}", val.read().unwrap().string_rep());
                                    }
                                    else {
                                        println!("NO VALUE");
                                    }
                                    Ok(mk_none())
                                })
                            })),
                            name: i.intern("print")
                        })
                    }));
                });
                if let Err(e) = interpreter.interpret_block(&t) {
                    println!("{}", e.string_rep());
                }
            }
        }
    });
}
