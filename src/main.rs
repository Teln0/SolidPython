use solid_python::lexer::{lex, Token};
use solid_python::parser::{Parser, SessionGlobals};

fn main() {
    let src = "\
def fib(n: int) -> int:
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)
";

    SessionGlobals::new_set(src, || {
        let tokens = lex(src);
        let mut tokens: Vec<Token> = tokens.collect();
        // println!("{:#?}", tokens.iter().map(|t| &t.kind).collect::<Vec<&TokenKind>>());
        let mut parser = Parser::new(tokens.drain(..));
        match parser.parse_root() {
            Err(e) => println!("{}", e.string_rep(src)),
            Ok(_) => ()/*println!("{:#?}", t)*/
        }
    });
}
