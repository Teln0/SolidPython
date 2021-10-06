use solid_python::lexer::{lex};
use solid_python::parser::{Parser, SessionGlobals};

fn main() {
    let src = "\
def a(a: b, c: d) -> a:
    def b() -> d:
        3 + (5 + 6) * 8 + 2
    return b
";

    SessionGlobals::new_set(src, || {
        let tokens = lex(src);
        let mut parser = Parser::new(tokens);
        match parser.parse_root() {
            Err(e) => println!("{}", e.string_rep(src)),
            Ok(t) => println!("{:#?}", t)
        }
    });
}
