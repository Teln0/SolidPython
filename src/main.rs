use solid_python::lexer::{lex, Token};

fn main() {
    let src = "\
def main(a: int, b: str):
    if str(a) == b:
        return True
    else:
        return False
";

    let tokens: Vec<Token> = lex(src).collect();

    println!("{:#?}", tokens);
}
