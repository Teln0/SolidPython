use std::str::Chars;

#[repr(transparent)]
#[derive(Debug)]
pub struct BytePos(usize);

#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
    Ident,
    IntegerLiteral,
    BooleanTrue,
    BooleanFalse,

    Indentation,

    // =
    Eq,
    // == != < > <= >=
    EqEq,
    BangEq,
    LABracket,
    RABracket,
    LsEq,
    GtEq,
    // [ ] { } ( )
    LSBracket,
    RSBracket,
    LCBracket,
    RCBracket,
    LParen,
    RParent,
    // + - * /
    Plus,
    Minus,
    Star,
    Slash,
    // : ; ,
    Colon,
    Semi,
    Comma,

    // def class assert if while for
    KwDef,
    KwClass,

    KwAssert,
    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwBreak,
    KwContinue,
    KwReturn,

    Error,
    Eof,
    Whitespace
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: (BytePos, BytePos)
}

struct ThinToken {
    kind: TokenKind,
    len: usize
}

const EOF_CHAR: char = '\0';

struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>
}

impl<'a> Cursor<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            initial_len: input.len(),
            chars: input.chars()
        }
    }

    fn consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }

    fn bump(&mut self) -> char {
        self.chars.next().unwrap_or(EOF_CHAR)
    }

    fn nth(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or(EOF_CHAR)
    }
}

fn is_valid_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_valid_ident_first(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn first_token(input: &str) -> ThinToken {
    let mut cursor = Cursor::new(input);

    let kind = match cursor.bump() {
        '=' => {
            match cursor.nth(0) {
                '=' => {
                    cursor.bump();
                    TokenKind::EqEq
                }
                _ => TokenKind::Eq
            }
        }
        '!' => {
            match cursor.nth(0) {
                '=' => {
                    cursor.bump();
                    TokenKind::BangEq
                }
                _ => {
                    cursor.bump();
                    TokenKind::Error
                }
            }
        }
        '<' => {
            match cursor.nth(0) {
                '=' => {
                    cursor.bump();
                    TokenKind::LsEq
                }
                _ => TokenKind::LABracket
            }
        }
        '>' => {
            match cursor.nth(0) {
                '=' => {
                    cursor.bump();
                    TokenKind::GtEq
                }
                _ => TokenKind::RABracket
            }
        }
        '[' => TokenKind::LSBracket,
        ']' => TokenKind::RSBracket,
        '{' => TokenKind::LCBracket,
        '}' => TokenKind::RCBracket,
        '(' => TokenKind::LParen,
        ')' => TokenKind::RParent,
        '+' => TokenKind::Plus,
        '-' => TokenKind::Minus,
        '*' => TokenKind::Star,
        '/' => TokenKind::Slash,
        ':' => TokenKind::Colon,
        ';' => TokenKind::Semi,
        ',' => TokenKind::Comma,
        ' ' => {
            while cursor.nth(0) == ' ' {
                cursor.bump();
            }
            TokenKind::Whitespace
        }
        '\n' => {
            while {
                let char = cursor.nth(0);
                char == '\n' || char == ' '
            } {
                cursor.bump();
            }
            TokenKind::Indentation
        }
        c if is_valid_ident_first(c) => {
            while is_valid_ident(cursor.nth(0)) {
                cursor.bump();
            }
            let string = &input[..cursor.consumed()];
            match string {
                "True" => TokenKind::BooleanTrue,
                "False" => TokenKind::BooleanFalse,
                "def" => TokenKind::KwDef,
                "class" => TokenKind::KwClass,
                "assert" => TokenKind::KwAssert,
                "if" => TokenKind::KwIf,
                "else" => TokenKind::KwElse,
                "while" => TokenKind::KwWhile,
                "for" => TokenKind::KwFor,
                "break" => TokenKind::KwBreak,
                "continue" => TokenKind::KwContinue,
                "return" => TokenKind::KwReturn,
                _ => TokenKind::Ident
            }
        }
        _ => TokenKind::Error
    };

    ThinToken {
        kind,
        len: cursor.consumed()
    }
}

pub fn lex(src: &str) -> impl Iterator<Item = Token> + '_ {
    let mut src = src;
    let mut current_pos = 0;
    let mut sent_eof = false;
    std::iter::from_fn(move || {
        loop {
            if src.is_empty() {
                if !sent_eof {
                    sent_eof = true;
                    return Some(Token {
                        kind: TokenKind::Eof,
                        span: (BytePos(current_pos), BytePos(current_pos))
                    });
                }
                return None;
            }
            let token = first_token(src);

            src = &src[token.len..];
            let start_pos = current_pos;
            current_pos += token.len;

            if let TokenKind::Whitespace = token.kind {
                continue;
            }

            break Some(Token {
                kind: token.kind,
                span: (BytePos(start_pos), BytePos(current_pos))
            });
        }
    })
}