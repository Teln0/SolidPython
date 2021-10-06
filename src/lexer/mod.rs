use std::str::Chars;
use crate::span::{BytePos};

#[derive(Debug, Eq, PartialEq, Clone)]
#[repr(u8)]
pub enum TokenKind {
    Ident,
    IntegerLiteral,
    BooleanTrue,
    BooleanFalse,

    BlockStart,
    BlockEnd,

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
    // + - * ** / //
    Plus,
    Minus,
    Star,
    StarStar,
    Slash,
    SlashSlash,
    // and or not
    And,
    Or,
    Not,
    // : ; , . ->
    Colon,
    Semi,
    Comma,
    Dot,
    Arrow,

    // def class assert if while for break continue return
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
    // Used by the parser (and not by the lexer)
    PostEof,

    Whitespace,
    Indentation,
}

impl TokenKind {
    pub fn string_rep(&self) -> String {
        match self {
            TokenKind::Ident => "<Identifier>".to_owned(),
            TokenKind::IntegerLiteral => "<Integer Literal>".to_owned(),
            TokenKind::BooleanTrue | TokenKind::BooleanFalse => "<Boolean Literal>".to_owned(),
            TokenKind::BlockStart => "<Indentation Block Start>".to_owned(),
            TokenKind::BlockEnd => "<Indentation Block End>".to_owned(),
            TokenKind::Error => "<Unrecognized Token (lexer error)>".to_owned(),
            tk => format!("<{:?}>", tk)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: (BytePos, BytePos)
}

impl Token {
    pub fn string_rep(&self, src: &str) -> String {
        format!("{} {:?}", self.kind.string_rep(), &src[self.span.0.0..self.span.1.0])
    }
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

fn is_valid_whitespace(c: char) -> bool { c == ' ' }

fn indentation_len(src: &str, end_pos: BytePos) -> usize {
    let str = &src[..end_pos.0];
    let mut count = 0;
    let mut iter = str.chars().rev();
    while let Some(c) = iter.next() {
        if !is_valid_whitespace(c) {
            break;
        }
        count += 1;
    }

    count
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
        '-' =>
            match cursor.nth(0) {
                '>' => {
                    cursor.bump();
                    TokenKind::Arrow
                }
                _ => TokenKind::Minus
            },
        '*' =>
            match cursor.nth(0) {
                '*' => {
                    cursor.bump();
                    TokenKind::StarStar
                }
                _ => TokenKind::Star
            },
        '/' =>
            match cursor.nth(0) {
                '/' => {
                    cursor.bump();
                    TokenKind::SlashSlash
                }
                _ => TokenKind::Slash
            },
        ':' => TokenKind::Colon,
        ';' => TokenKind::Semi,
        ',' => TokenKind::Comma,
        '.' => TokenKind::Dot,
        c if is_valid_whitespace(c) => {
            while is_valid_whitespace(cursor.nth(0)) {
                cursor.bump();
            }
            TokenKind::Whitespace
        }
        '\n' => {
            while {
                let char = cursor.nth(0);
                char == '\n' || is_valid_whitespace(char)
            } {
                cursor.bump();
            }
            TokenKind::Indentation
        }
        c if c.is_ascii_digit() => {
            while cursor.nth(0).is_ascii_digit() {
                cursor.bump();
            }
            TokenKind::IntegerLiteral
        }
        c if is_valid_ident_first(c) => {
            while is_valid_ident(cursor.nth(0)) {
                cursor.bump();
            }
            let string = &input[..cursor.consumed()];
            match string {
                "and" => TokenKind::And,
                "or" => TokenKind::Or,
                "not" => TokenKind::Not,

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

pub fn lex(original_src: &str) -> impl Iterator<Item = Token> + '_ {
    let mut src = original_src;
    let mut current_pos = 0;
    enum LexerState {
        BlockStart,
        BlockEnd,
        EOF,
        Done
    }
    let mut state = LexerState::BlockStart;
    let mut indentations = vec![0usize];
    std::iter::from_fn(move || {
        if let LexerState::BlockStart = state {
            state = LexerState::BlockEnd;
            return Some(Token {
                kind: TokenKind::BlockStart,
                span: (BytePos(current_pos), BytePos(current_pos))
            })
        }
        loop {
            if src.is_empty() {
                return match state {
                    LexerState::BlockStart => unreachable!(),
                    LexerState::BlockEnd => {
                        state = LexerState::EOF;
                        Some(Token {
                            kind: TokenKind::BlockEnd,
                            span: (BytePos(current_pos), BytePos(current_pos))
                        })
                    }
                    LexerState::EOF => {
                        state = LexerState::Done;
                        Some(Token {
                            kind: TokenKind::Eof,
                            span: (BytePos(current_pos), BytePos(current_pos))
                        })
                    }
                    LexerState::Done => {
                        None
                    }
                };
            }
            let token = first_token(src);

            let prev_src = src;
            let prev_current_pos = current_pos;
            src = &src[token.len..];
            let start_pos = current_pos;
            current_pos += token.len;

            match token.kind {
                TokenKind::Whitespace => continue,
                TokenKind::Indentation => {
                    let len = indentation_len(original_src, BytePos(current_pos));
                    if len > indentations[indentations.len() - 1] {
                        indentations.push(len);
                        break Some(Token {
                            kind: TokenKind::BlockStart,
                            span: (BytePos(start_pos), BytePos(current_pos))
                        })
                    }
                    if len < indentations[indentations.len() - 1] {
                        // We cancel all progress
                        src = prev_src;
                        current_pos = prev_current_pos;
                        indentations.pop();
                        break Some(Token {
                            kind: TokenKind::BlockEnd,
                            span: (BytePos(start_pos), BytePos(current_pos))
                        })
                    }
                    // Indentation didn't change : we just skip it
                    continue;
                }
                _ => {}
            }

            break Some(Token {
                kind: token.kind,
                span: (BytePos(start_pos), BytePos(current_pos))
            });
        }
    })
}