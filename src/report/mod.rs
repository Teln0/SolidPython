use crate::lexer::{Token, TokenKind};

pub struct ExpectedTokenKinds {
    pub token_kinds: Vec<TokenKind>,
    pub explanation: Option<String>
}

pub enum ParseErrorKind {
    UnexpectedToken {
        got_token: Token,
        expected: ExpectedTokenKinds
    }
}

pub struct ParseError {
    pub kind: ParseErrorKind
}

impl ParseError {
    pub fn string_rep(&self, src: &str) -> String {
        match &self.kind {
            ParseErrorKind::UnexpectedToken {
                got_token,
                expected
            } => {
                // TODO : indicate line number & print nearby lines
                let mut string_rep = format!("Unexpected token {}. Got : {}, expected : ", {
                    if got_token.span.0.0 == got_token.span.1.0 {
                        format!("at {}", got_token.span.0.0)
                    }
                    else {
                        format!("from {} to {}", got_token.span.0.0, got_token.span.1.0)
                    }
                }, got_token.string_rep(src));

                for token_kind in 0..expected.token_kinds.len() {
                    string_rep += &expected.token_kinds[token_kind].string_rep();
                    if token_kind != expected.token_kinds.len() - 1 {
                        string_rep += ", ";
                    }
                }
                if let Some(explanation) = &expected.explanation {
                    string_rep += " (";
                    string_rep += explanation;
                    string_rep += ")";
                }

                string_rep
            }
        }
    }
}