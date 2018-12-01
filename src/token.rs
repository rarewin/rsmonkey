#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, l: &str) -> Token {
        let literal = l.to_string();
        Token {
            token_type,
            literal,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::EoF
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EoF,

    Ident,
    Int,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    Eq,
    NotEq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
