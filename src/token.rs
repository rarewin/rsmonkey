/// struct for tokens
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    /// illegal token
    Illegal,
    /// End of File
    EoF,

    /// identifier
    Ident(String),
    /// integer
    Int(i64),

    /// assign `=`
    Assign,
    /// plus `+`
    Plus,
    /// minus `-`
    Minus,
    /// bang `!`
    Bang,
    /// asterisk `*`
    Asterisk,
    /// slash `/`
    Slash,

    /// less than `<`
    Lt,
    /// greater than `>`
    Gt,

    /// equal `==`
    Eq,
    /// not equal `!=`
    NotEq,

    /// comma `,`
    Comma,
    /// semicolon `;`
    Semicolon,
    /// colon ':'
    Colon,

    /// left parenthesis `(`
    LParen,
    /// right parenthesis `)`
    RParen,
    /// left curly brace `{`
    LBrace,
    /// right curly brace `}`
    RBrace,
    /// left bracket
    LBracket,
    /// right bracket
    RBracket,

    /// function `fn`
    Function,
    /// let
    Let,
    /// true
    True,
    /// false
    False,
    /// if
    If,
    /// else
    Else,
    /// return
    Return,

    /// string
    StringToken(String),
}

impl Token {
    pub fn new(token_type: TokenType, literal_str: &str) -> Token {
        let literal = literal_str.to_string();
        match token_type {
            TokenType::Illegal => Token::Illegal,
            TokenType::EoF => Token::EoF,
            TokenType::Ident => Token::Ident(literal),
            TokenType::Int => Token::Int(literal.parse().unwrap()),
            TokenType::Assign => Token::Assign,
            TokenType::Plus => Token::Plus,
            TokenType::Minus => Token::Minus,
            TokenType::Bang => Token::Bang,
            TokenType::Asterisk => Token::Asterisk,
            TokenType::Slash => Token::Slash,
            TokenType::Lt => Token::Lt,
            TokenType::Gt => Token::Gt,
            TokenType::Eq => Token::Eq,
            TokenType::NotEq => Token::NotEq,
            TokenType::Comma => Token::Comma,
            TokenType::Semicolon => Token::Semicolon,
            TokenType::Colon => Token::Colon,
            TokenType::LParen => Token::LParen,
            TokenType::RParen => Token::RParen,
            TokenType::LBrace => Token::LBrace,
            TokenType::RBrace => Token::RBrace,
            TokenType::LBracket => Token::LBracket,
            TokenType::RBracket => Token::RBracket,
            TokenType::Function => Token::Function,
            TokenType::Let => Token::Let,
            TokenType::True => Token::True,
            TokenType::False => Token::False,
            TokenType::If => Token::If,
            TokenType::Else => Token::Else,
            TokenType::Return => Token::Return,
            TokenType::StringToken => Token::StringToken(literal),
        }
    }

    pub fn is_eof(&self) -> bool {
        *self == Token::EoF
    }

    pub fn get_literal(&self) -> String {
        match self {
            Token::Illegal => "Illegal".into(),
            Token::EoF => "EoF".into(),
            Token::Ident(ident) => ident.to_string(),
            Token::Int(int) => int.to_string(),
            Token::Assign => "=".into(),
            Token::Plus => "+".into(),
            Token::Minus => "-".into(),
            Token::Bang => "!".into(),
            Token::Asterisk => "*".into(),
            Token::Slash => "/".into(),
            Token::Lt => "<".into(),
            Token::Gt => ">".into(),
            Token::Eq => "==".into(),
            Token::NotEq => "!=".into(),
            Token::Comma => ",".into(),
            Token::Semicolon => ";".into(),
            Token::Colon => ":".into(),
            Token::LParen => "(".into(),
            Token::RParen => ")".into(),
            Token::LBrace => "{".into(),
            Token::RBrace => "}".into(),
            Token::LBracket => "[".into(),
            Token::RBracket => "]".into(),
            Token::Function => "fn".into(),
            Token::Let => "let".into(),
            Token::True => "true".into(),
            Token::False => "false".into(),
            Token::If => "if".into(),
            Token::Else => "else".into(),
            Token::Return => "return".into(),
            Token::StringToken(string) => string.to_string(),
        }
    }

    pub fn get_token_type(&self) -> TokenType {
        match self {
            Token::Illegal => TokenType::Illegal,
            Token::EoF => TokenType::EoF,
            Token::Ident(_) => TokenType::Ident,
            Token::Int(_) => TokenType::Int,
            Token::Assign => TokenType::Assign,
            Token::Plus => TokenType::Plus,
            Token::Minus => TokenType::Minus,
            Token::Bang => TokenType::Bang,
            Token::Asterisk => TokenType::Asterisk,
            Token::Slash => TokenType::Slash,
            Token::Lt => TokenType::Lt,
            Token::Gt => TokenType::Gt,
            Token::Eq => TokenType::Eq,
            Token::NotEq => TokenType::NotEq,
            Token::Comma => TokenType::Comma,
            Token::Semicolon => TokenType::Semicolon,
            Token::Colon => TokenType::Colon,
            Token::LParen => TokenType::LParen,
            Token::RParen => TokenType::RParen,
            Token::LBrace => TokenType::LBrace,
            Token::RBrace => TokenType::RBrace,
            Token::LBracket => TokenType::LBracket,
            Token::RBracket => TokenType::RBracket,
            Token::Function => TokenType::Function,
            Token::Let => TokenType::Let,
            Token::True => TokenType::True,
            Token::False => TokenType::False,
            Token::If => TokenType::If,
            Token::Else => TokenType::Else,
            Token::Return => TokenType::Return,
            Token::StringToken(_) => TokenType::StringToken,
        }
    }
}

/// Token type
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    /// illegal token
    Illegal,
    /// End of File
    EoF,

    /// identifier
    Ident,
    /// integer
    Int,

    /// assign `=`
    Assign,
    /// plus `+`
    Plus,
    /// minus `-`
    Minus,
    /// bang `!`
    Bang,
    /// asterisk `*`
    Asterisk,
    /// slash `/`
    Slash,

    /// less than `<`
    Lt,
    /// greater than `>`
    Gt,

    /// equal `==`
    Eq,
    /// not equal `!=`
    NotEq,

    /// comma `,`
    Comma,
    /// semicolon `;`
    Semicolon,
    /// colon ':'
    Colon,

    /// left parenthesis `(`
    LParen,
    /// right parenthesis `)`
    RParen,
    /// left curly brace `{`
    LBrace,
    /// right curly brace `}`
    RBrace,
    /// left bracket
    LBracket,
    /// right bracket
    RBracket,

    /// function `fn`
    Function,
    /// let
    Let,
    /// true
    True,
    /// false
    False,
    /// if
    If,
    /// else
    Else,
    /// return
    Return,

    /// string
    StringToken,
}
