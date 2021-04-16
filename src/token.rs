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
    String(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.get_literal())
    }
}

impl Token {
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
            Token::String(string) => string.to_string(),
        }
    }
}
