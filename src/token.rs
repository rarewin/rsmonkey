/// struct for tokens
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    /// illegal token
    Illegal(String),
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
    /// true or false
    Boolean(bool),
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
        write!(f, "{}", String::from(self),)
    }
}

impl From<Token> for String {
    fn from(t: Token) -> Self {
        String::from(&t)
    }
}

impl From<&Token> for String {
    fn from(t: &Token) -> Self {
        match t {
            Token::Illegal(s) => s.into(),
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
            Token::Boolean(b) => (if *b { "true" } else { "false" }).into(),
            Token::If => "if".into(),
            Token::Else => "else".into(),
            Token::Return => "return".into(),
            Token::String(string) => string.to_string(),
        }
    }
}
