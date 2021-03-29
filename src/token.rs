/// struct for tokens
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    /// type of token
    token_type: TokenType,
    /// literal string
    literal: String,
}

impl Token {
    /// constructor for token
    ///
    /// # Arguments
    ///
    /// * `token_type` - type of token
    /// * `literal_str` - literal string
    ///
    /// # Return value
    ///
    /// return a new Token
    ///
    /// # Examples
    ///
    /// ```
    /// extern crate rsmonkey;
    /// use rsmonkey::token::{TokenType, Token};
    ///
    /// let token = Token::new(TokenType::Int, "4");
    /// let token = Token::new(TokenType::Ident, "hoge");
    /// ```
    pub fn new(token_type: TokenType, literal_str: &str) -> Token {
        let literal = literal_str.to_string();
        Token {
            token_type,
            literal,
        }
    }

    /// check if EoF or not
    ///
    /// # Return value
    ///
    /// return true if Token is EoF, otherwise false
    ///
    /// # Examples
    ///
    /// ```
    /// extern crate rsmonkey;
    /// use rsmonkey::token::{TokenType, Token};
    ///
    /// assert!(Token::new(TokenType::EoF, "\0").is_eof());
    /// assert!(!Token::new(TokenType::Int, "23").is_eof());
    /// ```
    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::EoF
    }

    /// get token literal string
    ///
    /// # Return value
    ///
    /// return literal string of Token.
    ///
    /// # Example
    ///
    /// ```
    /// extern crate rsmonkey;
    /// use rsmonkey::token::{TokenType, Token};
    ///
    /// let l = Token::new(TokenType::Ident, "hoge");
    /// assert_eq!(l.get_literal(), "hoge");
    ///
    /// let n = Token::new(TokenType::Int, "100");
    /// assert_eq!(n.get_literal(), "100");
    /// ```
    pub fn get_literal(&self) -> String {
        self.literal.clone()
    }

    /// get type of token
    ///
    /// # Return value
    ///
    /// token type of Token.
    ///
    /// # Example
    ///
    /// ```
    /// use rsmonkey::token::{TokenType, Token};
    ///
    /// let l = Token::new(TokenType::Ident, "hoge");
    /// assert_eq!(l.get_token_type(), TokenType::Ident);
    /// ```
    pub fn get_token_type(&self) -> TokenType {
        self.token_type
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
