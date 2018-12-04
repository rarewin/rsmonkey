/// struct for tokens
#[derive(Debug, PartialEq)]
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

    /// clone
    pub fn clone(&self) -> Token {
        Token::new(self.token_type, &self.literal)
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
    /// ```
    /// extern crate rsmonkey;
    /// use rsmonkey::token::{TokenType, Token};
    ///
    /// let l = Token::new(TokenType::Ident, "hoge");
    /// assert_eq!(l.token_literal(), "hoge");
    /// ```
    pub fn token_literal(&self) -> String {
        self.literal.to_string()
    }
}

/// Token type
#[derive(Debug, PartialEq, Clone, Copy)]
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
    LT,
    /// greater than `>`
    GT,

    /// equal `==`
    Eq,
    /// not equal `!=`
    NotEq,

    /// comma `,`
    Comma,
    /// semicolon `;`
    Semicolon,

    /// left parenthesis `(`
    LParen,
    /// right parenthesis `)`
    RParen,
    /// left curly brace `{`
    LBrace,
    /// right curly brace `}`
    RBrace,

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
}
