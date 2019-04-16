use crate::token::{Token, TokenType};

#[derive(Debug, Copy, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    /// create new Lexer
    ///
    /// # Argument
    ///
    /// * `input` - string to be tokenized
    ///
    /// # Return value
    ///
    /// crated Lexer
    ///
    /// # Example
    ///
    /// ```
    /// use rsmonkey::lexer::Lexer;
    ///
    /// let input = "let 1 + 1;";
    /// Lexer::new(input);
    /// ```
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();

        return l;
    }

    /// peek next character
    ///
    /// # Return value
    ///
    /// next character or '\0' if next character does not exist
    ///
    /// # Example
    ///
    /// ```
    /// use rsmonkey::lexer::Lexer;
    ///
    /// let input = "let 1 + 1;";
    /// let mut l = Lexer::new(input);
    ///
    /// assert_eq!(l.peek_char(), 'e');
    /// l.read_char();
    /// assert_eq!(l.peek_char(), 't');
    /// ```
    pub fn peek_char(&mut self) -> char {
        self.ch = match self.input.chars().nth(self.read_position) {
            Some(c) => c,
            _ => '\0',
        };
        self.ch
    }

    /// read next character
    pub fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    /// get next token
    ///
    /// # Return value
    ///
    /// next token
    ///
    /// # Example
    ///
    /// ```
    /// use rsmonkey::lexer::Lexer;
    /// use rsmonkey::token::TokenType;
    ///
    /// let input = "let 1 + 1;";
    /// let mut l = Lexer::new(input);
    ///
    /// assert_eq!(l.next_token().get_token_type(), TokenType::Let);
    /// assert_eq!(l.next_token().get_token_type(), TokenType::Int);
    /// assert_eq!(l.next_token().get_token_type(), TokenType::Plus);
    /// assert_eq!(l.next_token().get_token_type(), TokenType::Int);
    /// assert_eq!(l.next_token().get_token_type(), TokenType::Semicolon);
    /// ```
    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==")
                } else {
                    Token::new(TokenType::Assign, "=")
                }
            }
            '+' => Token::new(TokenType::Plus, "+"),
            '-' => Token::new(TokenType::Minus, "-"),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=")
                } else {
                    Token::new(TokenType::Bang, "!")
                }
            }
            '/' => Token::new(TokenType::Slash, "/"),
            '*' => Token::new(TokenType::Asterisk, "*"),
            '<' => Token::new(TokenType::LT, "<"),
            '>' => Token::new(TokenType::GT, ">"),
            '(' => Token::new(TokenType::LParen, "("),
            ')' => Token::new(TokenType::RParen, ")"),
            '{' => Token::new(TokenType::LBrace, "{"),
            '}' => Token::new(TokenType::RBrace, "}"),
            '[' => Token::new(TokenType::LBracket, "["),
            ']' => Token::new(TokenType::RBracket, "]"),
            ',' => Token::new(TokenType::Comma, ","),
            ';' => Token::new(TokenType::Semicolon, ";"),
            '\0' => Token::new(TokenType::EoF, "EOF"),
            '"' => {
                let p = self.read_position;
                self.read_char();
                while self.ch != '"' && self.read_position < self.input.len() {
                    self.read_char();
                }
                Token::new(TokenType::StringToken, &self.input[p..self.position])
            }
            'a'...'z' | 'A'...'Z' | '_' => {
                let p = self.position;
                while self.ch.is_ascii_alphabetic() || self.ch == '_' {
                    self.read_char();
                }
                let v = &self.input[p..self.position];
                return Token::new(lookup_ident(&v), v);
            }
            '0'...'9' => {
                let p = self.position;
                while self.ch.is_ascii_digit() {
                    self.read_char();
                }
                return Token::new(TokenType::Int, &self.input[p..self.position]);
            }

            _ => Token::new(TokenType::Illegal, "illegal"),
        };
        self.read_char();

        return token;
    }

    /// skip white space
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

/// lookup identifier
fn lookup_ident(s: &str) -> TokenType {
    match s {
        "let" => TokenType::Let,
        "fn" => TokenType::Function,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        "true" => TokenType::True,
        "false" => TokenType::False,
        _ => TokenType::Ident,
    }
}
